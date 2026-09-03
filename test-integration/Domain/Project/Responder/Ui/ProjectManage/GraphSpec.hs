{-# LANGUAGE OverloadedStrings #-}

{- | Integration coverage for the node chrome the server-computed graph
renders (#178).

These assertions are deliberately about markup rather than geometry.
The layout engine's own output is unit-tested
(@test\/Domain\/Project\/Graph\/@); what has no coverage below that
line is the contract between the rendered SVG and everything bound to
it -- @manage-project.css@ styles nodes off the @root@\/@work@ class,
the per-node refresh hook swaps into @#node-text-\<id\>@, and
@e2e\/tests\/graph.spec.ts@ finds nodes by @#node-\<id\>@. Each of
those is a string in one file matching a string in another: exactly
the pairing a compiler cannot check and a rename silently breaks.

See @docs\/architecture\/graph-rendering.md@ ("The DOM contract").
-}
module Domain.Project.Responder.Ui.ProjectManage.GraphSpec (spec) where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf)
import Database.Persist.Sql (ConnectionPool, fromSqlKey)
import Domain.Project.Responder.Ui.ProjectManage.Graph (handleProjectGraph)
import Integration.Support
  ( resetBetweenTests
  , seedDependency
  , seedProjectWithRootNode
  , seedWorkNode
  , withTestDatabase
  )
import Network.HTTP.Types (Query, methodGet)
import Network.Wai (Request, defaultRequest, queryString, requestMethod)
import Network.Wai.Test
  ( SResponse (..)
  , assertStatus
  , request
  , runSession
  )
import Test.Hspec
import Text.Read (readMaybe)

spec :: Spec
spec = aroundAll withTestDatabase $
  beforeWith resetBetweenTests $
    describe "handleProjectGraph (integration)" $ do
      describe "server-computed layout" $ do
        it "draws each node as a rounded rect classed by its kind" $ \pool -> do
          (projectKey, rootKey) <- seedProjectWithRootNode pool
          workKey <- seedWorkNode pool projectKey "Build the thing"
          seedDependency pool rootKey workKey

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- The root and the work node come off the same template and
          -- are told apart only by this class, which is what
          -- `#tree-container .node .root` / `.work` colour, hover and
          -- glow.
          body `shouldContainStr` "<rect class=\"root\""
          body `shouldContainStr` "<rect class=\"work\""
          -- Rounded, per the reference images' shape.
          body `shouldContainStr` "rx=\"6\""
          -- ...and no circles left behind. That was the old renderer's
          -- shape, removed outright in #182.
          body `shouldNotContainStr` "<circle"

        it "leaves the node's fill and stroke to the stylesheet" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- A `stroke` presentation attribute on the rect would still
          -- render, but it splits the node's appearance across two
          -- files and silently drops out of any theme change. The
          -- class is the whole styling surface.
          body `shouldNotContainStr` "<rect class=\"root\" stroke"
          body `shouldNotContainStr` "fill=\"white\""

        it "gives every node label its own id, matching the refresh hook" $ \pool -> do
          (projectKey, rootKey) <- seedProjectWithRootNode pool
          workKey <- seedWorkNode pool projectKey "Build the thing"
          seedDependency pool rootKey workKey

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- Per-node, not a constant: this element used to be emitted
          -- as a fixed `node-label` on every node, which both repeated
          -- one id throughout the document and left the hook below
          -- aimed at a target that did not exist.
          mapM_
            (shouldContainStr body . nodeTextId)
            [fromSqlKey rootKey, fromSqlKey workKey]
          body `shouldNotContainStr` "id=\"node-label\""

          -- The hook and its target have to name the same element.
          -- They sit ~40 lines apart in one module and nothing else
          -- checks that they still agree.
          mapM_
            (shouldContainStr body . nodeTextTarget)
            [fromSqlKey rootKey, fromSqlKey workKey]

        it "points the refresh hook at the refresh endpoint" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- The `layout=server` half of this link went with the flag in
          -- #181: one renderer, so one wrap width, so nothing left to
          -- tell the endpoint apart from the node itself.
          body `shouldContainStr` "/ui/project/node/refresh?nodeId="
          body `shouldNotContainStr` "layout=server"

      describe "viewport (#179)" $ do
        it "emits the natural size the client zooms in multiples of" $ \pool -> do
          (projectKey, rootKey) <- seedProjectWithRootNode pool
          workKey <- seedWorkNode pool projectKey "Build the thing"
          seedDependency pool rootKey workKey

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- The client rewrites width/height as it zooms, so it can't
          -- read the natural size back off them afterwards -- it has to
          -- be recorded separately or zooming accumulates drift.
          body `shouldContainStr` "data-base-width="
          body `shouldContainStr` "data-base-height="

        it "emits where the project root landed, as a scroll offset" $ \pool -> do
          (projectKey, rootKey) <- seedProjectWithRootNode pool
          workKey <- seedWorkNode pool projectKey "Build the thing"
          seedDependency pool rootKey workKey

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- The server placed the root, so the client never searches
          -- the DOM for it. Emitted relative to the drawing's top-left
          -- so it drops straight into `scrollLeft`/`scrollTop`.
          body `shouldContainStr` "data-root-x="
          body `shouldContainStr` "data-root-y="

        it "ships the controls and the viewport script with the graph" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- All three are inside the swapped fragment: #tree-container
          -- is replaced wholesale on every graph load, so anything
          -- bound to the drawing has to arrive with it.
          body `shouldContainStr` "id=\"graph-zoom-in\""
          body `shouldContainStr` "id=\"graph-zoom-out\""
          body `shouldContainStr` "id=\"graph-zoom-reset\""
          body `shouldContainStr` "/static/script/graph-viewport.js"
          -- The viewport is hand-rolled precisely so this page needs
          -- no layout library. #182 deleted the last one; this keeps it
          -- from creeping back in unnoticed.
          body `shouldNotContainStr` "d3"

      describe "containment (#198)" $ do
        it "draws the project root above its work" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool
          _ <- seedWorkNode pool projectKey "Build the thing"
          _ <- seedWorkNode pool projectKey "Build another thing"

          body <- graphBody pool (fromSqlKey projectKey) []

          -- The root's box must have the smallest y of any node. This
          -- is the assertion that would have caught #198 for eight
          -- issues: the layout engine was right, but membership was
          -- being handed to it as a dependency, so the root sank below
          -- everything in the project.
          let rootTops = nodeTops "root" body
              workTops = nodeTops "work" body
          length rootTops `shouldBe` 1
          length workTops `shouldBe` 2
          all (> maximum rootTops) workTops `shouldBe` True

        it "derives containment rather than reading a dependency row" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool
          _ <- seedWorkNode pool projectKey "Build the thing"

          body <- graphBody pool (fromSqlKey projectKey) []

          -- No dependency rows are seeded here at all, yet the graph
          -- still connects the root to its work: the edge comes from
          -- `node.project_id`, not from `project.dependency`.
          body `shouldContainStr` "class=\"link link-contains\""

        it "leaves containment edges without an arrowhead" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool
          _ <- seedWorkNode pool projectKey "Build the thing"

          body <- graphBody pool (fromSqlKey projectKey) []

          -- An arrow means "this must finish first". The root is not
          -- waiting on its own children, and drawing one is what made
          -- the graph read as a dependency in the first place.
          --
          -- This project has containment edges and nothing else, so no
          -- `marker-end` should appear on any path at all.
          body `shouldContainStr` "link-contains"
          body `shouldNotContainStr` "marker-end"

      describe "the cutover (#181)" $ do
        it "serves the computed layout with no query parameter" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          -- No flag. This is the assertion the whole effort was for.
          body <- graphBody pool (fromSqlKey projectKey) []

          body `shouldContainStr` "<rect class=\"root\""
          body `shouldContainStr` "/static/script/graph-viewport.js"

        it "leaves no trace of the client-rendered path behind" $ \pool -> do
          (projectKey, rootKey) <- seedProjectWithRootNode pool
          workKey <- seedWorkNode pool projectKey "Build the thing"
          seedDependency pool rootKey workKey

          body <- graphBody pool (fromSqlKey projectKey) []

          -- The graph no longer leaves the server as data at all: it
          -- leaves as finished SVG, so there is nothing for a client
          -- layout script to read.
          body `shouldNotContainStr` "graph-data"
          body `shouldNotContainStr` "nodetree"
          body `shouldNotContainStr` "<circle"
          body `shouldNotContainStr` "zoom-group"

        it "ignores a leftover ?layout=server rather than branching on it" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          -- A bookmarked URL from while the flag existed must not select
          -- some other renderer, because there isn't one -- the
          -- parameter is now just an unread query string.
          flagged <- serverGraphBody pool (fromSqlKey projectKey)
          plain <- graphBody pool (fromSqlKey projectKey) []
          flagged `shouldBe` plain

-- | GET the graph view with @?layout=server@.
serverGraphBody :: ConnectionPool -> Int64 -> IO String
serverGraphBody pool pid =
  graphBody pool pid [("layout", Just "server")]

-- | GET the graph view and hand its body back as a searchable 'String'.
graphBody :: ConnectionPool -> Int64 -> Query -> IO String
graphBody pool pid extraQuery =
  runSession
    ( do
        resp <- request (graphRequest pid extraQuery)
        assertStatus 200 resp
        pure . LC8.unpack . simpleBody $ resp
    )
    (handleProjectGraph pool)

graphRequest :: Int64 -> Query -> Request
graphRequest pid extraQuery =
  defaultRequest
    { requestMethod = methodGet
    , queryString =
        ("projectId", Just . C8.pack . show $ pid) : extraQuery
    }

shouldContainStr :: String -> String -> Expectation
shouldContainStr haystack needle =
  (needle `isInfixOf` haystack)
    `shouldSatisfyWith` ("expected the rendered graph to contain " <> show needle)

shouldNotContainStr :: String -> String -> Expectation
shouldNotContainStr haystack needle =
  not (needle `isInfixOf` haystack)
    `shouldSatisfyWith` ("expected the rendered graph not to contain " <> show needle)

{- | A plain 'shouldBe' on a 'Bool' reports "False /= True", which says
nothing about which string was missing; this keeps the needle in the
failure message.
-}
shouldSatisfyWith :: Bool -> String -> Expectation
shouldSatisfyWith True _ = pure ()
shouldSatisfyWith False msg = expectationFailure msg

nodeTextId :: Int64 -> String
nodeTextId nid = "id=\"node-text-" <> show nid <> "\""

nodeTextTarget :: Int64 -> String
nodeTextTarget nid = "hx-target=\"#node-text-" <> show nid <> "\""

{- | The @y@ of every node group whose rect carries the given kind
class, read straight out of the rendered SVG.

Deliberately parsing the markup rather than calling @layout@ directly:
the layout engine's own placement is unit-tested, and what #198 broke
was the /responder's/ conversion — which relationship it handed the
engine. That only shows up in the finished document.

Each node renders as
@\<g id="node-N" class="node" transform="translate(X,Y)"\>\<rect class="KIND"@,
so splitting on the group and reading forward is enough; no HTML parser
required for a shape this fixed.
-}
nodeTops :: String -> String -> [Double]
nodeTops kind body =
  [ y
  | chunk <- drop 1 (splitOn "<g id=\"node-" body)
  , ("class=\"" <> kind <> "\"") `isInfixOf` takeWhile (/= '>') (dropToRect chunk)
  , Just y <- [translateY chunk]
  ]
  where
    dropToRect = afterFirst "<rect "
    translateY chunk = case afterFirst "transform=\"translate(" chunk of
      "" -> Nothing
      rest -> case break (== ',') (takeWhile (/= ')') rest) of
        (_, ',' : ys) -> readMaybe ys
        _ -> Nothing

-- | Everything after the first occurrence of @needle@, or @""@.
afterFirst :: String -> String -> String
afterFirst needle hay = case splitOn needle hay of
  (_ : rest : _) -> rest
  _ -> ""

splitOn :: String -> String -> [String]
splitOn needle = go
  where
    go hay
      | null hay = [""]
      | needle `isPrefixOf` hay = "" : go (drop (length needle) hay)
      | otherwise = case go (drop 1 hay) of
          (c : cs) -> (head hay : c) : cs
          [] -> [[head hay]]
