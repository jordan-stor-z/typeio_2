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
import Data.List (isInfixOf)
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
          -- ...and no circles left behind. `circle` is the D3 path's
          -- shape; #182 removes it from the app, but until then the
          -- two must not turn up in one drawing.
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

        it "asks the refresh endpoint to re-wrap labels to the box" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          body <- serverGraphBody pool (fromSqlKey projectKey)

          -- A title re-wrapped after an edit must wrap to the width it
          -- was first drawn at. One endpoint serves both graphs, so
          -- this flag is what tells it which shape is asking -- and it
          -- has to precede `clientTitle`, which goes in unescaped and
          -- would otherwise swallow it.
          body `shouldContainStr` "&amp;layout=server&amp;clientTitle="

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
          -- The viewport is hand-rolled precisely so this page stops
          -- needing D3 (#182).
          body `shouldNotContainStr` "d3"

      describe "without the layout flag" $
        it "still renders the D3 path" $ \pool -> do
          (projectKey, _) <- seedProjectWithRootNode pool

          body <- graphBody pool (fromSqlKey projectKey) []

          -- #178 touches styling both paths share, so this pins the
          -- unflagged path as still being the D3 one until #181 cuts
          -- over deliberately.
          body `shouldContainStr` "<circle"
          body `shouldContainStr` "nodetree2.js"
          body `shouldNotContainStr` "<rect"

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
