{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Domain.Project.Responder.Ui.ProjectManage.Graph where

import Common.Validation
  ( ValidationErr
  , isNotEmpty
  , isThere
  , runValidation
  , valRead
  , (.$)
  )
import Common.Web.Attributes
import Common.Web.Elements
import Common.Web.Query (lookupVal)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither, runEitherT)
import Data.Aeson (encode, object, (.=))
import Data.Bifunctor (first)
import Data.Either (notNullEither)
import Data.Int (Int64)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Util (intToText, wrapLabel)
import Database.Esqueleto.Experimental
  ( from
  , fromSqlKey
  , in_
  , select
  , table
  , toSqlKey
  , val
  , valList
  , where_
  , (==.)
  )
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Domain.Project.Graph.Layout (layout)
import Domain.Project.Graph.Types
  ( Bounds (..)
  , Diagram (..)
  , EdgeId (..)
  , EdgeKind (..)
  , LayoutConfig (..)
  , LayoutEdge (..)
  , LayoutNode (..)
  , NodeId (..)
  , NodeKind (..)
  , PlacedEdge (..)
  , PlacedNode (..)
  , Point (..)
  , Size (..)
  , boundsSize
  , contains
  , defaultLayoutConfig
  , dependsOn
  )
import qualified Domain.Project.Model as M
  ( Dependency (..)
  , Node (..)
  , Project (..)
  , unNodeTypeKey
  )
import Domain.Project.Responder.Ui.ProjectManage.Link
import Lucid
import Network.HTTP.Types (queryToQueryText, status200, status403)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai
  ( Application
  , Request (queryString)
  , responseLBS
  )

data GetGraphError
  = InvalidParams [ValidationErr]
  | MissingNodes

{- | A node, as much of it as the graph's own rendering needs.

What used to be here alongside this was a @Graph@ record with @ToJSON@
instances, serialised into a @#graph-data@ script tag for the browser
to lay out. The server computes the layout now (#181), so the graph
never leaves the server as data — it leaves as finished SVG.
-}
data GraphNode = GraphNode
  { graphNodeId :: Int64
  , label :: Text
  , nodeType :: Text
  , projectId :: Int64
  }

handleProjectGraph :: ConnectionPool -> Application
handleProjectGraph pl req respond = do
  rslt <- flip runSqlPool pl . runEitherT $ do
    pid <-
      hoistEither
        . first InvalidParams
        . validateProjectId
        $ qt
    ns <-
      lift (queryNodes pid)
        >>= hoistEither
          . notNullEither MissingNodes
    ds <-
      lift
        . queryDependencies
        . fmap (fromSqlKey . entityKey)
        $ ns
    pure (pid, ns, ds)
  case rslt of
    Left (InvalidParams es) -> respondValErrs es
    Left MissingNodes -> respondMissingNodes
    Right (pid, ns, ds) ->
      respondSuccess . templateServerGraph $ toServerGraph pid ns ds
  where
    respondMissingNodes =
      respond
        . responseLBS
          status403
          [("Content-Type", "application/json")]
        . encode
        . object
        $ ["error" .= ("No nodes found for the project" :: Text)]
    respondValErrs es =
      respond
        . responseLBS
          status403
          [("Content-Type", "application/json")]
        . encode
        . object
        $ ["error" .= (mconcat . map (pack . show) $ es)]
    respondSuccess =
      respond
        . responseLBS
          status200
          [("Content-Type", "text/html")]
        . renderBS
    qt =
      queryToQueryText
        . queryString
        $ req

pushUrl :: Int64 -> Int64 -> Text
pushUrl nid pid =
  "/ui/project/vw"
    <> "?projectId="
    <> (pack . show $ pid)
    <> "&nodeId="
    <> (pack . show $ nid)

queryNodes :: Int64 -> ReaderT SqlBackend IO [Entity M.Node]
queryNodes pid = do
  select $ do
    n <- from $ table @M.Node
    where_ (n.projectId ==. val pkey)
    pure n
  where
    pkey = toSqlKey @M.Project pid

queryDependencies :: [Int64] -> ReaderT SqlBackend IO [Entity M.Dependency]
queryDependencies [] = return []
queryDependencies nids = do
  select $ do
    d <- from $ table @M.Dependency
    where_ (d.nodeId `in_` valList nkeys)
    pure d
  where
    nkeys = toSqlKey @M.Node <$> nids

toGraphNode :: Entity M.Node -> GraphNode
toGraphNode (Entity k e) =
  GraphNode
    { graphNodeId = fromSqlKey k
    , projectId = fromSqlKey . M.nodeProjectId $ e
    , label = pack . M.nodeTitle $ e
    , nodeType = pack . M.unNodeTypeKey . M.nodeNodeTypeId $ e
    }

{- | A node's label, re-wrapped and laid out, for 'Node.Refresh' to swap
in after an edit.

Wrapping is to the layout engine's own box ('cfgLabelWidth' x
'cfgLabelLines') — the same dimensions the label was first drawn at, so
a title re-wraps to what it already fitted. Until #181 this took a
parameter saying which shape was asking, because the old renderer's
circle fitted fewer characters per line than the box does; with one
renderer left there is one answer.
-}
nodeContents :: GraphNode -> Html ()
nodeContents n = do
  labelTspans . label $ n
  g_
    [ class_ "hidden"
    , hxGet_ $
        nodeRefreshLink
          (graphNodeId n)
          (projectId n)
          (label n)
    , hxTrigger_ $
        "nodePanel:onEditClosed[event.detail.nodeId=="
          <> (intToText . graphNodeId $ n)
          <> "] from:#node-panel"
    , hxTarget_ $
        "#node-text-"
          <> (intToText . graphNodeId $ n)
    , hxSwap_ "innerHTML"
    , hxPushUrl_ False
    ]
    empty
  where
    empty = mempty :: Html ()

-- | Wrap a raw title to the node box, then lay the lines out.
labelTspans :: Text -> Html ()
labelTspans =
  tspanLines
    . wrapLabel
      (cfgLabelWidth defaultLayoutConfig)
      (cfgLabelLines defaultLayoutConfig)

-- SVG `<text>` has no wrapping of its own, so a multi-line label has to
-- be emitted as one `<tspan>` per line. Each line resets `x` to the
-- node's own origin (otherwise tspans just continue along the same
-- line) and steps `dy` by one line height, with the first line lifted
-- by half the block's height so the whole label stays vertically
-- centred on the node however many lines it wraps to.
--
-- `x="0"` means "the text origin", which 'nodeLabel' arranges to be the
-- centre of the node box by translating the `<text>` there. That is
-- what lets Node.Refresh return one of these fragments and have it land
-- correctly without knowing anything about where the node sits.
tspanLines :: [Text] -> Html ()
tspanLines ls =
  forM_ (zip [0 :: Int ..] ls) $ \(i, l) ->
    tspan_
      [ x_ "0"
      , dy_ $ if i == 0 then firstDy else lineHeight
      ]
      $ toHtml l
  where
    lineHeight = "1.1em"
    -- Half the block's height, which the first line is lifted by. Zero
    -- is spelt out rather than negated: a one-line label would
    -- otherwise render as `dy="-0.0em"`, which is the same offset but
    -- reads like a bug in the markup.
    blockLift = 1.1 * fromIntegral (length ls - 1) / 2 :: Double
    firstDy
      | blockLift == 0 = "0em"
      | otherwise = (<> "em") . pack . show . negate $ blockLift

validateProjectId :: QueryText -> Either [ValidationErr] Int64
validateProjectId qt = runValidation id $ do
  lookupVal "projectId" qt
    .$ unpack
    >>= isThere "Project id must be present"
    >>= isNotEmpty "Project id must have a value"
    >>= valRead "Project id must be valid integer"

-- ---------------------------------------------------------------------
-- Server-computed layout (#173-#181)
--
-- Everything below renders a Diagram that Domain.Project.Graph.Layout
-- has already placed. It was opt-in behind ?layout=server while it was
-- being built; #181 made it the only renderer and removed both the flag
-- and the client-rendered template it used to sit beside.
-- See docs/architecture/graph-rendering.md.
-- ---------------------------------------------------------------------

data ServerGraph = ServerGraph
  { sgProjectId :: Int64
  , sgLabels :: Map NodeId Text
  {- ^ Untruncated titles, which 'PlacedNode' deliberately doesn't
  carry (it holds the label already wrapped to the box). The
  per-node refresh hook needs the original.
  -}
  , sgDiagram :: Diagram
  }

toServerGraph ::
  Int64 ->
  [Entity M.Node] ->
  [Entity M.Dependency] ->
  ServerGraph
toServerGraph pid ns ds =
  ServerGraph
    { sgProjectId = pid
    , sgLabels = Map.fromList [(lnId n, lnLabel n) | n <- lns]
    , sgDiagram = layout defaultLayoutConfig lns les
    }
  where
    lns = map toLayoutNode ns
    les = containmentEdges lns <> map toLayoutEdge ds

{- | The project root holds its work, and that is what puts it at the
head of the graph (#198).

Membership is not stored as an edge at all — @project.node.project_id@
already records it, and every node here came back from a query on that
column. So the edges are /derived/ rather than read: one from the root
to each other node in the project.

It used to be stored, as a @project.dependency@ row per node pointing
at the root, which is why the root sank to the bottom of the drawing:
layering correctly put the dependent above its dependency, and every
work node was recorded as depending on the root. Migration 000009
removes those rows and @Api.Node.Post@ no longer writes them.

Ids are negative so they cannot collide with a real
@project.dependency@ id. Nothing persists them; they exist only for the
duration of one layout.
-}
containmentEdges :: [LayoutNode] -> [LayoutEdge]
containmentEdges lns =
  case filter ((== RootNode) . lnKind) lns of
    [] -> []
    (root : _) ->
      [ contains (EdgeId (negate i)) (lnId root) (lnId n)
      | (i, n) <- zip [1 ..] lns
      , lnId n /= lnId root
      ]

toLayoutNode :: Entity M.Node -> LayoutNode
toLayoutNode (Entity k e) =
  LayoutNode
    { lnId = NodeId (fromSqlKey k)
    , lnKind =
        if M.unNodeTypeKey (M.nodeNodeTypeId e) == "project_root"
          then RootNode
          else WorkNode
    , lnLabel = pack (M.nodeTitle e)
    }

{- | @project.dependency@ stores @node_id@ /depends on/ @to_node_id@
(see @docs/development/backend/database-schema.md@), so @node_id@ is the
dependent — the end that carries the arrowhead — and @to_node_id@ is the
dependency.

This is the opposite of what the old client-side conversion built,
whose @source@/@target@ naming let the arrowhead end up on the
dependency.
-}
toLayoutEdge :: Entity M.Dependency -> LayoutEdge
toLayoutEdge (Entity k e) =
  dependsOn
    (EdgeId (fromSqlKey k))
    (NodeId (fromSqlKey (M.dependencyToNodeId e)))
    (NodeId (fromSqlKey (M.dependencyNodeId e)))

templateServerGraph :: ServerGraph -> Html ()
templateServerGraph sg = do
  graphControls
  svg_
    ( [ id_ "tree-view"
      , viewBox_ viewBox
      , -- Natural size, not scaled to fit: a large project is meant to
        -- overflow its container and be navigated, not shrunk until its
        -- titles stop being readable. #179's viewport is what makes the
        -- overflow usable.
        width_ (dblText (szW size))
      , height_ (dblText (szH size))
      , -- The viewport zooms in multiples of the natural size, so it
        -- still needs that size after it has rewritten width/height.
        dataBaseWidth_ (dblText (szW size))
      , dataBaseHeight_ (dblText (szH size))
      , h_ "on load transition my opacity to 1 over 200ms"
      ]
        <> rootAnchorAttrs
    )
    $ do
      defs_ [] arrowMarker
      g_ [id_ "graph-links"] $
        forM_ (diagramEdges d) edgeLine
      g_ [id_ "graph-nodes"] $
        forM_ (diagramNodes d) (nodeGroup sg)
  -- Loaded from inside the fragment rather than once at page load:
  -- htmx swaps this whole subtree into #tree-container on every graph
  -- load, so anything bound to the drawing has to arrive with it.
  script_ [src_ "/static/script/graph-viewport.js"] (mempty :: Html ())
  where
    d = sgDiagram sg
    Bounds mn _ = diagramBounds d
    size = boundsSize (diagramBounds d)
    viewBox =
      T.unwords
        [ dblText (ptX mn)
        , dblText (ptY mn)
        , dblText (szW size)
        , dblText (szH size)
        ]
    {- Where the viewport scrolls to on open. The server placed the
    root, so the client never has to hunt the DOM for it.

    Emitted relative to the drawing's own top-left rather than in
    diagram coordinates: the SVG is @szW@ x @szH@ CSS pixels showing a
    viewBox that starts at @mn@, so subtracting @mn@ is what turns a
    diagram point into the pixel offset the client can hand straight to
    @scrollLeft@. A graph with no root node (possible -- 'layout' is
    total) emits neither attribute, and the client falls back to the
    middle of the drawing.
    -}
    rootAnchorAttrs = case diagramRootAnchor d of
      Nothing -> []
      Just (Point ax ay) ->
        [ dataRootX_ (dblText (ax - ptX mn))
        , dataRootY_ (dblText (ay - ptY mn))
        ]

{- | Zoom and recentre controls. They sit inside the swapped fragment so
they arrive with the graph, but are pinned to the viewport's corner by
CSS rather than scrolling away with the drawing.

A recentre control is not decoration here: #179 hides the scrollbars,
and scrollbars were also the "there is more canvas, and here is where
you are in it" cue. Without them, a user who has panned into empty
space has no way back.
-}
graphControls :: Html ()
graphControls =
  div_ [id_ "graph-controls"] $
    div_ [class_ "graph-controls-inner"] $ do
      ctl "graph-zoom-out" "Zoom out" "remove"
      ctl "graph-zoom-reset" "Recentre on the project root" "my_location"
      ctl "graph-zoom-in" "Zoom in" "add"
  where
    ctl :: Text -> Text -> Html () -> Html ()
    ctl cid lbl icon =
      button_
        [ id_ cid
        , class_ "graph-control"
        , type_ "button"
        , titleAttr_ lbl
        , ariaLabel_ lbl
        ]
        $ i_ [class_ "material-icons"] icon

arrowMarker :: Html ()
arrowMarker =
  marker_
    [ id_ "arrow"
    , viewBox_ "0 -5 10 10"
    , refX_ "10"
    , refY_ "0"
    , markerWidth_ "6"
    , markerHeight_ "6"
    , orient_ "auto"
    ]
    $ path_ [d_ "M0,-5L10,0L0,5", fill_ "#999"] (mempty :: Html ())

{- | The polyline's last point is the dependent end, so @marker-end@
puts the arrowhead on the node that is waiting — not on the one that
has to finish first.
-}

{- | Only a dependency gets an arrowhead.

A containment edge says the project root holds this node; nothing is
waiting on anything, so there is no direction of work to point along.
Drawing an arrow on it is what made the graph read as "the root depends
on all its work" (#198). It also takes its own class, so the stylesheet
can distinguish structure from ordering.
-}
edgeLine :: PlacedEdge -> Html ()
edgeLine e =
  path_
    ( [ class_ (if dependency then "link" else "link link-contains")
      , d_ (polyline (peJumps e) (pePoints e))
      , fill_ "none"
      ]
        <> [markerEnd_ "url(#arrow)" | dependency]
    )
    (mempty :: Html ())
  where
    dependency = peKind e == DependsOn

{- | The edge's path, hopping over each of its 'peJumps' (#180).

Which crossings get a hop is decided by the layout engine; this only
draws them. A hop is a semicircular arc of 'cfgJumpRadius' replacing
the middle of the run, always bulging towards the top of the page so a
row of them reads as one convention rather than a wobble.
-}
polyline :: [Point] -> [Point] -> Text
polyline _ [] = ""
polyline jumps (p : ps) =
  "M" <> point p <> mconcat (zipWith run (p : ps) ps)
  where
    point (Point x y) = dblText x <> "," <> dblText y

    run (Point x0 y0) q@(Point x1 y1)
      -- Vertical runs are drawn straight through: only the horizontal
      -- side of a crossing hops (see 'addJumps').
      | y0 /= y1 || null hops = " L" <> point q
      | otherwise = mconcat (map hop hops) <> " L" <> point q
      where
        rightward = x1 > x0
        -- In travel order, so the arcs come out along the run rather
        -- than doubling back to an earlier one.
        hops =
          (if rightward then id else reverse)
            . sort
            $ [ jx
              | Point jx jy <- jumps
              , jy == y0
              , jx > min x0 x1
              , jx < max x0 x1
              ]

        hop jx =
          " L"
            <> point (Point (jx - dir * r) y0)
            <> " A"
            <> dblText r
            <> ","
            <> dblText r
            <> " 0 0 "
            -- The sweep flag has to flip with direction of travel to
            -- keep every hop bulging the same way. 1 is the
            -- positive-angle direction, which reads as clockwise in
            -- SVG's y-down space: clockwise from the left end goes over
            -- the top, and so does counter-clockwise from the right
            -- end. Both arcs below therefore bulge upward.
            <> (if rightward then "1 " else "0 ")
            <> point (Point (jx + dir * r) y0)
          where
            dir = if rightward then 1 else -1

    r = cfgJumpRadius defaultLayoutConfig

nodeGroup :: ServerGraph -> PlacedNode -> Html ()
nodeGroup sg n =
  g_
    [ id_ ("node-" <> nid)
    , class_ "node"
    , transform_ ("translate(" <> dblText (ptX tl) <> "," <> dblText (ptY tl) <> ")")
    , hxGet_ (nodePanelLink rawId pid)
    , hxTrigger_ "click"
    , hxTarget_ "#node-panel"
    , hxPushUrl'_ (pushUrl rawId pid)
    , hxSwap_ "innerHTML"
    ]
    $ do
      -- Only geometry is set here. Fill, stroke, hover, the
      -- `.node-highlight` glow and the `.flash` animation all live in
      -- manage-project.css, keyed off the `root`/`work` class -- the
      -- keyed off the class rather than the element name, which is
      -- what made removing the old circle (#182) cost nothing here.
      rect_
        [ class_ (kindClass (pnKind n))
        , width_ (dblText (szW sz))
        , height_ (dblText (szH sz))
        , rx_ "6"
        ]
        (mempty :: Html ())
      nodeLabel nid sz (pnLines n)
      -- Re-fetch this node's label when its detail panel closes after
      -- an edit.
      g_
        [ class_ "hidden"
        , hxGet_ (nodeRefreshLink rawId pid rawLabel)
        , hxTrigger_ $
            "nodePanel:onEditClosed[event.detail.nodeId=="
              <> nid
              <> "] from:#node-panel"
        , hxTarget_ ("#node-text-" <> nid)
        , hxSwap_ "innerHTML"
        , hxPushUrl_ False
        ]
        (mempty :: Html ())
  where
    NodeId rawId = pnId n
    nid = intToText rawId
    pid = sgProjectId sg
    tl = pnTopLeft n
    sz = pnSize n
    rawLabel = Map.findWithDefault "" (pnId n) (sgLabels sg)

kindClass :: NodeKind -> Text
kindClass RootNode = "root"
kindClass WorkNode = "work"

{- | SVG has no text wrapping, so each label line is its own @tspan@.
Lines arrive pre-wrapped from the layout engine ('pnLines'); this only
positions them, centred in the box however many there are.

Two details here are load-bearing rather than stylistic:

* The id is @node-text-<id>@. It is what the
  per-node refresh hook swaps into after an edit, and it has to be
  unique per node -- this element used to carry a constant
  @node-label@, which both repeated one id across every node in the
  document and left the refresh hook aimed at a target that was never
  there.
* The centring is a @transform@ on the @text@ element rather than
  @x@\/@y@ on it and on every @tspan@. That puts the text origin at the
  middle of the box, so the @tspan@s inside sit relative to a centre
  (see 'tspanLines') -- which is what lets the refresh endpoint return
  one fragment that lands correctly wherever the node sits.
-}
nodeLabel :: Text -> Size -> [Text] -> Html ()
nodeLabel nid (Size w h) ls =
  text_
    [ id_ ("node-text-" <> nid)
    , transform_
        ( "translate("
            <> dblText (w / 2)
            <> ","
            <> dblText (h / 2)
            <> ")"
        )
    , -- Centres a single line on the baseline; 'tspanLines' lifts the
      -- block up from there as it grows past one line.
      dy_ "0.35em"
    ]
    $ tspanLines ls

{- | Coordinates render as plain integers where they are whole, which
most are, rather than as @80.0@.
-}
dblText :: Double -> Text
dblText v
  | v == fromIntegral rounded = intToText rounded
  | otherwise = pack (show v)
  where
    rounded = round v :: Int
