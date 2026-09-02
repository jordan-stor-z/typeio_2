{-# LANGUAGE OverloadedStrings #-}

{- | Types for the dependency graph's layout pipeline.

See @docs/architecture/graph-rendering.md@ for the design this
implements, and @docs/solution-proposals/haskell-graph-rendering.md@
(#169) for why it was chosen.

Nothing in @Domain.Project.Graph.*@ may import @Database.*@,
@persistent@, @Esqueleto@, @Lucid@ or @Network.Wai@. That is what keeps
these modules in the pure, dependency-free tier the unit suite covers
(@docs/development/unit-testing.md@); the responder converts entities
into the types below and renders the 'Diagram' that comes back.
-}
module Domain.Project.Graph.Types
  ( NodeId (..)
  , EdgeId (..)
  , NodeKind (..)
  , LNode (..)
  , isDummy
  , LayoutNode (..)
  , LayoutEdge (..)
  , Point (..)
  , Size (..)
  , Bounds (..)
  , PlacedNode (..)
  , PlacedEdge (..)
  , Diagram (..)
  , LayoutConfig (..)
  , defaultLayoutConfig
  , boundsSize
  ) where

import Data.Int (Int64)
import Data.Text (Text)

newtype NodeId = NodeId Int64
  deriving (Eq, Ord, Show)

newtype EdgeId = EdgeId Int64
  deriving (Eq, Ord, Show)

data NodeKind
  = RootNode
  | WorkNode
  deriving (Eq, Show)

data LayoutNode = LayoutNode
  { lnId :: NodeId
  , lnKind :: NodeKind
  , lnLabel :: Text
  {- ^ The raw title. Wrapped to the node box during layout, so the
  engine never has to measure text (see 'cfgLabelWidth').
  -}
  }
  deriving (Eq, Show)

{- | One dependency edge.

The field names are semantic on purpose. @dependency@ must be completed
before @dependent@ can be, and the arrowhead is drawn at the
@dependent@ end — an edge points from the work that must finish toward
the work waiting on it.

Calling these @source@/@target@ is exactly how the existing D3 path
ended up drawing its arrowheads on the dependency: @project.dependency@
stores @node_id@ /depends on/ @to_node_id@, so @node_id@ is the
'leDependent' and @to_node_id@ is the 'leDependency', which reads
backwards under generic names.
-}
data LayoutEdge = LayoutEdge
  { leId :: EdgeId
  , leDependency :: NodeId
  -- ^ Must be completed first. @project.dependency.to_node_id@.
  , leDependent :: NodeId
  -- ^ Waits on it; carries the arrowhead. @project.dependency.node_id@.
  }
  deriving (Eq, Show)

{- | A slot in a row: either a real node, or a bend point standing in
for one row of a multi-row edge.

Dummies exist only inside the pipeline. They take a place in their row's
ordering and get coordinates like anything else, and then routing
consumes each one as a bend in its edge's polyline — nothing is ever
emitted for one, and 'diagramNodes' holds real nodes exclusively. Their
only visible effect is spacing: reserving room in the rows an edge
crosses is what opens the lane it travels along, and what stops a
multi-row edge being drawn straight through a node box.
-}
data LNode
  = Real NodeId
  | -- | Which edge is passing through, and which row it is passing.
    Dummy EdgeId Int
  deriving (Eq, Ord, Show)

isDummy :: LNode -> Bool
isDummy (Dummy _ _) = True
isDummy (Real _) = False

data Point = Point
  { ptX :: Double
  , ptY :: Double
  }
  deriving (Eq, Show)

data Size = Size
  { szW :: Double
  , szH :: Double
  }
  deriving (Eq, Show)

data Bounds = Bounds
  { bMin :: Point
  , bMax :: Point
  }
  deriving (Eq, Show)

{- | A node with its computed geometry. 'pnTopLeft' is the box's
top-left corner, not its centre — the D3 path positions by centre,
and that convention deliberately does not carry over.
-}
data PlacedNode = PlacedNode
  { pnId :: NodeId
  , pnKind :: NodeKind
  , pnLines :: [Text]
  -- ^ The label, already wrapped to the box.
  , pnTopLeft :: Point
  , pnSize :: Size
  }
  deriving (Eq, Show)

data PlacedEdge = PlacedEdge
  { peId :: EdgeId
  , pePoints :: [Point]
  {- ^ Polyline. The head sits on the dependency; the last point sits on
  the dependent and is where the arrowhead goes.
  -}
  , peReversed :: Bool
  {- ^ Reversed for layering only, to break a cycle. Does /not/ change
  which end carries the arrowhead.
  -}
  , peJumps :: [Point]
  {- ^ Points on this edge's /horizontal/ runs where another edge's
  vertical run passes underneath, so the renderer can hop over it
  rather than drawing a junction that looks like a connection.

  Only the horizontal side of a crossing carries the jump — the pair is
  asymmetric on purpose, since hopping both would just re-create the
  ambiguity. Points are in the same coordinate space as 'pePoints', and
  always lie strictly inside a run, never on an endpoint: two edges
  merely meeting at a shared port is not a crossing.
  -}
  }
  deriving (Eq, Show)

data Diagram = Diagram
  { diagramNodes :: [PlacedNode]
  {- ^ Real nodes only. Dummy nodes (from #176) are consumed by routing
  and never appear here.
  -}
  , diagramEdges :: [PlacedEdge]
  , diagramBounds :: Bounds
  , diagramRootAnchor :: Maybe Point
  {- ^ Centre of the project root's box, for the viewport's initial
  scroll position (#179). 'Nothing' when the project has no root node.
  -}
  }
  deriving (Eq, Show)

data LayoutConfig = LayoutConfig
  { cfgNodeSize :: Size
  , cfgLayerGap :: Double
  {- ^ Minimum vertical space between one row of nodes and the next. A
  gap grows past this when routing needs more tracks than it fits.
  -}
  , cfgNodeGap :: Double
  -- ^ Minimum horizontal space between two boxes in the same row.
  , cfgDummyWidth :: Double
  {- ^ How much horizontal room a dummy reserves in the row it crosses.
  Narrow on purpose: a passing edge needs a lane, not a node's worth
  of space, and charging it full width would balloon any graph with
  long edges in it.
  -}
  , cfgTrackGap :: Double
  {- ^ Vertical space between two routing tracks in the same gap. A gap
  carrying more horizontal runs than 'cfgLayerGap' has room for grows
  to fit them, so a busy graph stays legible and a simple one stays
  tight.
  -}
  , cfgLabelWidth :: Int
  -- ^ Characters per label line.
  , cfgLabelLines :: Int
  -- ^ Maximum label lines before truncation.
  , cfgMargin :: Double
  -- ^ Padding around the whole drawing.
  , cfgJumpRadius :: Double
  {- ^ Radius of the hop drawn where a horizontal run crosses a vertical
  one ('peJumps'). Small enough to read as a hop rather than a bend,
  and it has to stay well under half the smallest gap between two
  parallel verticals or neighbouring hops would run into each other.
  -}
  }
  deriving (Eq, Show)

{- | Node box sized to fit 'cfgLabelWidth' x 'cfgLabelLines' of the
graph's label font with room to spare, rather than the other way
round: fixing the box and wrapping the label to it is what lets the
server lay the graph out without measuring rendered text.
-}
defaultLayoutConfig :: LayoutConfig
defaultLayoutConfig =
  LayoutConfig
    { cfgNodeSize = Size 160 64
    , cfgLayerGap = 90
    , cfgNodeGap = 40
    , cfgDummyWidth = 12
    , cfgTrackGap = 18
    , cfgLabelWidth = 18
    , cfgLabelLines = 3
    , cfgMargin = 48
    , cfgJumpRadius = 4
    }

boundsSize :: Bounds -> Size
boundsSize (Bounds mn mx) =
  Size (ptX mx - ptX mn) (ptY mx - ptY mn)
