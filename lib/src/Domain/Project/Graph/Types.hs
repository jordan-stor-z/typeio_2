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
  -- ^ Vertical space between one row of nodes and the next.
  , cfgNodeGap :: Double
  -- ^ Minimum horizontal space between two boxes in the same row.
  , cfgLabelWidth :: Int
  -- ^ Characters per label line.
  , cfgLabelLines :: Int
  -- ^ Maximum label lines before truncation.
  , cfgMargin :: Double
  -- ^ Padding around the whole drawing.
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
    , cfgLabelWidth = 18
    , cfgLabelLines = 3
    , cfgMargin = 48
    }

boundsSize :: Bounds -> Size
boundsSize (Bounds mn mx) =
  Size (ptX mx - ptX mn) (ptY mx - ptY mn)
