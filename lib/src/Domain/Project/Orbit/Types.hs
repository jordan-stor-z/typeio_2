{- | The orbital dependency-weighted visualization's own geometry types.

See @docs/architecture/orbital-dependency-weighted-graph.md@ (#229).

__This tier deliberately does not import "Domain.Project.Graph".__ That
is the /layered/ layout engine — layers, dummy nodes, crossing
reduction, orthogonal routing — and none of it applies to a radial
drawing. Sharing its types to save one @newtype@ would couple the two
geometries and make "the layout engine" mean two unrelated things.
@docs/architecture/visualization-switching.md@ anticipates exactly this:
a visualization that is not layered brings its own geometry.

__The one hard rule:__ nothing under @Domain.Project.Orbit.*@ may import
@Database.*@, @persistent@, @Esqueleto@, @Lucid@, or anything from
@Network.Wai@. It is what keeps this in the pure, dependency-free tier
the unit suite covers, and the invariants in @OrbitSpec@ are only
expressible from inside that tier.
-}
module Domain.Project.Orbit.Types
  ( NodeId (..)
  , OrbitNode (..)
  , OrbitEdge (..)
  , OrbitTree (..)
  , Point (..)
  , Size (..)
  , Bounds (..)
  , boundsSize
  , Disc (..)
  , Link (..)
  , OrbitDiagram (..)
  , OrbitConfig (..)
  , defaultOrbitConfig
  ) where

import Data.Int (Int64)
import Data.Text (Text)

newtype NodeId = NodeId Int64
  deriving (Eq, Ord, Show)

data OrbitNode = OrbitNode
  { onId :: NodeId
  , onLabel :: Text
  -- ^ Raw title; wrapped to the disc during rendering.
  }
  deriving (Eq, Show)

{- | One dependency: @oeDependent@ is waiting on @oeDependency@.

Drawn with the dependent nearer the eye and the arrowhead on it, since
it is the end that is waiting — the same rule the rest of the app
follows.

__There is no smart constructor here, and that is deliberate.__
"Domain.Project.Graph.Types" needs 'Domain.Project.Graph.Types.dependsOn'
because its edge names its ends @leUpper@\/@leLower@ — positions, which
say nothing about which end waits, and which the app has already written
backwards twice (#181, #198). These fields name the relationship
directly, so the guard rail is in the field names and a constructor
would only add a second way to say the same thing.
-}
data OrbitEdge = OrbitEdge
  { oeDependent :: NodeId
  -- ^ The one waiting. Drawn nearer the eye.
  , oeDependency :: NodeId
  -- ^ The one waited on. Drawn further out.
  }
  deriving (Eq, Show)

{- | One node of the unfolded forest, and everything below it.

A tree node is a __disc__: one drawn circle. 'otNode' is /not/ unique
across a drawing — a node with several dependents is drawn once in each
work stream that reaches it, which is the whole premise of this
visualization. 'otReplica' distinguishes the copies.
-}
data OrbitTree = OrbitTree
  { otNode :: NodeId
  , otReplica :: Int
  -- ^ 0-based ordinal among this node's discs, in traversal order.
  , otRing :: Int
  -- ^ Depth from the head. 0 is the innermost ring.
  , otChildren :: [OrbitTree]
  {- ^ The dependencies this disc is waiting on, drawn one ring further
  out.
  -}
  }
  deriving (Eq, Show)

-- | SVG conventions: x right, y down. One unit is one CSS pixel.
data Point = Point {ptX :: Double, ptY :: Double}
  deriving (Eq, Show)

data Size = Size {szW :: Double, szH :: Double}
  deriving (Eq, Show)

data Bounds = Bounds {bMin :: Point, bMax :: Point}
  deriving (Eq, Show)

boundsSize :: Bounds -> Size
boundsSize (Bounds (Point x0 y0) (Point x1 y1)) = Size (x1 - x0) (y1 - y0)

{- | One placed circle.

'dNode' is not unique across a drawing — see 'OrbitTree'. 'dReplica' is
what distinguishes the copies, and what the renderer builds DOM ids
from.
-}
data Disc = Disc
  { dNode :: NodeId
  , dReplica :: Int
  , dRing :: Int
  , dAngle :: Double
  -- ^ Radians, clockwise from 12 o'clock.
  , dCentre :: Point
  , dLines :: [Text]
  -- ^ Label, already wrapped to the circle.
  }
  deriving (Eq, Show)

{- | The segment from a disc to the disc it is a dependency of, trimmed
to both rims.

'lTo' is the inner end — the dependent, the one waiting — and carries
the arrowhead, the same rule the rest of the app follows.
-}
data Link = Link
  { lFrom :: Point
  , lTo :: Point
  }
  deriving (Eq, Show)

data OrbitDiagram = OrbitDiagram
  { odDiscs :: [Disc]
  , odLinks :: [Link]
  , odBounds :: Bounds
  }
  deriving (Eq, Show)

data OrbitConfig = OrbitConfig
  { cfgDiscRadius :: Double
  -- ^ Every disc is the same size.
  , cfgDiscGap :: Double
  -- ^ Minimum arc clearance between two discs on one ring.
  , cfgMinRingGap :: Double
  {- ^ Minimum __clear__ radial space between the rims of discs on
  consecutive rings — not the distance between their centres.

  Clearance, matching 'cfgDiscGap', because the alternative is a trap:
  set as a centre-to-centre distance it silently becomes zero clearance
  the moment it equals the disc diameter, and radially adjacent discs
  end up exactly tangent with a zero-length link between them. That
  renders as two touching circles and no arrow at all, which is how it
  was first found (#238) — every disc-overlap assertion still passed,
  since tangency is not overlap.
  -}
  , cfgEyeRadius :: Double
  -- ^ Clear space at the centre, when there is more than one stream.
  , cfgLabelWidth :: Int
  -- ^ Characters per label line.
  , cfgLabelLines :: Int
  -- ^ Maximum label lines.
  , cfgMargin :: Double
  -- ^ Padding around the whole drawing.
  }
  deriving (Eq, Show)

{- | Sizes chosen to sit at roughly the layered drawing's weight on
screen.

'cfgLabelWidth' is 12, not the layered drawing's 18: a circle fits fewer
characters per line than the box it became in #178, and 12 is the figure
@docs\/architecture\/graph-rendering.md@ records from when this app last
drew circles.
-}
defaultOrbitConfig :: OrbitConfig
defaultOrbitConfig =
  OrbitConfig
    { cfgDiscRadius = 45
    , cfgDiscGap = 24
    , cfgMinRingGap = 55
    , cfgEyeRadius = 130
    , cfgLabelWidth = 12
    , cfgLabelLines = 3
    , cfgMargin = 60
    }
