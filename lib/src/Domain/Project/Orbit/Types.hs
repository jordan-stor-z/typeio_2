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
