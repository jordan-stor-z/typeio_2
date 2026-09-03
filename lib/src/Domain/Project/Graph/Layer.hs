{- | Phases 1 and 2 of the layout pipeline: break cycles, then assign
every node to a row.

See @docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Layer
  ( Arc (..)
  , Segment (..)
  , breakCycles
  , assignLayers
  , insertDummies
  ) where

import Data.Foldable (foldl')
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Domain.Project.Graph.Types
  ( EdgeId
  , EdgeKind (..)
  , LNode (..)
  , LayoutEdge (..)
  , LayoutNode (..)
  , NodeId
  )

{- | A dependency edge oriented for layout: 'arcFrom' is drawn in a row
above 'arcTo'.

Orienting every edge dependent-to-dependency is what puts a node above
the work it is waiting on, matching the reference images in #169: the
project root sits at the top because it depends on its work, and the
arrows point up from each dependency into the thing it unblocks
(@docs/solution-proposals/haskell-graph-rendering.md@ §3, D1).

Note this is a statement about edge /direction/, not about the root
specifically. Nothing guarantees the root has no dependents — a row
recorded as \"some task depends on the project root\" puts the root below
that task, which is exactly what the rule says should happen. Whether
root edges are recorded consistently is a data question, not a layout
one.
-}
data Arc = Arc
  { arcEdge :: EdgeId
  , arcKind :: EdgeKind
  -- ^ Carried through so the renderer can tell the two apart.
  , arcFrom :: NodeId
  -- ^ Drawn above 'arcTo'.
  , arcTo :: NodeId
  , arcReversed :: Bool
  {- ^ This edge pointed backwards and was flipped to make the graph
  acyclic. A layout-time device only: the renderer still draws the
  arrowhead at the true dependent end.
  -}
  }
  deriving (Eq, Show)

{- | Orient every edge for layout, reversing the minimum-ish set needed
to make the result acyclic.

@project.dependency@ permits cycles — its unique constraint stops
duplicate edges, not loops — and no application-level validation exists
yet, so this cannot assume a DAG. Back edges found by depth-first search
are reversed rather than dropped, and never rejected: refusing to draw a
cyclic graph would leave the user no way to see the cycle in order to
fix it.

Not the minimum feedback arc set, which is NP-hard. DFS back-edge
reversal is the standard linear-time answer. Node and edge visit order
is sorted by id so the choice is identical run to run.
-}
breakCycles :: [LayoutNode] -> [LayoutEdge] -> [Arc]
breakCycles ns es = map orient es
  where
    backs = backEdges ns es
    -- 'LayoutEdge' already knows which end goes on top -- its smart
    -- constructors resolve that from the relationship, so both a
    -- dependency and a containment edge arrive oriented and this only
    -- has to flip the ones that close a cycle.
    orient e
      | leId e `S.member` backs =
          Arc (leId e) (leKind e) (leLower e) (leUpper e) True
      | otherwise =
          Arc (leId e) (leKind e) (leUpper e) (leLower e) False

data Dfs = Dfs
  { dfsSeen :: Set NodeId
  , dfsOnStack :: Set NodeId
  , dfsBack :: Set EdgeId
  }

-- | Edge ids that point back at a node already on the DFS stack.
backEdges :: [LayoutNode] -> [LayoutEdge] -> Set EdgeId
backEdges ns es = dfsBack (foldl' fromRoot start (sort (map lnId ns)))
  where
    start = Dfs S.empty S.empty S.empty
    -- Arcs run upper -> lower, matching 'Arc'.
    outgoing =
      M.fromListWith
        (++)
        [(leUpper e, [e]) | e <- es]
    outOf n = sortOn leId (M.findWithDefault [] n outgoing)
    fromRoot st n
      | n `S.member` dfsSeen st = st
      | otherwise = visit st n
    visit st n =
      let entered =
            st
              { dfsSeen = S.insert n (dfsSeen st)
              , dfsOnStack = S.insert n (dfsOnStack st)
              }
          descended = foldl' step entered (outOf n)
       in descended {dfsOnStack = S.delete n (dfsOnStack descended)}
    step st e
      | tgt `S.member` dfsOnStack st =
          -- Points at something still open on this path: a cycle. A
          -- self-dependency lands here too, since a node is on its own
          -- stack while being visited.
          st {dfsBack = S.insert (leId e) (dfsBack st)}
      | tgt `S.member` dfsSeen st = st
      | otherwise = visit st tgt
      where
        tgt = leLower e

{- | Longest-path layering: a node with nothing above it is row 0, and
any other node sits one row below the lowest node that sits above it.

Row 0 is the top of the drawing. Each edge already knows which of its
ends belongs on top ('leUpper'), decided by the relationship it records:
a dependent is drawn above what it waits on, and a container above what
it holds. The project root therefore heads the graph because it
/contains/ its work (#198) — not, as this once assumed, because it
depends on it.

Every node in the input gets a layer, including nodes with no edges at
all and nodes in components disconnected from the root.
-}
assignLayers :: [LayoutNode] -> [Arc] -> Map NodeId Int
assignLayers ns arcs = snd (foldl' go (S.empty, M.empty) (sort (map lnId ns)))
  where
    above =
      M.fromListWith
        (++)
        [(arcTo a, [arcFrom a]) | a <- arcs]
    go acc@(active, memo) n
      | n `M.member` memo = acc
      -- Only reachable if a cycle survived 'breakCycles'. Treated as
      -- "no constraint from this path" rather than looping forever:
      -- layout is total, and never fails on its input.
      | n `S.member` active = acc
      | otherwise =
          let ps = sort (M.findWithDefault [] n above)
              (active', memo') = foldl' go (S.insert n active, memo) ps
              lvl = case mapMaybe (`M.lookup` memo') ps of
                [] -> 0
                ls -> 1 + maximum ls
           in (S.delete n active', M.insert n lvl memo')

{- | One step of an edge's route: from a slot in one row to a slot in
the row directly below it.

After 'insertDummies' every edge is a chain of these, so no segment ever
spans more than one gap — which is what lets ordering, placement and
routing all work one gap at a time.
-}
data Segment = Segment
  { segEdge :: EdgeId
  , segKind :: EdgeKind
  , segFrom :: LNode
  -- ^ In the upper row.
  , segTo :: LNode
  -- ^ In the row directly below.
  , segReversed :: Bool
  }
  deriving (Eq, Show)

{- | Break every multi-row edge into a chain through one dummy per row
it crosses.

An edge from row 2 to row 5 becomes 2→d3, d3→d4, d4→5. The dummies then
take part in ordering and placement like any other slot, which is what
reserves the edge a lane through the rows it passes rather than letting
it cut across whatever happens to be there.

Returns the segments, each row's slots, and the chain per edge so
routing can stitch a polyline back together.
-}
insertDummies ::
  Map NodeId Int ->
  [Arc] ->
  ([Segment], Map LNode Int, Map EdgeId [LNode])
insertDummies layers arcs =
  ( concatMap segmentsOf arcs
  , M.fromList (realSlots <> dummySlots)
  , M.fromList (map (\a -> (arcEdge a, chainOf a)) arcs)
  )
  where
    layerOf n = M.findWithDefault 0 n layers
    realSlots = [(Real n, l) | (n, l) <- M.toList layers]
    dummySlots =
      [ (d, l)
      | a <- arcs
      , (d, l) <- zip (dummiesOf a) [layerOf (arcFrom a) + 1 ..]
      ]

    dummiesOf a =
      [ Dummy (arcEdge a) l
      | l <- [layerOf (arcFrom a) + 1 .. layerOf (arcTo a) - 1]
      ]

    chainOf a = [Real (arcFrom a)] <> dummiesOf a <> [Real (arcTo a)]

    segmentsOf a =
      [ Segment (arcEdge a) (arcKind a) u v (arcReversed a)
      | (u, v) <- zip chain (drop 1 chain)
      ]
      where
        chain = chainOf a
