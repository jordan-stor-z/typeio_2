{- | Phases 1 and 2 of the layout pipeline: break cycles, then assign
every node to a row.

See @docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Layer
  ( Arc (..)
  , breakCycles
  , assignLayers
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
    orient e
      | leId e `S.member` backs =
          Arc (leId e) (leDependency e) (leDependent e) True
      | otherwise =
          Arc (leId e) (leDependent e) (leDependency e) False

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
    -- Arcs run dependent -> dependency, matching 'Arc'.
    outgoing =
      M.fromListWith
        (++)
        [(leDependent e, [e]) | e <- es]
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
        tgt = leDependency e

{- | Longest-path layering: a node nothing depends on is row 0, and any
other node sits one row below the lowest of its dependents.

Row 0 is the top of the drawing, so the project root — the one node with
no dependents — heads the graph and its dependencies descend from it.

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
