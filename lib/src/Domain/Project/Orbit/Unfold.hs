{- | Finding the heads, and unfolding the dependency graph into a forest
of trees.

This is the whole of what makes the orbital drawing different. See
@docs/architecture/orbital-dependency-weighted-graph.md@ (#229).

A node with several dependents cannot belong to a single tree, so it is
__replicated__ — drawn once in each work stream that reaches it, with
its own dependencies replicated along with it. Every drawn disc then has
exactly one dependent, and each stream can own a disjoint wedge of the
circle, which is what makes the drawing contain no crossing edges at
all: structurally absent, not heuristically minimised.

The price is that node identity stops being one-circle-one-node, which
is why 'Domain.Project.Orbit.Types.OrbitTree' carries a replica ordinal
and why colour has to carry identity in the rendered drawing.
-}
module Domain.Project.Orbit.Unfold
  ( heads
  , unfold
  , discs
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Domain.Project.Orbit.Types
  ( NodeId
  , OrbitEdge (..)
  , OrbitNode (..)
  , OrbitTree (..)
  )

{- | The nodes nothing is waiting on, in 'NodeId' order.

A __head__ is a node that appears as no edge's 'oeDependency': nothing
depends on it, so nothing sits between it and the eye. The term is
borrowed from "Domain.Project.Graph.Containment", where #211 already
uses it for this same set on this same data — two parts of the app
having different names for one idea is worse than the small duplication
of computing it twice in different tiers.

A node with neither dependencies nor dependents is its own head, and
gets a stream of one disc rather than floating out of the drawing.

Edges naming a node that was not supplied are ignored, here and
throughout: 'unfold' is total, and an edge into nothing would otherwise
put a disc in the drawing for a node that does not exist.
-}
heads :: [OrbitNode] -> [OrbitEdge] -> [NodeId]
heads ns es = [n | n <- S.toAscList present, not (n `S.member` waitedOn)]
  where
    present = S.fromList (map onId ns)
    waitedOn = S.fromList (map oeDependency (liveEdges present es))

{- | The forest: one tree per head, each node's children being the work
it is waiting on.

Ordering is fixed rather than inherited from the query — heads and
children both in 'NodeId' order — so the same project always draws the
same picture. Relying on the row order a @SELECT@ happens to return
would make the drawing quietly non-deterministic.

=== Size

The unfolding is __unbounded by design__: no cap, no truncation, no
collapse rule. The number of discs is the number of distinct dependency
paths from a head, which on a densely shared graph grows exponentially
in depth.

That is defensible only because the constraint belongs upstream, at the
point a dependency is recorded (#231): a project that unfolds into
thousands of discs is one that should have been split, and a renderer
that quietly draws less than it was given is a drawing that lies about
the project.

=== Cycles

@project.dependency@ permits cycles at the schema level, and the
decision recorded on #205 is to reject them when a dependency is
written. This module still guards against one, because the two failure
modes are not comparable: a layered drawing handed a cycle draws
something slightly wrong, whereas an unfolding handed a cycle __does not
terminate__ — it would hang the request from inside a phase that is
otherwise pure and total.

So a branch stops expanding when a node would repeat on its own ancestor
path. That is a safety property and not a feature: nothing in the
drawing announces that it fired, because by the time it does the data is
already invalid and the fix is upstream.

A wholly cyclic group has no head at all — every node in it is
something else's dependency — so nothing would reach it and it would
vanish from the drawing entirely. Rather than silently omit nodes, such
a group adopts an anchor, exactly as
'Domain.Project.Graph.Containment.containmentTargets' does for the same
situation (#211): after the heads are taken, any node still unreached
becomes an extra root, until every node is drawn somewhere.
-}
unfold :: [OrbitNode] -> [OrbitEdge] -> [OrbitTree]
unfold ns es = numberForest (map (raw S.empty) roots)
  where
    present = S.fromList (map onId ns)
    es' = liveEdges present es

    {- From a node to the work it is waiting on, deduplicated and in a
    fixed order.

    Deduplicating matters more here than it would in a layered drawing.
    @UNIQUE (node_id, to_node_id)@ makes a repeated row unreachable
    through the app, but if one arrived another way it would draw the
    dependency's whole subtree twice -- and in this visualization a node
    drawn twice /means/ two dependents. A duplicate row would look
    exactly like a shared dependency, which is the one thing the drawing
    is for. -}
    below :: Map NodeId [NodeId]
    below =
      M.map (S.toAscList . S.fromList) $
        M.fromListWith
          (<>)
          [(oeDependent e, [oeDependency e]) | e <- es']

    hs = heads ns es
    roots = hs <> anchors (S.toAscList present) (reach S.empty hs)

    {- Whatever the heads cannot reach is in a cycle. Adopt the first
    such node, extend the reachable set through it, and carry on -- so a
    cyclic island is still drawn rather than dropped. Sorted order keeps
    which node gets adopted deterministic. -}
    anchors :: [NodeId] -> Set NodeId -> [NodeId]
    anchors [] _ = []
    anchors (n : rest) seen
      | n `S.member` seen = anchors rest seen
      | otherwise = n : anchors rest (reach seen [n])

    reach :: Set NodeId -> [NodeId] -> Set NodeId
    reach seen [] = seen
    reach seen (n : rest)
      | n `S.member` seen = reach seen rest
      | otherwise = reach (S.insert n seen) (M.findWithDefault [] n below <> rest)

    {- The unfolding proper. @path@ is the ancestors of this disc, which
    is what stops a cycle expanding forever -- and note it is the /path/,
    not a global visited set: a node reached again down a different
    branch is a legitimate replica and must be expanded again. -}
    raw :: Set NodeId -> NodeId -> Raw
    raw path n =
      Raw n [raw path' c | c <- M.findWithDefault [] n below, not (c `S.member` path')]
      where
        path' = S.insert n path

-- | Every disc in a forest, parents before children.
discs :: [OrbitTree] -> [OrbitTree]
discs = concatMap go
  where
    go t = t : concatMap go (otChildren t)

-- | The shape, before replica ordinals and rings are assigned.
data Raw = Raw NodeId [Raw]

{- | Assign each disc its ring and its replica ordinal.

Ordinals run in traversal order across the __whole forest__, not per
tree, so a node's copies are numbered 0, 1, 2 wherever they fall. The
renderer uses them for DOM ids, which have to be unique per document
rather than per stream.
-}
numberForest :: [Raw] -> [OrbitTree]
numberForest = go M.empty
  where
    go _ [] = []
    go counts (r : rest) =
      let (t, counts') = number 0 counts r
       in t : go counts' rest

number :: Int -> Map NodeId Int -> Raw -> (OrbitTree, Map NodeId Int)
number ring counts (Raw n kids) =
  (OrbitTree n replica ring kids', counts'')
  where
    replica = M.findWithDefault 0 n counts
    counts' = M.insert n (replica + 1) counts
    (kids', counts'') = foldl step ([], counts') kids
    step (acc, c) k =
      let (k', c') = number (ring + 1) c k
       in (acc <> [k'], c')

{- | Edges with both ends among the nodes supplied.

A @project.dependency@ row pointing at a node that is not in the drawing
-- the project root, on this visualization, or a row recorded by hand
against something since deleted -- would otherwise put a disc in the
forest for a node that does not exist.
-}
liveEdges :: Set NodeId -> [OrbitEdge] -> [OrbitEdge]
liveEdges present =
  filter
    ( \e ->
        oeDependent e `S.member` present
          && oeDependency e `S.member` present
    )
