{- | Deriving the project root's edges to its work.

Membership is not stored as an edge: @project.node.project_id@ records
it, and duplicating that into @project.dependency@ is what put the root
at the bottom of the graph for eight issues (#198). So the root's edges
are derived on the way into layout, every time.

The question this module answers is /which/ nodes the root attaches to.
See @docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Containment
  ( containmentEdges
  , containmentTargets
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Domain.Project.Graph.Types
  ( EdgeId (..)
  , EdgeKind (..)
  , LayoutEdge (..)
  , LayoutNode (..)
  , NodeId
  , NodeKind (..)
  , contains
  )

{- | The root's edges into its work, given every node in the project and
the dependencies actually recorded between them.

The root attaches to the /heads/ of the work — nodes nothing else is
waiting on — and to nothing else. Everything further down already hangs
below a head, and reaches the root that way (#211).

Attaching the root to every node instead, which is what this did when
#198 first derived these, draws the project's real shape and then
buries it: on a chain of five nodes the root fans out to all five on top
of the four edges that describe the actual work. The rule that restores
it is that a node is attached to the root /or/ to other work, never
both.

Ids are negative so they cannot collide with a real
@project.dependency@ id. Nothing persists them; they exist only for the
duration of one layout.
-}
containmentEdges :: [LayoutNode] -> [LayoutEdge] -> [LayoutEdge]
containmentEdges lns les =
  case filter ((== RootNode) . lnKind) lns of
    [] -> []
    (root : _) ->
      [ contains (EdgeId (negate i)) (lnId root) t
      | (i, t) <- zip [1 ..] (containmentTargets root lns les)
      ]

{- | The work the root attaches to, in the order the nodes were given.

Two things decide this, and the second only ever matters because
@project.dependency@ permits cycles:

1.  A node with no dependent has nothing above it, so the root takes
    it. On a well-formed project this is the whole answer — one edge per
    chain head, and an isolated node with no dependencies at all still
    counts as its own head, so nothing floats away.

2.  A cycle has no head: every node in it is some other node's
    dependency. Left at rule 1 alone, a wholly cyclic group would be
    attached to nothing and drift off as an island, which is worse than
    what this is fixing. So after the heads are taken, anything still
    unreachable from them adopts the first such node as an extra anchor,
    and repeats until every node in the project hangs off the root
    somewhere.

Rule 2 is deliberately not "break the cycle first and then ask": layout
does break cycles (@Graph.Layer@), but which edge it reverses is its own
business, and reaching into that decision to answer a question about
membership would couple the two for no gain. Anchoring an unreachable
group is enough — the drawing stays connected however the cycle is
later broken.
-}
containmentTargets :: LayoutNode -> [LayoutNode] -> [LayoutEdge] -> [NodeId]
containmentTargets root lns les = go work S.empty []
  where
    rootId = lnId root
    work = [lnId n | n <- lns, lnId n /= rootId]

    {- Dependents sit above their dependency, so a node with something
    above it is one that appears as the lower end of a dependency edge.
    Containment edges are not consulted: they are what is being derived.

    An edge whose upper end is the root counts like any other. Such a
    row is the pre-migration-000009 way of recording membership, and it
    already puts the root above that node -- deriving a second edge
    alongside it would draw the same relationship twice. -}
    hasDependent :: Set NodeId
    hasDependent =
      S.fromList
        [ leLower e
        | e <- les
        , leKind e == DependsOn
        ]

    -- Downward adjacency: from a node to the work it is waiting on.
    below :: Map NodeId [NodeId]
    below =
      M.fromListWith
        (<>)
        [ (leUpper e, [leLower e])
        | e <- les
        , leKind e == DependsOn
        ]

    heads = [n | n <- work, not (n `S.member` hasDependent)]

    -- Walk down from the anchors taken so far; whatever that cannot
    -- reach still needs one.
    reach :: Set NodeId -> [NodeId] -> Set NodeId
    reach seen [] = seen
    reach seen (n : rest)
      | n `S.member` seen = reach seen rest
      | otherwise = reach (S.insert n seen) (M.findWithDefault [] n below <> rest)

    {- The root seeds the walk alongside the heads, so work it already
    sits above through a stored row is covered and does not get a
    derived edge on top. -}
    covered0 = reach S.empty (rootId : heads)

    {- Heads first, then one pass down the remaining nodes in their
    given order, anchoring any that is still unreachable. Order matters
    only for determinism: the same project must always produce the same
    drawing. -}
    go [] _ acc = heads <> reverse acc
    go (n : rest) extra acc
      | n `S.member` covered0 = go rest extra acc
      | n `S.member` extra = go rest extra acc
      | otherwise =
          let extra' = reach extra [n]
           in go rest extra' (n : acc)
