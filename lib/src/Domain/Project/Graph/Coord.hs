{- | Phase 5 of the layout pipeline: horizontal placement.

Vertical placement is trivial — a node's row is its layer — so this
module is only about @x@. See @docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Coord
  ( assignX
  ) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Domain.Project.Graph.Layer (Arc (..))
import Domain.Project.Graph.Types (NodeId)

{- | Assign every node a horizontal centre.

The /priority method/: each node wants to sit at the median of the
nodes it connects to in the row above (or below, on an upward pass),
and higher-priority nodes get their wish first, shoving lower-priority
neighbours aside to make room. Priority is a node's number of
connections into the reference row — the more edges a node has to keep
short, the more it matters where it sits.

Repeated as alternating downward and upward passes, so placement
settles against both the rows above and the rows below rather than only
one direction.

Two properties this guarantees, and both are tested:

* __Nodes never overlap.__ Every move preserves at least
  @nodeWidth + nodeGap@ between adjacent centres in a row.
* __Rows keep their left-to-right order.__ Nothing here reorders a row;
  a node can be pushed, never swapped past a neighbour. Reordering is
  #177's job, and keeping the two separate is what lets this run
  without undoing that one.

The result is what puts a parent over the middle of its children (R3)
and lines a single-child chain up vertically (R2).
-}
assignX ::
  -- | Minimum distance between the centres of two nodes in a row.
  Double ->
  -- | Each row, in left-to-right order.
  Map Int [NodeId] ->
  [Arc] ->
  Map NodeId Double
assignX sep rows arcs =
  foldl' pass initial (concat (replicate passes [Downward, Upward]))
  where
    passes = 4

    initial =
      M.fromList
        [ (n, sep * fromIntegral i)
        | row <- M.elems rows
        , (i, n) <- zip [0 :: Int ..] row
        ]

    -- Who a node should line up with, depending on which way the pass
    -- is sweeping. Arcs run from the upper row to the lower one.
    above = M.fromListWith (++) [(arcTo a, [arcFrom a]) | a <- arcs]
    below = M.fromListWith (++) [(arcFrom a, [arcTo a]) | a <- arcs]

    referenceOf Downward = above
    referenceOf Upward = below

    -- A downward pass places each row against the one above it, so it
    -- has to visit rows top-down; an upward pass is the mirror.
    rowOrder Downward = M.toAscList rows
    rowOrder Upward = M.toDescList rows

    pass xs dir = foldl' (placeRow dir) xs (map snd (rowOrder dir))

    placeRow dir xs row =
      foldl' (placeNode dir row) xs (byPriority dir row)

    -- Highest priority first, ties broken by node id so the outcome
    -- does not depend on the order rows happen to be built in.
    byPriority dir row =
      sortOn
        (\n -> (Down (length (neighboursOf dir n)), n))
        row

    neighboursOf dir n = M.findWithDefault [] n (referenceOf dir)

    placeNode dir row xs n =
      case medianOf (mapMaybe (`M.lookup` xs) (neighboursOf dir n)) of
        Nothing -> xs
        Just target -> shiftTo sep row (priorityIn dir row) xs n target

    priorityIn dir row =
      M.fromList [(n, length (neighboursOf dir n)) | n <- row]

data Direction = Downward | Upward

{- | The middle of a node's neighbours.

An even number of them averages the middle two rather than picking one,
which is what centres a parent between exactly two children — the
commonest shape in the reference images, and the one an odd-median
would visibly get wrong by parking the parent directly over one child.
-}
medianOf :: [Double] -> Maybe Double
medianOf [] = Nothing
medianOf vs
  | odd n = Just (sorted !! mid)
  | otherwise = Just ((sorted !! (mid - 1) + sorted !! mid) / 2)
  where
    sorted = sortOn id vs
    n = length vs
    mid = n `div` 2

{- | Move one node toward @target@, pushing whatever it can and
stopping where it cannot.

Nodes of lower priority get shoved along ahead of the mover. The first
node of equal or higher priority is immovable, and caps how far the
move can go — every node between the two still needs its own separation,
so the cap accounts for all of them.
-}
shiftTo ::
  Double ->
  [NodeId] ->
  Map NodeId Int ->
  Map NodeId Double ->
  NodeId ->
  Double ->
  Map NodeId Double
shiftTo sep row priority xs n target
  | target > current = move 1 (drop (idx + 1) row)
  | target < current = move (-1) (reverse (take idx row))
  | otherwise = xs
  where
    idx = length (takeWhile (/= n) row)
    current = xAt n
    xAt m = M.findWithDefault 0 m xs
    prio m = M.findWithDefault 0 m priority

    move sign neighbours =
      foldl' shove (M.insert n placed xs) (zip [1 :: Int ..] movable)
      where
        -- Everything softer than the mover, up to the first node that
        -- outranks it.
        movable = takeWhile (\m -> prio m < prio n) neighbours
        -- How far the move can go: the blocker's position, minus room
        -- for every node that has to fit between the two.
        cap = case drop (length movable) neighbours of
          [] -> target
          (b : _) -> xAt b - sign * sep * fromIntegral (length movable + 1)
        placed = clampTowards (min target cap) (max target cap)
        shove acc (j, m) =
          let wanted = placed + sign * sep * fromIntegral j
           in M.insert m (outward wanted (M.findWithDefault 0 m acc)) acc
        -- Pushing only moves a neighbour away from the mover; one
        -- already further out than it needs to be stays where it is.
        outward wanted keep
          | sign > 0 = max wanted keep
          | otherwise = min wanted keep
        clampTowards lo hi
          | sign > 0 = lo
          | otherwise = hi
