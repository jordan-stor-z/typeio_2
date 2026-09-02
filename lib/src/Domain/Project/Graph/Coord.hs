{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 5 of the layout pipeline: horizontal placement.

Vertical placement is decided by routing, since gap heights depend on
how many tracks cross them, so this module is only about @x@. See
@docs/architecture/graph-rendering.md@.
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
import Domain.Project.Graph.Layer (Segment (..))
import Domain.Project.Graph.Types (LNode, isDummy)

{- | Assign every slot a horizontal centre.

The /priority method/: each slot wants to sit at the median of the
slots it connects to in the row above (or below, on an upward pass),
and higher-priority slots get their wish first, shoving lower-priority
neighbours aside to make room.

Priority is @(is it a dummy, how many connections into that row)@, in
that order. Dummies outrank everything: a chain of them is one long edge
passing through, and letting it hold its line is what keeps that edge
straight instead of zig-zagging around whatever it passes. Among real
nodes, more connections wins — the more edges a node has to keep short,
the more it matters where it sits.

Repeated as alternating downward and upward passes, so placement settles
against both the rows above and the rows below rather than only one.

Two properties this guarantees, and both are tested:

* __Slots never overlap.__ Every move keeps adjacent centres at least
  half of each one's width plus @gap@ apart, so a narrow dummy costs a
  narrow lane and a full node costs a full box.
* __Rows keep their left-to-right order.__ Nothing here reorders a row;
  a slot can be pushed, never swapped past a neighbour. Reordering is
  #177's job, and keeping the two separate is what lets this run without
  undoing that one.
-}
assignX ::
  -- | How much horizontal room each slot occupies.
  (LNode -> Double) ->
  -- | Clear space required between two adjacent slots.
  Double ->
  -- | Each row, in left-to-right order.
  Map Int [LNode] ->
  [Segment] ->
  Map LNode Double
assignX widthOf gap rows segments =
  foldl' pass initial (concat (replicate passes [Downward, Upward]))
  where
    passes = 4

    -- Minimum distance between the centres of two adjacent slots.
    sepOf a b = (widthOf a + widthOf b) / 2 + gap

    initial =
      M.fromList
        [ (n, x)
        | row <- M.elems rows
        , (n, x) <- zip row (scanl (+) 0 (zipWith sepOf row (drop 1 row)))
        ]

    above = M.fromListWith (++) [(segTo s, [segFrom s]) | s <- segments]
    below = M.fromListWith (++) [(segFrom s, [segTo s]) | s <- segments]

    referenceOf Downward = above
    referenceOf Upward = below

    rowOrder Downward = M.toAscList rows
    rowOrder Upward = M.toDescList rows

    pass xs dir = foldl' (placeRow dir) xs (map snd (rowOrder dir))

    placeRow dir xs row = foldl' (placeNode dir row) xs (byPriority dir row)

    byPriority dir row =
      sortOn (\n -> (Down (isDummy n), Down (length (neighboursOf dir n)), n)) row

    neighboursOf dir n = M.findWithDefault [] n (referenceOf dir)

    placeNode dir row xs n =
      case medianOf (mapMaybe (`M.lookup` xs) (neighboursOf dir n)) of
        Nothing -> xs
        Just target -> shiftTo sepOf row (priorityIn dir row) xs n target

    priorityIn dir row =
      M.fromList [(n, (isDummy n, length (neighboursOf dir n))) | n <- row]

data Direction = Downward | Upward

{- | The middle of a slot's neighbours.

An even number of them averages the middle two rather than picking one,
which is what centres a parent between exactly two children — the
commonest shape in the reference images, and one an odd-median would
visibly get wrong by parking the parent directly over one child.
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

{- | Move one slot toward @target@, pushing whatever it can and stopping
where it cannot.

Slots of lower priority get shoved along ahead of the mover. The first
slot of equal or higher priority is immovable, and caps how far the move
can go — every slot in between still needs its own clearance, so the cap
accounts for all of them.
-}
shiftTo ::
  (LNode -> LNode -> Double) ->
  [LNode] ->
  Map LNode (Bool, Int) ->
  Map LNode Double ->
  LNode ->
  Double ->
  Map LNode Double
shiftTo sepOf row priority xs n target
  | target > current = move 1 (drop (idx + 1) row)
  | target < current = move (-1) (reverse (take idx row))
  | otherwise = xs
  where
    idx = length (takeWhile (/= n) row)
    current = xAt n
    xAt m = M.findWithDefault 0 m xs
    prio m = M.findWithDefault (False, 0) m priority

    move sign neighbours =
      foldl' shove (M.insert n placed xs) (zip offsets movable)
      where
        movable = takeWhile (\m -> prio m < prio n) neighbours
        -- Running clearance from the mover out to each pushed slot.
        offsets = drop 1 (scanl (+) 0 (zipWith sepOf (n : movable) movable))
        cap = case drop (length movable) neighbours of
          [] -> target
          (b : _) ->
            let toBlocker =
                  sum (zipWith sepOf (n : movable) (movable <> [b]))
             in xAt b - sign * toBlocker
        placed
          | sign > 0 = min target cap
          | otherwise = max target cap
        shove acc (off, m) =
          let wanted = placed + sign * off
           in M.insert m (outward wanted (M.findWithDefault 0 m acc)) acc
        outward wanted keep
          | sign > 0 = max wanted keep
          | otherwise = min wanted keep
