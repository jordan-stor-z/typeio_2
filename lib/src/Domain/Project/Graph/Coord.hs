{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 5 of the layout pipeline: horizontal placement.

Vertical placement is decided by routing, since gap heights depend on
how many tracks cross them, so this module is only about @x@. See
@docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Coord
  ( assignX
  , componentsOf
  , packComponents
  ) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
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
* __Rows keep their left-to-right order within a component.__ Placement
  pushes a slot, never swaps it past a neighbour. Reordering is #177's
  job, and keeping the two separate is what lets this run without undoing
  that one. The final packing pass may move whole components past each
  other, which cannot disturb #177's work — see 'packComponents'.
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
  packComponents widthOf gap (componentsOf rows segments) $
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

{- | The weakly-connected components of the slot graph, each as an
ascending list of its slots.

Segments are followed in both directions: two slots share a component
when the drawing connects them at all, whichever way the dependency
runs. A slot no segment touches — an isolated node — is its own
component.

Every component holds at least one row-0 slot, because a component's own
head has nothing above it. Components are therefore always anchored at
the top of the drawing rather than starting at some arbitrary depth,
which is what makes packing them side by side meaningful.
-}
componentsOf :: Map Int [LNode] -> [Segment] -> [[LNode]]
componentsOf rows segments = go (concat (M.elems rows)) S.empty
  where
    adjacent =
      M.fromListWith
        (<>)
        (concat [[(segFrom s, [segTo s]), (segTo s, [segFrom s])] | s <- segments])

    go [] _ = []
    go (n : ns) seen
      | n `S.member` seen = go ns seen
      | otherwise =
          let found = reach S.empty [n]
           in S.toAscList found : go ns (seen <> found)

    reach :: Set LNode -> [LNode] -> Set LNode
    reach seen [] = seen
    reach seen (m : ms)
      | m `S.member` seen = reach seen ms
      | otherwise =
          reach (S.insert m seen) (M.findWithDefault [] m adjacent <> ms)

{- | Slide whole components apart until their bounding boxes no longer
overlap.

Without this, a component that is narrow at the top and wide further
down spreads out underneath its neighbour, and the neighbour ends up
sitting /inside/ its span rather than beside it — several independent
graphs drawn as one tangle. #174 scoped this and it was never built;
#214 is the record of that, and of the fixture that shows it.

Each component is translated rigidly, so nothing inside one moves
relative to anything else inside it. Two consequences, and they are what
make this safe to run after ordering has already been decided:

* __Crossings cannot change.__ Edges exist only within a component, and
  every component keeps its internal order, so no pair of edges swaps
  which side of the other it lies on. The count is invariant.
* __Boxes cannot start overlapping.__ Slots within a component keep
  their separation exactly; slots in different components end up in
  disjoint x-spans at least @gap@ apart.

Components are placed left to right in the order they already sat in,
and a component is only ever pushed right, never pulled left — so a
drawing whose components were already clear of each other comes back
untouched, and this can only widen one that was overlapping to begin
with. Ties break on the component's own slots, so it is deterministic.
-}
packComponents ::
  -- | How much horizontal room each slot occupies.
  (LNode -> Double) ->
  -- | Clear space required between two components.
  Double ->
  -- | The components, as returned by 'componentsOf'.
  [[LNode]] ->
  Map LNode Double ->
  Map LNode Double
packComponents widthOf gap comps xs
  | length comps < 2 = xs
  | otherwise = snd (foldl' place (Nothing, xs) ordered)
  where
    xAt n = M.findWithDefault 0 n xs
    extent c =
      ( minimum [xAt n - widthOf n / 2 | n <- c]
      , maximum [xAt n + widthOf n / 2 | n <- c]
      )

    ordered = sortOn (\c -> (fst (extent c), c)) comps

    place (prevRight, acc) c =
      ( Just (right + shift)
      , foldl' (\m n -> M.adjust (+ shift) n m) acc c
      )
      where
        (left, right) = extent c
        shift = case prevRight of
          Nothing -> 0
          Just pr -> max 0 (pr + gap - left)

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
