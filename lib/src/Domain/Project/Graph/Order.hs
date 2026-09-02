{- | Phase 4 of the layout pipeline: decide the left-to-right order of
each row.

Phases 1–3 produce a /correct/ drawing; this one produces a /readable/
one. Nothing here moves a node to a different row or changes any
coordinate — it only decides who sits left of whom, which is what
determines how many edges have to cross each other. See
@docs/architecture/graph-rendering.md@.
-}
module Domain.Project.Graph.Order
  ( orderRows
  , countCrossings
  ) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Domain.Project.Graph.Layer (Segment (..))
import Domain.Project.Graph.Types (LNode)

data Direction = Downward | Upward

{- | Reorder each row to cut the number of edge crossings.

The /median heuristic/, swept in both directions: each node is placed at
the median position of the nodes it connects to in the row above (on a
downward pass) or below (on an upward pass), then the row is sorted by
those medians. A node with no connections in the reference row keeps its
current place rather than being flung to one end.

Sweeping alternates, because ordering a row to suit the row above will
happily make things worse for the row below, and vice versa. After every
sweep the crossings are counted exactly, and the best ordering seen is
what comes back — a sweep that makes things worse is discarded rather
than built upon, so the result can never be worse than the input.

Deterministic: the sort is stable and every tie falls back to the
current position, so the same input always produces the same order.
-}
orderRows :: Map Int [LNode] -> [Segment] -> Map Int [LNode]
orderRows rows segments = best
  where
    passes = 4
    (_, best, _) =
      foldl'
        step
        (rows, rows, countCrossings rows segments)
        (concat (replicate passes [Downward, Upward]))

    step (current, bestSoFar, bestCount) dir =
      let next = sweep dir current
          count = countCrossings next segments
       in if count < bestCount
            then (next, next, count)
            else (next, bestSoFar, bestCount)

    above = M.fromListWith (++) [(segTo s, [segFrom s]) | s <- segments]
    below = M.fromListWith (++) [(segFrom s, [segTo s]) | s <- segments]

    -- A downward pass orders each row against the one above it, so it
    -- has to work top-down and use the orders it has already settled;
    -- an upward pass is the mirror.
    sweep dir current = foldl' reorder current (rowsToVisit dir current)
      where
        rowsToVisit Downward rs = drop 1 (M.keys rs)
        rowsToVisit Upward rs = drop 1 (reverse (M.keys rs))

        reorder acc l =
          M.insert l (map thd (sortOn key keyed)) acc
          where
            reference = case dir of
              Downward -> l - 1
              Upward -> l + 1
            positions = positionsIn (M.findWithDefault [] reference acc)
            neighbours n =
              M.findWithDefault [] n $ case dir of
                Downward -> above
                Upward -> below
            keyed =
              [ (medianOf (mapMaybe (`M.lookup` positions) (neighbours n)) i, i, n)
              | (i, n) <- zip [0 :: Int ..] (M.findWithDefault [] l acc)
              ]
            key (m, i, _) = (m, i)
            thd (_, _, n) = n

    -- A node with nothing to line up against stays put: its current
    -- index is its key, so it neither moves nor drags anything else.
    medianOf [] fallback = fromIntegral fallback :: Double
    medianOf ps _
      | odd n = fromIntegral (sorted !! mid)
      | otherwise = fromIntegral (sorted !! (mid - 1) + sorted !! mid) / 2
      where
        sorted = sortOn id ps
        n = length ps
        mid = n `div` 2

positionsIn :: [LNode] -> Map LNode Int
positionsIn row = M.fromList (zip row [0 ..])

{- | Count edge crossings exactly.

Two edges between the same pair of rows cross when their endpoints are
in opposite orders — so, having sorted the edges by where they start in
the upper row, the crossings are exactly the inversions in where they
end in the lower one.

Exact rather than estimated on purpose: it is what 'orderRows' compares
sweeps with, and it lets a test assert a number instead of a judgement.
-}
countCrossings :: Map Int [LNode] -> [Segment] -> Int
countCrossings rows segments =
  sum (map crossingsBelow (M.keys rows))
  where
    positions = M.unions (map positionsIn (M.elems rows))
    layerOf n = M.lookup n rowOf
    rowOf = M.fromList [(n, l) | (l, row) <- M.toList rows, n <- row]
    posOf n = M.findWithDefault 0 n positions

    crossingsBelow l =
      inversions
        [ posOf (segTo s)
        | s <- sortOn (posOf . segFrom) (segmentsFrom l)
        ]

    segmentsFrom l = [s | s <- segments, layerOf (segFrom s) == Just l]

    -- Quadratic, which is ample: a row pair carries at most as many
    -- edges as the graph has, and this runs a fixed number of times.
    inversions xs =
      length
        [ ()
        | (i, a) <- zip [0 :: Int ..] xs
        , (j, b) <- zip [0 ..] xs
        , i < j
        , a > b
        ]
