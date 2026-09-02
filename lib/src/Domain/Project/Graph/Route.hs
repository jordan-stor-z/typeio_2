{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 6 of the layout pipeline: turn placed slots into orthogonal
edge polylines.

Every segment leaves its upper slot vertically, crosses horizontally
along a track reserved in the gap below it, and arrives vertically at
the slot beneath — at most two bends. A multi-row edge is several such
segments stitched together through its dummies, which is what makes it
travel in a reserved lane rather than across whatever it passes. See
@docs/architecture/graph-rendering.md@.

This module also owns __vertical__ spacing, which is why it returns the
rows' y positions alongside the edges: how tall a gap needs to be is a
function of how many horizontal runs have to fit through it, so the two
cannot be decided separately. Horizontal placement stays in
"Domain.Project.Graph.Coord".
-}
module Domain.Project.Graph.Route
  ( Routed (..)
  , routeEdges
  ) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Domain.Project.Graph.Layer (Segment (..))
import Domain.Project.Graph.Types

data Routed = Routed
  { routedEdges :: [PlacedEdge]
  , routedLayerTops :: Map Int Double
  {- ^ Top edge of each row, once every gap has been sized to fit the
  tracks running through it.
  -}
  }
  deriving (Eq, Show)

{- | Route every edge, and decide how far apart the rows have to sit to
carry them.

Points run dependency-first, so the polyline's last point is always the
end that carries the arrowhead — the dependent, the node waiting on the
work. For an edge reversed to break a cycle that means the arrow points
down the page instead of up, which is correct: the reversal is a layout
device and does not change which end depends on which.
-}
routeEdges ::
  LayoutConfig ->
  -- | Row per slot.
  Map LNode Int ->
  -- | Centre x per slot.
  Map LNode Double ->
  [Segment] ->
  -- | Each edge's chain of slots, top to bottom.
  Map EdgeId [LNode] ->
  Routed
routeEdges cfg layers centres segments chains =
  Routed
    { routedEdges = map polyline (M.toAscList chains)
    , routedLayerTops = layerTops
    }
  where
    Size nodeW nodeH = cfgNodeSize cfg
    layerOf n = M.findWithDefault 0 n layers
    centreOf n = M.findWithDefault 0 n centres

    reversedOf = M.fromList [(segEdge s, segReversed s) | s <- segments]

    -- --- Ports -------------------------------------------------------
    --
    -- Each side's slots are handed out in the order the other ends sit
    -- horizontally, so two edges meeting the same side never cross just
    -- to reach it. Every edge gets its own slot: a shared dependency
    -- draws as several arrows into distinct points on the node's edge,
    -- not one merged trunk (#169 §3, D2).
    --
    -- Only real nodes have sides to share out. A dummy is a point on a
    -- passing edge, so the edge simply runs through where it sits.
    portsOn ownerOf otherOf =
      M.fromList
        [ (segEdge s, (owner, slot, total))
        | (owner, ss) <- M.toList grouped
        , not (isDummy owner)
        , let ordered = sortOn (\x -> (centreOf (otherOf x), segEdge x)) ss
        , let total = length ordered
        , (slot, s) <- zip [1 :: Int ..] ordered
        ]
      where
        grouped = M.fromListWith (++) [(ownerOf s, [s]) | s <- segments]

    bottomPorts = portsOn segFrom segTo
    topPorts = portsOn segTo segFrom

    portX ports fallback s =
      case M.lookup (segEdge s) ports of
        Just (owner, slot, total)
          | not (isDummy (fallback s)) ->
              centreOf owner
                - nodeW / 2
                + nodeW * fromIntegral slot / fromIntegral (total + 1)
        _ -> centreOf (fallback s)

    upperX s = portX bottomPorts segFrom s
    lowerX s = portX topPorts segTo s

    -- --- Tracks ------------------------------------------------------
    needsTrack s = upperX s /= lowerX s
    spanOf s = (min (upperX s) (lowerX s), max (upperX s) (lowerX s))
    gapOf s = layerOf (segFrom s)

    byGap = M.fromListWith (++) [(gapOf s, [s]) | s <- segments, needsTrack s]

    -- Greedy interval colouring: take the runs left to right and give
    -- each the lowest track no overlapping run already occupies. Two
    -- runs may share a track whenever their spans don't overlap, which
    -- is what keeps a wide graph from stacking one track per edge.
    tracksIn :: [Segment] -> (Map EdgeId Int, Map Int [(Double, Double)])
    tracksIn ss = foldl' place (M.empty, M.empty) ordered
      where
        ordered = sortOn (\s -> (spanOf s, segEdge s)) ss
        place (assigned, occupied) s =
          let t = firstFree 0
              firstFree i
                | any (overlaps (spanOf s)) (M.findWithDefault [] i occupied) = firstFree (i + 1)
                | otherwise = i
           in (M.insert (segEdge s) t assigned, M.insertWith (++) t [spanOf s] occupied)
        overlaps (a1, b1) (a2, b2) = a1 < b2 && a2 < b1

    gapTracks = M.map tracksIn byGap
    trackOf s =
      case M.lookup (gapOf s) gapTracks of
        Just (assigned, _) -> M.findWithDefault 0 (segEdge s) assigned
        Nothing -> 0
    trackCount g =
      case M.lookup g gapTracks of
        Just (_, occupied) -> M.size occupied
        Nothing -> 0

    -- --- Vertical spacing --------------------------------------------
    gapHeight g =
      max
        (cfgLayerGap cfg)
        (fromIntegral (trackCount g + 1) * cfgTrackGap cfg)

    lastLayer = if M.null layers then 0 else maximum (M.elems layers)
    layerTops =
      M.fromList (zip [0 ..] (scanl step (cfgMargin cfg) [0 .. lastLayer - 1]))
      where
        step y g = y + nodeH + gapHeight g

    topOf l = M.findWithDefault (cfgMargin cfg) l layerTops
    trackY s =
      let g = gapOf s
          gapTop = topOf g + nodeH
          slots = fromIntegral (trackCount g + 1)
       in gapTop + gapHeight g * (fromIntegral (trackOf s) + 1) / slots

    -- --- Polylines ---------------------------------------------------
    --
    -- A dummy has no box, so an edge passing one runs from the top of
    -- its row straight to the bottom; consecutive segments then join
    -- into a single vertical run once collinear points are dropped.
    exitY n
      | isDummy n = topOf (layerOf n) + nodeH
      | otherwise = topOf (layerOf n) + nodeH
    entryY n = topOf (layerOf n)

    segmentPoints s
      | ux == lx = [Point ux (exitY (segFrom s)), Point lx (entryY (segTo s))]
      | otherwise =
          [ Point ux (exitY (segFrom s))
          , Point ux (trackY s)
          , Point lx (trackY s)
          , Point lx (entryY (segTo s))
          ]
      where
        ux = upperX s
        lx = lowerX s

    segmentsFor e = sortOn (layerOf . segFrom) (filter ((== e) . segEdge) segments)

    polyline (e, _chain) =
      PlacedEdge
        { peId = e
        , pePoints = orient (simplify (concatMap segmentPoints (segmentsFor e)))
        , peReversed = reversed
        }
      where
        reversed = M.findWithDefault False e reversedOf
        -- 'Arc' orients an edge for layout, not for reading: on an
        -- ordinary edge the upper end is the dependent, and on a
        -- reversed one it is the dependency. Either way the arrowhead
        -- belongs on the dependent, so the points end there.
        orient ps
          | reversed = ps
          | otherwise = reverse ps

{- | Drop repeated points, then fold runs of collinear points into one
segment — which is what turns an edge's per-row pieces back into a
single straight drop where it never had to change direction.
-}
simplify :: [Point] -> [Point]
simplify = collinear . dedupe
  where
    dedupe (p : q : rest)
      | p == q = dedupe (q : rest)
      | otherwise = p : dedupe (q : rest)
    dedupe ps = ps

    collinear (p : q : r : rest)
      | (ptX p == ptX q && ptX q == ptX r)
          || (ptY p == ptY q && ptY q == ptY r) =
          collinear (p : r : rest)
      | otherwise = p : collinear (q : r : rest)
    collinear ps = ps
