{-# LANGUAGE ScopedTypeVariables #-}

{- | Phase 6 of the layout pipeline: turn placed nodes into orthogonal
edge polylines.

Every edge leaves its lower node vertically, crosses horizontally along
a track reserved in the gap between two rows, and arrives vertically at
its upper node — at most two bends. See
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
import Domain.Project.Graph.Layer (Arc (..))
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
  -- | Row per node.
  Map NodeId Int ->
  -- | Centre x per node.
  Map NodeId Double ->
  [Arc] ->
  Routed
routeEdges cfg layers centres arcs =
  Routed
    { routedEdges = map polyline (sortOn arcEdge arcs)
    , routedLayerTops = layerTops
    }
  where
    Size nodeW nodeH = cfgNodeSize cfg
    layerOf n = M.findWithDefault 0 n layers
    centreOf n = M.findWithDefault 0 n centres

    -- --- Ports -------------------------------------------------------
    --
    -- Each side's slots are handed out in the order the other ends sit
    -- horizontally, so two edges meeting the same side never cross just
    -- to reach it. Every edge gets its own slot: a shared dependency
    -- draws as several arrows into distinct points on the node's edge,
    -- not one merged trunk (#169 §3, D2).
    portsOn keyOf otherOf =
      M.fromList
        [ (arcEdge a, (owner, slot, total))
        | (owner, as) <- M.toList grouped
        , let ordered = sortOn (\x -> (centreOf (otherOf x), arcEdge x)) as
        , let total = length ordered
        , (slot, a) <- zip [1 :: Int ..] ordered
        ]
      where
        grouped = M.fromListWith (++) [(keyOf a, [a]) | a <- arcs]

    -- An arc leaves the bottom of its upper node and arrives at the top
    -- of its lower one.
    bottomPorts = portsOn arcFrom arcTo
    topPorts = portsOn arcTo arcFrom

    portX ports a =
      case M.lookup (arcEdge a) ports of
        Nothing -> centreOf (arcFrom a)
        Just (owner, slot, total) ->
          centreOf owner
            - nodeW / 2
            + nodeW * fromIntegral slot / fromIntegral (total + 1)

    upperX a = portX bottomPorts a
    lowerX a = portX topPorts a

    -- --- Tracks ------------------------------------------------------
    --
    -- Only edges that actually travel sideways need one; a straight
    -- vertical drop occupies no track and costs the gap nothing.
    needsTrack a = upperX a /= lowerX a
    spanOf a = (min (upperX a) (lowerX a), max (upperX a) (lowerX a))
    gapOf a = layerOf (arcFrom a)

    byGap =
      M.fromListWith
        (++)
        [(gapOf a, [a]) | a <- arcs, needsTrack a]

    -- Greedy interval colouring: take the runs left to right and give
    -- each the lowest track no overlapping run already occupies. Two
    -- runs may share a track whenever their spans don't overlap, which
    -- is what keeps a wide graph from stacking one track per edge.
    tracksIn :: [Arc] -> (Map EdgeId Int, Map Int [(Double, Double)])
    tracksIn as = foldl' place (M.empty, M.empty) ordered
      where
        ordered = sortOn (\a -> (spanOf a, arcEdge a)) as
        place (assigned, occupied) a =
          let t = firstFree 0
              firstFree i
                | any (overlaps (spanOf a)) (M.findWithDefault [] i occupied) = firstFree (i + 1)
                | otherwise = i
           in ( M.insert (arcEdge a) t assigned
              , M.insertWith (++) t [spanOf a] occupied
              )
        overlaps (a1, b1) (a2, b2) = a1 < b2 && a2 < b1

    gapTracks = M.map tracksIn byGap
    trackOf a =
      case M.lookup (gapOf a) gapTracks of
        Just (assigned, _) -> M.findWithDefault 0 (arcEdge a) assigned
        Nothing -> 0
    trackCount g =
      case M.lookup g gapTracks of
        Just (_, occupied) -> M.size occupied
        Nothing -> 0

    -- --- Vertical spacing --------------------------------------------
    --
    -- A gap is at least cfgLayerGap tall, and taller when its tracks
    -- need the room.
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
    trackY a =
      let g = gapOf a
          gapTop = topOf g + nodeH
          slots = fromIntegral (trackCount g + 1)
       in gapTop + gapHeight g * (fromIntegral (trackOf a) + 1) / slots

    -- --- Polylines ---------------------------------------------------
    polyline a =
      PlacedEdge
        { peId = arcEdge a
        , pePoints = orient (dedupe points)
        , peReversed = arcReversed a
        }
      where
        ux = upperX a
        lx = lowerX a
        fromUpper = Point ux (topOf (layerOf (arcFrom a)) + nodeH)
        toLower = Point lx (topOf (layerOf (arcTo a)))
        points
          | ux == lx = [fromUpper, toLower]
          | otherwise =
              [ fromUpper
              , Point ux (trackY a)
              , Point lx (trackY a)
              , toLower
              ]
        -- 'Arc' orients an edge for layout, not for reading: on an
        -- ordinary edge the upper node is the dependent, and on a
        -- reversed one it is the dependency. Either way the arrowhead
        -- belongs on the dependent, so the points end there.
        orient ps
          | arcReversed a = ps
          | otherwise = reverse ps

    dedupe (p : q : rest)
      | p == q = dedupe (q : rest)
      | otherwise = p : dedupe (q : rest)
    dedupe ps = ps
