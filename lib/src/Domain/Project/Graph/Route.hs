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
  , addJumps
  ) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
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
    { routedEdges = addJumps (map polyline (M.toAscList chains))
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
    rawLowerX s = portX topPorts segTo s

    -- --- Separating shared columns (#190) ----------------------------
    --
    -- An edge's vertical runs sit at its own port columns: the upper one
    -- spans from the row above down to the edge's track, the lower one
    -- from that track down to the row below. Two *different* edges that
    -- happen to share a column therefore draw on top of each other
    -- wherever those spans overlap — not a crossing, an overlap, and one
    -- nothing else here prevents.
    --
    -- Reordering tracks cannot fix it. Where edge A's upper column is
    -- edge B's lower column, avoiding the overlap needs A's track above
    -- B's; two edges that *swap* ports (K(2,2)) demand that in both
    -- directions at once, so they would have to share a track — which
    -- they can't, having identical x-spans. The columns themselves have
    -- to differ.
    --
    -- So a lower port that lands on a column some other edge is already
    -- leaving from is nudged along its own node's edge until it doesn't.
    -- Only the arriving end moves, and only within its node's own width,
    -- so every other guarantee holds: the run is still vertical, still
    -- lands on the node it belongs to, and still carries the arrowhead.
    -- Every column an edge already occupies on the way *out* of the row
    -- above. Straight drops are in here too, and matter most: with no
    -- track to stop at, one occupies its column for the gap's whole
    -- height, so anything else landing on it overlaps completely.
    upperColumns :: Map Int (Set Double)
    upperColumns =
      M.fromListWith
        (<>)
        [(gapOf s, S.singleton (upperX s)) | s <- segments]

    -- A third of the node's own port spacing: enough to clear the
    -- column, never enough to reach the neighbouring port.
    nudgeStep s =
      case M.lookup (segEdge s) topPorts of
        Just (_, _, total) -> nodeW / fromIntegral (total + 1) / 3
        Nothing -> cfgDummyWidth cfg / 3

    lowerX s
      -- Both ends on one column is the straight-drop case: there is no
      -- horizontal run and nothing to separate.
      | ux == raw = raw
      | otherwise = clear raw
      where
        ux = upperX s
        raw = rawLowerX s
        claimed = M.findWithDefault S.empty (gapOf s) upperColumns
        step = max 1e-9 (nudgeStep s)
        clear x
          | x `S.member` claimed = clear (x + step)
          | otherwise = x

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
        , -- Filled in by 'addJumps' once every edge has been placed:
          -- a crossing is a fact about a pair of edges, so it cannot be
          -- known while routing one of them in isolation.
          peJumps = []
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

{- | Mark every place a horizontal run passes over a vertical one
(#180), so the renderer can draw a hop there.

Where two edges cross, nothing in an orthogonal drawing distinguishes
"these lines cross" from "these lines meet" — both are a black `+`.
A small hop in one of the two says which it is.

Only the horizontal side of each crossing is marked. Hopping both would
put two arcs at the same point and restore exactly the ambiguity the
hop exists to remove, so the choice of which line hops is arbitrary but
has to be consistent; horizontal is the conventional one.

The intersection must be __strictly inside__ both runs. Two edges
meeting at a shared port touch at an endpoint, and that is a junction,
not a crossing — drawing a hop there would claim the lines pass by each
other when they genuinely join.
-}
addJumps :: [PlacedEdge] -> [PlacedEdge]
addJumps es = map mark es
  where
    verticals =
      [ (peId e, x, min y1 y2, max y1 y2)
      | e <- es
      , (Point x y1, Point x' y2) <- runsOf e
      , x == x'
      , y1 /= y2
      ]

    mark e = e {peJumps = jumpsFor e}

    jumpsFor e =
      [ Point vx hy
      | (Point x1 hy, Point x2 hy') <- runsOf e
      , hy == hy'
      , x1 /= x2
      , (vid, vx, vTop, vBot) <- verticals
      , -- An edge crosses itself only where its own bend meets its own
      -- run, which is a corner rather than a crossing.
      vid /= peId e
      , strictlyBetween vx (min x1 x2) (max x1 x2)
      , strictlyBetween hy vTop vBot
      ]

    strictlyBetween v lo hi = v > lo && v < hi

    runsOf e = zip (pePoints e) (drop 1 (pePoints e))

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
