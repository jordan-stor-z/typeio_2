{- | The layout pipeline's entry point: dependency graph in, geometry
out.

See @docs/architecture/graph-rendering.md@ for the full design. What is
built so far (#173) is cycle breaking, layer assignment, a naive
within-row ordering, and straight edges. Median x placement (#174),
orthogonal routing (#175), dummy nodes for multi-row edges (#176) and
crossing reduction (#177) each replace one step below.
-}
module Domain.Project.Graph.Layout
  ( layout
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text.Util (wrapLabel)
import Domain.Project.Graph.Layer (Arc (..), assignLayers, breakCycles)
import Domain.Project.Graph.Types

{- | Lay a dependency graph out.

Total by construction: every input produces a 'Diagram', including a
cyclic one (see 'breakCycles'), a disconnected one, one with no project
root, and an empty one. Nothing here can fail, and nothing here is
allowed to reject its input — a graph that will not draw is a worse
outcome than one drawn with an edge reversed.

Deterministic: the same input yields the same 'Diagram', with every
tie broken by node id rather than by traversal order.
-}
layout :: LayoutConfig -> [LayoutNode] -> [LayoutEdge] -> Diagram
layout cfg ns es =
  Diagram
    { diagramNodes = placed
    , diagramEdges = map placeEdge es
    , diagramBounds = bounds
    , diagramRootAnchor = rootAnchor
    }
  where
    arcs = breakCycles ns es
    layers = assignLayers ns arcs

    -- Naive placement: order each row by node id and space the boxes
    -- evenly. #174 replaces this with median placement, which is what
    -- centres a node over the ones it depends on.
    rows :: Map Int [NodeId]
    rows =
      M.fromListWith
        (flip (++))
        [(layerOf (lnId n), [lnId n]) | n <- sortOn lnId ns]
    layerOf n = M.findWithDefault 0 n layers

    Size nodeW nodeH = cfgNodeSize cfg
    margin = cfgMargin cfg

    columnOf n =
      let row = M.findWithDefault [] (layerOf n) rows
       in length (takeWhile (/= n) row)

    topLeftOf n =
      Point
        (margin + fromIntegral (columnOf n) * (nodeW + cfgNodeGap cfg))
        (margin + fromIntegral (layerOf n) * (nodeH + cfgLayerGap cfg))

    placed =
      [ PlacedNode
          { pnId = lnId n
          , pnKind = lnKind n
          , pnLines = wrapLabel (cfgLabelWidth cfg) (cfgLabelLines cfg) (lnLabel n)
          , pnTopLeft = topLeftOf (lnId n)
          , pnSize = cfgNodeSize cfg
          }
      | n <- ns
      ]

    boxes = M.fromList [(pnId p, p) | p <- placed]
    centreOf p =
      Point
        (ptX (pnTopLeft p) + szW (pnSize p) / 2)
        (ptY (pnTopLeft p) + szH (pnSize p) / 2)

    reversedIds = [arcEdge a | a <- arcs, arcReversed a]

    -- A straight line between the two boxes, stopping at each box's
    -- edge rather than its centre so the arrowhead stays visible.
    -- #175 replaces this with real orthogonal routing.
    placeEdge e =
      PlacedEdge
        { peId = leId e
        , pePoints = case (M.lookup (leDependency e) boxes, M.lookup (leDependent e) boxes) of
            (Just from, Just to) -> [exitPoint from to, exitPoint to from]
            _ -> []
        , peReversed = leId e `elem` reversedIds
        }

    -- Where the line between two boxes meets the first box's edge.
    -- Vertical when they sit in different rows, horizontal when they
    -- share one.
    exitPoint from to
      | ptY cTo < ptY cFrom - halfH = Point (ptX cFrom) (ptY cFrom - halfH)
      | ptY cTo > ptY cFrom + halfH = Point (ptX cFrom) (ptY cFrom + halfH)
      | ptX cTo < ptX cFrom = Point (ptX cFrom - halfW) (ptY cFrom)
      | otherwise = Point (ptX cFrom + halfW) (ptY cFrom)
      where
        cFrom = centreOf from
        cTo = centreOf to
        halfW = szW (pnSize from) / 2
        halfH = szH (pnSize from) / 2

    bounds
      | null placed = Bounds (Point 0 0) (Point (2 * margin) (2 * margin))
      | otherwise =
          Bounds
            (Point (minimum xs - margin) (minimum ys - margin))
            (Point (maximum xs' + margin) (maximum ys' + margin))
      where
        xs = map (ptX . pnTopLeft) placed
        ys = map (ptY . pnTopLeft) placed
        xs' = map (\p -> ptX (pnTopLeft p) + szW (pnSize p)) placed
        ys' = map (\p -> ptY (pnTopLeft p) + szH (pnSize p)) placed

    rootAnchor =
      case filter ((== RootNode) . pnKind) placed of
        (p : _) -> Just (centreOf p)
        [] -> Nothing
