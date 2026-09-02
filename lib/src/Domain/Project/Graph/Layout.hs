{- | The layout pipeline's entry point: dependency graph in, geometry
out.

See @docs/architecture/graph-rendering.md@ for the full design. What is
built so far is cycle breaking and layer assignment (#173), median x
placement (#174) and orthogonal routing (#175). Dummy nodes for
multi-row edges (#176) and crossing reduction (#177) each replace one
step below.
-}
module Domain.Project.Graph.Layout
  ( layout
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text.Util (wrapLabel)
import Domain.Project.Graph.Coord (assignX)
import Domain.Project.Graph.Layer (assignLayers, breakCycles)
import Domain.Project.Graph.Route (Routed (..), routeEdges)
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
    , diagramEdges = routedEdges routed
    , diagramBounds = bounds
    , diagramRootAnchor = rootAnchor
    }
  where
    arcs = breakCycles ns es
    layers = assignLayers ns arcs

    -- Rows are ordered by node id for now. #177 reorders them to cut
    -- edge crossings; 'assignX' below is written to leave whatever
    -- order it is given intact, so the two compose without fighting.
    rows :: Map Int [NodeId]
    rows =
      M.fromListWith
        (flip (++))
        [(layerOf (lnId n), [lnId n]) | n <- sortOn lnId ns]
    layerOf n = M.findWithDefault 0 n layers

    Size nodeW _ = cfgNodeSize cfg
    margin = cfgMargin cfg

    -- 'assignX' works in its own coordinate space, which can run
    -- negative. Shift the whole drawing so the leftmost box's left edge
    -- lands on the margin, and hand the shifted centres to routing --
    -- both node boxes and edge ports have to be measured from the same
    -- origin, or the edges draw somewhere the nodes are not.
    rawCentres = assignX (nodeW + cfgNodeGap cfg) rows arcs
    shift
      | M.null rawCentres = 0
      | otherwise = margin + nodeW / 2 - minimum (M.elems rawCentres)
    centres = M.map (+ shift) rawCentres
    centreXOf n = M.findWithDefault (margin + nodeW / 2) n centres

    -- Routing owns vertical spacing: how tall a gap must be is a
    -- function of how many horizontal runs cross it, so rows cannot be
    -- positioned before the edges through them are known.
    routed = routeEdges cfg layers centres arcs

    topLeftOf n =
      Point
        (centreXOf n - nodeW / 2)
        (M.findWithDefault margin (layerOf n) (routedLayerTops routed))

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

    centreOf p =
      Point
        (ptX (pnTopLeft p) + szW (pnSize p) / 2)
        (ptY (pnTopLeft p) + szH (pnSize p) / 2)

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
