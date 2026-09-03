{- | The layered visualization: the project root heads the drawing, and
its edges to the work are derived.

This is the drawing documented in @docs/architecture/graph-rendering.md@
and selected by @GRAPH_VISUALIZATION=Layered@. Everything about /how/ it
is drawn is shared (see "Domain.Project.Visualization.Common"); what is
specific to it is here, and it is exactly one decision — that the
project root is part of the drawing.
-}
module Domain.Project.Visualization.Layered.Responder
  ( handleProjectGraph
  , buildGraph
  ) where

import Database.Persist.Sql (ConnectionPool)
import Domain.Project.Graph.Containment (containmentEdges)
import Domain.Project.Visualization.Common
  ( BuildGraph
  , handleGraphWith
  , serverGraph
  , toLayoutEdge
  , toLayoutNode
  )
import Network.Wai (Application)

handleProjectGraph :: ConnectionPool -> Application
handleProjectGraph = handleGraphWith buildGraph

{- | Every node in the project, plus the recorded dependencies and the
root's derived containment edges.

The root's edges are derived, not read (#198): membership lives in
@project.node.project_id@, and every node here came back from a query on
that column. Which nodes it attaches to is a question about the shape of
the dependencies, so it is answered in the layout tier where it can be
tested as the pure thing it is (@Graph.Containment@, #211) rather than
only through rendered SVG.
-}
buildGraph :: BuildGraph
buildGraph pid ns ds = serverGraph pid lns les
  where
    lns = map toLayoutNode ns
    deps = map toLayoutEdge ds
    les = containmentEdges lns deps <> deps
