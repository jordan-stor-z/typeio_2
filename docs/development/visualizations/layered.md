# The Layered Visualization

`viz:layered`. This is the drawing the app has always served — the
project root heads the graph, and its edges to the work are derived
rather than stored.

For the mechanism that selects this visualization, see
[`../../architecture/visualization-switching.md`](../../architecture/visualization-switching.md).
For the layout pipeline it draws with (layer assignment, ordering,
coordinates, edge routing), see
[`../../architecture/graph-rendering.md`](../../architecture/graph-rendering.md) —
that pipeline is shared with [Rootless](rootless.md) and isn't repeated
here.

## What it draws

- **Every node in the project**, the root included.
- **Every recorded `project.dependency` edge.**
- **One derived containment edge from the root to each *head* of the
  work** — a node nothing else is waiting on — so the whole project
  hangs off the root without membership ever being duplicated as a
  stored `project.dependency` row. The root attaches only to heads, not
  to every node: attaching to everything draws the project's real shape
  and then buries it (#198, #211). See
  `Domain.Project.Graph.Containment.containmentEdges`.

Containment edges render with their own CSS class (`link-contains`) so
they read as membership rather than as a dependency, but they still
carry an arrowhead: a project's completion genuinely depends on its
work being complete (#206).

Contrast with [Rootless](rootless.md), which omits the root and derives
no containment edges at all — not depicting membership is that
visualization's entire premise.

## Selecting it

Today: `GRAPH_VISUALIZATION=Layered` in `.env`, read once at boot. There
is no default — an absent or unrecognised value fails startup rather
than silently drawing the wrong graph (see visualization-switching.md).

[#223](https://github.com/v12-Industry/typeio_2/issues/223) proposes
moving selection to a request-time `visualizationMode` query parameter
with a hardcoded default; until that lands, the environment variable
above is the only way to select this visualization.

## Where the code lives

- `Domain.Project.Visualization.Layered.Responder` — this visualization's
  entire distinguishing logic: `buildGraph` keeps every node and adds
  `containmentEdges` on top of the recorded dependencies. That's the one
  decision this visualization makes.
- Everything else is shared infrastructure it uses rather than owns:
  `Domain.Project.Graph.*` (the layout engine), and
  `Domain.Project.Visualization.Common` (request parsing, the queries,
  error responses, and the SVG vocabulary). See
  visualization-switching.md's isolation rule for what "shared" means
  here and where the line sits if this visualization ever needs its own
  document assembly.

## Testing

- **The derivation itself** (`containmentEdges`/`containmentTargets` —
  which nodes the root attaches to) is pure and unit-tested directly:
  `test/Domain/Project/Graph/ContainmentSpec.hs`.
- **The conversion, end to end** is covered by
  `test-integration/Domain/Project/Responder/Ui/ProjectManage/GraphSpec.hs`'s
  `"containment (#198)"` block, which asserts against rendered markup
  that the root draws above its work and that its edges are derived
  rather than read from a stored `project.dependency` row. The rest of
  that spec covers node chrome and viewport behaviour shared with
  Rootless, not anything specific to this visualization.
- The shared layout engine underneath both stays in the unit tier
  (pure, dependency-free), per visualization-switching.md's Testing
  section.
