# Solution Proposal: Rendering the Dependency Graph from Haskell

- **Status:** Proposed — recommendation in §10, delivery plan in §11. Not
  yet decided, and nothing here has been built. (Per `CLAUDE.md`'s #50
  note: this document's existence is not evidence any of it landed.)
- **Date:** 2026-09-02
- **Related:** #169 (this spike), #162 (replaced the force simulation
  with the current hand-rolled radial layout — see §2, it changes the
  premise), #120 (the positioning bug that motivated #162),
  `docs/development/ui/haskell-rendering.md` (Lucid/SVG conventions this
  builds on), `docs/development/frontend/index.md` (currently documents
  D3 as what renders the graph), `docs/development/unit-testing.md`
  (what's in scope for the unit suite — §7 leans on this),
  `docs/development/backend/database-schema.md` (`project.dependency`'s
  shape and its explicit lack of cycle prevention — see §8.1).

## 1. Problem statement

The ticket asks whether the dependency graph can be computed and
rendered in Haskell instead of D3, on two stated grounds:

1. D3's force layout has constraints that make total customization
   difficult if not impossible.
2. It forces computation onto the client browser, which is bad UX.

**Premise 1 is now stale, and saying so up front changes what this
proposal has to argue.** The force simulation is already gone. #162
(merged 2026-09-01, the day before this spike was filed) replaced
`d3.forceSimulation` with a hand-written radial tidy-tree
layout in `static/script/nodetree2.js` — BFS spanning tree, angular
wedge allocation, explicit radius scaling. Nothing in the current graph
is force-directed, and the layout is already fully custom. What D3 still
provides on that page is `d3-selection` (binding the computed positions
onto the pre-rendered SVG elements), `d3-transition` (the 300ms
fade-in), and `d3-zoom` (pan/zoom).

So the honest case for this change rests on premise 2 plus a third thing
the ticket implies but doesn't state: **the target look in the attached
reference images is not the layout the app currently draws.** The app
draws concentric rings of circles around the project root; the images
show a layered, orthogonal, boxes-and-right-angles diagram. That is a
different layout algorithm regardless of which language computes it —
and once it has to be rewritten anyway, "in Haskell, server-side, where
it is pure and testable" becomes a genuinely cheap upgrade rather than a
rewrite for its own sake.

This proposal answers the three questions the ticket asks — how Haskell
would calculate such a layout (§4), what the images actually require
(§3), and how Haskell would render it (§5) — then proposes the issue
breakdown to deliver it (§11).

## 2. What's actually there today

Verified by reading the code, not assumed:

| Piece | Where | What it does |
|---|---|---|
| Graph data | `ProjectManage/Graph.hs` `toGraph` | Queries nodes + dependencies, serialises `{nodes, links}` to JSON, embeds it in a `<script id="graph-data">` |
| SVG skeleton | `ProjectManage/Graph.hs` `templateGraph` | Emits one empty `<path class="link">` per edge and one `<g class="node">` (circle + wrapped `<tspan>` label) per node — **with no coordinates** |
| Layout | `static/script/nodetree2.js` (220 lines) | BFS spanning tree from the project root, angular-wedge allocation, ring radii, then writes `transform`/`d` onto the elements above |
| Pan/zoom | same file | `d3.zoom()` on the `<svg>`, transforming `.zoom-group` |
| D3 itself | `static/script/d3.js` | **279,706 bytes**, v7.9.0, the full build, loaded from `IndexView.hs`'s `<head>` on **every page in the app**, not just the graph page |

Two things worth pulling out of that table:

- **The server already emits every node and edge element.** Only the
  geometry is client-side. Moving layout to Haskell means filling in
  attributes the server is already emitting the elements for — not
  inventing a new rendering path.
- **D3 is loaded globally.** The project index, the create-project form
  and every other view pay for 280KB of D3 they never touch. Removing it
  is a whole-app win, not just a graph-page one.

Also found while reading, and out of scope here (filed separately per
`CLAUDE.md`'s out-of-scope-findings rule): `static/script/nodetree.js` —
the pre-#162 force-simulation script — is dead code, referenced from
nothing.

## 3. Requirements derived from the reference images

The ticket attaches five images and asks for the requirements they
imply. Each requirement below cites the image(s) that evidence it.
Images 1–3 are hand-drawn-style (white boxes, dark outlines); images 4–5
are from a diagramming tool (blue boxes). Where they disagree, that is
called out rather than averaged.

**Structure**

- **R1 — Layered.** Nodes sit in discrete rows; every node in a row
  shares a baseline. Rows correspond to dependency depth. *(all five)*
- **R2 — Columnar alignment.** Nodes align on a small set of x
  positions across rows, rather than being free-floating. *(1, 2, 5 —
  image 5's four columns repeat exactly across its five rows)*
- **R3 — A parent sits centred over its children where it can.** *(1:
  P is centred between its two W's; 2: the chain is vertically
  colinear)*
- **R4 — Uniform node boxes.** Every node is the same size, a rounded
  rectangle with a centred single-line label. *(all five — note this
  differs from the app's current circles)*
- **R5 — Two visually distinct node kinds.** `P` (project root) and `W`
  (work) are labelled distinctly; image 5 also shows P participating in
  the graph like any other node rather than being a special centre.
  *(all five)*

**Edges**

- **R6 — Orthogonal routing only.** Every edge is a polyline of
  axis-aligned segments joined at right angles. No curves, no diagonals,
  anywhere in any image. *(all five)*
- **R7 — Directed, with an arrowhead at one end**, entering the target
  perpendicular to the side it meets. *(all five)*
- **R8 — Edges attach to any of the four sides.** Top and bottom for
  same-column edges; left and right for edges arriving from elsewhere.
  *(3: into P's right side and the far-right W's left side; 4: into the
  bottom W's left and right sides; 5: all four sides in use)*
- **R9 — Multiple edges into one node get distinct, spread ports.**
  Verified by cropping image 5: three separate arrowheads land on one
  node's top edge at three distinct x offsets, and two more land on
  another node's right edge at two distinct y offsets. They are not
  merged. *(5)*
- **R10 — Horizontal runs occupy distinct tracks.** In the gap between
  two rows, different edges' horizontal segments sit at different y
  values so they never overlap collinearly. Image 5's row-1→row-2 gap
  carries two clearly separated tracks. *(5)*
- **R11 — Edges spanning more than one row route around, not through.**
  A long edge leaves its source, travels in a channel clear of the
  intervening rows, and comes back in at its target — including all the
  way around the outside of the drawing. *(3: around the right side into
  P; 4: down the left margin; 5: around the bottom and back up the left)*
- **R12 — Crossings are allowed.** No image tries to be planar; image 5
  is explicitly the "lines that cross" example. *(5)*
- **R13 — Crossings are drawn with a line jump.** Cropping image 5's
  busiest junction shows a small hop where a horizontal segment crosses
  a vertical one, so the reader can see the lines don't connect. *(5)*
- **R14 — Shared dependencies draw as several ordinary edges** to/from
  the shared node, not as anything special-cased. *(3, 5)*

**Canvas**

- **R15 — The whole graph fits a bounded canvas with margins**, at a
  consistent scale, with the drawing centred in it. *(all five)*

**Where the images disagree — decisions, not requirements**

- **D1 — Flow direction.** Images 1–3 put the project root at the *top*
  with arrows pointing *up* into it; images 4–5 point arrows *down*
  (image 5 puts P at the *bottom*). These are opposite conventions for
  the same relationship. This is a one-line difference in the renderer
  (which end of the layer range is y=0, and which end of each edge gets
  the marker), not an algorithmic one. **Recommend images 1–3's
  convention** — root at top, arrows pointing up from dependency to
  dependent — since it matches the majority of the images and the app's
  existing "project root is the anchor" framing.
- **D2 — Merged trunks vs. per-edge arrowheads.** Images 1 and 2 merge
  sibling edges into one trunk with a *single* arrowhead into the
  target; images 4 and 5 give every edge its own arrowhead (R9).
  **Recommend per-edge (R9)**: it is strictly simpler, and it keeps each
  edge individually addressable in the DOM, which merged trunks would
  destroy (no per-dependency hover/highlight later). Merged trunks are
  listed as an optional refinement in §11.
- **Not requirements.** The grid backgrounds, the circular selection
  handles on images 4–5, and image 4/5's title chips are artifacts of the
  tools the diagrams were drawn in.

## 4. How Haskell would calculate the layout

Requirements R1–R14 describe, almost exactly, a **layered (Sugiyama)
graph drawing** — the same family of algorithm behind Graphviz's `dot`
and Mermaid's flowcharts, which is unsurprising given images 4–5 look
like they came from one. That is a well-specified pipeline of pure
passes over immutable data, which is the best possible news for doing it
in Haskell: every phase is a total function from one graph
representation to the next, with no I/O, no mutation, and no ordering
subtleties beyond what the types already carry.

The pipeline, and the specific choice recommended at each phase:

### 4.1 Normalise and break cycles

**Why it's needed:** `project.dependency` has `UNIQUE (node_id,
to_node_id)`, which prevents duplicate edges but — as
`database-schema.md` states explicitly — **does not prevent cycles**,
and nothing in the application enforces acyclicity either. Layering is
only defined on a DAG, so a cycle would make layer assignment
non-terminating or arbitrary. Today's radial layout never had to care
(it does undirected BFS, where a cycle is just an edge it doesn't use);
a layered layout must.

**Approach:** depth-first search from every unvisited node; any edge
found pointing back at a node currently on the DFS stack is a back edge.
Reverse those edges for layout purposes, keeping a flag so the renderer
can draw the arrowhead at the original end. Ties broken by node id so
the choice is deterministic across runs (§8.3).

```haskell
breakCycles :: [Edge] -> ([Edge], Set EdgeId)   -- (acyclic edges, reversed set)
```

This is not the minimum feedback arc set (NP-hard); DFS back-edge
reversal is the standard, linear-time, good-enough answer used by every
practical implementation.

### 4.2 Assign layers

**Longest-path layering:** `layer n = 0` for a node with no
dependencies, otherwise `1 + max (layer of each dependency)`. Computed
as a memoised fold over a topological order — linear, and it guarantees
every edge points strictly downward by at least one layer, which is what
the rest of the pipeline assumes.

The alternative worth naming is *network-simplex* layering (Graphviz's
default), which minimises total edge length and produces visibly tighter
drawings. It is substantially more code. **Recommend longest-path
first**, with network simplex explicitly available later as a drop-in
replacement for this one function if drawings look too stretched — the
phase boundary makes that a contained change, which is the point of
keeping the phases separate.

Disconnected components (a node with no path to the project root — the
current JS defensively handles this, so the data evidently can produce
it) are layered independently and packed side by side in §4.5.

### 4.3 Insert dummy nodes for long edges

An edge spanning layers 2→5 is replaced by a chain 2→3→4→5 through
*dummy* nodes, one per intervening layer. After this pass **every edge
connects adjacent layers**, which is what makes the remaining phases
simple, and it is also precisely what satisfies **R11**: because dummies
occupy real slots in their layers' orderings, the intervening rows'
ordering phase (§4.4) reserves horizontal space for the long edge to
pass through, so it routes *around* the nodes rather than over them. The
"long edge takes the outside lane" look in images 3, 4 and 5 falls out
of this rather than needing a special case: an edge whose endpoints are
far apart tends to get pushed to the margin by crossing reduction.

```haskell
data LNode = Real NodeId | Dummy EdgeId Int   -- which edge, which layer
```

### 4.4 Order within layers (crossing reduction)

**Iterated median heuristic:** initialise each layer's order by a DFS
from the roots, then sweep down (order each layer by the median position
of each node's neighbours in the layer above) and up (same, using the
layer below), alternating for a fixed number of passes, keeping the
ordering with the fewest crossings seen. Median is the standard choice
over barycentre for handling uneven degrees; a fixed pass count (4 down/
up pairs is the usual figure) keeps runtime bounded and output
deterministic.

Crossings are counted exactly — for two adjacent layers with a fixed
ordering, counting crossings is a straightforward inversion count — so
"did this change make it better" is a *number*, not a judgement, which
matters for both the algorithm's own stopping condition and its tests
(§7).

This phase is what **R12** needs (crossings tolerated but minimised) and
what makes image 5's shape achievable.

### 4.5 Assign coordinates

- **y** is trivial: `y = layer * (nodeHeight + layerGap)`.
- **x** uses the **priority/median method**: each node wants to sit at
  the median x of its neighbours in the previous layer; conflicts are
  resolved by processing nodes in priority order (dummies first — long
  edges should stay straight — then by degree) and pushing lower-priority
  neighbours aside to maintain minimum separation. A few down/up sweeps
  converge.

This is what produces **R3** (parents centred over children) and **R2**
(consistent columns), and keeping long-edge dummy chains straight is
what stops image 5's cross-level edges from looking like staircases.

*Brandes–Köpf* is the better-known alternative (it guarantees straight
long-edge chains and at most two bends per edge). It is roughly four
times the code for a visible-but-not-dramatic improvement. **Recommend
priority/median first**, with Brandes–Köpf as an explicitly optional
follow-up issue (§11) — again contained to one function.

Components are packed left to right by placing each component's bounding
box after the previous one plus a gap.

### 4.6 Route edges orthogonally

With every edge now spanning exactly one layer, each edge is drawn as at
most **two bends**:

```
   (source port, on the source's bottom edge)
        │
        │  vertical
        ├──────────────────────  horizontal, on this edge's assigned track
                               │
                               │  vertical
                               ▼
                   (target port, on the target's top edge)
```

Three sub-decisions, each mapping directly to a requirement:

- **Ports (R8, R9).** Each node side is divided into slots; an edge
  claims the slot whose position best matches the direction it comes
  from, and the slots on a side are ordered by the opposite endpoint's x
  (or y) so that edges attaching to the same side never cross each other
  right at the boundary. Same-column edges use top/bottom; edges arriving
  from far off to the side use left/right, which is what image 3's
  side-entries and image 5's stacked right-edge arrivals need.
- **Tracks (R10).** The gap between two layers is divided into
  horizontal tracks. Each edge with a horizontal run is assigned a track
  such that no two edges share both a track and an overlapping x
  interval — a straightforward interval-graph colouring, greedy by span.
  The gap's height is then set from the number of tracks actually used,
  so simple graphs stay tight and busy ones get room.
- **Line jumps (R13).** After routing, crossings between a horizontal
  and a vertical segment are computed pairwise and the horizontal
  segment's path gets a small arc inserted at each crossing. This is
  pure geometry on the already-computed polylines, and it is the one
  piece that is genuinely optional polish — hence its own issue in §11.

### 4.7 The shape of it, in types

The whole pipeline is one pure function, and that is the single most
important property of this design:

```haskell
-- Domain.Project.Graph.Layout
layout :: LayoutConfig -> [LayoutNode] -> [LayoutEdge] -> Diagram

data Diagram = Diagram
  { diagramNodes  :: [PlacedNode]   -- id, label lines, kind, x, y, w, h
  , diagramEdges  :: [PlacedEdge]   -- id, polyline points, arrow end, reversed?
  , diagramBounds :: Bounds         -- for the SVG viewBox
  }
```

`LayoutNode`/`LayoutEdge` are plain records — **no `persistent`
entities, no `Esqueleto`, no `Text` formatting concerns, nothing from
`Database.*`**. That is deliberate and load-bearing: it keeps every
module under `Domain.Project.Graph.*` inside the "pure, dependency-free"
tier that `docs/development/unit-testing.md` says is exactly what the
unit suite covers (§7). The responder converts entities into these
records; the layout engine never learns where they came from.

### 4.8 One thing the server genuinely cannot do

**The server cannot measure rendered text.** A browser can ask for a
string's pixel width in the actual font; Haskell cannot. Layered layout
needs node dimensions up front.

This is already solved in this codebase, which is worth noticing:
`Data.Text.Util.wrapLabel` (added for #162) wraps a title to a fixed
character budget and ellipsises the rest, precisely so a node's size
doesn't depend on its label. **Keep that inversion** — fix the node box
size, wrap the label to fit it, and the "can't measure text" problem
disappears entirely rather than needing a font-metrics table. Node boxes
being uniform is R4 anyway, so the constraint and the design agree.

## 5. How Haskell would render it

Rendering is the easy half, and mostly already exists.

`templateGraph` today emits the right elements with no geometry, then
lets JS fill the geometry in. The change is to emit the geometry too, in
the same pass, from the `Diagram` computed in §4 — and to drop the
`<script id="graph-data">` JSON payload entirely, since nothing on the
client needs the graph's data any more.

```haskell
templateGraph :: Diagram -> Html ()
templateGraph d =
  svg_ [ id_ "tree-view", viewBox_ (boundsViewBox (diagramBounds d)), ... ] $ do
    defs_ [] arrowMarker
    g_ [id_ "graph-links"] $ forM_ (diagramEdges d) $ \e ->
      path_ [ class_ "link", d_ (polylinePath e), markerEnd_ "url(#arrow)" ] mempty
    g_ [id_ "graph-nodes"] $ forM_ (diagramNodes d) $ \n ->
      g_ [ id_ ("node-" <> intToText (placedId n))
         , class_ "node"
         , transform_ ("translate(" <> ...)
         , hxGet_ (nodePanelLink ...), hxTarget_ "#node-panel", ... ] $ do
        rect_ [ class_ (kindClass n), rx_ "6", width_ ..., height_ ... ] mempty
        text_ [...] (labelTspans (placedLabel n))
```

Four notes on that:

- **SVG, not HTML/CSS boxes.** Positioned `<div>`s could draw the nodes,
  but R6/R7/R13's multi-bend arrowed polylines and line jumps are
  awkward-to-absurd in CSS, and the app already renders SVG here. SVG
  keeps one primitive for both.
- **`viewBox` replaces the fit-to-screen JS.** The layout's own bounding
  box goes straight into `viewBox`, and the SVG scales itself to its
  container with zero script. The current implementation computes an
  initial zoom transform in JS to achieve the same thing (§2); that code
  disappears.
- **New Lucid vocabulary goes in the established place.** `rect_`,
  `transform_`, `rx_`, `x_`/`y_`/`width_`/`height_` and `viewBox_` (some
  already present) belong in `Common.Web.Elements`/`Common.Web.Attributes`
  — `docs/development/ui/haskell-rendering.md` names those as the
  extension point for exactly this.
- **The DOM contract must not change.** `#graph-nodes`, `#graph-links`,
  `.node`, `#node-<id>`, `.node-highlight`, the `hx-get`/`hx-target`
  wiring, and the per-node refresh hook are all depended on by
  `manage-project.css`, the node-detail panel's htmx interactions, and
  `e2e/tests/graph.spec.ts`. Keeping the same ids and classes means the
  existing e2e suite keeps passing across the cutover — which makes it a
  regression check on the rewrite, for free. `circle` → `rect` is the one
  unavoidable exception (CSS selectors and one e2e assertion need
  updating with it).

## 6. What happens to D3

After §4 and §5, the graph page needs D3 for exactly one thing: pan and
zoom. Selection and transitions go away with the layout code (the
"reveal when settled" fade exists because a force simulation used to
need settling; a server-rendered diagram is already final when it
arrives).

Options for pan/zoom, in preference order:

1. **~50 lines of vanilla JS driving `viewBox`.** `wheel` adjusts the
   viewBox's width/height about the cursor; `pointerdown`/`move`/`up`
   translates its origin. No dependency, and it is genuinely the whole
   behaviour — d3-zoom's bulk is in features this page never uses
   (touch gesture arbitration, transition interpolation, extent
   constraints, programmatic transforms).
2. **CSS-only:** an `overflow: auto` container plus a zoom control that
   sets a `scale()` transform. Less smooth, near-zero code.
3. **Keep D3 for zoom alone.** Rejected: 280KB, loaded app-wide, for one
   behaviour.

Either of 1 or 2 lets `static/script/d3.js` (279,706 bytes),
`static/script/nodetree2.js` and the dead `nodetree.js` all be deleted,
and the `<script src="/static/script/d3.js">` tag come out of
`IndexView.hs`'s `<head>` — which is where the "computation on the
client" objection in the ticket actually gets paid off: not just moved
work, but ~280KB that every page in the app stops downloading and
parsing, plus a graph that is complete in its first paint instead of
after a script runs.

## 7. Why this is a real win beyond the ticket's two reasons

**The layout becomes testable, and today it is completely untested.**

`nodetree2.js` contains the entire layout algorithm and has no test
coverage of any kind. The repo does have a JS test runner — Playwright —
but it drives a browser against the finished page; it has no way to call
`nodetree2.js`'s layout code directly. The only automated check that
touches the algorithm at all is `e2e/tests/graph.spec.ts`'s assertion
that four nodes end up at finite, non-overlapping coordinates. That is a
smoke test, not a specification.

Moving the algorithm into `Domain.Project.Graph.*` as pure functions
puts it squarely in the tier `docs/development/unit-testing.md` says the
unit suite exists for ("pure, dependency-free modules"), alongside
`Common.Validation`, `Data.Either` and `Data.Text.Util`. The properties
worth asserting are unusually crisp for UI code:

- No two node boxes overlap, on every fixture.
- Every edge polyline consists only of axis-aligned segments (R6).
- Every edge connects the ports it claims, on the sides it claims (R8).
- Every non-reversed edge points from a lower layer to a higher one.
- A cyclic input still terminates and produces a complete diagram (§4.1).
- Crossing count on a set of fixtures does not regress — a plain number
  in an assertion, which is rare and valuable for a layout algorithm.
- Determinism: the same input yields byte-identical output (§8.3).

None of those are expressible today. All of them are ordinary Hspec
assertions once the layout is a pure Haskell function.

## 8. Risks, constraints and open questions

### 8.1 Cycles in the data (must be handled, not assumed away)

Covered in §4.1. Flagged separately here because it is the one input
condition that can make a layered layout fail outright rather than look
bad, and because the database explicitly permits it while the current
layout is accidentally immune to it. Whether reversed edges should be
*visually* marked (dashed, or a different colour) is an open question —
recommend not, initially, matching #162's conclusion that marking
"special" edges added noise readers didn't ask for.

### 8.2 Response size and large graphs

Server-rendered geometry means the HTML response carries every
coordinate. A 200-node graph is perhaps 60–100KB of SVG — still far less
than the 280KB of D3 it replaces, and gzip handles repetitive path data
well. The pipeline itself is linear-ish per phase with bounded sweeps,
so compute time is not the concern; response size is the thing to keep
an eye on. **Open question:** whether a node-count threshold should fall
back to something simpler. Recommend measuring before building anything
for it.

### 8.3 Layout stability

Adding one node should not reshuffle the whole drawing — a user who
adds a task and finds the graph rearranged has lost their bearings.
Every tie-break in §4 must therefore be resolved deterministically by
node id, never by traversal-order accident. This is cheap to do up front
and expensive to retrofit. Note that it delivers *determinism* (same
input, same output), not *stability under change* (similar input,
similar output) — the stronger property, if it turns out to matter,
means seeding the ordering phase from the previous layout, which is a
much larger design change and is explicitly out of scope here.

### 8.4 Interaction parity

Click-to-open-panel is htmx and unaffected. Highlight/flash are CSS on
`.node`, unaffected apart from the `circle` → `rect` selector change.
Pan/zoom is §6. Nothing else on that page touches the graph.

### 8.5 Not in scope

Dragging nodes to reposition them by hand, collapsing subtrees,
incremental/animated relayout, and edge labels. None are visible in the
reference images or the current app.

## 9. Options considered

| Option | Verdict |
|---|---|
| **Keep D3, restyle the existing radial layout** | Rejected — the reference images are layered/orthogonal; no amount of restyling turns concentric rings into that, so the layout is being rewritten either way. |
| **Keep layout on the client, hand-written JS (status quo, post-#162)** | Rejected — this is what exists; it leaves the algorithm untestable (§7) and the 280KB D3 dependency in place for pan/zoom alone. |
| **Server-side layout via Graphviz (`dot`) as a subprocess** | Rejected — produces exactly this layout and is battle-tested, but adds a native binary to every deployment and to CI, and makes node identity/htmx wiring awkward (parsing `dot`'s output back into per-node elements). The algorithm is not the hard part; the integration would be. |
| **A Haskell graph-layout library from Hackage** | Investigated as an option, not recommended as a dependency: the drawing-oriented packages (`graphviz` bindings, `diagrams`) either wrap the same subprocess or bring a very large rendering stack for a job that is ~600 lines of pure code with no runtime dependency at all. Worth a second look at package-selection time, not worth blocking on. |
| **Server-side layout, hand-written in Haskell (this proposal)** | **Recommended** — see §10. |

## 10. Recommendation

Build it, in Haskell, as a pure layered-graph-drawing pipeline
(§4), rendered as server-side SVG via Lucid (§5), and retire D3
entirely (§6).

The case, in order of weight:

1. **The layout has to be rewritten regardless**, because the target
   look in the reference images is a different algorithm from what the
   app draws today (§1). Given a rewrite, choosing "pure, server-side,
   unit-tested" over "untested JS" is close to free.
2. **It makes the app's most complex piece of logic testable** for the
   first time (§7).
3. **It removes 280KB of JS from every page** and makes the graph
   complete on first paint (§6).
4. It fits the codebase's grain: Lucid-rendered HTML, no client-side
   application logic, pure modules under test — the same reasoning
   `docs/development/frontend/index.md` already applies to everything
   except this one page.

The honest counterweight: this is **a substantial amount of new
code** — realistically 500–700 lines of layout plus tests — to replace
220 lines of working JS, and the algorithm has real subtleties (crossing
reduction and coordinate assignment are where layout engines earn their
reputation). §11's breakdown is shaped to de-risk exactly that: the old
path stays live behind a flag until the new one is demonstrably better,
and each phase lands separately with its own tests rather than as one
large drop.

## 11. Delivery plan: proposed issues

Shaped to INVEST. Two structural choices make that possible, and they
are worth stating before the list:

- **A `?layout=server` query flag** on the graph view, added by issue 1
  and removed by issue 9. Every issue in between changes only the new
  path, so each one is independently mergeable and independently
  revertable, and `main` keeps a working graph the whole way through.
  Without it, issues 2–8 would each be "half a rewrite" and none could
  ship alone.
- **Phase boundaries as module boundaries** (§4.7). Each issue owns one
  function in the pipeline, which is what keeps them small and separately
  testable rather than one 700-line drop.

Sizes are S (<½ day), M (~1 day), L (multi-day).

### Core sequence

**1. `feat: server-computed graph layout behind a ?layout=server flag`**
`type:feature`, `area:backend`, `area:ui` — **M**
- The walking skeleton: `Domain.Project.Graph.{Types,Layer}`, cycle
  breaking (§4.1) and longest-path layering (§4.2); naive x (index
  within layer); straight vertical edges; rounded-rect nodes; `viewBox`
  from the layout bounds. Rendered only when `?layout=server` is present.
- *Value:* a real, viewable, server-rendered graph on day one — a simple
  chain project already looks like reference image 2.
- *AC:* `?layout=server` renders every node and edge with server-computed
  coordinates and no `#graph-data` JSON; without the flag the existing D3
  path is untouched; unit tests cover layering, including a cyclic input.

**2. `feat: median x-coordinate assignment so parents centre over their children`**
`type:feature`, `area:backend` — **M** — *needs 1*
- `Domain.Project.Graph.Coord`, priority/median method with separation
  enforcement (§4.5).
- *Value:* satisfies R2/R3 — the layout stops looking like a left-aligned
  list and starts looking like the reference images.
- *AC:* a parent with two children is centred between them (image 1); no
  two boxes overlap on any fixture; unit tests assert both.

**3. `feat: orthogonal edge routing with node ports and horizontal tracks`**
`type:feature`, `area:backend` — **L** — *needs 2*
- `Domain.Project.Graph.Route`: ports per side (R8/R9), track assignment
  in the inter-layer gap (R10), two-bend polylines, arrowheads oriented
  to the side they enter (R7).
- *Value:* the single most visually defining requirement — this is what
  makes it read as the reference images rather than a generic node graph.
- *AC:* every emitted segment is axis-aligned; two edges never share a
  track and an overlapping x-interval; multiple edges into one node land
  on distinct ports; unit tests assert each.

**4. `feat: route multi-level edges through dummy nodes`**
`type:feature`, `area:backend` — **M** — *needs 3*
- Dummy-node insertion and the corresponding polyline reconstruction
  (§4.3), delivering R11.
- *Value:* reference images 3, 4 and 5 are all unreachable without it —
  today such an edge would cut straight through intervening rows.
- *AC:* an edge spanning ≥2 layers renders as a polyline that intersects
  no node box; unit test asserts the no-intersection property directly.

**5. `feat: crossing reduction via iterated median sweeps`**
`type:feature`, `area:backend` — **L** — *needs 4*
- `Domain.Project.Graph.Order` (§4.4), plus an exact crossing counter.
- *Value:* the difference between image 5 being legible and being a
  tangle; the first four issues produce a correct drawing, this one
  produces a *readable* one.
- *AC:* crossing count on a set of committed fixtures is at or below a
  recorded baseline (regression-tested as a number); output is
  deterministic across runs.

**6. `feat: node chrome — rounded rects, wrapped labels, root vs work styling`**
`type:feature`, `area:ui`, `run-e2e` — **M** — *needs 1*
- R4/R5: `rect` + `rx`, `wrapLabel` reused at the new box width, the
  `root`/`work` fill distinction, and `.node-highlight`/`.flash` CSS
  ported from `circle` to `rect`.
- *Value:* independently visible polish; can land any time after 1.
- *AC:* nodes match the reference images' box style; highlight and flash
  behave as they do today; `graph.spec.ts` passes against the flagged
  path.

**7. `feat: replace d3-zoom with viewBox-based pan and zoom`**
`type:feature`, `area:frontend`, `run-e2e` — **M** — *independent*
- §6 option 1, ~50 lines, wired to the new SVG.
- *Value:* the last functional dependency on D3; can be developed and
  merged in parallel with 2–6 since it touches no layout code.
- *AC:* wheel zooms about the cursor and drag pans, on the flagged path;
  no D3 call remains in the new script.

**8. `feat: line jumps where edges cross`**
`type:feature`, `area:backend` — **S** — *needs 3* — **negotiable**
- R13's crossing hops (§4.6), pure geometry over the finished polylines.
- *Value:* readability at crossings; explicitly the first thing to drop
  if the effort needs trimming.
- *AC:* a fixture with a known crossing renders an arc at it; no arc
  where segments merely touch at a shared port.

**9. `feat: make the server-computed layout the default and remove the flag`**
`type:feature`, `area:backend`, `area:ui`, `run-e2e` — **S** — *needs 2–7*
- Flip the default, delete the flag and the old template branch, update
  `graph.spec.ts` for `rect`.
- *Value:* the actual cutover — everything before this is behind a flag.
- *AC:* the graph renders server-side with no query parameter; the full
  e2e suite passes; no `#graph-data` element remains in the response.

**10. `chore: delete D3 and the nodetree scripts`**
`type:chore`, `area:frontend` — **S** — *needs 9*
- Remove `static/script/d3.js`, `nodetree2.js`, the dead `nodetree.js`,
  and the `<script>` tag in `IndexView.hs`.
- *Value:* ~280KB off every page in the app; the payoff for the whole
  effort, and deliberately its own issue so the win is visible in one
  diff rather than buried in the cutover.
- *AC:* no D3 reference remains anywhere in the repo; every page still
  works; e2e passes.

**11. `docs: document the Haskell graph-rendering pipeline`**
`type:documentation` — **M** — *needs 9*
- New `docs/development/ui/graph-rendering.md` (the pipeline as built),
  plus corrections to `docs/development/frontend/index.md` (which
  currently documents D3 as what renders the graph),
  `docs/development/ui/haskell-rendering.md`'s "Passing server data to
  client JS" section (the `#graph-data` pattern it describes would no
  longer exist), and `docs/development/e2e-testing.md`'s "the D3-rendered
  dependency graph" phrasing.
- *Value:* three existing docs become actively wrong at issue 9; this is
  what stops that.
- *AC:* the new doc describes each phase and where it lives; no doc still
  claims D3 renders the graph.

### Optional refinements (file only if wanted; none block the effort)

**12. `feat: Brandes–Köpf coordinate assignment`** — `type:feature`,
`area:backend`, **L**. Straighter long-edge chains, at most two bends
per edge; swap for §4.5's function if drawings look kinked.

**13. `feat: network-simplex layering`** — `type:feature`,
`area:backend`, **L**. Tighter drawings than longest-path (§4.2); swap
for one function.

**14. `feat: merged edge trunks for sibling dependencies`** —
`type:feature`, `area:backend`, **M**. Reference images 1–2's single-
arrowhead look (decision D2 in §3). Note it trades away per-edge DOM
identity.

### How this satisfies INVEST

- **Independent** — the flag (issues 1–8) and the phase-per-module split
  mean any of 2–8 can land in any order after its predecessor; 7 is
  fully parallel.
- **Negotiable** — 8, 12, 13 and 14 are explicitly droppable, and each
  algorithm choice in §4 is stated as "this one first, that one later"
  precisely so scope can be traded without redesign.
- **Valuable** — each of 1–11 changes something a person can see or
  verify: a rendered graph, a better-looking one, a passing test, a
  smaller payload, a correct doc. Issue 1 is the one that comes closest
  to being an enabler, which is exactly why it is scoped as a rendering
  slice rather than "add the types".
- **Estimable** — each owns one named function or file, with the
  algorithm already chosen here rather than left to discovery.
- **Small** — one L-sized issue among the required nine (3 and 5 are the
  genuinely hard phases); the rest are S/M.
- **Testable** — every AC above is machine-checkable, and §7 lists the
  properties. No issue's AC is "looks right".

## 12. Decision

**None yet — this is a spike deliverable awaiting a call.** The
recommendation in §10 is to proceed with §11's sequence, but nothing
here is settled until that is confirmed, and no code has been written.

Three specific things worth deciding before issue 1 is filed:

1. **Flow direction (D1 in §3)** — root at top with arrows pointing up
   (images 1–3, recommended), or root at bottom with arrows pointing
   down (images 4–5).
2. **Whether the effort is worth it at all**, given §10's counterweight:
   this replaces working code with substantially more code, and the
   user-visible win is a different-looking graph plus a faster page, not
   a new capability.
3. **Whether to do it incrementally behind the flag** as proposed, or as
   a single branch. §11 assumes the flag; a single branch is cheaper in
   total but gives up every INVEST property above.
