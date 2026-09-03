# Visualization Switching

> **Status: built.** `GRAPH_VISUALIZATION` selects the drawing, and two
> visualizations exist: `Layered` and `Rootless`. #213 wrote this design
> down before either of them, so the conventions were decided
> deliberately rather than set by whichever implementation landed first;
> #215 built the switch and the second visualization.
>
> **The isolation rule changed while it was being built, deliberately.**
> #213 said visualizations share *no* code and each owns a private copy
> of everything, including the layout engine. Building #215 priced that:
> it meant duplicating a ~1,500-line geometry engine so that one
> visualization could decline to draw one node, and every future fix to
> that geometry — #214's component packing was one, landed the same day —
> would have had to be applied twice. That was judged utopian and
> revised. The rule now draws the line at *policy* rather than at
> *directory*, and [The isolation rule](#3-the-isolation-rule) below is
> the current, normative version.

The dependency graph may be the most important thing in this
application, and there is more than one good way to draw it. This
describes how the app holds several visualizations at once and picks
one.

## The shape of it in one paragraph

A single configuration value, read from the environment at boot, selects
the active visualization. The selection happens **once**, when the
container is constructed — not per request, and not from a query
parameter. Each visualization owns the decision of *what the drawing is
of*: which nodes and edges exist. The machinery for turning that into a
document — the layout engine, the queries, the SVG vocabulary — is
shared infrastructure any visualization may use.

## 1. The configuration value

`GRAPH_VISUALIZATION`, read from `.env` like every other setting. Never
hardcoded — see `CLAUDE.md`.

```haskell
-- Config.Visualization

keyVisualization :: String
keyVisualization = "GRAPH_VISUALIZATION"

data Visualization
  = Layered   -- root heads the drawing; containment edges derived
  | Rootless  -- the work only, nothing forced to converge (#215)
  deriving (Eq, Read, Show)
```

Parsed with `valRead`, so the environment value is the constructor name
— `Layered` or `Rootless` — exactly how `ENV` already parses into
`Config.App.EnvironmentName`. It is surfaced as `visualization` on
`AppConfig`, alongside `dbConf` and `webConf`.

**The name is prefixed `GRAPH_` rather than bare.** `DB_*` and `WEB_*`
group the multi-field configs and `ENV` stands alone; a bare
`VISUALIZATION` would not say *what* is being visualized, and this app
may well grow a second thing worth drawing.

### Missing or unrecognised values fail at boot

There is deliberately **no fallback default**. If `GRAPH_VISUALIZATION`
is absent or names no visualization, startup fails with the same
accumulated-error report as a missing `DB_HOST`.

A server running with a silently defaulted visualization is worse than
one that refuses to start, because the misconfiguration does not
announce itself — it surfaces much later as "the graph looks wrong".
Both cases are pinned in `Config.AppSpec`.

## 2. Where the switch happens

One binding, in `Domain.Project.Responder.Ui.Container`:

```haskell
graphHandler :: Visualization -> ConnectionPool -> Application
graphHandler Layered = Layered.handleProjectGraph
graphHandler Rootless = Rootless.handleProjectGraph
```

`Container.Build` already held `AppConfig`, so it passes
`visualization (appConf ev)` down through `ProjectContainer` to the UI
container. Note it passes the **selected visualization**, not the whole
`AppConfig`: the container pattern is about handing each level only what
its handlers actually need.

### The choice is made once, at construction

Not per request. Not by a query parameter. Everything downstream of
`graphHandler` holds a single `Application` and is unaware there was a
choice at all.

This is worth stating flatly because the app has already been here:
`?layout=server` was the previous mechanism and was removed in
#181/#192 once the flag was unreachable from the browser and the second
renderer it selected was gone. A request-time switch means every handler
carries a branch and both visualizations stay live in the same process.

## 3. The isolation rule

> A visualization decides **what the drawing is of**. Everything that
> turns that decision into a document is shared infrastructure.

Concretely, a visualization supplies one function:

```haskell
-- Domain.Project.Visualization.Common
type BuildGraph = Int64 -> [Entity M.Node] -> [Entity M.Dependency] -> ServerGraph
```

That is the whole per-visualization surface. `Layered.buildGraph` keeps
every node and derives the root's containment edges;
`Rootless.buildGraph` drops the root, derives nothing, and drops any
stored edge that referred to it.

### What is shared

| Shared | Because |
|---|---|
| `Domain.Project.Graph.*` — the layered layout engine | Geometry, not policy. It takes nodes and edges and returns coordinates; it has no opinion about which nodes it was given. One copy means one place to fix a layout bug. |
| `Domain.Project.Model` and the esqueleto queries | The domain, not a drawing of it. Two visualizations asking the same question of the database is not coupling; duplicating the query would let them silently disagree about what "the project" is. |
| Request parsing, error responses (`handleGraphWith`) | Identical whichever drawing is selected. |
| The SVG vocabulary — `edgeLine`, `nodeGroup`, `nodeLabel`, `arrowMarker`, `templateServerGraph` | Presentation primitives, the same tier as `Common.Web.Elements`. |
| `Data.*` / `Common.*` utilities | General-purpose library code. `wrapLabel` wraps text; it does not know what a graph is. |

### What is not shared

The conversion: which nodes exist in the drawing, which edges exist,
and whether any are derived. That is where `Layered` and `Rootless`
actually differ, and it is the only place they differ.

### Where the seam moves if a visualization needs more

The shared pieces are shared because nothing yet needs them to differ,
not because they may never. A visualization that wants its own document
assembly — a frame drawn around the work instead of a root node, say —
takes over `templateServerGraph` for itself rather than bending the
shared one with a flag. A visualization that is not layered at all — a
radial or force-directed one — simply does not import
`Domain.Project.Graph.*`; it brings its own geometry.

**The rule to hold is that a flag never crosses the seam.** If a shared
function grows a parameter whose only purpose is to say which
visualization is calling, that function has stopped being shared
infrastructure and should be moved into the visualizations that need it.
That is the failure mode the original no-shared-code rule was reaching
for, and it is worth keeping even though the blanket version was not.

## 4. Directory layout

```
lib/src/Domain/Project/
  Graph/                     -- shared: the layered layout engine
    Types.hs  Layer.hs  Order.hs  Coord.hs  Route.hs  Layout.hs
    Containment.hs           -- root-to-work derivation, used by Layered
  Visualization/
    Common.hs                -- shared: queries, request/response, SVG
    Layered/Responder.hs     -- buildGraph: root included
    Rootless/Responder.hs    -- buildGraph: root left out
```

`Domain.Project.Graph.*` did not move. It was already a pure,
dependency-free, visualization-agnostic tier — the neutral home the
revised rule calls for — so making it shared was a matter of saying so,
not of relocating it.

## 5. Testing

- **The shared engine stays in the unit tier.** It is pure and
  dependency-free (`docs/development/unit-testing.md`), and the hard
  rule in [`graph-rendering.md`](graph-rendering.md) — no `Database.*`,
  `persistent`, `Esqueleto`, `Lucid` or `Network.Wai` under
  `Domain.Project.Graph.*` — is what keeps it there.
- **Each visualization's conversion is integration-tested**, because it
  takes `Entity` values and renders markup. `Rootless.ResponderSpec`
  asserts what its conversion decides: no root drawn, no containment
  derived, root-referring edges dropped — plus the positive case, since
  every one of those would also pass on a visualization that drew
  nothing at all.
- **Every visualization is exercised**, not only the configured one.
  Both specs construct their handler directly rather than reading
  `GRAPH_VISUALIZATION`, so adding a visualization cannot quietly change
  what an existing test asserts.

## 6. How an issue says which visualization it is for

Labels, applied at issue-creation time. See
[`../development/labels.md`](../development/labels.md)'s `viz:*` section
— it is the normative reference, and this is a pointer to it.
