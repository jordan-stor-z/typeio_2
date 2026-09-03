# Visualization Switching

> **Status: designed, not built.** Nothing described here exists in the
> code yet. #213 wrote this design down *before* the first alternative
> visualization, so the conventions are decided deliberately rather than
> established by accident by whichever implementation lands first.
>
> What exists today is a single visualization, hard-wired: the
> server-computed layered orthogonal SVG documented in
> [`graph-rendering.md`](graph-rendering.md). Under the scheme below it
> becomes the first visualization among several, and nothing about how it
> draws changes.
>
> This directory is the one allowed to describe an unbuilt design — see
> [`README.md`](README.md). `docs/development/` is not; when the switch
> is implemented, the day-to-day side of it (how to set the value
> locally, how to run the suites against a given visualization) gets
> written up there and this document loses its status marker.

The dependency graph may be the most important thing in this
application, and there is more than one good way to draw it. This
describes how the app holds several visualizations at once and picks
one.

## The shape of it in one paragraph

A single configuration value, read from the environment at boot, selects
the active visualization. The selection happens **once**, when the
container is constructed — not per request, and not from a query
parameter. Each visualization lives in its own directory and shares no
code with any other: it is handed the project's nodes and dependencies
as domain entities, and it returns rendered HTML. Everything between
those two points is private to it.

## 1. The configuration value

`GRAPH_VISUALIZATION`, read from `.env` like every other setting. Never
hardcoded — see `CLAUDE.md`.

It follows the existing `Config.*` pattern exactly, so there is nothing
novel to learn:

```haskell
-- Config.Visualization

keyVisualization :: String
keyVisualization = "GRAPH_VISUALIZATION"

data Visualization
  = Layered   -- the layered orthogonal graph (graph-rendering.md)
  deriving (Eq, Read, Show)

loadVisualizationConfig :: IO (Either [ValidationErr] Visualization)
```

`Visualization` is parsed with `valRead` the way `Config.App`'s
`EnvironmentName` already is, and surfaced as a field on `AppConfig`
alongside `dbConf` and `webConf`.

**The name is prefixed `GRAPH_` rather than bare.** `DB_*` and `WEB_*`
group the multi-field configs and `ENV` stands alone; a bare
`VISUALIZATION` would not say *what* is being visualized, and this app
may well grow a second thing worth drawing.

### Missing or unrecognised values fail at boot

`Config.App.loadConfig` already `error`s on any validation failure, and
that is the right answer here too: a server running with a silently
defaulted visualization is worse than one that refuses to start, because
the misconfiguration would only surface as "the graph looks wrong" much
later. There is deliberately **no fallback default** — if
`GRAPH_VISUALIZATION` is absent or unparseable, startup fails with the
same message shape as a missing `DB_HOST`.

## 2. Where the switch happens

The seam already exists. `Domain.Project.Responder.Ui.Container`
currently binds one handler unconditionally:

```haskell
defaultContainer :: ConnectionPool -> Container
defaultContainer pl =
  Container
    { ...
    , getProjectGraph = handleProjectGraph pl
    }
```

Selecting a visualization means binding a different handler there, so
`defaultContainer` takes the config:

```haskell
defaultContainer :: AppConfig -> ConnectionPool -> Container
defaultContainer cfg pl =
  Container
    { ...
    , getProjectGraph = graphHandlerFor (visualization cfg) pl
    }
```

`Container.Build` already holds `AppConfig` and already threads it into
a domain container — `SystemContainer.defaultContainer (appConf ev) lg`
— so the precedent is established and only `ProjectContainer` and
`Ui.Container` need to start taking it.

### The choice is made once, at construction

Not per request. Not by a query parameter.

This is worth stating flatly because the app has already been here:
`?layout=server` was the previous mechanism and was removed in #181/#192
once the flag was unreachable from the browser and the second renderer
it selected was gone. A request-time switch means every handler carries
a branch, both visualizations stay live in the same process, and the
"no shared code" rule below becomes unenforceable in practice. Binding
one handler at construction keeps the rest of the app unaware that
there is a choice at all.

## 3. The isolation rule

**Visualizations share no code with each other.** The rule that makes
that precise:

> A visualization is handed the project's nodes and dependencies as
> domain entities, and returns rendered HTML. Everything between those
> two points is private to it.

### What is shared, and why it isn't a violation

Shared code is code that is not *about* visualizing anything:

| Shared | Because |
|---|---|
| `Domain.Project.Model` (`Node`, `Dependency`, keys) | The domain, not a drawing of it. Every visualization reads the same project. |
| The esqueleto queries that fetch a project's nodes and dependencies | Fetching is I/O against the domain; two visualizations asking the same question of the database is not coupling, and duplicating the query would let them silently disagree about what "the project" is. |
| `Data.*` and `Common.*` utilities — `intToText`, `wrapLabel`, `Common.Validation`, `Common.Web.*` | General-purpose library code, the same tier as anything from `base`. `wrapLabel` greedily wraps text; it does not know what a graph is. |

### What is not shared

Everything else, including things it is tempting to hoist:

- **Layout and geometry.** `Graph.Layer`, `Order`, `Coord`, `Route`,
  `Layout`, `Containment`.
- **Geometry types.** `Graph.Types` too — `Point`, `Size`, `Bounds`,
  `NodeId`, `LayoutNode`. These read like neutral vocabulary, and that
  is exactly the trap: sharing them means the next visualization
  inherits a `LayoutNode` shaped for layered drawing, and every change
  to it becomes a negotiation. A radial visualization wants polar
  coordinates; a force one wants velocity. Re-declaring a two-field
  `Point` costs four lines and buys independence.
- **Entity → layout conversion.** Each visualization decides for itself
  what to make of a `Node` and a `Dependency` — including whether to
  derive containment edges at all, and whether to draw the project root
  (see #215).
- **Lucid rendering**, and **static assets**: CSS and JS.

### The one hard rule, restated for this level

`graph-rendering.md` has a hard rule that nothing under
`Domain.Project.Graph.*` may import `Database.*`, `persistent`,
`Esqueleto`, `Lucid` or `Network.Wai`. That rule is per-visualization
and stays. This document adds one alongside it:

**No module under `Domain.Project.Visualization.<Name>` may import a
module under `Domain.Project.Visualization.<Other>`.**

Nothing enforces this at compile time — it is reviewed at PR time. If it
starts being violated, the cheap enforcement is a test that reads the
import lines, not a refactor.

### Why duplication is the right trade here

This is the unusual choice in this document, so the reasoning belongs
with it. Duplicating the entity-to-layout conversion across
visualizations is a real, ongoing cost. Sharing it is a real, permanent
coupling: it makes every visualization's needs a constraint on every
other's, and the whole reason for having several is that they disagree
about how to draw the same data. A shared "neutral" layer between them
would accumulate every visualization's requirements and end up neutral
for none of them.

We are choosing the cost that stays local.

## 4. Directory layout

```
lib/src/Domain/Project/Visualization/
  Layered/            -- engine, conversion and responder
    Graph/            -- Layer.hs, Order.hs, Coord.hs, Route.hs, ...
    Responder.hs
  <Next>/
    ...

static/styles/visualization/layered/
static/script/visualization/layered/
```

The existing implementation moves under `Layered/` when the switch is
built. That move is mechanical and changes no behaviour, and it is what
turns [`graph-rendering.md`](graph-rendering.md) from "how the graph
works" into "how *this* visualization works".

## 5. Testing

- **Each visualization's engine stays in the unit tier.** The current
  one is unit-testable because it is pure and dependency-free
  (`docs/development/unit-testing.md`), and the per-visualization hard
  rule above is what preserves that. A visualization that cannot be
  unit-tested this way is a visualization that broke the rule.
- **Integration tests name the visualization they exercise** rather
  than relying on whatever `.env` happens to say, so adding a
  visualization cannot quietly change what an existing test asserts.
  The suites configure it explicitly.
- **Every visualization is exercised**, not only the configured one.
  A visualization nobody tests is a visualization that is broken the
  first time someone selects it.

## 6. How an issue says which visualization it is for

Labels, applied at issue-creation time. See
[`../development/labels.md`](../development/labels.md)'s `viz:*` section
for the full convention — it is the normative reference, and this is a
pointer to it.
