# Visualizations

The project dependency graph can be drawn more than one way. This directory
documents each drawing the app actually serves — what it draws and where
its code lives. For *how the app holds several visualizations and picks
one* (the switching mechanism, the shared queries, the isolation rule
between visualizations), see
[`../../architecture/visualization-switching.md`](../../architecture/visualization-switching.md) —
that document is the normative reference for the mechanism; this directory
is the per-visualization reference for what each one actually draws.

- **Layered** (`viz:layered`) — the project root heads the drawing, with
  its edges to the work derived. Doc tracked in
  [#225](https://github.com/v12-Industry/typeio_2/issues/225).
- [**Rootless**](rootless.md) — the work only, with the project root left
  out entirely (`viz:rootless`, #215).

Both share one layout engine and SVG vocabulary — see
[`../../architecture/graph-rendering.md`](../../architecture/graph-rendering.md)
for that pipeline, which neither of these two docs repeats.
