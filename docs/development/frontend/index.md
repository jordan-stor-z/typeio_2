# Client-Side Interactivity

Three JS libraries are loaded (all from `IndexView.hs`'s `<head>`), each
doing a distinct job:

- [**HTMX**](htmx.md) — page navigation and in-page updates, driven by
  `hx-*` attributes emitted from Haskell. This is the one doing the
  actual work of turning server-rendered fragments into an SPA-feeling
  app; see [`../ui/components.md`](../ui/components.md) for the
  `#container`/`#view` elements it swaps.
- [**hyperscript**](hyperscript.md) — small, declarative, per-element
  visual effects (`_`/`h_` attribute) that don't need real JS.
- **D3** (`static/script/d3.js`, `nodetree.js`/`nodetree2.js`) — renders
  the dependency graph in `ProjectManage/Graph.hs`. Not covered by a doc
  here yet; the Haskell side (embedding the graph's JSON data, the SVG
  element helpers in `Common.Web.Elements`) is covered in
  [`../ui/haskell-rendering.md`](../ui/haskell-rendering.md).
