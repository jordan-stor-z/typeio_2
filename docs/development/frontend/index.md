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
- **`graph-viewport.js`** (`static/script/`) — scroll, zoom and
  pointer-pan for the dependency graph. It does *not* lay the graph out:
  the server sends finished SVG with every coordinate already computed
  (see
  [`../../architecture/graph-rendering.md`](../../architecture/graph-rendering.md)),
  and this script only moves the viewport over it.

  There was a D3 build here until #182 — `d3.js` plus
  `nodetree.js`/`nodetree2.js` — which computed the layout in the
  browser from JSON the server embedded in the page. All of it is gone,
  including the ~280KB `d3.js` that loaded on *every* page in the app.
  If you are looking for graph layout code on the client, there isn't
  any; it's Haskell now.
