# UI Architecture

How this app's UI is put together, for anyone (human or agent) about to
add or change a page.

The short version: every page is a Haskell function that returns HTML
(no template files), the browser navigates by asking the server for a
fragment of that HTML and swapping it in with [htmx](https://htmx.org),
and styling is split between a small global layer and one stylesheet per
page.

- [**Components**](components.md) — the `#container` / `#view` element
  pattern that every page is built around.
- [**Haskell rendering**](haskell-rendering.md) — how HTML gets built as
  plain Haskell values (Lucid), with no templating language.
- [**Styles**](styles.md) — what belongs in the global stylesheets versus
  a per-view one.

See also [`docs/development/frontend/`](../frontend/) for how htmx and
hyperscript — the client-side libraries doing the swapping and the small
visual effects — actually work, once that doc lands.
