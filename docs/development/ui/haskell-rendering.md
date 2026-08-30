# UI implemented directly in Haskell

There is no template language and no template files. A "page" is a plain
Haskell function that returns `Html ()` (from [Lucid](https://hackage.haskell.org/package/lucid)),
built with do-notation combinators, and rendered to bytes at the very end
of a request handler.

## The basic shape

```haskell
projectIndexVwTemplate :: Html ()
projectIndexVwTemplate = do
    templateNavHeader "Projects"
    div_ [id_ "view"] $ do
      button_
        [ class_     "action-button"
        , hxGet_     "/ui/create-project/vw"
        , hxPushUrl_ True
        , hxTarget_  "#container"
        , hxSwap_    "innerHTML"
        ] "Create Project"
      ...
```

`div_`, `button_`, `class_`, etc. are ordinary functions — `class_ "x"` is
an `Attributes` value, `div_ [attrs] body` is a `Term`. There's nothing
Haskell-specific to learn beyond Lucid's own API; the point is that
building HTML is just building a value, so it composes, takes arguments,
and pattern-matches like any other Haskell code (see the `unless (null
errs) $ ...` in `ProjectCreate/View.hs` for a template branching on
validation errors).

The convention is one module per page/feature under
`responder/ui/<Feature>/`, usually named `View.hs`, exposing:

- a WAI handler (`handleProjectCreateVw`, `handleGetNodeRefresh`, ...)
  that does I/O (DB queries, request parsing) and eventually calls
  `renderBS` on a template value to produce the response body, and
- a pure template function (`projectCreateVwTemplate`, `templateRefresh`,
  ...) that only knows how to turn already-fetched data into `Html ()`.

Keeping those separate means the HTML-shape logic doesn't depend on WAI
or the database at all.

## Escaping Lucid's built-in vocabulary

Lucid ships combinators for standard HTML5, which isn't enough for htmx
attributes, hyperscript, or SVG. Two small local modules extend it via
Lucid's own escape hatches (`Lucid.Base`):

- **`Common.Web.Attributes`** — arbitrary attributes via `makeAttributes`:
  htmx's `hx-*` family (`hxGet_`, `hxSwap_`, `hxTarget_`, ...), hyperscript's
  `_` attribute (`h_`), and SVG attributes Lucid doesn't cover
  (`stroke_`, `strokeWidth_`, `markerEnd_`, `viewBox_`, ...). There are
  two variants for htmx's boolean attributes, e.g. `hxPushUrl_ :: Bool ->
  Attributes` (renders `"true"`/`"false"`) vs. `hxPushUrl'_ :: Text ->
  Attributes` for when the value itself needs to be a dynamic string.
  `hxVals_`/`hxVals'_` encode a Haskell value as the JSON `hx-vals`
  htmx expects.
- **`Common.Web.Elements`** — arbitrary elements via `term`, for the SVG
  tags Lucid has no combinator for: `circle_`, `path_`, `g_`, `line_`,
  `marker_`, `defs_`, `text_`, `tspan_`. Used exclusively by the
  dependency-graph template (`ProjectManage/Graph.hs`).

If you need an attribute or element that isn't already in one of these
two modules, add it there rather than reaching for a raw string
elsewhere — that's the established extension point.

## Passing server data to client JS

The dependency graph needs its data on the client for D3 to render, and
that's done by embedding JSON directly in the page rather than a separate
API call the JS makes on load:

```haskell
templateGraph g = do
  script_ [id_ "graph-data", type_ "application/json"] $ encode g
  script_ [src_ "/static/script/nodetree2.js"] empty
  svg_ [...] $ ...
```

`nodetree2.js` reads `#graph-data`'s text content and parses it. This
keeps the graph's data and its container HTML in the same server
response/htmx swap, rather than needing a second round trip.

## Forms

Forms are plain `form_`/`input_`/`textarea_`, but submit via
`hxPost_`/`hxPut_` instead of a native form POST, so the response can be
swapped in-place instead of navigating (e.g. `ProjectCreate/View.hs`'s
`form_ [..., hxPost_ "/ui/create-project/submit"]`). There is no
client-side form-handling JS to write for this — htmx handles serializing
the form and firing the request.
