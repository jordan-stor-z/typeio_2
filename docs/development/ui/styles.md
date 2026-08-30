# Styles: global vs. scoped

CSS lives in `static/styles/`, split into two tiers with different
loading rules.

## Global — `global.css` and `material.css`

Linked exactly once, in `<head>`, by
`Domain.Central.Responder.Ui.IndexView.indexTemplate` — the one full HTML
document the server renders (see [components.md](components.md)).
Because every page passes through the `#container` shell, these two
files are the only stylesheets guaranteed to be present on every page.

`global.css` holds:

- CSS custom properties / theme tokens on `:root` (`--bg-start`,
  `--accent`, `--border-color`, `--error-color`, ...) — change a color
  here, not at each call site.
- Base element styles (`body`) and shell layout (`#container`).
- Classes shared across more than one feature (`.action-button`, ...).

**Convention:** if a class or token is used by more than one view, it
belongs here, not duplicated into a scoped file.

## Scoped — `static/styles/views/<feature>.css`

One stylesheet per top-level view (`add-project.css`,
`manage-project.css`, ...), linked with an ordinary `<link
rel="stylesheet">` **from within that view's own Haskell template**, not
from the shell:

```haskell
-- ProjectCreate/View.hs
projectCreateVwTemplate payload errs = do
  templateNavHeader "Add Project"
  link_ [rel_ "stylesheet", href_ "/static/styles/views/add-project.css"]
  div_  [id_ "view"] $ do
    ...
```

**Worth knowing:** because `#container` swaps only ever replace
`innerHTML`, this `<link>` tag becomes part of the swapped-in fragment
and ends up living in `<body>`, not `<head>`. Browsers apply stylesheet
links anywhere in the document just fine, so this works — but it's an
unusual enough mechanism that it's easy to "clean up" into `<head>` by
reflex and break the scoping (the whole point is that this stylesheet
only loads when this view is the one being rendered). Leave it where it
is.

**Convention:** styles that only target one view's own element `id`s
(e.g. `#node-detail` in `manage-project.css`) go in that view's file.

Not every view currently has one — `ProjectIndex/View.hs` and
`ProjectIndex/List.hs` have no scoped stylesheet of their own today and
rely entirely on global classes. That's fine as long as it stays true;
if project-index-specific styling gets added, give it its own
`project-index.css` following the same pattern rather than growing
`global.css`.
