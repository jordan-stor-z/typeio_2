# E2E Tests

A [Playwright](https://playwright.dev/) suite that drives the app
through a real browser against a real running server and real seeded
Postgres — not a Docker-managed disposable database like
`test-integration/`, and not CI-wired yet. See
`docs/solution-proposals/e2e-testing.md` (#17) for the design
rationale; this file only covers how to run what's here today.

## Prerequisites

- Everything [`docs/development/onboarding.md`](../docs/development/onboarding.md)
  already requires to run the app locally (GHC/cabal, Docker, a `.env`
  at the repo root).
- Node.js (any version current enough to run Playwright; this suite was
  built against Node 20).

## Running it locally

This suite doesn't start the app itself — it drives a browser against
whatever's already running at `E2E_BASE_URL` (default
`http://localhost:3000`). Start the app the same way local development
always does, from the repo root — the one-command path:

```
make start-app
```

— or the same steps by hand, one per terminal:

```
make run-postgres      # start Postgres in Docker
make migrate-up        # apply all migrations
cabal run server        # start the app, reads .env
make seed-db           # seed reference data (NodeStatus/NodeType; needs the server already running)
```

See [`onboarding.md`](../docs/development/onboarding.md) for more on
either path. Then, in a separate terminal, from the repo root:

```
make e2e-install   # first time only (npm install + Playwright's Chromium)
make test-e2e      # cd e2e && npm test
```

(Or, from this directory directly: `npm install`,
`npx playwright install --with-deps chromium`, `npm test` — same thing,
what the `make` targets wrap.)

`npm test`/`make test-e2e` runs `playwright test` headless against every
spec in `tests/`. To point at a different host/port (e.g. a non-default
`WEB_PORT`):

```
E2E_BASE_URL=http://localhost:4000 npm test
```

### Watching it run

`make test-e2e`/`npm test` runs headless (no visible browser window) —
that's the default for a reason (faster, no display needed), but to
actually *watch* it drive a browser, run Playwright directly from this
directory instead:

```
npx playwright test --headed              # opens a real browser window
npx playwright test --headed --slow-mo=500 # ...and pauses 500ms between actions, so you can actually follow along
npx playwright test --ui                   # Playwright's UI mode: a scrubbable timeline, live browser view, and DOM snapshot per step
```

`--ui` mode is the best way to actually see what a spec did after the
fact, action by action, not just pass/fail.

## What's covered

- `tests/create-project.spec.ts` — the pilot: drives the add-project
  form end to end (navigate → open the form → fill it in → submit →
  assert the new project appears back on the project index). See that
  file's comments for why create-project was chosen as the pilot and
  the specific htmx-swap timing it's asserting around.
- `tests/edit-node.spec.ts` — adds a node (via a direct API call, not a
  UI interaction — the app has no UI affordance to create a node yet,
  see the spec's comments), then edits its title and description
  through the node-detail panel, asserting on each field's settled
  save-success indicator and on the re-fetched detail view afterward.
- `tests/node-status.spec.ts` — changes a node's status via the
  node-detail panel's status dropdown, asserting on the immediate
  save-success indicator and, separately, on the plain (non-edit)
  detail view's status text to confirm it actually persisted. See the
  spec's comments for a real app bug found while writing this (the edit
  dropdown never actually shows the node's real current status,
  regardless of what's in the database).
- `tests/graph.spec.ts` — clicks a node in the D3-rendered dependency
  graph, asserting its detail panel opens and it picks up the
  `.node-highlight` glow, then that closing the panel clears both. See
  the spec's comments for a severe app bug found while writing this
  (the graph never positions any node past the first one — #120).
- `tests/helpers.ts` — shared setup (`createProject()`, `addNode()`)
  every spec above uses, so creating a project/node isn't duplicated
  across specs that need one but aren't testing its creation.

All four candidate workflows from the proposal's §7 are now covered.
CI wiring is tracked separately in #98. A formal `docs/development/`
page covering all of this (this file will likely shrink to a plain
"how to run it" pointer once that lands) is tracked in #117.

## Notes

- **No database reset between runs.** Unlike `test-integration/`
  (which truncates mutable tables before every test via
  `Integration.Support.resetBetweenTests`), nothing here resets the
  database — this suite drives the actual dev Postgres you started by
  hand. Specs give their fixture data timestamped titles so re-running
  locally doesn't collide with a previous run's rows, but the database
  will accumulate projects/nodes across runs until you reset it
  yourself (e.g. `make migrate-down-all && make migrate-up`).
- **Single browser (Chromium) for now** — broaden only if a real
  cross-browser bug surfaces.
- **Locators and web-first assertions only, never fixed sleeps.**
  htmx's async partial swaps race a network-idle or sleep-based wait; a
  locator-based, auto-retrying assertion model doesn't need to know
  anything about the swap's timing. Every spec added to this suite
  should follow the same convention.
- **`locator.fill()` doesn't reliably trigger htmx's `changed` trigger
  modifier.** Confirmed directly: a field wired to `hx-trigger="input
  changed delay:500ms"` (e.g. the node-edit panel's title/description
  fields) never fires its request after `.fill()`, no matter how long
  you wait — not a timing issue. Use `locator.selectText()` then
  `locator.pressSequentially()` instead for any field with a `changed`
  trigger; see `edit-node.spec.ts`'s comments for the full story
  (including why `fill('')` as a "clear first" step doesn't work
  either).
- **A freshly htmx-swapped-in element can need an explicit `click()`
  before Playwright's non-pointer interaction helpers (e.g.
  `selectOption()`) reliably trigger its own `hx-trigger`.** Confirmed
  on the node-edit panel's status `<select>`: calling `selectOption()`
  alone, right after the edit form swaps in, never fires its `change`
  PUT — but `locator.click()` immediately before it does, every time.
  Not a settle-timing issue (an artificial wait between the two doesn't
  fix it on its own) — see `node-status.spec.ts`'s comments. Reach for
  this if a spec's htmx request never fires despite the value/state
  visibly updating correctly client-side.
- **`locator.dispatchEvent('click')` for an element a real pointer
  genuinely can't reach.** The dependency graph's D3 layout can leave a
  node positioned off-screen or overlapping other page content (#120),
  which fails `locator.click()`'s actionability check no matter how
  long you wait. `dispatchEvent('click')` fires the same event
  `hx-trigger="click"` reacts to without needing the element to be
  visually clickable first — see `graph.spec.ts`'s comments. Prefer a
  real `click()` whenever the element is actually reachable; reach for
  `dispatchEvent()` only when a known, separately-tracked rendering bug
  is what's actually in the way, not as a default habit.
