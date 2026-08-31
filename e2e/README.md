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
always does, from the repo root:

```
make run-postgres      # start Postgres in Docker
make migrate-up        # apply all migrations
make seed-db           # seed reference data (NodeStatus/NodeType)
cabal run server        # start the app, reads .env
```

Then, in a separate terminal, from the repo root:

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
- `tests/helpers.ts` — shared setup (`createProject()`) both specs
  above use, so creating a project isn't duplicated across specs that
  need one but aren't testing project creation itself.

The other two candidate workflows (change a node's status, view and
interact with the dependency graph) aren't covered yet — tracked as
follow-ups in #96–#97, each adding its own spec under `tests/`
following this same pattern. CI wiring is tracked separately in #98,
once real workflow coverage exists to wire in.

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
