# E2E Tests

A [Playwright](https://playwright.dev/) suite that drives the app
through a real browser against a real running server and real seeded
Postgres — not a Docker-managed disposable database like
`test-integration/`, and not CI-wired yet. See
`docs/solution-proposals/e2e-testing.md` (#17, decided in §8) for why
Playwright and what tradeoffs were considered; this file only covers
how to run what's here today.

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

Then, in a separate terminal, from this directory (`e2e/`):

```
npm install
npx playwright install --with-deps chromium   # first time only
npm test
```

`npm test` runs `playwright test` against every spec in `tests/`. To
point at a different host/port (e.g. a non-default `WEB_PORT`):

```
E2E_BASE_URL=http://localhost:4000 npm test
```

## What's covered

Just the pilot workflow so far: `tests/create-project.spec.ts` drives
the add-project form end to end (navigate → open the form → fill it in
→ submit → assert the new project appears back on the project index).
See that file's comments for why create-project was chosen as the pilot
and the specific htmx-swap timing it's asserting around.

The other three candidate workflows from the proposal's §7 (add/edit a
node, change a node's status, view and interact with the dependency
graph) aren't covered yet — tracked as follow-ups in #95–#97, each
adding its own spec under `tests/` following this same pattern. CI
wiring is tracked separately in #98, once real workflow coverage exists
to wire in.

## Notes

- **No database reset between runs.** Unlike `test-integration/`
  (which truncates mutable tables before every test via
  `Integration.Support.resetBetweenTests`), nothing here resets the
  database — this suite drives the actual dev Postgres you started by
  hand. `create-project.spec.ts` gives its project a timestamped title
  so re-running it locally doesn't collide with a previous run's row,
  but the database will accumulate projects across runs until you reset
  it yourself (e.g. `make migrate-down-all && make migrate-up`).
- **Single browser (Chromium) for now** — per the proposal's §7, start
  narrow and broaden only if a real cross-browser bug surfaces.
- **Locators and web-first assertions only, never fixed sleeps** — see
  the proposal's §5 for why (htmx's async partial swaps race a
  network-idle or sleep-based wait; a locator-based, auto-retrying
  assertion model doesn't need to know anything about the swap's
  timing). Every spec added to this suite should follow the same
  convention.
