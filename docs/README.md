# docs

Repository for architecture notes, developer documentation, and solution
proposals for this project.

## Layout

- `solution-proposals/` — spike/investigation write-ups that compare options
  for a problem and recommend a path forward, before implementation work is
  ticketed. One document per proposal, named for its topic. These are a
  point-in-time decision record, not a live source of truth: a proposal's
  own confident "Decision" section is not proof the decision was actually
  implemented, or still holds. Always check the doc's `Status` line, and
  cross-check `development/` for whether it actually happened, before
  treating a proposal as current guidance.
- `development/` — reference docs on how the app *actually, currently*
  works, grouped by area (`ui/`, `frontend/`, `backend/`), plus onboarding.
  Each area has an `index.md` linking its own files. This is the primary
  reference for development decisions — if something isn't reflected here,
  it either isn't built yet or isn't true.

## Index

- [`development/onboarding.md`](development/onboarding.md) — **start here.** Setup steps and a request-lifecycle walkthrough linking everything else below.
- [`development/ui/index.md`](development/ui/index.md) — the `#container`/`#view` component pattern, how UI is rendered directly in Haskell (Lucid), and the global-vs-scoped CSS split.
- [`development/frontend/index.md`](development/frontend/index.md) — HTMX and hyperscript: the attribute-driven client-side interactivity, with concrete patterns from the codebase.
- [`development/backend/routing.md`](development/backend/routing.md) — the `Data.HashTree`-based router: how routes are built, and the prefix-match/per-request-rebuild behavior worth knowing about.
- [`development/backend/environment.md`](development/backend/environment.md) — the `Env` record (config, logger, DB pool) acquired once at startup, and how it differs from Containers.
- [`development/backend/containers.md`](development/backend/containers.md) — the Container dependency-injection pattern: Root → per-domain → API/UI sub-containers.
- [`development/backend/logging.md`](development/backend/logging.md) — the two independent structured-JSON logging pipelines (request/response, and database queries) and why they're separate.
- [`development/ci.md`](development/ci.md) — what the GitHub Actions workflow runs, why it's PR-only, and how to reproduce it locally.
- [`solution-proposals/haskell-auto-formatting.md`](solution-proposals/haskell-auto-formatting.md) — options for an auto-formatting setup for `.hs` files that works for both human editors (format-on-save) and AI agents.
- [`solution-proposals/unit-testing.md`](solution-proposals/unit-testing.md) — which test framework to use, which modules are worth testing, and a mocking strategy for the Container-based responder modules.
- [`solution-proposals/integration-testing.md`](solution-proposals/integration-testing.md) — the deferred responder-testing question from the unit-testing decision: a disposable Postgres via `testcontainers`, truncate-based test isolation, and a recommended pilot flow.
- [`solution-proposals/lazy-request-transactions.md`](solution-proposals/lazy-request-transactions.md) — **decided against**: explored lifting the transaction boundary for cross-domain atomicity, kept as a record of why that turned out to be unnecessary.
