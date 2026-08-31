# Solution Proposal: End-to-End Testing

- **Status:** Decided — see §8. Implementation not yet ticketed.
- **Date:** 2026-08-31
- **Related:** #17 (this spike), `docs/solution-proposals/integration-testing.md`
  (§9 cross-reference: shares the "ephemeral seeded Postgres" question —
  integration testing landed first via #65, so this proposal reuses that
  answer rather than inventing a second one), `docs/development/ci.md`
  (CI now exists — the ticket predates it; see §6)

## 1. Problem statement

No automated browser-driven testing exists for the essential user paths
(create a project, add/edit nodes, change node status, view the
dependency graph). This proposal compares E2E tooling options against
this app's actual shape and recommends one, plus what CI support it
would need — without installing or wiring anything up (spike only).

**One framing in the original ticket is now stale and worth correcting
up front**: it was written when "there is currently no CI at all"
(`.github/workflows/` didn't exist). #41 has since landed
`.github/workflows/test.yml`. This changes the CI question from "design
CI from scratch" to "add an E2E job alongside the existing one" — see
§6.

## 2. What this app's shape demands from a tool

- **Server-rendered + htmx-swapped, not a JS SPA.** Lucid/Haskell
  renders HTML; htmx swaps `#view` fragments in via `hx-*` attributes
  (see `docs/development/ui/index.md`). A tool that only knows how to
  wait for full page navigations will race htmx's partial swaps and
  produce flaky tests. What actually works reliably here is a tool whose
  primary interaction model is "locate the element you expect to exist
  *after* the action, and retry until it does" — not "wait for the
  network to go idle, then act." htmx swaps are typically a single fast
  XHR; a **locator-based, auto-retrying assertion model** sidesteps
  needing to know anything about htmx's request lifecycle at all, which
  a network-event-based wait model would have to be taught explicitly.
- **hyperscript-driven transient effects** (e.g. the flash-on-update in
  `ProjectManage/Node/Refresh.hs`) are exactly the kind of thing that
  produces flaky tests if a test asserts on the transient state itself
  (mid-flash) rather than the settled end state. This is a **test-design
  discipline**, not something any tool solves automatically — call it
  out explicitly in whatever gets built, regardless of which tool is
  chosen.
- **D3-rendered SVG** (the dependency graph, `ProjectManage/Graph.hs`).
  SVG elements are real DOM nodes with attributes/classes, so this isn't
  a special case for a capable tool — `<svg>`/`<g>`/`<path>` are
  selectable and assertable the same as any other element. The risk here
  is the same timing risk as above (D3 renders after data arrives, not
  synchronously with page load), not something SVG-specific.
- **Backend is Haskell; the E2E tool doesn't need to be.** Browser
  automation is a distinct concern from the app's implementation
  language — evaluate language/ecosystem fit as one tradeoff among
  several, not a hard requirement either way.

## 3. Options compared

| Tool | Waiting model fit (§2) | Language/ecosystem | GitHub Actions story | Maintenance |
|---|---|---|---|---|
| **Playwright** | Auto-waits on every action (attached, visible, stable, enabled) and retries web-first assertions until they pass or time out — exactly the locator-and-retry model §2 calls for, with no manual wait code needed for htmx's async swaps. | Node/TS (also Python/.NET/Java bindings) — separate toolchain from Haskell, same tradeoff as any option here | `npx playwright install --with-deps` in a normal job (no published marketplace action needed); official Docker image available; built-in test sharding across workers | Active; Microsoft-backed; by far the largest and fastest-growing community of the JS options as of 2026 (npm downloads and GitHub stars both well ahead of Cypress) |
| **Cypress** | Also auto-retries assertions, but its execution model runs inside the browser itself and is Chromium-family-first (Firefox and an experimental WebKit runner exist, but Chromium is the primary target) — narrower browser coverage than Playwright for no corresponding advantage here | Node/TS, same tradeoff as Playwright | Well-supported via `cypress-io/github-action`, but independent 2026 benchmarks put it noticeably slower and heavier (per-action latency, RAM under parallel runs) than Playwright on comparable suites | Active, but community metrics have been shifting toward Playwright industry-wide |
| **Puppeteer** | Chrome/Chromium-only automation library; lower-level than Playwright (no built-in test-runner, no auto-retrying assertions out of the box — those would need to be hand-built or layered on with another test framework) | Node/TS | No official GitHub Action; same manual `npm install`+launch pattern, more setup work than Playwright for less out of the box | Active (also a Google project), but Playwright is the direct successor built by largely the same original team, with the auto-wait/retry model already built in |
| **Selenium/WebDriver** (`hspec-webdriver`/`webdriver` on Hackage) | WebDriver's explicit-wait model is the one §2 warns against defaulting to — reliable htmx/hyperscript timing would mean hand-writing polling waits per interaction, not something the tool gives you | **The only Haskell-native option** — real ecosystem-fit upside if it were otherwise competitive | Selenium itself has CI support (`selenium/standalone-chrome` images, etc.), but that's Selenium's story, not this binding's | **Not viable as-is**: `hspec-webdriver`'s last Hackage release is 1.2.2 (June 2023, three years stale as of this writing) with negligible recent download volume — effectively unmaintained. Its underlying `webdriver` binding is in a similar position. Haskell-native is a real point in this option's favor in the abstract, but not enough to outweigh reaching for an unmaintained library. |

## 4. Recommendation: Playwright

Best fit on every axis that matters here: its locator/auto-retry model
is the closest match to what htmx's async partial swaps actually need
(§2), it has the strongest 2026 GitHub Actions story (a plain `npx
playwright install --with-deps` step plus a maintained official Docker
image, no marketplace action dependency), and it's the most actively
developed and fastest of the options compared. The "separate
toolchain" tradeoff (Node in a Haskell repo) applies to every
JS-based option equally and isn't a Playwright-specific cost; the one
option that would avoid it (`hspec-webdriver`) isn't viable on its own
merits (§3).

**Not recommending Cypress or Puppeteer as fallbacks** — Playwright
strictly dominates both here (Cypress: same language cost, weaker
performance/browser coverage; Puppeteer: same language cost, less
built in, no corresponding upside) rather than trading one axis for
another.

## 5. Test-writing implications for this app specifically

- **Assert on settled state, never mid-transition.** Applies directly
  to hyperscript flash effects (§2) — a test should wait for and assert
  the final DOM state, not the flash class itself, or it becomes exactly
  the kind of timing-flaky test this whole evaluation is trying to
  avoid.
- **Locators, not fixed sleeps, for every htmx-swapped region** — e.g.
  after submitting the new-node form, assert on the swapped `#view`
  fragment's content via a Playwright locator/expect, not a hardcoded
  wait. This is Playwright's default posture already (§2), but worth
  stating as the convention new E2E tests should follow, the same way
  `docs/development/unit-testing.md` states conventions for that suite.
- **D3/SVG assertions** target the rendered `<svg>` structure directly
  (e.g. a `page.locator('svg .node')` count or attribute check) — no
  different from asserting on any other rendered element once the graph
  has settled.

## 6. CI implications

`.github/workflows/test.yml` exists now (#41) — this is "add a second
workflow," not "design CI from nothing," correcting the original
ticket's framing. An E2E job needs meaningfully more scaffolding than
the unit suite:

1. **Headless browser** — `npx playwright install --with-deps chromium`
   (start with one browser; broaden to Firefox/WebKit only if a real gap
   shows up, not preemptively). Cacheable the same way `test.yml`
   already caches the cabal store/`dist-newstyle` — key on the installed
   Playwright version.
2. **A real, seeded Postgres the running app can talk to** — a
   meaningfully different shape from `docs/development/integration-testing.md`'s
   `testcontainers`-managed database: that setup is a Haskell test
   *process* managing its own container. Here, the actual compiled
   `server` executable needs to be running and reachable over HTTP for
   Playwright to drive a browser against, with a real Postgres behind
   it — so the container needs to be started **before** `cabal run
   server`, independently of the test process. The right building block
   to reuse is the **migration approach**
   `test-integration/docker/apply-migrations.sh` already established
   (bind-mount `migrations/` into `postgres:15`'s
   `docker-entrypoint-initdb.d/`, apply via `psql`, no `migrate` CLI
   needed on the runner) — started as a GitHub Actions service container
   or a plain `docker run` step, not reinvented.
3. **Reference-data seeding** — unlike the integration suite (which
   seeds `NodeStatus`/`NodeType` directly via
   `Domain.Central.Responder.Api.Seed`'s data lists inside the test
   process), nothing here calls into that Haskell code directly. The
   app already exposes this as `POST /api/central/seed-database`
   (`make seed-db` is exactly `curl`-ing it) — so the CI job starts the
   server, then hits that endpoint once before the E2E suite runs,
   reusing the app's own existing seeding path rather than duplicating
   `Seed.nodeStatuses`/`nodeTypes` a third time.
4. **Start the server itself** — `cabal run server` (built the same way
   `test.yml`'s `cabal build all` step already does, so no new build
   step, just running the result) backgrounded, pointed at the step-2
   database via the same `.env`-driven config (`Config.Db`) the app
   always uses, with a wait-for-port step before Playwright starts.

**Trigger and blocking model**: recommend starting this as an
**on-demand or scheduled** workflow, not a required PR check — this is a
slower, multi-process suite (browser + server + database, versus the
unit suite's pure in-process run), and making it a required check
inherits the same required-check + `paths:`-filter hazard
`docs/development/ci.md` documents (a job-level `if:` pattern, not a
top-level `paths:` filter, if it's ever made required). Revisit
required-vs-informational once real runtime and flakiness data exist
from actually running it — not decided here, since deciding it now would
be guessing at numbers this proposal doesn't have.

## 7. Open questions

- Single browser (Chromium) vs. multi-browser from the start — start
  narrow (§6.1); broaden only if a real cross-browser bug surfaces.
- Whether E2E specs live in this repo (a `e2e/` or `test-e2e/` directory,
  parallel to `test-integration/`) or a separate repository — recommend
  **this repo**, for the same reason `test-integration/` lives alongside
  the app rather than separately: one place to find all of a change's
  tests, and one CI checkout covers both.
- Which specific user paths are the first suite's scope (the ticket
  lists create-project, add/edit node, status change, view graph as
  candidates) — a call for whoever implements this, informed by which
  flows have broken in practice, not designed in the abstract here.
- Trace/video capture on failure (Playwright supports both natively) —
  worth turning on once the suite exists and CI runtime/artifact-storage
  cost is a known quantity, not decided sight-unseen.

## 8. Decision

Confirmed 2026-08-31 (PR #76 review comment: "These recommendations
make sense to me"). Every recommendation above is adopted as written,
with nothing changed on reconsideration:

- **Tool: Playwright** (§3/§4) — over Cypress, Puppeteer, and
  Selenium/`hspec-webdriver` (the one Haskell-native option, ruled out
  on its own merits: unmaintained since June 2023).
- **Test-writing convention: locators and web-first assertions, never
  fixed sleeps, for htmx-swapped regions; assert on settled state, not
  mid-transition** (§2/§5) — applies directly to the hyperscript
  flash-on-update effect.
- **CI shape** (§6): a headless-browser install step, a Postgres started
  via the same `docker-entrypoint-initdb.d`/`psql` migration approach
  `test-integration/` already established (not the `migrate` CLI), the
  app's own `POST /api/central/seed-database` endpoint for reference
  data, and the actual compiled `server` process running and reachable
  for Playwright to drive a browser against.
- **Trigger/blocking model: on-demand or scheduled, not a required PR
  check** (§6) — revisit once real runtime/flakiness data exists.

**Left open, on purpose — not indecision:** §7's four open questions
(single-browser-first vs. multi-browser, specs living in this repo,
which user paths the first suite covers, and trace/video capture) are
explicitly deferred to whoever implements this, per that section's own
reasoning — nothing here is being pre-decided in the absence of the
information (actual flows that break, actual CI cost) those questions
depend on.

**Implementation not yet ticketed.** No follow-up issue exists yet for
building the E2E suite itself (parallel to how #65–#69 tracked
`integration-testing.md`'s implementation) — filing one is a separate
step from recording this decision.
