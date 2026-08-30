# Solution Proposal: Unit Testing

- **Status:** Proposed
- **Date:** 2026-08-30
- **Related:** #26 (this spike), #17 (E2E testing spike — cross-referenced
  below on the "ephemeral test Postgres" question so the two don't solve
  it twice), `docs/development/backend/containers.md`

## 1. Problem statement

There is no test suite at all today — no `test-suite` stanza in
`typeio.cabal`, no test-only dependency, no `*Spec.hs` file anywhere in
the repo. The only existing "test" is `make test-migrations`, which is
about schema migrations, not application code, and is currently broken
(see `CLAUDE.md`).

This app is mostly CRUD + form validation + HTML templating over a
Postgres schema, not complex pure computation — so "which library" is a
smaller question here than "which modules are actually worth testing,
and what do we do about the ones that touch the database," which is
where most of this doc's weight goes.

## 2. Framework options

| Tool | What it is | Fit here |
|---|---|---|
| [Hspec](https://hspec.github.io/) | BDD-style (`describe`/`it`), `hspec-discover` auto-finds `*Spec.hs` files, native `prop` support for QuickCheck | Best default — least ceremony, matches this codebase's "keep it simple" style, huge ecosystem |
| [Tasty](https://hackage.haskell.org/package/tasty) | A test-tree runner that composes multiple providers (HUnit/QuickCheck/SmallCheck/golden) under one CLI | More power than needed right now — worth it if golden-testing Lucid output or heavy property testing grows into a real need later, not for a first test suite |
| [HUnit](https://hackage.haskell.org/package/HUnit) | Low-level assertion library | Not used standalone — Hspec's `shouldBe` etc. already wrap this style |
| [QuickCheck](https://hackage.haskell.org/package/QuickCheck) | Property-based testing, integrates natively with Hspec (`prop`) | Use for the handful of genuinely property-testable pure functions (see below) — no new dependency beyond what Hspec already pulls in |
| [Hedgehog](https://hedgehog.qa/) | Property-based testing, better shrinking/generators, needs `hspec-hedgehog` or `tasty-hedgehog` to plug in | Not worth the extra adapter dependency here — this codebase's data is simple enough that QuickCheck's shrinking is plenty |
| [tasty-golden](https://hackage.haskell.org/package/tasty-golden) | Compare output against a stored fixture file | Worth considering specifically for the Lucid template functions (see §4) if template regressions become a real pain point — not a day-one need |

**Recommendation: Hspec**, with `QuickCheck` used through Hspec's own
`prop` for the small set of pure functions where a property is more
natural than an example (see §4, tier 1). No `Tasty`, no `Hedgehog`, no
golden testing yet — all three are reasonable additions later if a
specific need shows up, but none pull their weight for a first test
suite on a codebase this shape.

## 3. Wiring it in (illustrative — not implemented in this ticket)

```cabal
test-suite spec
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  build-depends:    base, src, hspec, QuickCheck
  build-tool-depends: hspec-discover:hspec-discover
  default-language: Haskell2010
```

`hspec-discover` auto-generates the `Spec.hs` that finds every
`test/**/*Spec.hs`, so adding a test module is just adding a file. `cabal
test` runs it; a `make test` Makefile target (parallel to the existing
`make test-migrations`) should wrap that the same way the rest of the
Makefile wraps `cabal`/`migrate` commands.

## 4. Which modules actually make sense to test

Not "test everything" — tiered by how testable/valuable each layer
actually is:

**Tier 1 — pure, dependency-free, test first:**

- `Common.Validation` — the accumulating-validation combinators
  (`isThere`, `isNotEmpty`, `valRead`, `isBetween`, `runValidation`,
  `.$`) used by every form/config validator in the app. Worth calling
  out one real edge case already sitting in `runValidation`: a `(Nothing,
  [])` result (no errors recorded, but the value is still `Nothing`)
  falls through to a generic `"Unknown error in validation"` — whether
  that's reachable in practice, and what should happen if it is, is
  exactly the kind of thing a test suite should pin down rather than
  leaving to be discovered in production.
- `Data.Either` (`listToEither`, `maybeToEither`, `notNullEither`) and
  `Data.Text.Util` (`intToText`) — trivial, but trivial is cheap to test
  and these are used everywhere.
- `Data.HashTree` — the router's core data structure (`addT`, `findPath`,
  `emptyT`, and the `<+>`/`-|`/`-<` combinators, see
  `docs/development/backend/routing.md`). This is the best QuickCheck
  candidate in the codebase: e.g. "for any set of distinct path lists
  inserted as leaves, `findPath` on each returns the value it was
  inserted with" is a real property, not a rephrased example.

**Tier 2 — pure business logic embedded in responder modules:** the
`validate*`/`showNodeType`/`classNodeType`/`toGraph`/`formatUpdated`/
link-building (`pushUrl`, `nodePanelLink`, etc.) functions scattered
through `responder/ui/ProjectManage/*` and friends. These are pure given
their inputs even though they live in otherwise IO-heavy modules — worth
testing directly rather than only indirectly through a handler.

**Tier 3 — config validation:** `Config.App`/`Config.Db`/`Config.Web`'s
`validateConfig` functions are pure given a constructed `LookupDbConfig`/
`LookupWebConfig` value — testable without touching real environment
variables at all, and worth it given how much silent failure they're
guarding against at startup (see `docs/development/backend/environment.md`).

**Tier 4 — the hard case: the WAI handlers themselves** (`handleGetNodes`,
`handlePostNode`, `handleGetNodeRefresh`, ...). These are IO-bound and
call `runSqlPool`/esqueleto directly against a real `ConnectionPool` —
addressed in §5 rather than glossed over.

## 5. Mocking strategy

### The good news: Containers already are test doubles

This project's DI ([`docs/development/backend/containers.md`](../development/backend/containers.md))
is a record of already-applied functions, not a typeclass-based effects
system — which means **constructing an alternate `Container` by hand,
with stub functions instead of real ones, is already how you'd mock
something here.** No mocking library needed or wanted. For example,
`Domain.Central.Middleware.IndexRender.renderIndexMiddleware` takes a
`UI.Container` — a test can hand it a `Container { indexView = \_ _ _ ->
respond canned, emptyView = ... }` and assert on the middleware's
branching logic (the `isHx`/`isHxRestore`/`isVwPath` conditions) without
a real router, container tree, or database anywhere in sight.

The same trick works for logging: `EntryLog` is a `newtype` around one
function. A test can construct `EntryLog $ \src lvl msg -> modifyIORef
ref (JsonLog src lvl msg :)` in place of the real `fast-logger`-backed
one, and assert on what got logged (see
`docs/development/backend/logging.md`) — again, a hand-built stand-in
value, not a mocking framework.

### The hard case: handlers that call the database directly

The Container pattern stops helping once you're *inside* a handler like
`handleGetNodes cpl` — `cpl :: ConnectionPool` is used directly with
`runSqlPool`/esqueleto, not through another swappable record field. Two
real options, not a false choice:

1. **Add a repository layer.** One more Container-shaped record per
   aggregate (e.g. a `NodeRepo { getNodes :: IO [Entity M.Node], ... }`)
   injected the same way everything else already is, so handler logic
   can run against a hand-built fake repo in a test. Consistent with the
   existing architecture, but it's a real refactor of the responder
   modules, not just "add a test suite" — don't do this as part of
   adopting testing, do it later if/when query logic grows complex
   enough that testing it without a database becomes worth the
   restructuring.
2. **Don't unit test these — treat them as integration tests** against a
   real, ephemeral test Postgres instance (seeded via the existing
   `migrations/`), and draw the boundary explicitly: pure logic (tiers
   1–3 above) gets fast unit tests with no dependencies; DB-touching
   handlers get a smaller number of integration tests that accept the
   cost of a real database.

### Sketch: what option 1 would actually look like

Today (`Domain.Project.Responder.Api.Node.Get`):

```haskell
handleGetNodes :: ConnectionPool -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetNodes pl respond = do
  ns <- encode . map toSchema <$> runSqlPool query pl
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
  where
    query = select $ from $ table @M.Node
```

With a repository record in front of the pool:

```haskell
-- new: Node/Repo.hs
newtype NodeRepo = NodeRepo
  { repoGetNodes :: IO [Entity M.Node]
  }

defaultNodeRepo :: ConnectionPool -> NodeRepo
defaultNodeRepo pl = NodeRepo
  { repoGetNodes = runSqlPool (select $ from $ table @M.Node) pl
  }

-- Node/Get.hs: depends on NodeRepo instead of ConnectionPool directly
handleGetNodes :: NodeRepo -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleGetNodes repo respond = do
  ns <- encode . map toSchema <$> repoGetNodes repo
  respond $ responseLBS status200 [("Content-Type", "application/json")] ns
```

Wired in exactly the way every other dependency already is (see
[containers.md](../development/backend/containers.md)) — the container
now builds a `NodeRepo` from the pool instead of handing the pool
straight to the handler:

```haskell
-- Domain.Project.Responder.Api.Container
defaultContainer :: ConnectionPool -> Container
defaultContainer cpl = Container
  { getNodes = handleGetNodes (defaultNodeRepo cpl)
  , ...
  }
```

And the test constructs a fake `NodeRepo` by hand — no mocking library,
same trick as everywhere else in §5:

```haskell
spec :: Spec
spec = describe "handleGetNodes" $ do
  it "returns 200 with the repo's nodes JSON-encoded" $ do
    let fakeRepo = NodeRepo { repoGetNodes = pure [sampleNodeEntity] }
        app _req respond = handleGetNodes fakeRepo respond  -- lift to Application shape
    resp <- runSession (srequest $ SRequest defaultRequest "") app
    assertStatus 200 resp
    assertBodyContains "\"title\":\"Sample Node\"" resp
```

**One real gotcha this surfaced:** `ResponseReceived` (from `Network.Wai`)
has no public constructor — you can't just write a fake `respond`
callback that returns one yourself, which rules out the obvious naive
approach to testing these handlers directly. `runSession`/`srequest`
(from `Network.Wai.Test`, part of `wai-extra` — **already a dependency**,
no new package needed) exist specifically to solve this, by running the
handler as a real `Application` inside a fake, in-process HTTP session
and handing back an inspectable `SResponse`. Any handler-level test —
with or without a repository layer — needs to go through
`Network.Wai.Test`, not a hand-rolled fake `respond`.

**Recommendation: option 2 for now.** Standing up an ephemeral test
Postgres is exactly the same infrastructure #17's E2E spike will need for
its own seeded test database — solve it once, likely as part of whichever
of the two spikes gets implemented first, and have the other reuse it
rather than inventing a second answer to "how do we get a disposable test
database."

## 6. CI

There's no CI in this repo yet (same gap #17 already flagged). Whichever
lands first between this and #17, worth noting: a unit suite (tier 1–3,
no database needed) is cheap and fast enough that it should be the first
thing wired into GitHub Actions, well before the heavier lift of
browser-driven E2E tests or DB-backed integration tests — it's the
highest signal-per-minute-of-CI-time option available.

## 7. Open questions

- Does the team want a repository-layer refactor (§5, option 1) on the
  roadmap at all, or is "integration tests for DB-touching handlers"
  an acceptable permanent boundary? This doc recommends *not* deciding
  that as part of adopting testing — but it's a real architectural
  question worth having explicitly rather than by default.
- Whichever of this ticket or #17 lands first should own building the
  ephemeral-test-Postgres setup; the other should consume it.
- Revisit `Tasty`/golden testing if the Lucid template functions start
  seeing enough regressions that eyeballing diffs in review isn't
  sufficient.
