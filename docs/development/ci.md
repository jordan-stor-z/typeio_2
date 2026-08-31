# CI

There are two GitHub Actions workflows:

- `.github/workflows/test.yml` — builds the app and runs the unit test
  suite. **Required** to merge into `main`.
- `.github/workflows/integration-test.yml` — runs the Docker-backed
  integration test suite. Informational only, not required — see
  [Integration test workflow](#integration-test-workflow) below.

## What it does

The `test` job runs on **every** pull request into `main` — it is not
path-filtered at the workflow level (see "Why it always runs" below).
Its steps:

1. Check out the PR's code (full history, needed for step 2's diff).
2. Diff against the PR's base branch to check whether anything that
   could affect the build/test result actually changed (`**/*.hs`,
   `*.cabal`, `cabal.project`, or the workflow file itself).
3. **If nothing relevant changed** (e.g. a docs-only PR): every
   remaining step is skipped via `if:`, and the job reports success
   quickly with no GHC/cabal setup at all.
4. **If something relevant did change**: install GHC 9.6.7 / Cabal
   3.12.1.0 via
   [`haskell-actions/setup`](https://github.com/haskell-actions/setup) —
   the same versions used locally (see
   [`onboarding.md`](onboarding.md)/`typeio.cabal`'s `base ^>=4.18.3.0`
   bound) — cache the cabal store and `dist-newstyle` (keyed on
   `typeio.cabal`), then `cabal build all` and `cabal test spec`
   (see [`unit-testing.md`](unit-testing.md)).

No database or service container is involved — `spec` is entirely pure
(see [`unit-testing.md`](unit-testing.md) for what's covered and why).
This step deliberately runs `cabal test spec`, not a bare `cabal test`:
the integration test-suite from
`docs/solution-proposals/integration-testing.md` (#65) also exists in
this package now, and has its own CI coverage — see
[Integration test workflow](#integration-test-workflow) below — but a
bare `cabal test` would build and run every test-suite in the package
from *this* job too, silently pulling a Docker-dependent suite into
this required, Docker-less check.

## Integration test workflow

`.github/workflows/integration-test.yml` runs `cabal test integration`
(the suite from `docs/solution-proposals/integration-testing.md` §11
and #65) on every PR into `main` that touches Haskell-relevant files —
resolved in #72, following the pattern the solution proposal's §8 had
left open.

A few ways this deliberately differs from the `test` workflow above:

- **A separate workflow file**, not a second job in `test.yml` — keeps
  this suite's different needs (Docker, longer runtime) isolated from
  the required, fast, DB-free `test` job, and makes it trivial to
  promote or demote independently later.
- **Not a required check (yet).** This is a newer, Docker-dependent
  suite; requiring it immediately, on a repo with `enforce_admins: true`
  and therefore no bypass, was judged too much risk before it's proven
  reliable. Once it's been stable for a while, promoting it to required
  is a separate, deliberate branch-protection change — not bundled into
  standing the workflow up.
- **A plain top-level `paths:` filter**, unlike `test.yml`'s
  always-runs-and-skips-internally pattern. That pattern exists
  specifically to protect a *required* check from the "stuck missing
  forever" trap (see [Why it always runs](#why-it-always-runs-and-skips-internally-instead-of-using-paths)
  below) — a trap that only bites required checks. Since this workflow
  isn't required, a docs-only PR simply not triggering it at all is
  fine.
- **No `migrate` CLI setup step.** GitHub-hosted Ubuntu runners already
  have Docker running, and migrations apply themselves from inside the
  disposable container (`test-integration/Integration/Support.hs`'s
  `docker-entrypoint-initdb.d` approach) — nothing extra to install on
  the runner beyond the same GHC/cabal setup `test.yml` already uses.

## Why it always runs, and skips internally instead of using `paths`

The first version of this workflow used a top-level `on.pull_request.paths`
filter, so it wouldn't trigger at all for a docs-only PR. That turned out
to be broken the moment `main`'s branch protection was configured to
**require** this check: GitHub has no concept of "this check doesn't
apply to this PR," only "passed" or "missing" — a required check that a
path filter prevents from ever running stays permanently missing, which
blocks the merge forever, with no bypass (`enforce_admins: true` means
even an admin override can't get past it). The fix is the job-level
`if:` pattern above: the workflow (and the check GitHub tracks) always
runs, so it can always report a real result, while the actual expensive
work is still skipped when it isn't needed.

## Why pull requests only, not `main`

Anything that lands on `main` only got there via a PR that already ran
this exact check — re-running it again on `main` itself would just be
repeating a check that already passed, for no new information. So the
workflow triggers on `pull_request` only.

**Worth knowing**: this reasoning depends on nothing landing on `main`
except through a checked PR. That's now actually enforced, not just a
convention — `main` has branch protection requiring a pull request (0
required approvals, so it's about the PR requirement, not review) and
this `test` check to pass, with `enforce_admins: true` (no bypass, for
anyone). It was previously just `CLAUDE.md`'s "never push directly to
main" rule for agents, which bound agents but not humans or GitHub
itself — see the note above about what configuring this actually
required from the workflow.

`integration-test.yml` triggers on `pull_request` only too, for the
same reason — it's just not part of what branch protection enforces.

## Running the same checks locally

Before CI existed, running `cabal test`/`make test` locally before
pushing was part of the standard workflow. It no longer has to be — the
PR itself is the enforcement point now — but it's still the fastest way
to find a failure before waiting on a CI run:

```
cabal build all
cabal test spec   # or: make test
```

This is the unit suite only, matching the required `test` job. The
integration suite is separate:

```
cabal test integration   # or: make test-integration
```

It needs Docker locally (see [Integration test workflow](#integration-test-workflow)
above for what it runs in CI — informational only, not required). See
`docs/solution-proposals/integration-testing.md` for the full rationale
for now; a `docs/development/` write-up lands with #53.

**Running tests locally is now optional; writing/updating them is not.**
CI catching a missing or broken test after the fact is not a substitute
for adding or updating tests as part of the change that needs them —
see `CLAUDE.md`'s Code & Style Conventions.
