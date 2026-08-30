# CI

There is one GitHub Actions workflow, `.github/workflows/test.yml`,
which builds the app and runs the unit test suite.

## What it does

On every pull request into `main`:

1. Check out the PR's code.
2. Install GHC 9.6.7 / Cabal 3.12.1.0 via
   [`haskell-actions/setup`](https://github.com/haskell-actions/setup) —
   the same versions used locally (see
   [`onboarding.md`](onboarding.md)/`typeio.cabal`'s `base ^>=4.18.3.0`
   bound).
3. Cache the cabal store and `dist-newstyle`, keyed on `typeio.cabal`, so
   dependencies aren't rebuilt from scratch on every run.
4. `cabal build all` — the whole project has to compile.
5. `cabal test` — the unit suite (`docs/solution-proposals/unit-testing.md`)
   has to pass.

No database or service container is involved — the current test suite
is entirely pure (Tier 1 + Tier 3 modules, see the unit-testing decision
doc). That changes once integration tests
(`docs/solution-proposals/integration-testing.md`) exist; this workflow
doesn't cover those yet.

## Why pull requests only, not `main`

Anything that lands on `main` only got there via a PR that already ran
this exact check — re-running it again on `main` itself would just be
repeating a check that already passed, for no new information. So the
workflow triggers on `pull_request` only.

**Worth knowing**: this reasoning depends on nothing landing on `main`
except through a checked PR. That's currently a *convention*
(`CLAUDE.md`'s "never push directly to main" rule for agents), not a
GitHub-enforced guarantee — there's no branch protection rule actually
blocking a direct human push to `main` today. If that ever changes,
this workflow's premise should be revisited.

## Running the same checks locally

Before CI existed, running `cabal test`/`make test` locally before
pushing was part of the standard workflow. It no longer has to be — the
PR itself is the enforcement point now — but it's still the fastest way
to find a failure before waiting on a CI run:

```
cabal build all
cabal test   # or: make test
```

**Running tests locally is now optional; writing/updating them is not.**
CI catching a missing or broken test after the fact is not a substitute
for adding or updating tests as part of the change that needs them —
see `CLAUDE.md`'s Code & Style Conventions.
