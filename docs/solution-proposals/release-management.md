# Solution Proposal: Release Management

- **Status:** Proposed
- **Date:** 2026-08-31
- **Related:** #58 (this spike), `docs/development/ci.md` (where the
  recommended release workflow fits alongside `test.yml`), #46/#47
  (pending Terraform/Terragrunt CI — noted, not designed here)

## 1. Problem statement

There's no release process at all today, confirmed directly rather than
assumed:

- `typeio.cabal`'s `version:` has never moved off the scaffold value,
  `0.1.0.0`.
- `git tag -l` and `gh release list` are both empty — no tags, no
  GitHub Releases, ever.
- `typeio.cabal` declares `extra-doc-files: CHANGELOG.md`, but no
  `CHANGELOG.md` exists anywhere in the repo — a dangling reference this
  proposal has to resolve one way or the other, not leave as-is.

## 2. What this project's actual history says

Reasoned from this repo's real activity, not a green-field guess: 31
merged PRs and 42 issues filed in under two days (2026-08-30 through
2026-08-31, per `gh pr list`/`gh issue list`), one maintainer (`git
log` shows a single author), strict single-`main` + feature-branch +
required-PR-check workflow already enforced (`docs/development/ci.md`,
branch protection with `enforce_admins: true`), and no evidence
anywhere in the repo of a deployment target or external users/consumers
pinning to a version today. This is a **pre-1.0, single-maintainer,
extremely fast-iterating project with no external release consumers
yet** — every recommendation below is sized for that, not for a
multi-team project with a release cadence to protect.

## 3. Versioning scheme

**Recommendation: semver, tracked in `typeio.cabal`'s existing
`version:` field** — the obvious default for a Haskell project (it's
already the field `cabal`/Hackage tooling expects), so no new
source-of-truth is introduced.

**What triggers a bump, and by whom**: manual, via a normal PR that
edits `version:` — deliberately not tied to commit message conventions
or automatic detection of "what kind of change happened." This repo
doesn't enforce a commit-message format today (see `git log` — messages
are free-form `type: description`-shaped but not machine-parsed
anywhere), and inventing one just to drive version bumps would be
solving a problem in service of automating a step (deciding "is this
release major/minor/patch") that genuinely needs a human judgment call
at this project's size. The PR that bumps the version can be
version-only or paired with the change that motivated it — either way,
it's the trigger event tagging (§4) watches for.

## 4. Git tagging and GitHub Releases

**Recommendation: automate tag + release creation, gated on the version
bump landing on `main`** — not a fully manual `git tag && gh release
create` process a human has to remember to run, and not a release
triggered by anything else (a schedule, a manual `workflow_dispatch`
with no connection to what actually changed).

Concretely: a new GitHub Actions workflow, triggered on `push` to
`main`, that diffs the pushed commit against its parent for a change to
`typeio.cabal`'s `version:` line (the same diff-gating shape
`docs/development/ci.md` documents for `test.yml`, applied to a
different question — "did the version change?" instead of "did
anything test-relevant change?"). When it did: create tag `vX.Y.Z`
(reading the new value straight out of `typeio.cabal`) and run `gh
release create vX.Y.Z --generate-notes`, leaning on GitHub's own
auto-generated release notes (built from merged PR titles since the
last tag) rather than a hand-maintained changelog file. When the
version didn't change, the job does nothing — same "always runs, skips
internally" shape as `test.yml`, though this one isn't a required
check at all (see §7), so the path-filter hazard that shape exists to
avoid doesn't actually apply here — using it anyway keeps one consistent
pattern for "gate on a diff" across both workflows rather than two.

This resolves the "manual vs. automated" question the ticket raises:
**bumping the version is manual (a real decision), creating the tag and
release from that bump is automatic** (a mechanical step with one
correct answer once the decision's made) — removing the "forgot to tag
after merging the bump" failure mode without automating the part that
actually needs judgment.

**Tag format: `vX.Y.Z`** (e.g. `v0.2.0`) — the conventional prefix
GitHub's own tooling and `gh release` expect by default.

## 5. Resolving `CHANGELOG.md`: remove the reference, don't create the file

**Recommendation: drop `extra-doc-files: CHANGELOG.md` from
`typeio.cabal`; don't create or maintain a hand-written changelog.**
`extra-doc-files` exists to bundle extra docs into a Hackage `sdist`
when a package is *published* — this app isn't a published library,
it's a deployed web app, so the field is vestigial scaffold boilerplate
that predates any real release process, not a deliberate choice. Given
§4's recommendation to lean on GitHub's auto-generated release notes,
a hand-maintained `CHANGELOG.md` would be a second, redundant place
recording the same information, with a real risk of drifting out of
sync with what the auto-generated notes already say correctly for
free. Removing the reference is a one-line `typeio.cabal` edit, left
for the implementation ticket (spike only here, per the acceptance
criteria) rather than made in this PR.

## 6. Milestones: not yet

**Recommendation: don't adopt GitHub Milestones now.** §2's numbers —
42 issues total, one maintainer, no release cut yet to even group a
milestone around — mean there's no backlog volume or multi-person
coordination problem for milestones to solve today; adding them now
would be process weight ahead of a need. Revisit once there's an actual
recurring release cadence (from §4) to group issues/PRs toward, or once
issue volume or contributor count grows enough that "what's actually
going into the next release" stops being obvious from the issue tracker
alone.

## 7. Branch strategy: none beyond what already exists

**Recommendation: tag-based releases directly off `main`, no
`release/x.y` branches or equivalent.** The ticket asks this be a real
position, reasoned from this project's actual history, not a generic
best-practice default — so, concretely: this is a pre-1.0 project (§2)
with no external consumers who've ever pinned to a released version
(there's never been one), a single maintainer, and a `main` that's
already required to be shippable at every commit (branch protection +
required `test` check, `docs/development/ci.md`). A release branch's
entire purpose — patching an *already-shipped* version while `main` has
moved on and diverged — has no problem to solve yet: there's no shipped
version anyone depends on that could need a hotfix independent of
`main`'s current state. Introducing one now would be exactly the
"branch-strategy complexity without a concrete reason tied to this
project" the ticket warns against. Revisit if/when this genuinely ships
to external users who pin versions and a real need to patch an older
release while `main` has moved on shows up — not before.

## 8. CI interaction

§4's release workflow triggers on `push` to `main`, not `pull_request` —
orthogonal to `test.yml`'s required, PR-only check (`docs/development/ci.md`'s
"why pull requests only" reasoning: anything on `main` already passed
that check via its PR, and the release workflow isn't re-checking
anything, it's reacting to a version bump that already landed). No
interaction between the two beyond both existing as separate
`.github/workflows/*.yml` files, same pattern
`docs/solution-proposals/security-scanning.md` §8 already established
for keeping an unrelated-purpose workflow separate from `test.yml`
rather than folding it in as another job.

**#46/#47 (pending Terraform/Terragrunt CI)**: a separate concern —
infrastructure changes and application releases are different
lifecycles with different triggers. Noted for whoever picks those up;
not designed here.

## 9. Recommended shape, summarized

1. **Versioning**: semver in `typeio.cabal`'s `version:`, bumped
   manually via a normal PR — the human decision this proposal
   deliberately doesn't try to automate (§3).
2. **Tagging/releases**: a new workflow, `push`-to-`main`-triggered,
   gated on whether `version:` changed (§4) — creates a `vX.Y.Z` tag and
   a GitHub Release via `gh release create --generate-notes` when it
   did, does nothing when it didn't.
3. **`CHANGELOG.md`**: resolve the dangling reference by removing
   `extra-doc-files: CHANGELOG.md` from `typeio.cabal` — don't create
   the file (§5).
4. **Milestones**: not adopted now (§6).
5. **Branching**: no release branches; tags directly off `main` (§7).

## 10. Open questions

- Whether to add a `.github/release.yml` config so GitHub's
  auto-generated notes group PRs by the existing `type:*` labels
  (`docs/development/labels.md`) instead of one flat list — a real,
  low-cost nice-to-have once release notes actually start getting
  read by someone, not worth deciding sight-unseen here.
- What v1.0.0 should actually mean for this project (a real
  "production-ready" bar, vs. just another version bump) — a product
  decision outside this proposal's scope, not a release-*process*
  question.
- Whether the version-bump PR should also be the place a
  `CHANGELOG`-equivalent summary gets written into the PR description
  for `--generate-notes` to pick up more richly — worth revisiting once
  a few real releases exist and it's clear whether GitHub's default
  notes are actually good enough on their own.
