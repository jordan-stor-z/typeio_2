# Solution Proposal: Warming `main`'s CI Cache

- **Status:** Proposed — awaiting decision (§6).
- **Date:** 2026-09-01
- **Related:** #159 (this spike, supersedes #130 — see #130's closing
  comment for why), #157/#158 (the merge queue landing is what made this
  visible, not what caused it), `docs/development/ci.md` (`test.yml`'s
  cache step and "why pull requests only, not `main`" section, both
  referenced below).

## 1. Problem statement

Watching a full merge-queue batch (5 entries queued at once behind #158)
sit in `AWAITING_CHECKS` for 10+ minutes turned up more than "the queue
is slow" — every one of those five was doing an independent, complete
`cabal build all` from scratch, at the same time, with nothing cached to
restore. That's the actual thing worth fixing: **`main`'s CI cache scope
has never been populated**, so any genuinely new ref — a PR's first
push, or (now) every merge-queue entry — pays a full cold build no
matter what's already been built and cached elsewhere in the repo.

#130 was filed first as a plausible-sounding candidate for this, but it's
scoped to a different problem (`test.yml`/`integration-test.yml` both
building the same commit in one PR run) that doesn't actually explain
what's happening here — see its closing comment. This proposal replaces
it with the right scope.

## 2. Confirmed root cause

GitHub Actions cache reads are restricted to two scopes: the run's own
**current branch**, or the repository's **default branch** (`main`).
A matching cache *key* is necessary but not sufficient — the run also
has to be allowed to see that scope. Per GitHub's docs, only certain
trigger types are allowed to *write* a cache into the true
default-branch scope: `push`, `workflow_dispatch`,
`repository_dispatch`, `delete`, `registry_package`, and `schedule`.
`test.yml` — the only workflow with the cache step that matters, since
it's the required check — triggers on `pull_request` and `merge_group`
only. **Neither is on that list.** No run has ever written a cache into
`main`'s actual default-branch scope; every cache that's ever existed
lives under some PR's own merge-ref scope, restorable only by later runs
of that same PR.

This was verified directly, not just reasoned about, using three real
runs against the identical dependency hash
(`Linux-cabal-d099dc1ef68c49215bbd8b7204b0b4e9d3c327b6875db5e5454dc4c64f0a2582`,
from `typeio.cabal`/`cabal.project` unchanged across all three):

| Run | Ref | `cabal build all` | Cache step log |
|---|---|---|---|
| First push to a new PR branch (`feature/issue-119-duplicate-project-card-id`, run 33525888116) | own branch, no prior cache anywhere | **7m36s** | *(cold — nothing to restore yet)* |
| Third push, **same branch** (run 33530149463) | own branch, cache from the first push now exists | **23s** | `Cache hit for: Linux-cabal-d099dc1...` → `Cache restored successfully` |
| Merge-queue entry for a **different** PR (#156, run 33534902505), ~45 min after that PR's own first-push run had already built and cached this exact key | `gh-readonly-queue/main/pr-156-...` — brand-new synthetic branch | **7m53s** | `Cache not found for input keys: Linux-cabal-d099dc1..., Linux-cabal-` |

The third row is the proof: the exact same key (down to the hash) had
already been cached by an unrelated PR less than an hour earlier, and
the `restore-keys: Linux-cabal-` prefix fallback — which should match
*any* previously-saved cache regardless of exact hash — still came back
empty. That's not a key mismatch; that's a run with no eligible scope to
read from at all. Every merge-queue entry hits this, every time, because
its synthetic branch (`gh-readonly-queue/main/pr-<n>-<sha>`) is brand new
by construction — there is no "current branch" cache for it to fall back
to, and (per the above) no populated default-branch cache either.

**Cost**: roughly 7–8 minutes of pure `cabal build all` time, cold,
per affected run — every PR's first push, and now every merge-queue
entry unconditionally. The merge queue didn't cause this; it just made
several cold builds happen at once instead of spread out one-per-PR with
gaps between them, which is what made it visible.

## 3. Options considered

**Option A — a `push`-to-`main` trigger that warms the cache, build only.**
Add a small, separate workflow (not folded into `test.yml`) that
triggers on `push: branches: [main]`, does exactly the setup/cache/build
steps `test.yml` already has (same key, same `path`), and stops there —
no `cabal test`. Since every merge (direct or via the queue) results in
a push to `main`, this runs right after the one event that can actually
invalidate the cache (a dependency change landing), and nowhere else.

Steady-state cost: the *first* run after this lands pays one full cold
build (~8 min) to seed the scope. Every run after that is cheap unless
`typeio.cabal`/`cabal.project`'s hash actually changed — in that case
it's the same 23-second-style incremental build the same-branch-repeat-push
case above showed (cabal doesn't re-fetch/rebuild unchanged dependencies,
and `actions/cache`'s own log even skips the save step entirely when the
resolved key already exists, as seen in the same-branch case: `Cache hit
occurred on the primary key ..., not saving cache`). From then on, every
PR's first push and every merge-queue entry has a real default-branch
cache to fall back to instead of nothing.

Deliberately **not** running `cabal test` on this trigger: `ci.md`'s
existing "why pull requests only, not `main`" reasoning — anything on
`main` already passed this exact check via its PR/queue entry, so
re-checking correctness again is redundant — still holds for
*correctness*. It just wasn't written with cache-population as a
consideration. Scoping this new trigger to build-only keeps that
reasoning intact for the test itself while adding the one thing it
didn't cover.

**Option B — a `schedule`-triggered cache-warm workflow** (e.g. hourly),
decoupled from merges entirely. Avoids tying any extra CI time to the
merge path, but has a real gap: after a dependency change lands, PRs and
queue entries cold-build for however long remains until the next
scheduled run, rather than immediately — the exact failure mode this
proposal exists to remove, just bounded instead of permanent. It also
doesn't obviously cost less than Option A: on a repo with infrequent
dependency changes, a periodic run pays the same near-instant
incremental-rebuild cost Option A's steady state does, just on a timer
instead of tied to the event that actually matters.

**Option C — leave it as-is.** Justifiable if the measured cost were
small or `typeio.cabal`/`cabal.project` changed often enough that most
builds would be full rebuilds regardless of scope. Neither holds here:
the dependency set is stable (all three timed runs above used the
identical hash), and 7–8 minutes cold vs. 23 seconds warm is a real,
repeated cost on every PR's first push and — now, with the merge queue —
on every queue entry, concurrently.

## 4. Recommendation

**Option A** — a small, dedicated `push`-to-`main`, build-only
cache-warming workflow. It targets the exact event that can invalidate
the cache, costs one cold build to adopt and stays cheap afterward
(confirmed by the same-hash incremental-build timing already measured
above, not projected), and leaves `test.yml`'s own required-check
semantics and `ci.md`'s existing "pull requests only" reasoning
untouched — this is additive, not a change to what already works.

A separate workflow file rather than extending `test.yml`: `test.yml` is
already juggling the `pull_request`/`merge_group` split (concurrency
group, base-ref diffing) added for the merge queue; a third trigger with
different semantics (build-only, and possibly gated differently — see
§5) is cleaner as its own file, consistent with this repo's existing
one-workflow-per-concern pattern (`test.yml`/`integration-test.yml`/
`security-scan.yml`/etc. are already split the same way).

## 5. Open questions / follow-up

- Should the warm-cache workflow run on *every* push to `main`
  unconditionally, or reuse `test.yml`'s "relevant changed" check so a
  docs-only merge doesn't even attempt it? Given the steady-state cost
  of a no-op run is small (same near-instant path as any other
  cache-hit build) and the `if: relevant` machinery exists specifically
  to skip GHC setup entirely, reusing that gate seems worth doing rather
  than skipping it, but it's a small enough decision to leave to
  implementation.
- Confirm the new workflow's cache `path`/`key` are copied exactly from
  `test.yml`'s (`${{ runner.os }}-cabal-${{ hashFiles('typeio.cabal',
  'cabal.project') }}`) — any drift between the two would silently
  recreate this same scope problem for a different reason (mismatched
  keys instead of mismatched scope).
- If Option A is chosen, a follow-up implementation ticket covers:
  the new workflow file, and a short addition to `docs/development/ci.md`
  documenting it alongside the existing workflow list.

## 6. Decision

Pending — this write-up is for review, not yet confirmed. Recommending
Option A per §4; follow-up implementation ticket (#160) filed with that
design, but not started, pending sign-off here.
