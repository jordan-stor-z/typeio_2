# Solution Proposal: Security Scanning for CI/CD

- **Status:** Decided — see §11. Implementation tracked in #99–#101.
- **Date:** 2026-08-31
- **Related:** #62 (this spike), `docs/development/ci.md` (required-check
  path-filter gotcha this proposal designs around), #46/#47 (pending
  Terraform/Terragrunt CI — infrastructure-dependency scanning is a
  separate surface, noted but not designed here)

## 1. Problem statement

The repo has no dependency or vulnerability scanning today. The only
GitHub Actions workflow (`.github/workflows/test.yml`) builds and tests
the app; nothing checks either the Haskell or JS dependency surface
against known CVEs. The repo is public, which changes what "free" means
here — several relevant tools are free specifically *because* the repo
is public, not free in general.

## 2. What's actually in the dependency surface

### 2.1 Haskell deps: real, unpinned

`typeio.cabal`'s `build-depends` sets version bounds (e.g.
`base ^>=4.18.3.0`) but there's no `cabal.project.freeze` — no file
records the exact resolved version set a build actually lands on. This
matters for scanning: a tool that reads *bounds* can only tell you a
vulnerable version is theoretically allowed, not that it's what you're
actually shipping. A tool that reads a *freeze file* can tell you
exactly what's in the build. This proposal recommends generating one (see
§5).

### 2.2 JS deps: not real — `wifi-password` is orphaned

`package.json`/`package-lock.json` exist at the repo root with exactly
one dependency, `wifi-password`. Tracing it down:

- `git log --follow -- package.json` shows it was added in a single
  commit, `664da9b` ("refactor: make tree type for routing") — an
  unrelated Haskell routing change. It rode along incidentally, not as
  a deliberate addition.
- Nothing in the repo references it: `grep -rn "wifi-password"` across
  `.hs`/`.md`/`.json`/`.sh`/`Makefile` finds only its own two entries in
  `package.json`/`package-lock.json`.
- `docs/development/frontend/index.md` confirms what actually ships:
  HTMX, hyperscript, and D3 are all loaded from vendored static files
  (`static/script/{htmx,d3,nodetree,nodetree2}.js`), embedded directly
  in `IndexView.hs`'s `<head>`. There is no JS build step, no `npm
  install`/`npm run build` anywhere in `Makefile`, `.github/`, or the
  docs — confirmed by grepping all three for `npm`/`node_modules`/
  `package.json` and finding nothing.

**Resolution: `wifi-password` is not part of the shipped app.** It's a
stray artifact of a local `npm install` that got committed alongside an
unrelated change, not a real dependency of anything that runs. It
doesn't need scanning because it isn't shipped — but leaving it in place
means any JS scan (Dependabot or otherwise) reports on a package that
does nothing for this app, which is pure noise. Recommend removing
`package.json`/`package-lock.json`/`node_modules` entirely in the
follow-up implementation ticket, and scoping the "JS ecosystem" question
in this proposal to **if JS dependencies are ever real again** (e.g. a
future build step), not to what exists today.

This also simplifies the rest of this proposal: with `wifi-password`
gone, there is currently **no JS dependency surface to scan at all**.
Everything below that discusses "npm" is about what to have in place
*if/when* real JS deps show up, not a present need.

## 3. What's free, given the repo is public

GitHub Advanced Security features that are normally paid on private
repos are free on public ones, no billing enrollment required:

| Feature | Free on public repos? | Covers |
|---|---|---|
| Secret scanning | Yes | Credentials/tokens committed to the repo |
| Dependabot alerts | Yes | Known-vulnerable dependencies, via GitHub's own advisory database |
| Dependabot security updates | Yes | Auto-opened PRs bumping a vulnerable dep to a fixed version |
| Dependabot version updates | Yes | Routine version-bump PRs (not security-specific) |

None of these need a new CI workflow — they're repo settings
(`Settings → Code security`) plus, for Dependabot, a
`.github/dependabot.yml` config file.

**The catch: ecosystem support.** Dependabot's alerts/updates are
ecosystem-specific — it only understands a dependency file if it
recognizes the ecosystem. Checked directly against GitHub's current
supported-ecosystems list (2026-08-31):

- **npm**: fully supported (alerts, security updates, version updates).
  Moot for now per §2.2, but ready to go the moment real JS deps exist.
- **Hackage/cabal**: **not supported.** GitHub's ecosystem table has no
  Haskell/Hackage/Cabal entry. This has been an open ask (tracked
  upstream in `dependabot/dependabot-core#2745`) but isn't shipped as of
  this writing — confirmed by checking GitHub's docs directly rather
  than assuming either way, since the ticket flagged this as something
  that changes over time.

So: **secret scanning is a full, free answer on its own.** Dependabot
alerts/updates are a full, free answer for JS (currently moot) but
**not** for Haskell — the one ecosystem this repo actually has real,
unpinned dependencies in. That's the gap this proposal has to fill.

## 4. Filling the Hackage gap: OSV-Scanner

[OSV-Scanner](https://github.com/google/osv-scanner) (Google, Apache-2.0,
backed by [OSV.dev](https://osv.dev/)) is free, OSS, and — as of its V2
release — supports both ecosystems this repo needs from one tool and one
CI job:

- **Haskell**: reads `cabal.project.freeze` (and `stack.yaml.lock`,
  unused here) directly, matching against OSV.dev's Hackage advisory
  data.
- **npm**: reads `package-lock.json`, for whenever real JS deps exist
  again.
- Ships an official reusable GitHub Action
  (`google/osv-scanner-action`), so wiring it in doesn't mean hand-rolling
  scan logic.

Compared to ecosystem-specific alternatives (Haskell's own
`hsec-tools`/cabal-audit-style tooling, `npm audit`): those would mean
two tools, two config surfaces, and two places to update if either
ecosystem's tooling changes, for no coverage OSV-Scanner doesn't already
give in one job. **Recommendation: OSV-Scanner, one job, both
ecosystems** — not a per-ecosystem tool split.

This makes the actual shape: **Dependabot (native, free, zero workflow)
handles alerting and auto-remediation for whatever it supports (npm
today; possibly Hackage later, unprompted, if GitHub ships it) and
secret scanning; OSV-Scanner (one CI job) is specifically the Hackage
gap-filler** for as long as Dependabot doesn't cover it. If GitHub adds
Hackage support to Dependabot later, OSV-Scanner's Haskell scanning
becomes redundant with a free native feature — worth revisiting then
(see §8), not a reason to wait now.

## 5. The missing freeze file

OSV-Scanner needs `cabal.project.freeze` to know exact resolved
versions; this repo doesn't have one (§2.1). Two ways to get one for the
scan to read:

| Option | How | Fit |
|---|---|---|
| Generate at scan time (`cabal freeze` as a CI step, before invoking OSV-Scanner) | No new committed file; freeze reflects whatever the current lockless bounds resolve to *right now* | **Recommended.** Matches "no freeze file today" as a deliberate-enough status quo not to overturn as a side effect of this spike — scanning shouldn't quietly change how the project manages Haskell dependency pinning. Cheap: one extra `cabal freeze` invocation in the same job that already sets up GHC/cabal. |
| Commit `cabal.project.freeze` to the repo | Pin exact versions permanently, update deliberately | Bigger decision (whether this project wants pinned builds at all) that's out of scope for a security-scanning spike — don't fold it in here. |

**Recommendation: generate the freeze file at scan time, don't commit
one.** If the project later decides to commit a real freeze file for
build-reproducibility reasons, that's a separate proposal — the scanning
job would just use the committed one instead with no other change.

## 6. Trigger model: per-PR and scheduled, both — different problems

- **Per-PR** catches a newly *introduced* vulnerable dependency (someone
  bumps a version, or adds a new dep, that's already known-bad).
- **Scheduled** catches a newly *disclosed* CVE in a dependency that
  hasn't changed — the vulnerability didn't exist (or wasn't known) when
  the dependency was last touched, so no PR would ever trigger a check
  for it.

These aren't redundant — a repo that only ran per-PR scanning could sit
on a dependency that becomes known-vulnerable the day after its last PR
and never find out. **Recommendation: both.**

- **Per-PR**: same trigger shape as `test.yml` — runs on every PR into
  `main`.
- **Scheduled**: weekly (`schedule: cron: ...`), matching the cadence
  Dependabot itself defaults to for version updates and reasonable for a
  low-churn dependency set — daily would be scanning-for-scanning's-sake
  here, not evidence-based.

## 7. Blocking vs. informational: informational, not required

**Recommendation: informational — a workflow annotation/summary, not a
required check.** Reasoning:

- A required check that's ever skipped via a top-level `paths:` filter
  is the exact incident `docs/development/ci.md` documents for
  `test.yml`: GitHub has no "doesn't apply" state, only pass/missing, so
  a required check a path filter prevents from running stays
  permanently missing and blocks merging with no admin bypass
  (`enforce_admins: true` on this repo). §6's scheduled run in
  particular has no PR to attach a required-check result to at all,
  which a required-check design would need to account for separately.
- More fundamentally: a scan finding on a per-PR run isn't necessarily
  about code the PR introduced (a pre-existing vulnerable dep the PR
  didn't touch would still be flagged), so blocking that specific PR on
  it conflates "this PR is bad" with "this repo has a finding right
  now." Dependabot alerts already model this correctly — findings
  surface in the Security tab, not as merge gates.
- If this ever needs to become required later, the same job-level `if:`
  pattern `test.yml` already uses (always run the job, skip the
  expensive steps internally) is available and should be reused rather
  than a top-level `paths:` filter — noting this so a future change
  doesn't have to rediscover it, not designing it now since nothing
  above calls for it yet.

Findings should be visible without digging: GitHub Actions' job summary
(`$GITHUB_STEP_SUMMARY`) for the OSV-Scanner output, plus Dependabot's
existing Security tab for what it already covers. No new dashboard or
third-party service.

## 8. Where this fits relative to `test.yml`

**Recommendation: a separate workflow file**
(`.github/workflows/security-scan.yml`), not a new job in `test.yml`.

- Different trigger shape: `test.yml` is pull-request-only (§ "Why pull
  requests only" in `docs/development/ci.md` — reasoning specific to
  "anything on `main` already passed this exact check via its PR").
  Security scanning needs a `schedule` trigger too (§6), which doesn't
  fit that reasoning — a scheduled run isn't re-checking something a PR
  already covered, it's checking for drift in the *outside world*
  (newly disclosed CVEs), which `main` is exactly the right target for.
- Different purpose and blocking semantics: `test.yml` is a required
  correctness gate; this is an informational report (§7). Keeping them
  in one file would mean one workflow mixing a required and a
  non-required job, and mixing PR-only and scheduled triggers — harder
  to reason about than two files with one clear job each.
- Both still benefit from `test.yml`'s established pattern (job always
  runs, `if:`-gated expensive steps) *if* this job is ever made
  required — see §7.

**Infrastructure deps (#46/#47) are a separate surface, not designed
here.** If/when Terragrunt/Terraform CI lands, its provider/module
dependencies would need their own scanning story (likely `tfsec`/
`checkov`/similar, not OSV-Scanner) — flagged for whoever picks that up,
out of scope for this proposal.

## 9. Recommended shape, summarized

1. **Enable natively, no workflow needed**: Dependabot alerts,
   Dependabot security updates, and secret scanning, via repo settings
   + a minimal `.github/dependabot.yml` (npm ecosystem entry — ready for
   whenever real JS deps exist; no Hackage entry, since GitHub doesn't
   support one yet).
2. **New workflow** `.github/workflows/security-scan.yml`:
   - Triggers: `pull_request` into `main`, and a weekly `schedule`.
   - Steps: set up GHC/cabal (reuse `test.yml`'s
     `haskell-actions/setup` step shape), `cabal freeze` to produce
     `cabal.project.freeze` for the scan to read (§5), then run
     `google/osv-scanner-action` against the repo root (picks up both
     the generated freeze file and `package-lock.json` if/when one is
     real again).
   - Informational only (§7): not a required check, findings surface via
     job summary.
3. **Cleanup, same PR or a fast-follow**: remove
   `package.json`/`package-lock.json`/`node_modules` (§2.2) — orphaned,
   not part of the app, and noise for any future JS scan.

## 10. Open questions

- Exact weekly `schedule` cron time — pick something low-traffic (e.g.
  early UTC morning); not load-bearing enough to decide here.
- Whether OSV-Scanner should also gain a `.osv-scanner.toml` ignore list
  for accepted-risk findings once the job has run for a while and
  produced real output to triage — premature to configure against
  hypothetical findings.
- If GitHub ships native Dependabot support for Hackage (tracked
  upstream in `dependabot/dependabot-core#2745`), revisit whether
  OSV-Scanner's Haskell coverage is still pulling weight or has become
  redundant with a free native feature — not a reason to wait on this
  proposal now, since there's no committed timeline for that upstream
  work.

## 11. Decision

Confirmed 2026-08-31. Every recommendation in §9 is adopted as written:

- **Native features, no workflow: Dependabot alerts, Dependabot
  security updates, and secret scanning**, plus a `.github/dependabot.yml`
  with an `npm` entry and no Hackage entry (§3) — tracked in #99.
- **Gap-filler: OSV-Scanner, one job, both ecosystems** — reads a
  scan-time-generated `cabal.project.freeze` (not committed) and
  `package-lock.json` (§4/§5) — tracked in #101.
- **Trigger model: both per-PR and weekly-scheduled**, since they
  catch different problems (newly introduced vs. newly disclosed CVEs)
  (§6) — tracked in #101.
- **Blocking: informational, not a required check** (§7) — tracked in
  #101.
- **A separate workflow file**, `security-scan.yml`, not a job in
  `test.yml` (§8) — tracked in #101.
- **Cleanup: remove `package.json`/`package-lock.json`** — orphaned,
  not part of the shipped app (§2.2/§9) — tracked in #100.

**Implementation tracked in #99–#101**, not yet landed as of this
Decision — unlike `release-management.md`'s decision, recorded here
*before* the code exists rather than after, so the doc doesn't drift
into "Proposed" limbo while the tickets sit open.

**Left open, on purpose — not indecision:** §10's three open questions
(the exact weekly cron time, an `.osv-scanner.toml` ignore list for
accepted-risk findings, and revisiting OSV-Scanner's Haskell coverage
if GitHub ever ships native Hackage support) are explicitly deferred —
the first is genuinely inconsequential, and the other two depend on
information (real scan output, an upstream ship date) that doesn't
exist yet.
