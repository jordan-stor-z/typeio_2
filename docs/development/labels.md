# Issue Labels

Every GitHub issue gets two labels: one `type:*` (what kind of work) and
one `area:*` (what part of the system). This taxonomy was derived from
an actual review of every issue filed in this repo so far (#1–#54), not
picked abstractly — see the mapping below.

## `type:*` — what kind of work

| Label | Meaning | Example |
|---|---|---|
| `type:bug` | Something isn't working correctly | #38, #48 — wrong/misleading behavior found and fixed |
| `type:chore` | Maintenance: cleanup, tooling adoption, refactors with no behavior change | #1, #6, #15 — formatting, Fourmolu adoption, dead-code removal |
| `type:documentation` | Adding or updating documentation | #7–#13, #35, #53, #54 |
| `type:feature` | New capability or behavior | #28–#34, #36, #41, #46, #47 |
| `type:spike` | Research/investigation producing a solution-proposal doc, not code | #3, #14, #17, #26, #42, #50, #58 |

## `area:*` — what part of the system

| Label | Meaning | Example |
|---|---|---|
| `area:backend` | Server-side Haskell: routing, containers, environment, config, logging | #8–#11, #15, #36, #38, #48 |
| `area:frontend` | Client-side interactivity: htmx, hyperscript | #12 — mirrors `docs/development/frontend/` |
| `area:ui` | Lucid-rendered HTML and CSS (`#container`/`#view`, global vs. scoped styles) | #7 — mirrors `docs/development/ui/`, deliberately kept separate from `area:frontend` since the docs tree already splits these two |
| `area:testing` | Unit, integration, or E2E test suites and their tooling | #17, #26, #28–#35, #42, #53 |
| `area:ci-cd` | GitHub Actions workflows and required checks | #41, #47 |
| `area:infrastructure` | Terraform/Terragrunt and other non-application infrastructure | #46 |
| `area:tooling` | Dev tooling: formatting, linting, build config | #1, #3, #6 |
| `area:process` | How the team/docs/repo operate — not the application itself | #54, #58, #59 (this one) |

## Conventions

- Every issue gets exactly one `type:*` label. Most get exactly one
  `area:*` label; an issue that genuinely spans more than one area (rare
  — most of this repo's issues have been cleanly single-area) can carry
  more than one.
- `area:ui` and `area:frontend` are intentionally separate, mirroring
  `docs/development/ui/` vs. `docs/development/frontend/` — don't
  collapse them into one "frontend" label just because they're both
  client-facing; they're different concerns (server-rendered markup/CSS
  vs. client-side interactivity).
- `area:testing` covers testing work regardless of *what* is being
  tested — a unit test for a backend module is `area:testing`, not
  `area:backend`, since the interesting fact about that issue is that
  it's testing work, not which module it happens to cover.
- This taxonomy is a snapshot of what this repo's issues have actually
  needed, not a fixed spec — if a new kind of issue doesn't fit either
  dimension well, that's a signal to reconsider the taxonomy the same
  way it was derived (review recent real issues), not to force-fit it or
  invent a label in isolation.
- GitHub's stock `bug`/`documentation`/`enhancement` labels were removed
  from this repo — they overlapped with `type:bug`/`type:documentation`/
  `type:feature` and having both would just be duplicate, confusing
  bookkeeping. The other stock labels (`duplicate`, `good first issue`,
  `help wanted`, `invalid`, `question`, `wontfix`) are untouched — they
  don't overlap with this taxonomy and remain available if needed.

## `run-e2e`

A special-purpose label, outside the `type:*`/`area:*` taxonomy above —
same bucket as the untouched stock labels the Conventions section calls
out (`good first issue`, `question`, etc.), not a `type:*` or `area:*`
label itself.

Opts a PR into `.github/workflows/e2e-test.yml`'s Playwright suite,
which otherwise doesn't run on every PR. It can be applied two ways:

- **On a PR directly** — the PR itself needs E2E coverage (e.g. no
  linked issue, or an already-open PR turns out to need it).
- **On an issue, at creation/triage time** — when the work is expected
  to need E2E coverage as part of its requirements, before any PR
  exists for it. `e2e-test.yml` also reads the labels of any issue a PR
  closes (via GitHub's closing-issue-references), so whichever PR later
  closes that issue picks up the requirement automatically — nobody has
  to remember to also label the PR.

See `docs/development/ci.md`'s "E2E test workflow" section for the full
mechanics.

## `review:approved`

Another special-purpose label outside the `type:*`/`area:*` taxonomy —
same bucket as `run-e2e` above, not a `type:*`/`area:*` label itself.

Applied by the user to a PR they've reviewed and want merged. It's the
merge-authorization signal `CLAUDE.md`'s Git Safety & Branch Boundaries
section requires before merging any PR is allowed — without it, a PR
sits open regardless of how ready its checks/diff look. Applying it is
the user's call to make, not something to add unprompted or infer from
a PR just having green checks or looking finished.

## When to apply labels

Per `CLAUDE.md`'s Ticket & Branching Conventions: apply the appropriate
`type:*` and `area:*` labels at issue-creation time, not as an
afterthought — `gh issue create` accepts `--label` directly. Also apply
`run-e2e` at issue-creation time when the work will need E2E coverage —
see the `run-e2e` section above.
