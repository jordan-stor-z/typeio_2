# CLAUDE.md — Project Guide & Rules

## Project Overview

A Haskell web application, backed by PostgreSQL, for tracking project
tasks ("nodes") and the dependencies between them. The UI presents a
graph-based layout for visualizing and managing those dependencies.

## Tech Stack & Architecture

- **Language & Runtime:** Haskell, built with `cabal` (GHC via `ghcup`).
- **Web layer:** no external framework (no Scotty/Servant/Yesod) — a
  hand-rolled stack on WAI/Warp:
  - Routing: a custom tree-based router (`Platform.Web.Router`, built on
    `Data.HashTree` combinators), not string-pattern matching.
  - Dependency wiring: a `Container` pattern (`Container.Root`,
    `Container.Build`) — root → per-domain → API/UI sub-containers, each
    holding just the dependencies its handlers need. This project's form
    of DI; no typeclass-based effects system.
  - Middleware: an ordered pipeline in `Platform.Web.Middleware`
    (request-id tagging, request/response logging, index rendering,
    static file serving) — order matters, it's composed via `foldr1 (.)`.
  - Logging: structured JSON via `Logging.Core` (`fast-logger`), with two
    consumers — DB query logging (`Logging.Database`) and HTTP
    request/response logging (`Domain.System.Middleware.Logging.*`),
    correlated by a per-request UUID.
  - Full write-up: [`docs/development/backend/`](docs/development/backend/)
    (one file each for routing, environment, containers, logging).
- **HTML rendering:** Lucid — UI is built as Haskell combinators
  (`Html ()` values, e.g. `div_`, `header_`) evaluated server-side to
  HTML. There are no template files; a `View.hs`/template module per
  feature is the pattern (see `responder/ui/*/View.hs`). Full write-up:
  [`docs/development/ui/`](docs/development/ui/).
- **Client-side:** htmx (partial-page swaps between a persistent
  `#container` shell and per-page `#view` fragments), hyperscript.org
  (the `h_ "..."` attribute, for small declarative effects like
  flash-on-update), and D3.js (the dependency-graph visualization). Full
  write-up: [`docs/development/frontend/`](docs/development/frontend/).
- **Database:** PostgreSQL 15 (Docker), accessed via esqueleto/persistent.
- **Migrations:** SQL files in `migrations/`, managed via the `migrate`
  CLI, paired `.up.sql`/`.down.sql`.

## Docs Map

The bullets above are a fast-orientation summary, not the full picture —
`docs/` has the actual depth (rationale, gotchas, code examples). Start
at [`docs/README.md`](docs/README.md) for the full index; the common
cases:

| Need to know about... | Read |
|---|---|
| Getting set up / how a request flows end-to-end | `docs/development/onboarding.md` |
| `#container`/`#view`, Lucid rendering, CSS conventions | `docs/development/ui/` |
| htmx or hyperscript attribute patterns | `docs/development/frontend/` |
| The router, `Env`, containers (DI), or logging | `docs/development/backend/` (one file each) |
| CI: what it runs, when, and how to reproduce it locally | `docs/development/ci.md` |
| Which GitHub issue labels to use | `docs/development/labels.md` |
| An open design question / pending decision | `docs/solution-proposals/` |

If you're about to touch code in one of these areas and haven't read its
doc yet, read it first — that's the point of it existing.

## Setup & Local Development

- **Build:** `cabal build all`
- **Run the server:** `cabal run server` (loads config from `.env` — see
  that file for the required variables; none are hardcoded).
- **Start Postgres:** `make run-postgres`
- **Migrations:** `make migrate-up` / `make migrate-down` /
  `make migrate-down-all` / `make migrate-new NAME=<name>` /
  `make migrate-version` / `make migrate-force VERSION=<v>`
- **Seed the database:** `make seed-db`
- **Verifying a change:** `cabal build all` (compiles clean, `-Wall` is
  on). A unit test suite exists (`cabal test` / `make test`) and CI runs
  it on every PR into `main` (GitHub Actions, `.github/workflows/test.yml`
  — see [`docs/development/ci.md`](docs/development/ci.md)), so running
  it locally before pushing is no longer required — the PR is the
  enforcement point. Run relevant `make migrate-*` commands for
  migration changes.
- ⚠️ **`make test-migrations` is currently broken** — it calls
  `./scripts/test-migrations.sh`, which does not exist anywhere in the
  repo. Don't rely on it; use `cabal build all` instead until it's fixed.

## Database Schema (`project`)

- `project.project`: Core project container.
- `project.node`: Project nodes/tasks — JSONB attributes, description,
  title, timestamps, references to project/status/type.
- `project.node_type` / `project.node_status`: Valid node categories and
  status states.
- `project.node_status_change`: Audit trail of node status transitions.
- `project.dependency`: Graph edges, `node_id` → `to_node_id`.
- `project.project_vw`: View of root project nodes with a `last_updated`
  aggregate.

## Code & Style Conventions

- Explicit type signatures, clear module exports.
- Responder modules are one file per HTTP verb under
  `responder/api/<Domain>/<Verb>.hs` (e.g. `Get.hs`, `Post.hs`), and one
  `View.hs`/template module per feature under `responder/ui/<Feature>/`.
- SQL migrations: always paired `.up.sql`/`.down.sql`, sequential
  numbering.
- Never hardcode DB credentials or web ports — pull config from `.env`
  via `Config.App`/`Config.Db`/`Config.Web`.
- **Tests are expected alongside the code they cover** — currently pure,
  dependency-free modules (`Common.Validation`, `Data.*`, `Config.*`;
  see `docs/solution-proposals/unit-testing.md` for what's in/out of
  scope and why). CI running the suite on a PR (see Setup section) is a
  check that tests exist and pass, not a substitute for writing them —
  don't skip adding/updating a test for a change because CI will "catch
  it anyway."
- **Formatting is currently manual**: this codebase hand-aligns `=` in
  `let`/`where` bindings and record literals (see the style in e.g.
  `IndexRender.hs`). Match existing alignment in any file you touch, but
  don't go re-align unrelated lines as a drive-by change. This convention
  is being retired in favor of automated Fourmolu formatting — see
  `docs/solution-proposals/haskell-auto-formatting.md` and issue #6. Once
  that lands, run the formatter instead of hand-aligning.

## Ticket & Branching Conventions

- Tickets are tracked as **GitHub Issues** — use `gh issue list` /
  `gh issue view <n>`, not a local file.
- **When creating an issue**, apply one `type:*` and one `area:*` label
  per [`docs/development/labels.md`](docs/development/labels.md) —
  `gh issue create` takes `--label` directly. Not optional/an
  afterthought; do it at creation time.
- Branch naming: `feature/issue-$N-<short-description>` for issue `$N`.
- Workflow:
  1. `gh issue view <n> --comments` to read the ticket — **`--comments`
     matters**: plain `gh issue view <n>` only shows a comment *count*,
     not their content, so a heads-up left on a ticket (e.g. a snag
     found implementing a related issue) is silently invisible without
     the flag.
  2. Confirm a clean workspace (`git status`), then
     `git checkout main && git pull`.
  3. `git checkout -b feature/issue-$N-<short-description>`.
  4. Implement the change, adding/updating tests for it (see Code &
     Style Conventions).
  5. Verify with `cabal build all` (and migration commands if relevant).
     Running `cabal test`/`make test` locally is optional — CI runs it
     on the PR — but it's the fastest way to find a failure early.
  6. Commit referencing the issue, push, and open a PR with
     `gh pr create` — include `Closes #$N` in the body when the PR fully
     resolves the ticket.
  7. If the ticket auto-closed from an earlier merge, add a closing
     comment linking the PR instead of re-closing it.
- **PR comments come from two separate GitHub APIs — check both, every
  time, or you will miss feedback.** `gh pr view <n> --json comments`
  only returns top-level conversation comments. Inline/file-anchored
  review comments (left on a specific line in the GitHub UI's "Files
  changed" tab) do **not** show up there — they need
  `gh api repos/<owner>/<repo>/pulls/<n>/comments`. When asked to check
  a PR for feedback, run both before concluding there's nothing to
  address. To reply to an inline comment specifically (not just leave a
  new top-level comment), use
  `gh api repos/<owner>/<repo>/pulls/<n>/comments -f body="..." -F in_reply_to=<comment_id>`,
  with the `id` from the inline-comments listing above.

## Git Safety & Branch Boundaries (STRICT)

- **NEVER merge branches or PRs.** No `git merge`, `git rebase`,
  `gh pr merge`, or other merge actions.
- **NEVER check out `main` to edit it directly.** Only check it out to
  sync (`git checkout main && git pull`) before branching.
- **NEVER push directly to `main` or `master`.**
- **Hand-off Rule:** once a feature branch is pushed and verified
  (`cabal build all`, plus migration checks if relevant), open a PR and
  stop — merging is left to the user.

## Known Gotchas

- **`make test-migrations` is broken** — see Setup section above.

## Resolved gotchas (kept for context — don't reintroduce)

- **Every `lib/src` directory used to be lowercase while every module
  name is PascalCase** (`lib/src/platform/Web.hs` for module
  `Platform.Web`, etc.) — invisible on macOS's case-insensitive
  filesystem, which is why local development and manual verification
  never caught it. It surfaced the moment CI (#41) ran on a Linux
  runner: GHC couldn't find any module at all
  (`Cabal-7554: can't find source for Platform/Web in lib/src`). Fixed
  by renaming every directory to match its module path's exact casing —
  `lib/src` now mirrors the module tree byte-for-byte. Two of the
  mismatches were at the **git index** level specifically (tracked case
  differed from the on-disk case shown by `ls`/`find`, which a plain
  filesystem walk can't detect on a case-preserving filesystem) — if a
  future rename ever needs to fix a case mismatch again, use a two-step
  `git mv old old_tmp && git mv old_tmp New`; a direct `git mv old New`
  fails outright on macOS with "Invalid argument" for a case-only
  rename.
