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
- **Verifying a change:** there is currently no `cabal test` suite —
  `cabal build all` (compiles clean, `-Wall` is on) is the standard
  verification step. Run relevant `make migrate-*` commands for
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
  4. Implement the change.
  5. Verify with `cabal build all` (and migration commands if relevant).
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

- **Case-sensitive git paths on a case-insensitive filesystem:** a few
  tracked paths differ in case from their on-disk directory (e.g.
  `lib/src/domain/project/responder/api/node/Get.hs` is tracked lowercase
  even though the sibling directories are capitalized). `git checkout --
  <path>` and similar must use the case `git status`/`git ls-files`
  report, not what `ls`/`find` show, or it fails with "pathspec did not
  match any file(s) known to git."
- **`make test-migrations` is broken** — see Setup section above.
