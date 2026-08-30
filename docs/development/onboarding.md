# Developer Onboarding

Start here. This page gets you running locally, then walks one request
through the whole app so the rest of `docs/development/` has somewhere to
hang off of.

## Prerequisites

- GHC + `cabal`, via [ghcup](https://www.haskell.org/ghcup/).
- Docker, for PostgreSQL.
- A `.env` file at the repo root (see the keys `Config.App`/`Config.Db`/
  `Config.Web` look up — `DB_HOST`, `DB_PORT`, `DB_DATABASE`, `DB_USER`,
  `DB_PASS`, `DB_POOL_COUNT`, `DB_SCHEMA`, `ENV`, `WEB_PORT`,
  `WEB_INDEX_REDIRECT`, `WEB_REQUEST_ID_HEADER`). Loading is silently
  best-effort (`Platform.Web.loadDotEnv` swallows a missing file), but
  the app will fail fast at startup with every missing/invalid variable
  listed at once if any of these aren't actually set — see
  [`backend/environment.md`](backend/environment.md).

## Getting it running

```
make run-postgres      # start Postgres in Docker
make migrate-up        # apply all migrations
make seed-db           # seed sample data
cabal build all        # build everything
cabal run server        # start the app, reads .env
```

Other `make migrate-*` targets (`migrate-down`, `migrate-down-all`,
`migrate-new NAME=...`, `migrate-version`, `migrate-force VERSION=...`)
cover the rest of the migration lifecycle — see the `Makefile`.

**Verifying a change:** there's no `cabal test` suite yet — `cabal build
all` (with `-Wall` on) is the standard check. `make test-migrations` is
currently broken (it calls a script, `scripts/test-migrations.sh`, that
doesn't exist in the repo) — don't rely on it.

## How a request flows through the app

Roughly, from `cabal run server` down to a response:

1. **`Platform.Web.main`** loads `.env`, loads and validates config
   (`Config.App`), and acquires the process-lifetime resources —
   config/logger/DB pool — as an `Env` (see
   [`backend/environment.md`](backend/environment.md)).
2. That `Env` is used to build a `RootContainer` — a tree of
   already-wired handler functions, one branch per domain, each split
   into API/UI sub-containers (see
   [`backend/containers.md`](backend/containers.md)).
3. Every request passes through an ordered middleware pipeline
   (`Platform.Web.Middleware`) before routing: a request-id gets tagged
   on, then request logging, then response logging, then the
   index-render middleware (`Domain.Central.Middleware.IndexRender` —
   for a direct/non-htmx request, or an htmx history-restore request, to
   a `ui/.../vw` path, it re-wraps the response in the full `#container`
   shell instead of returning a bare fragment, so a refreshed or
   bookmarked URL still works), then static file serving. Order is
   load-bearing here — request-id has to run before the two logging
   middleware, or they'd have nothing to log (see
   [`backend/logging.md`](backend/logging.md)).
4. **`Platform.Web.Router.routeRequest`** takes what's left, matches the
   request path and method against a hand-built route tree, and pulls
   the specific handler out of the `RootContainer` for that route (see
   [`backend/routing.md`](backend/routing.md)).
5. The handler runs — usually a DB query via esqueleto/persistent (which
   is where the *second* logging pipeline, DB query logging, kicks in;
   also covered in `backend/logging.md`) — and renders `Html ()` via
   Lucid into the HTTP response body (see
   [`ui/haskell-rendering.md`](ui/haskell-rendering.md)).
6. In the browser, that response is usually the result of an
   [htmx](frontend/htmx.md) request swapping into `#container` or a
   narrower target — see [`ui/components.md`](ui/components.md) for what
   `#container`/`#view` are, and [`frontend/hyperscript.md`](frontend/hyperscript.md)
   for the small per-element effects layered on top.

## Where to go next

- [`ui/index.md`](ui/index.md) — the `#container`/`#view` pattern,
  Lucid-as-templating, and global vs. scoped CSS.
- [`frontend/index.md`](frontend/index.md) — htmx and hyperscript.
- [`backend/routing.md`](backend/routing.md),
  [`backend/environment.md`](backend/environment.md),
  [`backend/containers.md`](backend/containers.md),
  [`backend/logging.md`](backend/logging.md) — the bespoke backend stack,
  one concern per file.
- [`../solution-proposals/`](../solution-proposals/) — spikes and
  decisions, e.g. the plan to adopt Fourmolu for auto-formatting.

## A couple of things that will trip you up

- **Case-sensitive git paths on this (probably case-insensitive)
  filesystem**: at least one tracked path's case doesn't match its
  on-disk directory (`lib/src/domain/project/responder/api/node/Get.hs`
  is tracked lowercase; its sibling directories are capitalized). If
  `git checkout -- <path>` says "pathspec did not match," check
  `git status`/`git ls-files` for the actual tracked case rather than
  what `ls`/`find` shows you.
- **Routes match on a path prefix, not an exact path** — see
  [`backend/routing.md`](backend/routing.md) for why.
