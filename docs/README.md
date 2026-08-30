# docs

Repository for architecture notes, developer documentation, and solution
proposals for this project.

## Layout

- `solution-proposals/` — spike/investigation write-ups that compare options
  for a problem and recommend a path forward, before implementation work is
  ticketed. One document per proposal, named for its topic.
- `development/` — reference docs on how the app actually works, grouped by
  area (`ui/`, `frontend/`, `backend/`), plus onboarding. Each area has an
  `index.md` linking its own files.

## Index

- [`solution-proposals/haskell-auto-formatting.md`](solution-proposals/haskell-auto-formatting.md) — options for an auto-formatting setup for `.hs` files that works for both human editors (format-on-save) and AI agents.
- [`development/ui/index.md`](development/ui/index.md) — the `#container`/`#view` component pattern, how UI is rendered directly in Haskell (Lucid), and the global-vs-scoped CSS split.
- [`development/backend/routing.md`](development/backend/routing.md) — the `Data.HashTree`-based router: how routes are built, and the prefix-match/per-request-rebuild behavior worth knowing about.
- [`development/backend/environment.md`](development/backend/environment.md) — the `Env` record (config, logger, DB pool) acquired once at startup, and how it differs from Containers.
