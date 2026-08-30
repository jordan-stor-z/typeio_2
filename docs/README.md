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
- [`development/backend/containers.md`](development/backend/containers.md) — the Container dependency-injection pattern: Root → per-domain → API/UI sub-containers.
