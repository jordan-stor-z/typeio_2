# Solution Proposal: Haskell Auto-Formatting

- **Status:** Proposed
- **Date:** 2026-08-30
- **Related:** #3 (this spike), #1 (manual equals-sign alignment fix that motivated it)

## 1. Problem statement

Right now nothing in this repo formats `.hs` files automatically. Style
drift gets caught by hand, in code review, or via one-off cleanup tickets
(#1 was exactly this: several `let`/`where` bindings and record literals
had fallen out of alignment with the rest of the file).

We want an auto-formatting setup that:

1. Can run on save for a human editing in an IDE.
2. Can be invoked by an AI agent (this one included) as part of its normal
   edit loop, without needing to be told "now go format the file."
3. Respects the conventions this codebase has already established, or lets
   us explicitly configure it to.

That third requirement turns out to be the crux of this spike, so most of
this document is about it.

## 2. Conventions currently in use

Surveying `lib/src/`, the codebase relies on two habits that most Haskell
formatters treat as a first-class target for *removal*, not preservation:

- **Manual vertical alignment of `=`** in `let`/`where` blocks and record
  literals, e.g. `lib/src/domain/central/middleware/IndexRender.hs`:

  ```haskell
  let
    path         = pathInfo req
    isHx         = hasHexHeader "HX-Request" hs
    isHxRestore  = hasHexHeader "HX-History-Restore-Request" hs
    isVwPath     = isView path
  ```

- **Manual alignment of import lists**, e.g. `lib/src/environment/Db.hs`:

  ```haskell
  import Config.Db                   (DbConfig(..), connStr)
  import Control.Monad               (void)
  import Control.Monad.Cont          (ContT(..))
  ```

There is no `.hlint.yaml`, `.stylish-haskell.yaml`, `fourmolu.yaml`, or
`.editorconfig` in the repo today — these conventions are enforced purely
by habit and review, which is why they drift (#1).

## 3. Options considered

| Tool | What it does | Configurable? | Preserves manual `=`/import alignment? | Maintenance status |
|---|---|---|---|---|
| [Ormolu](https://github.com/tweag/ormolu) | Full-file formatter, GHC-parser-based | No — "one true style" by design | No | Actively maintained |
| [Fourmolu](https://fourmolu.github.io/) | Fork of Ormolu, same engine, configurable knobs | Yes — indentation, import/export style, record braces, comma placement, `where`/`let`/`if` styling, etc. ([full option list](https://fourmolu.github.io/config/)) | No — none of its options expose column alignment of `=` or import lists | Actively maintained, tracks Ormolu upstream |
| [stylish-haskell](https://github.com/haskell/stylish-haskell) | Formats *imports, language pragmas, module exports* only — not function bodies | Yes, extensively, via `simple_align` steps | **Yes, for imports** — has an explicit global-alignment mode for import lists. Does not touch `let`/`where`/record bodies at all | Maintained, low velocity |
| [HIndent](https://hackage.haskell.org/package/hindent) | Full-file formatter | Some (indent width, line length) | No — reflows layout | Low activity |
| [Floskell](https://github.com/ennocramer/floskell) | Full-file formatter with named base styles | Yes, fairly granular per-construct | Partial — has some alignment toggles for `case`/`record` but not a documented `=`-in-`let` alignment mode, and the project has had long maintenance gaps | Largely dormant |
| [Brittany](https://github.com/lspitzner/brittany) | Full-file formatter | Some | No | Effectively unmaintained |
| [HLint](https://github.com/ndmitchell/hlint) | Not a formatter — a linter for code smells/idiom suggestions | Yes, via `.hlint.yaml` | N/A | Actively maintained |

**The key finding:** every actively-maintained full-body formatter
(Ormolu, Fourmolu) treats "line things up in columns by hand" as exactly
the kind of fragile, bit-rot-prone style it exists to eliminate, and gives
you no config knob to opt back in. Fourmolu's configurability covers
*layout choices* (brace placement, comma style, indent width, import
grouping) — not manual whitespace alignment. `stylish-haskell` is the one
tool that *can* preserve/enforce our import-alignment convention, but it
doesn't reach into function bodies, so it can't help with the `=`-alignment
convention that #1 was about.

This means the "respect current conventions" requirement doesn't have a
single tool that satisfies it end-to-end — it's a decision about which
convention we're willing to give up.

## 4. Integration story (assuming a formatter is chosen)

### Humans (format-on-save)

`haskell-language-server` (HLS — just added to this environment as a
plugin) exposes a pluggable `formattingProvider` setting that can be
pointed at `ormolu`, `fourmolu`, `brittany`, `floskell`, or
`stylish-haskell`. Any editor with an LSP client (VS Code + the Haskell
extension, Neovim, etc.) gets "format on save" for free once that setting
points at the chosen tool — no editor-specific plugin needed beyond HLS
itself.

### AI agents

Claude Code (and similar tools) support a `PostToolUse` hook in
`.claude/settings.json` that fires after `Write`/`Edit` calls. A hook
scoped to `*.hs` files that shells out to the formatter turns every agent
edit into a formatted one automatically, the same way format-on-save does
for a human — the agent doesn't need to be instructed to format, and
CLAUDE.md doesn't need to carry formatting rules that a tool can just
enforce. Example shape:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          { "type": "command", "command": "fourmolu --mode inplace \"$CLAUDE_TOOL_INPUT_FILE_PATH\" 2>/dev/null || true" }
        ]
      }
    ]
  }
}
```

(Illustrative only — not wired up as part of this spike; see Acceptance
Criteria in #3, which asks for a proposal, not an implementation.)

A `make format` / `make format-check` Makefile target is worth adding
regardless of which option below is chosen, so CI and pre-commit can share
the same command humans and agents use locally.

## 5. Recommendation

Two viable paths, depending on how much the team values the existing
alignment style versus eliminating this class of ticket permanently:

**Option A — Adopt Fourmolu, retire manual alignment. (Recommended)**
Configure `fourmolu.yaml` to match current conventions as closely as its
knobs allow (indentation, import grouping, record-brace style), run it
once over the whole codebase, wire it into HLS's `formattingProvider`, a
`make format` target, and a `PostToolUse` hook. Manual `=`/import-column
alignment goes away as a style — Fourmolu will keep single-space
consistency instead. Trade-off: a one-time large diff and the loss of a
stylistic preference. Benefit: formatting becomes fully automatic and
idempotent for both humans and agents, and tickets like #1 stop recurring
by construction.

**Option B — Keep manual alignment, automate only what's automatable.**
Adopt `stylish-haskell` for import lists only (it can enforce our existing
import-alignment convention), add `hlint` for idiom suggestions, and leave
`let`/`where`/record-body alignment as a manual/reviewed convention —
accepting that it will occasionally drift and need a cleanup pass like #1.
Lower short-term cost, but doesn't fully satisfy requirement #1 (format on
save / agent-invocable) for the body-alignment convention specifically,
since there's no tool to invoke for it.

Recommend **Option A**. The manual-alignment style is exactly the kind of
thing that generates recurring, low-value tickets (#1) and merge-conflict
noise; a one-time reformat is a small, boundable cost against permanently
removing that class of work for both humans and agents.

## 6. Open questions / follow-up

- Confirm the team is willing to give up manual `=`/import alignment
  before committing to Option A — this is a visible, repo-wide diff.
- If Option A is chosen, a follow-up ticket should cover: writing
  `fourmolu.yaml`, the one-time reformat PR, HLS config, `make format`
  target, and the `PostToolUse` hook.
- Re-evaluate Floskell/Brittany if either sees renewed maintenance
  activity — at the time of this spike both are effectively frozen.
