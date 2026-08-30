# CLAUDE.md — Project Guide & Rules

## Project Overview
This repository contains a Haskell web application backed by a PostgreSQL database and a lightweight frontend (D3.js). It manages relational project nodes, dependencies, node statuses, and status changes.

## Application
The application being developed is for tracking project tasks (known as nodes) and the dependencies between the tasks. The UI uses "graph-based" layouts to display dependencies and allows users to visualize and manage the dependencies.

## Tech Stack & Architecture
- **Language & Runtime:** Haskell
- **Database:** PostgreSQL 15 (managed via Docker)
- **Database Migrations:** SQL files in `migrations/` managed via `migrate` CLI
- **Frontend:** HTML/CSS & D3.js (located in `static/`)
- **Containerization & Tooling:** Docker, Bash scripts in `local/script/`, Makefile

## Setup & Local Development Commands
All key operational commands are centralized in the `Makefile`:

- **Start PostgreSQL Container:** `make run-postgres`
- **Apply Up Migrations:** `make migrate-up`
- **Roll Back 1 Migration:** `make migrate-down`
- **Reset Database (Roll Back All):** `make migrate-down-all`
- **Create New Migration:** `make migrate-new NAME=<migration_name>`
- **Check Migration Version:** `make migrate-version`
- **Force Migration Version:** `make migrate-force VERSION=<version>`
- **Run Migration Tests:** `make test-migrations`
- **Seed Database:** `make seed-db` (or `./local/script/seed-database.sh`)

## Code Architecture & Database Schema
- **Database Schema (`project`):**
  - `project.project`: Core project container.
  - `project.node`: Project nodes/tasks containing JSONB attributes, description, title, timestamps, and references to project, status, and type.
  - `project.node_type`: Valid node categories/types.
  - `project.node_status`: Valid status states.
  - `project.node_status_change`: Audit trail of node status transitions over time.
  - `project.dependency`: Graph connections mapping `node_id` to `to_node_id`.
  - `project.project_vw`: SQL view displaying root project nodes alongside `last_updated` aggregate timestamps.

## Style & Guidelines
- **Haskell Rules:** Maintain clean functional design patterns, explicit type signatures, and clear module exports.
- **SQL / Database Rules:** Always write paired `.up.sql` and `.down.sql` files for schema changes using standard sequential numbering.
- **Environment:** Do not hardcode database credentials or web ports; pull configuration values from `.env`.

## Ticket & Branching Conventions

- **Ticket File:** The root file `todo.txt` contains numbered task items (e.g., `1. Add node status validation`, `2. Fix migration script`).
- **Branch Naming Rule:** When starting work on a ticket number `$N`, check out a new Git feature branch formatted as `feature/ticket-$N-<short-description>`.
  - Example: For ticket `3. Add JSONB indexing`, create and checkout `feature/ticket-3-jsonb-indexing`.
- **Workflow Steps:**
  1. Read `todo.txt` to find the target ticket by number.
  2. Verify the workspace is clean using `git status`.
  3. Create and switch to the new feature branch: `git checkout -b feature/ticket-$N-<short-description>`.
  4. Implement the changes requested in the ticket.
  5. Run `make test-migrations` (or your build checks) to verify.
  6. Stage changed files and commit with a clear message referencing the ticket number: `git commit -m "feat(ticket-$N): short summary"`.

## Git Safety & Branch Boundaries (STRICT)

- **NEVER merge branches.** You must never run `git merge`, `git rebase`, or execute PR merge actions.
- **NEVER check out or modify `main` directly.** All edits, writes, and test scripts must happen inside feature branches (e.g., `feature/ticket-$N-*`).
- **NEVER push directly to `main` or `master`.** You may push feature branches to remote only if explicitly requested, but production/main branch pushes are strictly forbidden.
- **Hand-off Rule:** Once a feature branch is updated and local verification (`make test-migrations`) passes, stop and inform the user that the branch is ready for human review and merging.
