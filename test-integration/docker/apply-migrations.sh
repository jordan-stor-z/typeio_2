#!/bin/sh
# Runs automatically, once, the first time the disposable Postgres
# container started by test-integration/Integration/Support.hs comes
# up -- picked up via the official postgres image's
# docker-entrypoint-initdb.d convention (Support.hs mounts this file
# there, and mounts the real migrations/ directory alongside it at
# docker-entrypoint-initdb.d/migrations).
#
# Applies migrations/*.up.sql the same way `make migrate-up` does, in
# filename order (they're zero-padded and sequential, so the shell
# glob's default lexicographic expansion is already correct), skipping
# the .down.sql files entirely -- *.up.sql doesn't match them.
#
# Deliberately uses `psql` (already in the postgres image) rather than
# the `migrate` CLI, so running `cabal test integration` doesn't need
# `migrate` installed on whatever machine runs it -- only Docker.
set -e

for f in /docker-entrypoint-initdb.d/migrations/*.up.sql; do
  echo "apply-migrations.sh: applying $f"
  psql -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB" -f "$f"
done
