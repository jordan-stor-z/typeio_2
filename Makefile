include .env
export

# --- Config ---
CONTAINER_NAME=typeio_db
DB_URL=postgres://$(DB_USER):$(DB_PASS)@$(DB_HOST):$(DB_PORT)/$(DB_DATABASE)?sslmode=disable
MIGRATE=migrate
MIGRATIONS_DIR=migrations

# --- Commands ---
.PHONY: migrate-up migrate-down migrate-new migrate-force migrate-down-all migrate-version test test-integration test-e2e e2e-install

## Run migratin tests
test-migrations:
	./scripts/test-migrations.sh

## Run postgres container
run-postgres:
	./local/script/start-postgres.sh $(CONTAINER_NAME)

## Echo back the database URL
print-db-url:
	@echo $(DB_URL)

## Apply all up migrations
migrate-up:
	$(MIGRATE) -path $(MIGRATIONS_DIR) -database "$(DB_URL)" up

## Roll back last migration
migrate-down:
	$(MIGRATE) -path $(MIGRATIONS_DIR) -database "$(DB_URL)" down 1

## Show current migration version
migrate-version:
	$(MIGRATE) -path $(MIGRATIONS_DIR) -database "$(DB_URL)" version

## Force migration to a specific version: make migrate-force VERSION=2
migrate-force:
	$(MIGRATE) -path $(MIGRATIONS_DIR) -database "$(DB_URL)" force $(VERSION)

## Roll back to 0
migrate-down-all:
	$(MIGRATE) -path $(MIGRATIONS_DIR) -database "$(DB_URL)" down

## Create a new migration file: make migrate-new NAME=add_table
migrate-new:
	$(MIGRATE) create -ext sql -dir $(MIGRATIONS_DIR) -seq $(NAME)

## run program to seed database
seed-db:
	curl --location --request POST 'localhost:$(or $(WEB_PORT),3000)/api/central/seed-database'

## Run the Haskell unit test suite
test:
	cabal test spec

## Run the Haskell integration test suite (needs Docker -- starts and
## tears down its own disposable, already-migrated Postgres via
## testcontainers, no manually-started database or `migrate` CLI
## required)
test-integration:
	cabal test integration

## Install the E2E suite's dependencies (Playwright + Chromium). One-time
## setup, or re-run after e2e/package.json changes.
e2e-install:
	cd e2e && npm install && npx playwright install --with-deps chromium

## Run the E2E test suite. Unlike test/test-integration, this doesn't
## start its own database or server -- needs a real app already running
## against a real, migrated + seeded Postgres (run-postgres, migrate-up,
## seed-db, then `cabal run server` in another terminal). See
## e2e/README.md for the full sequence and how to run it headed/in UI
## mode to watch it drive a browser.
test-e2e:
	cd e2e && npm test
