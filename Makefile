include .env
export

# --- Config ---
CONTAINER_NAME=typeio_db
DB_URL=postgres://$(DB_USER):$(DB_PASS)@$(DB_HOST):$(DB_PORT)/$(DB_DATABASE)?sslmode=disable
MIGRATE=migrate
MIGRATIONS_DIR=migrations

# --- Commands ---
.PHONY: migrate-up migrate-down migrate-new migrate-force migrate-down-all migrate-version

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
	curl --location --request POST 'localhost:3000/api/central/seed-database'
