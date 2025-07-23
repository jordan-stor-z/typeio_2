#!/bin/bash

set -e

# --- Load env ---
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
else
  echo "❌ .env file not found"
  exit 1
fi

CONTAINER_NAME="$1"
MAX_TRIES=15
RETRY_INTERVAL=1

# --- Start container ---
echo "🚀 Starting PostgreSQL container: $CONTAINER_NAME"
docker run --rm -d \
  --name "$CONTAINER_NAME" \
  -e POSTGRES_USER="$DB_USER" \
  -e POSTGRES_PASSWORD="$DB_PASS" \
  -e POSTGRES_DB="$DB_DATABASE" \
  -p "${DB_PORT}:5432" \
  postgres:15 > /dev/null

# --- Wait for readiness ---
echo "⏳ Waiting for PostgreSQL to become ready..."
TRIES=0
until docker exec "$CONTAINER_NAME" pg_isready -U "$DB_USER" -d "$DB_DATABASE"; do
  ((TRIES++))
  if [ "$TRIES" -ge "$MAX_TRIES" ]; then
    echo "❌ PostgreSQL not ready after $MAX_TRIES tries"
    echo "Shutting down container"
    docker stop "$CONTAINER_NAME" > /dev/null
    exit 1
  fi
  echo "  🔁 Try $TRIES/$MAX_TRIES..."
  sleep "$RETRY_INTERVAL"
done

echo "✅ PostgreSQL container ready"
