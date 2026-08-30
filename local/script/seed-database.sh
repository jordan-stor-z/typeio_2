#!/bin/bash

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
env_file="$script_dir/../../.env"

if [ -f "$env_file" ]; then
  set -a
  source "$env_file"
  set +a
fi

curl --location --request POST "localhost:${WEB_PORT:-3000}/api/central/seed-database"
