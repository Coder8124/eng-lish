#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

if curl -sf -o /dev/null http://127.0.0.1:8080/; then
    exit 0
fi

mkdir -p .devcontainer/logs
nohup ./playground/target/release/playground \
    > .devcontainer/logs/playground.log 2>&1 &

echo "Playground starting at http://localhost:8080 (log: .devcontainer/logs/playground.log)"
