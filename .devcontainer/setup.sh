#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building the eng-lish compiler..."
cargo build --release

echo "Building the playground..."
(cd playground && cargo build --release)

# Make .eng files highlight in this container using the extension in the repo.
for dir in "$HOME/.vscode-server/extensions" "$HOME/.vscode-remote/extensions"; do
    if [ -d "$dir" ]; then
        ln -sfn "$PWD/editors/vscode" "$dir/eng-lish" || true
    fi
done

echo
echo "Ready. The playground starts automatically at http://localhost:8080"
echo "Or compile from the terminal:  ./target/release/englishc examples/hello.eng"
