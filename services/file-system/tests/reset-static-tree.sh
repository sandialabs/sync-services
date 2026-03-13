#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
cp "$ROOT_DIR/tests/static-tree.baseline.json" "$ROOT_DIR/tests/static-tree.json"
echo "Reset tests/static-tree.json from tests/static-tree.baseline.json"
