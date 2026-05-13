#!/usr/bin/env bash
set -euo pipefail

# Regenerate the temporal-test generated Haskell modules from the
# upstream Temporal API proto files.
#
# Prerequisites:
#   - wireform-gen must be built: cabal build exe:wireform-gen
#   - git must be available
#
# Usage:
#   ./scripts/regen-temporal-test.sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEMPORAL_API_REPO="https://github.com/temporalio/api.git"
WORK_DIR="$(mktemp -d)"

cleanup() { rm -rf "$WORK_DIR"; }
trap cleanup EXIT

echo "==> Cloning temporal API protos..."
git clone --depth 1 --quiet "$TEMPORAL_API_REPO" "$WORK_DIR/temporal-api"

echo "==> Finding proto files..."
PROTO_FILES=$(find "$WORK_DIR/temporal-api/temporal/api" -name "*.proto" | sort)
NUM_FILES=$(echo "$PROTO_FILES" | wc -l)
echo "    Found $NUM_FILES proto files"

echo "==> Regenerating temporal-test modules..."
OUT_DIR="$PROJECT_ROOT/wireform-proto/temporal-test"
mkdir -p "$OUT_DIR"
# wireform-gen accepts a single -i FILE per invocation, so loop.
while IFS= read -r proto; do
  cabal exec wireform-gen -- proto generate \
    -I "$WORK_DIR/temporal-api" \
    -I "$PROJECT_ROOT/proto" \
    -i "$proto" \
    -o "$OUT_DIR" \
    -m Proto
done <<< "$PROTO_FILES"

echo "==> Done. Regenerated $NUM_FILES modules in wireform-proto/temporal-test/"
