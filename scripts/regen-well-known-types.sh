#!/usr/bin/env bash
set -euo pipefail

# Regenerate the well-known type Haskell modules from the bundled
# proto/google/protobuf/*.proto files.
#
# Prerequisites:
#   - hs-proto-gen must be built: cabal build exe:hs-proto-gen
#
# Usage:
#   ./scripts/regen-well-known-types.sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

PROTOS=(
  proto/google/protobuf/timestamp.proto
  proto/google/protobuf/duration.proto
  proto/google/protobuf/empty.proto
  proto/google/protobuf/any.proto
  proto/google/protobuf/field_mask.proto
  proto/google/protobuf/source_context.proto
  proto/google/protobuf/wrappers.proto
  proto/google/protobuf/struct.proto
)

echo "==> Regenerating well-known type modules..."
for f in "${PROTOS[@]}"; do
  cabal exec hs-proto-gen -- generate \
    -I "$PROJECT_ROOT/proto" \
    -o "$PROJECT_ROOT/src" \
    --module-prefix Proto \
    "$PROJECT_ROOT/$f"
done

echo "==> Done. Regenerated ${#PROTOS[@]} well-known type modules in src/"
