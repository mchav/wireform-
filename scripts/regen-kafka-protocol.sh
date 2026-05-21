#!/usr/bin/env bash
set -euo pipefail

# Regenerate every wireform-kafka/src/Kafka/Protocol/Generated/*.hs
# module from a checkout of the upstream Apache Kafka source tree.
#
# The Kafka project ships its protocol message definitions in
# clients/src/main/resources/common/message/*.json. The kafka-codegen
# executable (lib:wireform-kafka-codegen + exe:kafka-codegen) walks
# that directory and emits one Haskell module per request/response/data
# message into the wireform-kafka package.
#
# Prerequisites:
#   - kafka-codegen built: cabal build wireform-kafka:exe:kafka-codegen
#   - A local Kafka checkout, e.g.
#       git clone --depth=1 --branch=trunk https://github.com/apache/kafka.git ~/src/kafka
#
# Usage:
#   KAFKA_SRC=~/src/kafka ./scripts/regen-kafka-protocol.sh
#   # or pass the message directory directly:
#   ./scripts/regen-kafka-protocol.sh /path/to/kafka/clients/src/main/resources/common/message

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUT_DIR="$PROJECT_ROOT/wireform-kafka/src/Kafka/Protocol/Generated"

if [[ $# -ge 1 ]]; then
  MSG_DIR="$1"
elif [[ -n "${KAFKA_SRC:-}" ]]; then
  MSG_DIR="$KAFKA_SRC/clients/src/main/resources/common/message"
else
  echo "Usage: $0 <kafka-message-dir>" >&2
  echo "   or: KAFKA_SRC=/path/to/kafka $0" >&2
  exit 1
fi

if [[ ! -d "$MSG_DIR" ]]; then
  echo "error: $MSG_DIR is not a directory" >&2
  exit 1
fi

echo "==> Regenerating wireform-kafka generated modules from $MSG_DIR"
mkdir -p "$OUT_DIR"

cabal run -v0 wireform-kafka:exe:kafka-codegen -- "$MSG_DIR" "$OUT_DIR"

# Sync the inventory next to the message-data sidecar so it travels
# with the package, and is easy to diff alongside the generated code.
INVENTORY_SRC="$OUT_DIR/message-inventory.json"
INVENTORY_DST="$PROJECT_ROOT/wireform-kafka/data/message-inventory.json"
if [[ -f "$INVENTORY_SRC" ]]; then
  mv "$INVENTORY_SRC" "$INVENTORY_DST"
  echo "==> Wrote $INVENTORY_DST"
fi

count="$(find "$OUT_DIR" -maxdepth 1 -name '*.hs' | wc -l)"
echo "==> Done. $count generated modules under $OUT_DIR"

echo "==> Syncing wireform-kafka-protocol module list in wireform-kafka.cabal"
"$SCRIPT_DIR/sync-kafka-protocol-cabal-modules.sh"
