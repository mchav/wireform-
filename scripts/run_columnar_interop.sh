#!/usr/bin/env bash
# Run the full columnar interop matrix.
#
# Exits non-zero if anything regresses. Designed for CI; safe
# to run locally too. Prints a summary at the end.
#
# Requires:
#   - cabal + ghc available on $PATH
#   - python3 with pyarrow, duckdb, polars installed
#   - liblz4-dev, libsnappy-dev, libzstd-dev (system packages)
#   - cargo (for the Rust portion); skipped if missing

set -euo pipefail

cd "$(dirname "$0")/.."
ROOT="$(pwd)"

echo "== build wireform-{arrow,parquet,orc,columnar} + probes"
cabal build wireform-arrow wireform-parquet wireform-orc wireform-columnar \
            wireform-iceberg \
            wireform-parquet:wireform-parquet-interop-probe \
            wireform-parquet:wireform-parquet-reverse-probe \
            wireform-orc:wireform-orc-interop-probe \
            wireform-orc:wireform-orc-reverse-probe \
            wireform-arrow:wireform-arrow-pyarrow-probe \
            wireform-iceberg:wireform-iceberg-interop-probe \
  > /dev/null

PASS=0
FAIL=0

run() {
  local name="$1"; shift
  echo
  echo "== $name"
  if "$@"; then
    PASS=$((PASS + 1))
    echo "  -> $name PASS"
  else
    FAIL=$((FAIL + 1))
    echo "  -> $name FAIL"
  fi
}

run "Parquet forward (pyarrow + duckdb + polars)" \
  python3 wireform-parquet/scripts/parquet_interop.py
run "Parquet reverse (read engine output)" \
  python3 wireform-parquet/scripts/parquet_reverse_interop.py
run "ORC forward (pyarrow)" \
  python3 wireform-orc/scripts/orc_interop.py
run "ORC reverse (read pyarrow output)" \
  python3 wireform-orc/scripts/orc_reverse_interop.py
run "Arrow IPC (pyarrow)" \
  python3 wireform-arrow/scripts/pyarrow_interop.py
run "Iceberg metadata (pyiceberg + fastavro)" \
  python3 wireform-iceberg/scripts/iceberg_interop.py

if command -v cargo >/dev/null 2>&1; then
  echo
  echo "== build Rust interop binaries"
  ( cd interop/arrow-rs && cargo build --release > /dev/null )

  TMPPQ=$(mktemp -d -t wf-pq.XXXX)
  TMPAR=$(mktemp -d -t wf-arrow.XXXX)
  trap "rm -rf $TMPPQ $TMPAR" EXIT
  cabal run wireform-parquet:wireform-parquet-interop-probe -- "$TMPPQ" > /dev/null
  cabal run wireform-arrow:wireform-arrow-pyarrow-probe   -- "$TMPAR" > /dev/null

  run "Rust arrow-rs (Parquet)" \
    "$ROOT/interop/arrow-rs/target/release/read_parquet" "$TMPPQ"

  # arrow-rs 53 doesn't implement ListView in its IPC convert
  # module; we emit a known-failing 'ours_listview.arrows' that
  # we expect the Rust reader to reject. Filter that out before
  # deciding pass/fail.
  echo
  echo "== Rust arrow-rs (Arrow IPC) [skipping known ListView upstream gap]"
  out=$("$ROOT/interop/arrow-rs/target/release/read_arrow_ipc" "$TMPAR" 2>&1 || true)
  echo "$out"
  unexpected=$(echo "$out" | grep "FAIL" | grep -v "ListView" | grep -v "listview" || true)
  if [ -z "$unexpected" ]; then
    PASS=$((PASS + 1))
    echo "  -> Rust arrow-rs (Arrow IPC) PASS (only known ListView failure)"
  else
    FAIL=$((FAIL + 1))
    echo "  -> Rust arrow-rs (Arrow IPC) FAIL with unexpected errors:"
    echo "$unexpected"
  fi
else
  echo "(cargo not found; skipping Rust interop probes)"
fi

echo
echo "================================================================"
echo "Columnar interop summary: $PASS PASS, $FAIL FAIL"
echo "================================================================"
exit "$FAIL"
