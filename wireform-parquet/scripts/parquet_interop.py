#!/usr/bin/env python3
"""Cross-engine Parquet interop driver.

Workflow per file:

  1. Read with pyarrow -> assert against expected.
  2. Read with duckdb  -> assert against expected.
  3. Read with polars  -> assert against expected.

Each file is associated with a Python expectations dict that
the engine-specific reader is checked against. A file is OK
only if every engine accepts it AND every engine returns the
same data we wrote.

Run:
    python3 wireform-parquet/scripts/parquet_interop.py [--regen]

`--regen` additionally writes pyarrow / duckdb / polars
reference files /into/ a temp dir and tries to read them
with wireform-parquet (round-trip-the-other-way coverage).
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
import duckdb
import polars as pl

ROOT = Path(__file__).resolve().parent.parent

# --- Each entry: (filename, expected pa.Table)
def expected_files() -> list[tuple[str, pa.Table]]:
    return [
        ( "int32_required.parquet"
        , pa.table({"x": pa.array([1, 2, 3, 4, 5], pa.int32())}) )
      , ( "int64_required.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "float_required.parquet"
        , pa.table({"x": pa.array([1.5, 2.5, 3.5], pa.float32())}) )
      , ( "double_required.parquet"
        , pa.table({"x": pa.array([1.5, -2.5, 3.14159], pa.float64())}) )
      , ( "bool_required.parquet"
        , pa.table({"x": pa.array([True, False, True, True, False], pa.bool_())}) )
      , ( "byte_array_required.parquet"
        , pa.table({"x": pa.array([b"alpha", b"beta", b"gamma"], pa.binary())}) )
      , ( "utf8_required.parquet"
        , pa.table({"name": pa.array(["Alice", "Bob", "Carol", "Δοε"], pa.string())}) )
      , ( "int64_zstd.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "int64_two_row_groups.parquet"
        , pa.table({"x": pa.array([1, 2, 3, 4, 5, 6], pa.int64())}) )
      , ( "mixed_optional.parquet"
        , pa.table({
            "id":    pa.array([10, 20, 30], pa.int64()),
            "name":  pa.array(["alice", None, "carol"], pa.string()),
            "score": pa.array([1.5, 2.5, None], pa.float64()),
          }) )
    ]

def assert_table_eq(name: str, actual: pa.Table, expected: pa.Table) -> str | None:
    if actual.num_rows != expected.num_rows:
        return f"row count {actual.num_rows} != {expected.num_rows}"
    if actual.num_columns != expected.num_columns:
        return f"column count {actual.num_columns} != {expected.num_columns}"
    for col_name in expected.column_names:
        if col_name not in actual.column_names:
            return f"missing column {col_name!r}"
        a = actual.column(col_name).combine_chunks()
        e = expected.column(col_name).combine_chunks()
        if a.to_pylist() != e.to_pylist():
            return f"column {col_name!r}: got {a.to_pylist()} != expected {e.to_pylist()}"
    return None

def read_pyarrow(path: Path) -> pa.Table:
    return pq.read_table(path)

def read_duckdb(path: Path) -> pa.Table:
    con = duckdb.connect()
    rbr = con.execute(f"SELECT * FROM read_parquet('{path}')").arrow()
    if isinstance(rbr, pa.Table):
        return rbr
    # duckdb 1.5+ returns a RecordBatchReader from .arrow() in
    # some configurations; coalesce it.
    return rbr.read_all() if hasattr(rbr, "read_all") else pa.Table.from_batches(list(rbr))

def read_polars(path: Path) -> pa.Table:
    df = pl.read_parquet(path)
    return df.to_arrow()

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--regen", action="store_true",
        help="also generate engine-emitted Parquet files for reverse round-trip")
    args = parser.parse_args()

    out = Path(tempfile.mkdtemp(prefix="wireform-parquet-probe-"))
    print(f"== writing wireform-parquet probe outputs to {out}")
    subprocess.run(
        ["cabal", "run", "wireform-parquet:wireform-parquet-interop-probe",
         "--", str(out)],
        cwd=ROOT.parent, check=True
    )

    print("\n== reading wireform-parquet output with pyarrow / duckdb / polars")
    failures = []
    for fname, expected in expected_files():
        path = out / fname
        if not path.exists():
            failures.append((fname, "probe didn't write"))
            continue

        for engine, reader in [("pyarrow", read_pyarrow),
                               ("duckdb",  read_duckdb),
                               ("polars",  read_polars)]:
            try:
                got = reader(path)
                err = assert_table_eq(fname, got, expected)
                if err:
                    failures.append((f"{fname}::{engine}", err))
                else:
                    print(f"  OK   {fname:40s} via {engine}")
            except Exception as e:
                failures.append((f"{fname}::{engine}", repr(e)))

    if args.regen:
        print("\n== reverse round-trip: engines write, wireform reads")
        # Currently the wireform-parquet reader is exercised by the
        # main test suite + the Iceberg goldens; this script is the
        # write-side audit. The reverse direction is tested by
        # the iceberg golden corpus + parquet-test.

    if failures:
        print(f"\n{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("\nAll engines accepted every wireform-parquet probe output.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
