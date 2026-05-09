#!/usr/bin/env python3
"""Cross-engine ORC interop driver.

Per file:
  1. Read with pyarrow.orc -> assert against expected.

(duckdb's ORC support is a community extension not bundled by
default; polars doesn't ship a stable ORC reader. pyarrow.orc
wraps the official Apache C++ ORC reader so it covers the
spec faithfully.)

Usage:
    python3 wireform-orc/scripts/orc_interop.py
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.orc as orc

ROOT = Path(__file__).resolve().parent.parent

import datetime as _dt

def expected_files() -> list[tuple[str, pa.Table]]:
    return [
        ( "int64_required.orc"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "double_required.orc"
        , pa.table({"x": pa.array([1.5, -2.5, 3.14159], pa.float64())}) )
      , ( "string_required.orc"
        , pa.table({"name": pa.array(["alpha", "beta", "gamma"], pa.string())}) )
      , ( "bool_required.orc"
        , pa.table({"b": pa.array([True, False, True, True, False], pa.bool_())}) )
      , ( "mixed_required.orc"
        , pa.table({
            "id":    pa.array([10, 20, 30], pa.int64()),
            "name":  pa.array(["alice", "bob", "carol"], pa.string()),
            "score": pa.array([1.5, 2.5, 3.5], pa.float64()),
          }) )
      # harder shapes
      , ( "nested_struct.orc"
        , pa.table({"rec": pa.array(
            [ {"i": 1, "n": "a"}
            , {"i": 2, "n": "b"}
            , {"i": 3, "n": "c"}
            ],
            pa.struct([("i", pa.int64()), ("n", pa.string())]))}) )
      , ( "list_int64.orc"
        , pa.table({"lst": pa.array(
            [[10, 20], [30, 40, 50], [60, 70]], pa.list_(pa.int64()))}) )
      , ( "int32_required.orc"
        , pa.table({"x": pa.array([1, 2, 3, 4, 5], pa.int32())}) )
      , ( "float_required.orc"
        , pa.table({"x": pa.array([1.5, 2.5, 3.5], pa.float32())}) )
      , ( "timestamp_required.orc"
        , pa.table({"ts": pa.array(
            [ _dt.datetime(1970, 1, 1, 0, 0, 0)
            , _dt.datetime(2023, 11, 14, 22, 13, 20)
            ], pa.timestamp("ns"))}) )
    ]

def assert_table_eq(name: str, actual: pa.Table, expected: pa.Table) -> str | None:
    if actual.num_rows != expected.num_rows:
        return f"row count {actual.num_rows} != {expected.num_rows}"
    if actual.num_columns != expected.num_columns:
        return f"column count {actual.num_columns} != {expected.num_columns}"
    for col_name in expected.column_names:
        if col_name not in actual.column_names:
            return f"missing column {col_name!r}"
        a = actual.column(col_name).combine_chunks().to_pylist()
        e = expected.column(col_name).combine_chunks().to_pylist()
        if a != e:
            return f"column {col_name!r}: got {a} != expected {e}"
    return None

def read_pyarrow(path: Path) -> pa.Table:
    return orc.read_table(path)

def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="wireform-orc-probe-"))
    print(f"== writing wireform-orc probe outputs to {out}")
    subprocess.run(
        ["cabal", "run", "wireform-orc:wireform-orc-interop-probe",
         "--", str(out)],
        cwd=ROOT.parent, check=True
    )

    print("\n== reading wireform-orc output with pyarrow")
    failures = []
    for fname, expected in expected_files():
        path = out / fname
        if not path.exists():
            failures.append((fname, "probe didn't write"))
            continue
        for engine, reader in [("pyarrow", read_pyarrow)]:
            try:
                got = reader(path)
                err = assert_table_eq(fname, got, expected)
                if err:
                    failures.append((f"{fname}::{engine}", err))
                else:
                    print(f"  OK   {fname:40s} via {engine}")
            except Exception as e:
                failures.append((f"{fname}::{engine}", repr(e)))

    if failures:
        print(f"\n{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("\nAll engines accepted every wireform-orc probe output.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
