#!/usr/bin/env python3
"""Reverse-direction Parquet interop driver.

Generates Parquet files with each of pyarrow, duckdb, polars
(emitting different encodings, compression codecs, and shapes)
then invokes a wireform-side reader probe and reports which
files wireform-parquet successfully decoded vs failed.

Goal: catalogue exactly which engine-produced shapes we can't
read today.

Usage:
    python3 wireform-parquet/scripts/parquet_reverse_interop.py
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

import pyarrow as pa
import pyarrow.parquet as pq
import duckdb
import polars as pl

ROOT = Path(__file__).resolve().parent.parent

def write_pyarrow(out: Path) -> list[Path]:
    """Generate a variety of pyarrow Parquet files."""
    written = []

    def write(name: str, table: pa.Table, **kwargs):
        path = out / name
        pq.write_table(table, path, **kwargs)
        written.append(path)

    # 1. Plain int32 + int64 + double + utf8 + bool
    t1 = pa.table({
        "i32":  pa.array([1, 2, 3], pa.int32()),
        "i64":  pa.array([10, 20, 30], pa.int64()),
        "f64":  pa.array([1.5, 2.5, 3.5], pa.float64()),
        "s":    pa.array(["a", "b", "c"], pa.string()),
        "b":    pa.array([True, False, True], pa.bool_()),
    })
    write("pyarrow_basic_v1.parquet", t1, version="1.0")
    write("pyarrow_basic_v2.parquet", t1, version="2.6", data_page_version="2.0")

    # 2. With compression
    write("pyarrow_snappy.parquet", t1, compression="snappy")
    write("pyarrow_zstd.parquet",   t1, compression="zstd")
    write("pyarrow_gzip.parquet",   t1, compression="gzip")

    # 3. Nullable columns
    t2 = pa.table({
        "id":    pa.array([1, None, 3], pa.int64()),
        "name":  pa.array(["alice", None, "carol"], pa.string()),
    })
    write("pyarrow_nullable.parquet", t2)

    # 4. Dictionary encoding
    write("pyarrow_dict.parquet",
          pa.table({"x": pa.array(["a", "b", "a", "c", "b", "a"], pa.string())}),
          use_dictionary=True)

    # 5. Multiple row groups
    big = pa.table({"x": pa.array(list(range(100)), pa.int64())})
    write("pyarrow_multi_rg.parquet", big, row_group_size=25)

    return written


def write_duckdb(out: Path) -> list[Path]:
    written = []
    con = duckdb.connect()
    # Use COPY to write a Parquet file from a SQL query
    base = "(SELECT * FROM (VALUES (1::INT, 10::BIGINT, 1.5::DOUBLE, 'a', TRUE), (2, 20, 2.5, 'b', FALSE), (3, 30, 3.5, 'c', TRUE)) AS t(i32, i64, f64, s, b))"
    p = out / "duckdb_basic.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet')")
    written.append(p)
    p = out / "duckdb_snappy.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet', COMPRESSION 'snappy')")
    written.append(p)
    p = out / "duckdb_zstd.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet', COMPRESSION 'zstd')")
    written.append(p)
    return written


def write_polars(out: Path) -> list[Path]:
    written = []
    df = pl.DataFrame({
        "i32": pl.Series("i32", [1, 2, 3], dtype=pl.Int32),
        "i64": pl.Series("i64", [10, 20, 30], dtype=pl.Int64),
        "f64": pl.Series("f64", [1.5, 2.5, 3.5], dtype=pl.Float64),
        "s":   pl.Series("s",   ["a", "b", "c"]),
        "b":   pl.Series("b",   [True, False, True]),
    })
    p = out / "polars_basic.parquet"
    df.write_parquet(p)
    written.append(p)
    p = out / "polars_zstd.parquet"
    df.write_parquet(p, compression="zstd")
    written.append(p)
    # Polars's "lz4" maps to the legacy CompressionCodec.LZ4
    # (codec 5) which is the Hadoop-frame variant. Modern
    # writers use LZ4_RAW (codec 7); we test that via the
    # 'lz4_raw' name which both pyarrow and polars accept.
    p = out / "polars_lz4_raw.parquet"
    try:
        df.write_parquet(p, compression="lz4_raw")
        written.append(p)
    except Exception:
        pass
    p = out / "pyarrow_lz4_raw.parquet"
    try:
        pq.write_table(
            pa.table({
                "i32": pa.array([1, 2, 3], pa.int32()),
                "s":   pa.array(["a", "b", "c"], pa.string()),
            }),
            p,
            compression="lz4_raw",
        )
        written.append(p)
    except Exception:
        pass
    return written


def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="parquet-reverse-"))
    print(f"== generating engine-emitted Parquet files in {out}")

    all_files = []
    all_files += write_pyarrow(out)
    all_files += write_duckdb(out)
    all_files += write_polars(out)
    print(f"== {len(all_files)} files written")

    # Run the wireform-side reader probe.
    print(f"\n== reading with wireform-parquet")
    result = subprocess.run(
        ["cabal", "run", "wireform-parquet:wireform-parquet-reverse-probe",
         "--", str(out)],
        cwd=ROOT.parent, capture_output=True, text=True
    )
    print(result.stdout)
    if result.returncode != 0:
        print("STDERR:", result.stderr)
    return result.returncode


if __name__ == "__main__":
    sys.exit(main())
