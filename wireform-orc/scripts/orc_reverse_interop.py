#!/usr/bin/env python3
"""Reverse-direction ORC interop driver.

Generates ORC files with pyarrow.orc, then asks the
companion wireform-side reverse probe to decode each one and
report which shapes work and which fail.

Usage:
    python3 wireform-orc/scripts/orc_reverse_interop.py
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.orc as orc

ROOT = Path(__file__).resolve().parent.parent

def write_pyarrow(out: Path) -> list[Path]:
    written = []

    def write(name: str, table: pa.Table, **kwargs):
        path = out / name
        orc.write_table(table, path, **kwargs)
        written.append(path)

    # Each column in its own file so failures are isolated.
    write("pyarrow_int64.orc",
          pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}))
    write("pyarrow_double.orc",
          pa.table({"x": pa.array([1.5, -2.5, 3.14], pa.float64())}))
    write("pyarrow_string.orc",
          pa.table({"name": pa.array(["alpha", "beta", "gamma"], pa.string())}))
    write("pyarrow_bool.orc",
          pa.table({"b": pa.array([True, False, True, True, False], pa.bool_())}))
    write("pyarrow_mixed.orc",
          pa.table({
              "id":    pa.array([10, 20, 30], pa.int64()),
              "name":  pa.array(["alice", "bob", "carol"], pa.string()),
              "score": pa.array([1.5, 2.5, 3.5], pa.float64()),
          }))
    # With compression
    write("pyarrow_zstd.orc",
          pa.table({"x": pa.array([10, 20, 30], pa.int64())}),
          compression="zstd")
    write("pyarrow_snappy.orc",
          pa.table({"x": pa.array([10, 20, 30], pa.int64())}),
          compression="snappy")
    write("pyarrow_zlib.orc",
          pa.table({"x": pa.array([10, 20, 30], pa.int64())}),
          compression="zlib")
    return written


def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="orc-reverse-"))
    print(f"== generating engine-emitted ORC files in {out}")
    files = write_pyarrow(out)
    print(f"== {len(files)} files written")

    print(f"\n== reading with wireform-orc")
    result = subprocess.run(
        ["cabal", "run", "wireform-orc:wireform-orc-reverse-probe",
         "--", str(out)],
        cwd=ROOT.parent, capture_output=True, text=True
    )
    print(result.stdout)
    if result.returncode != 0:
        print("STDERR:", result.stderr)
    return result.returncode


if __name__ == "__main__":
    sys.exit(main())
