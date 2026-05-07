#!/usr/bin/env python3
"""wireform-lance ↔ pylance interop driver.

* Writes a small Lance dataset with pylance.
* Runs ``wireform-lance-interop-probe`` against the underlying
  ``.lance`` data file.
* Asserts that wireform's typed footer (column count, version,
  CMO / GBO offsets, per-column position+size table) lines up
  with what pylance sees / reports.

Usage:
    python3 wireform-lance/scripts/lance_interop.py
"""

from __future__ import annotations

import json
import os
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import lance

ROOT = Path(__file__).resolve().parent.parent


class Failures(list):
    def add(self, where: str, msg: str) -> None:
        self.append((where, msg))


def expect_eq(failures: Failures, where: str, label: str, got, want) -> None:
    if got != want:
        failures.add(where, f"{label} = {got!r}, want {want!r}")


def ground_truth_footer(path: Path) -> dict:
    """Decode the 40-byte Lance footer ourselves so we have an
    independent ground truth alongside pylance's higher-level
    view of the file."""
    data = path.read_bytes()
    footer = data[-40:]
    if footer[-4:] != b"LANC":
        raise RuntimeError(f"trailing magic missing in {path}")
    col0, cmo, gbo = struct.unpack_from("<QQQ", footer, 0)
    ngb, ncol = struct.unpack_from("<II", footer, 24)
    maj, mn = struct.unpack_from("<HH", footer, 32)
    return {
        "file_size": len(data),
        "column_meta_0_offset": col0,
        "cmo_table_offset": cmo,
        "gbo_table_offset": gbo,
        "num_global_buffers": ngb,
        "num_columns": ncol,
        "major_version": maj,
        "minor_version": mn,
    }


def first_data_file(dataset_dir: Path) -> Path:
    """The pylance writer emits a single .lance per fragment under
    /data/. Pick the first one."""
    candidates = sorted(dataset_dir.glob("data/*.lance"))
    if not candidates:
        raise RuntimeError(f"no .lance data file under {dataset_dir}")
    return candidates[0]


def main() -> int:
    failures = Failures()
    out = Path(tempfile.mkdtemp(prefix="wireform-lance-probe-"))
    dataset_dir = out / "ds.lance"
    print(f"== writing pylance dataset to {dataset_dir}")

    table = pa.table(
        {
            "id":   pa.array(list(range(50)),  type=pa.int64()),
            "name": pa.array([f"row-{i}" for i in range(50)], type=pa.string()),
            "v":    pa.array([float(i) * 0.5 for i in range(50)], type=pa.float64()),
        }
    )
    ds = lance.write_dataset(table, str(dataset_dir))
    expected_cols = len(ds.schema)

    data_file = first_data_file(dataset_dir)
    print(f"== driving wireform-lance probe against {data_file.name}")
    truth = ground_truth_footer(data_file)
    print(f"  ground truth footer: {truth}")

    probe_out = out / "probe.json"
    subprocess.run(
        [
            "cabal", "run",
            "wireform-lance:wireform-lance-interop-probe",
            "--",
            str(data_file), str(probe_out),
        ],
        cwd=ROOT.parent, check=True,
    )

    with probe_out.open() as f:
        summary = json.load(f)

    # ---------------------------------------------------------------
    # Footer fields must exactly match the bytes we decoded ourselves.
    # ---------------------------------------------------------------
    expect_eq(failures, "lance footer", "file_size",
              summary["file_size"], truth["file_size"])
    f = summary["footer"]
    for key in (
        "column_meta_0_offset", "cmo_table_offset", "gbo_table_offset",
        "num_global_buffers", "num_columns", "major_version", "minor_version",
    ):
        expect_eq(failures, "lance footer", key, f[key], truth[key])

    # ---------------------------------------------------------------
    # And footer.num_columns must equal the pylance-reported column
    # count (which is the cross-check that our footer parser is
    # actually walking the right portion of the file).
    # ---------------------------------------------------------------
    expect_eq(failures, "lance footer (vs pylance)", "num_columns",
              f["num_columns"], expected_cols)

    # ---------------------------------------------------------------
    # Column slice table must have num_columns entries, each with
    # a position + size pair pointing inside the file.
    # ---------------------------------------------------------------
    cols = summary["columns"]
    expect_eq(failures, "lance columns", "len",
              len(cols), f["num_columns"])
    for i, c in enumerate(cols):
        if not (0 <= c["position"] < f["cmo_table_offset"]):
            failures.add(f"lance columns[{i}]",
                         f"position {c['position']} not in [0, {f['cmo_table_offset']})")
        if c["size"] <= 0:
            failures.add(f"lance columns[{i}]",
                         f"size {c['size']} <= 0")
        if c["position"] + c["size"] > f["cmo_table_offset"]:
            failures.add(f"lance columns[{i}]",
                         f"slice {c['position']}+{c['size']} runs into CMO table")

    # ---------------------------------------------------------------
    # Global-buffer slice table cardinality matches num_global_buffers.
    # pylance always writes at least the schema, so this is > 0.
    # ---------------------------------------------------------------
    gbs = summary["global_buffers"]
    expect_eq(failures, "lance global_buffers", "len",
              len(gbs), f["num_global_buffers"])
    if f["num_global_buffers"] == 0:
        failures.add("lance global_buffers", "expected pylance to emit ≥1")

    if failures:
        print()
        print(f"{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print()
    print(f"OK   wireform-lance footer round-trips through pylance "
          f"(num_columns={f['num_columns']}, version={f['major_version']}.{f['minor_version']}).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
