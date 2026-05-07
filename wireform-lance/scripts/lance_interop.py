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
            "--file", str(data_file), str(probe_out),
        ],
        cwd=ROOT.parent, check=True,
    )

    with probe_out.open() as fh:
        summary = json.load(fh)
    expect_eq(failures, "lance footer", "mode", summary.get("mode"), "file")

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

    # ---------------------------------------------------------------
    # Dataset mode: now drive --dataset against the .lance directory
    # so the new Lance.IO opener (manifest enumeration + data-file
    # listing) is exercised.
    # ---------------------------------------------------------------
    print(f"\n== driving wireform-lance dataset probe against {dataset_dir.name}")

    # Append a second fragment so there is more than one version /
    # data file to enumerate.
    lance.write_dataset(
        pa.table({"id":   pa.array([100, 101], type=pa.int64()),
                  "name": pa.array(["x", "y"], type=pa.string()),
                  "v":    pa.array([1.0, 2.0], type=pa.float64())}),
        str(dataset_dir),
        mode="append",
    )

    ds_probe_out = out / "ds_probe.json"
    subprocess.run(
        [
            "cabal", "run",
            "wireform-lance:wireform-lance-interop-probe",
            "--",
            "--dataset", str(dataset_dir), str(ds_probe_out),
        ],
        cwd=ROOT.parent, check=True,
    )
    with ds_probe_out.open() as fh:
        ds_summary = json.load(fh)

    pylance_versions = sorted(int(v["version"]) for v in lance.dataset(str(dataset_dir)).versions())
    pylance_data_files = sorted(p.name for p in dataset_dir.glob("data/*.lance"))

    expect_eq(failures, "lance dataset", "mode", ds_summary["mode"], "dataset")
    expect_eq(failures, "lance dataset", "latest_version",
              ds_summary["latest_version"], pylance_versions[-1])

    wireform_versions = sorted(v["version"] for v in ds_summary["versions"])
    expect_eq(failures, "lance dataset", "versions",
              wireform_versions, pylance_versions)

    # Latest manifest's footer must be a valid Lance manifest
    # footer (16-byte format: u64 protobuf position + 2 u16
    # versions + LANC magic, distinct from the 40-byte data-file
    # footer). The 'manifest_position' must be inside the
    # manifest file and the trailing magic must match.
    mf = ds_summary["latest_manifest_footer"]
    if mf is None:
        failures.add("lance dataset", "latest_manifest_footer = null")
    else:
        # Cross-check against an independent struct.unpack of the
        # most recent _versions/*.manifest tail.
        manifest_path = max(dataset_dir.glob("_versions/*.manifest"),
                            key=lambda p: p.stat().st_mtime)
        b = manifest_path.read_bytes()
        if b[-4:] != b"LANC":
            failures.add("lance dataset", "manifest trailing magic missing")
        true_pos = struct.unpack_from("<Q", b, len(b) - 16)[0]
        true_maj, true_min = struct.unpack_from("<HH", b, len(b) - 8)
        expect_eq(failures, "lance dataset",
                  "manifest_footer.manifest_position",
                  mf["manifest_position"], true_pos)
        expect_eq(failures, "lance dataset",
                  "manifest_footer.major_version",
                  mf["major_version"], true_maj)
        expect_eq(failures, "lance dataset",
                  "manifest_footer.minor_version",
                  mf["minor_version"], true_min)
        if not (0 < mf["manifest_position"] < manifest_path.stat().st_size - 16):
            failures.add("lance dataset",
                         f"manifest_position {mf['manifest_position']} "
                         f"not inside the manifest file (size {manifest_path.stat().st_size})")

    # Data files: wireform sees the same .lance basenames pylance
    # has on disk (we don't filter against the manifest body, so
    # this is a "files present" comparison, not "files active").
    wireform_data_files = sorted(ds_summary["data_file_names"])
    expect_eq(failures, "lance dataset", "data_file_names",
              wireform_data_files, pylance_data_files)

    # ---------------------------------------------------------------
    # Decoded manifest body (protobuf -> typed Manifest record).
    # Pylance is the canonical source-of-truth for the dataset
    # version + active fragment list; cross-check against it.
    # ---------------------------------------------------------------
    wf_manifest = ds_summary["manifest"]
    if wf_manifest is None:
        failures.add("lance manifest", "wireform manifest decoder returned null")
    else:
        ld = lance.dataset(str(dataset_dir))
        expect_eq(failures, "lance manifest", "version",
                  wf_manifest["version"], pylance_versions[-1])

        # Writer version: pylance writes a 'lance' library tag.
        wv = wf_manifest["writer_version"]
        if wv is None:
            failures.add("lance manifest", "writer_version missing")
        else:
            expect_eq(failures, "lance manifest", "writer_version.library",
                      wv["library"], "lance")
            if not wv["version"]:
                failures.add("lance manifest", "writer_version.version is empty")

        # Fragments: count + per-fragment file paths must match
        # what pylance enumerates from the same dataset.
        pylance_fragments = list(ld.get_fragments())
        expect_eq(failures, "lance manifest", "fragment_count",
                  wf_manifest["fragment_count"], len(pylance_fragments))

        wf_frag_files = []
        for frag in wf_manifest["fragments"]:
            for fff in frag["files"]:
                wf_frag_files.append(fff["path"])
        wf_frag_files.sort()

        pyl_frag_files = []
        for pf in pylance_fragments:
            for df in pf.metadata.files:
                pyl_frag_files.append(df.path)
        pyl_frag_files.sort()

        expect_eq(failures, "lance manifest", "fragments[*].files[*].path",
                  wf_frag_files, pyl_frag_files)

        # data_format: pylance writes 'lance' / version like '2.0'.
        dfmt = wf_manifest["data_format"]
        if dfmt is None:
            failures.add("lance manifest", "data_format missing")
        else:
            expect_eq(failures, "lance manifest", "data_format.file_format",
                      dfmt["file_format"], "lance")
            if not dfmt["version"]:
                failures.add("lance manifest", "data_format.version is empty")

    # ---------------------------------------------------------------
    # New (this round): cross-check the typed schema readout, the
    # active data file enumeration, and the writer-version flat
    # surface against pylance.
    # ---------------------------------------------------------------
    pylance_ds = lance.dataset(str(dataset_dir))

    # Active data files: should equal the on-disk
    # data/*.lance basenames pylance reports.
    pylance_active = sorted(
        df.path.split("/")[-1]
        for frag in pylance_ds.get_fragments()
        for df in frag.metadata.files
    )
    wf_active = sorted(p.split("/")[-1] for p in ds_summary["active_data_files"])
    expect_eq(failures, "lance opener", "active_data_files",
              wf_active, pylance_active)

    # Schema readout: every top-level field name should appear,
    # in id-ascending order. (Lance schemas can have nested
    # children flattened; we just check the top-level slice.)
    sch = ds_summary["schema_fields"]
    if not sch:
        failures.add("lance opener", "schema_fields empty / null")
    else:
        wf_top = sorted(f["name"] for f in sch if f["parent_id"] == -1)
        pyl_top = sorted(f.name for f in pylance_ds.schema)
        expect_eq(failures, "lance opener", "top-level schema field names",
                  wf_top, pyl_top)

    # Writer version: pylance always tags 'lance' as the library.
    wv = ds_summary["writer_version_flat"]
    if wv is None:
        failures.add("lance opener", "writer_version_flat = null")
    else:
        expect_eq(failures, "lance opener", "writer_version_flat.library",
                  wv["library"], "lance")
        if not wv["version"]:
            failures.add("lance opener", "writer_version_flat.version empty")

    # Timestamp millis: should be > 0 for any pylance-written
    # dataset and within ±5s of pylance's reported timestamp.
    ts_millis = ds_summary["timestamp_millis"]
    if ts_millis is None:
        failures.add("lance opener", "timestamp_millis = null")
    else:
        from datetime import datetime, timezone
        pylance_ts_ms = int(
            pylance_ds.versions()[-1]["timestamp"].replace(tzinfo=timezone.utc)
              .timestamp() * 1000
        )
        if abs(ts_millis - pylance_ts_ms) > 5_000:
            failures.add("lance opener",
                         f"timestamp_millis {ts_millis} differs from "
                         f"pylance {pylance_ts_ms} by > 5s")

    if failures:
        print()
        print(f"{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print()
    print(f"OK   wireform-lance footer round-trips through pylance "
          f"(num_columns={f['num_columns']}, version={f['major_version']}.{f['minor_version']}).")
    print(f"OK   wireform-lance dataset opener: latest_version="
          f"{ds_summary['latest_version']}, "
          f"{len(wireform_data_files)} data files, "
          f"{len(wireform_versions)} versions, "
          f"{len(ds_summary['active_data_files'])} active data files, "
          f"{len(ds_summary['schema_fields'])} schema fields.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
