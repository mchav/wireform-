#!/usr/bin/env python3
"""wireform-iceberg -> pyiceberg interop driver.

Runs the wireform-iceberg-interop-probe to emit manifest /
manifest list / table-metadata files, then reads each one with
pyiceberg's official decoders and asserts the structural
fields match.

Usage:
    python3 wireform-iceberg/scripts/iceberg_interop.py
"""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
from pathlib import Path

import fastavro
from pyiceberg.table.metadata import new_table_metadata, TableMetadataUtil

ROOT = Path(__file__).resolve().parent.parent

def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="wireform-iceberg-probe-"))
    print(f"== writing wireform-iceberg probe outputs to {out}")
    subprocess.run(
        ["cabal", "run", "wireform-iceberg:wireform-iceberg-interop-probe",
         "--", str(out)],
        cwd=ROOT.parent, check=True
    )

    failures = []

    # ------------------------------------------------------------
    # Manifest file (Avro container of manifest_entry records)
    # ------------------------------------------------------------
    print("\n== manifest_v2.avro")
    try:
        path = out / "manifest_v2.avro"
        with path.open("rb") as f:
            reader = fastavro.reader(f)
            entries = list(reader)
        if len(entries) != 2:
            failures.append(("manifest_v2.avro", f"expected 2 entries, got {len(entries)}"))
        else:
            for i, (entry, expected_path) in enumerate(zip(
                entries,
                ["data/file_a.parquet", "data/file_b.parquet"],
            )):
                # status: 1 = added
                if entry.get("status") != 1:
                    failures.append((f"manifest_v2.avro entry {i}",
                                     f"status {entry.get('status')!r} != 1"))
                df = entry.get("data_file") or {}
                fp = df.get("file_path")
                if fp != expected_path:
                    failures.append((f"manifest_v2.avro entry {i}",
                                     f"file_path {fp!r} != {expected_path!r}"))
                rc = df.get("record_count")
                expected_rc = 100 if i == 0 else 200
                if rc != expected_rc:
                    failures.append((f"manifest_v2.avro entry {i}",
                                     f"record_count {rc!r} != {expected_rc}"))
            print(f"  OK   manifest_v2.avro ({len(entries)} entries)")
    except Exception as e:
        failures.append(("manifest_v2.avro", repr(e)))

    # ------------------------------------------------------------
    # Manifest list (Avro container of manifest_file records)
    # ------------------------------------------------------------
    print("\n== manifest_list_v2.avro")
    try:
        path = out / "manifest_list_v2.avro"
        with path.open("rb") as f:
            reader = fastavro.reader(f)
            files = list(reader)
        if len(files) != 1:
            failures.append(("manifest_list_v2.avro", f"expected 1 file, got {len(files)}"))
        else:
            mf = files[0]
            if mf.get("manifest_path") != "metadata/manifest_v2.avro":
                failures.append(("manifest_list_v2.avro",
                                 f"manifest_path {mf.get('manifest_path')!r}"))
            if mf.get("added_data_files_count") != 2:
                failures.append(("manifest_list_v2.avro",
                                 f"added_data_files_count {mf.get('added_data_files_count')!r}"))
            if mf.get("added_rows_count") != 300:
                failures.append(("manifest_list_v2.avro",
                                 f"added_rows_count {mf.get('added_rows_count')!r}"))
            print(f"  OK   manifest_list_v2.avro (1 manifest_file)")
    except Exception as e:
        failures.append(("manifest_list_v2.avro", repr(e)))

    # ------------------------------------------------------------
    # Table metadata JSON (parsed via pyiceberg's TableMetadata)
    # ------------------------------------------------------------
    print("\n== table_metadata_v2.json")
    try:
        path = out / "table_metadata_v2.json"
        raw_bytes = path.read_bytes()
        meta = TableMetadataUtil.parse_raw(raw_bytes)
        # spot-check important invariants pyiceberg exposes
        if meta.format_version != 2:
            failures.append(("table_metadata_v2.json",
                             f"format-version {meta.format_version}"))
        if str(meta.table_uuid) != "550e8400-e29b-41d4-a716-446655440000":
            failures.append(("table_metadata_v2.json",
                             f"table-uuid {meta.table_uuid}"))
        if meta.location != "s3://example/tbl":
            failures.append(("table_metadata_v2.json",
                             f"location {meta.location!r}"))
        if meta.current_snapshot_id != 1234567890:
            failures.append(("table_metadata_v2.json",
                             f"current-snapshot-id {meta.current_snapshot_id}"))
        if len(meta.schemas) != 1 or len(meta.schemas[0].fields) != 2:
            failures.append(("table_metadata_v2.json",
                             f"schema shape: {meta.schemas}"))
        if len(meta.snapshots) != 1:
            failures.append(("table_metadata_v2.json",
                             f"expected 1 snapshot, got {len(meta.snapshots)}"))
        if not failures or all(f[0] != "table_metadata_v2.json" for f in failures):
            print(f"  OK   table_metadata_v2.json (pyiceberg parsed cleanly)")
    except Exception as e:
        failures.append(("table_metadata_v2.json", repr(e)))

    print()
    if failures:
        print(f"{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("All wireform-iceberg outputs round-trip through pyiceberg + fastavro.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
