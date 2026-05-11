#!/usr/bin/env python3
"""wireform-iceberg -> pyiceberg interop driver.

Runs the wireform-iceberg-interop-probe to emit a small catalogue
of Iceberg table-format files (manifests, manifest lists, table
metadata) and reads each one with pyiceberg's official table
metadata parser (``pyiceberg.table.metadata.TableMetadataUtil``)
and ``fastavro`` for the manifest / manifest-list Avro
containers. Asserts every structural field round-trips.

This is the *table-format* interop story (the Iceberg metadata
files that any reader has to consume to plan a scan); it is
deliberately separate from the in-process catalog client interop,
which is exercised by the per-catalog HUnit tests under
``wireform-iceberg/test/Test/Iceberg/Catalog*``.

Usage:
    python3 wireform-iceberg/scripts/iceberg_interop.py
"""

from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path

import fastavro
from pyiceberg.table.metadata import TableMetadataUtil

ROOT = Path(__file__).resolve().parent.parent


# ---------------------------------------------------------------
# Tiny tracker so we collect every assertion failure rather
# than aborting on the first.
# ---------------------------------------------------------------
class Failures(list):
    def add(self, where: str, msg: str) -> None:
        self.append((where, msg))


def expect_eq(failures: Failures, where: str, label: str, got, want) -> None:
    if got != want:
        failures.add(where, f"{label} = {got!r}, want {want!r}")


def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="wireform-iceberg-probe-"))
    print(f"== writing wireform-iceberg probe outputs to {out}")
    subprocess.run(
        ["cabal", "run", "wireform-iceberg:wireform-iceberg-interop-probe",
         "--", str(out)],
        cwd=ROOT.parent, check=True,
    )

    failures = Failures()

    # ------------------------------------------------------------
    # 1. Manifest entries (data) via fastavro.
    # ------------------------------------------------------------
    print("\n== manifest_v2.avro (fastavro)")
    try:
        path = out / "manifest_v2.avro"
        with path.open("rb") as f:
            entries = list(fastavro.reader(f))
        if len(entries) != 2:
            failures.add("manifest_v2.avro", f"expected 2 entries, got {len(entries)}")
        else:
            expected = ["data/file_a.parquet", "data/file_b.parquet"]
            for i, (entry, expected_path) in enumerate(zip(entries, expected)):
                expect_eq(failures, f"manifest_v2.avro[{i}]", "status",
                          entry.get("status"), 1)  # 1 = ADDED
                df = entry.get("data_file") or {}
                expect_eq(failures, f"manifest_v2.avro[{i}]", "file_path",
                          df.get("file_path"), expected_path)
                expect_eq(failures, f"manifest_v2.avro[{i}]", "record_count",
                          df.get("record_count"), 100 if i == 0 else 200)
            print(f"  OK   manifest_v2.avro ({len(entries)} entries)")
    except Exception as e:
        failures.add("manifest_v2.avro", repr(e))

    # ------------------------------------------------------------
    # 2. Manifest list (single-manifest variant) via fastavro
    # ------------------------------------------------------------
    print("\n== manifest_list_v2.avro (fastavro)")
    try:
        path = out / "manifest_list_v2.avro"
        with path.open("rb") as f:
            files = list(fastavro.reader(f))
        if len(files) != 1:
            failures.add("manifest_list_v2.avro",
                         f"expected 1 manifest_file, got {len(files)}")
        else:
            mf = files[0]
            expect_eq(failures, "manifest_list_v2.avro", "manifest_path",
                      mf.get("manifest_path"), "metadata/manifest_v2.avro")
            expect_eq(failures, "manifest_list_v2.avro", "added_data_files_count",
                      mf.get("added_data_files_count"), 2)
            expect_eq(failures, "manifest_list_v2.avro", "added_rows_count",
                      mf.get("added_rows_count"), 300)
            print(f"  OK   manifest_list_v2.avro (1 manifest_file)")
    except Exception as e:
        failures.add("manifest_list_v2.avro", repr(e))

    # ------------------------------------------------------------
    # 3. Table metadata JSON via pyiceberg.
    # ------------------------------------------------------------
    print("\n== table_metadata_v2.json (pyiceberg)")
    try:
        path = out / "table_metadata_v2.json"
        meta = TableMetadataUtil.parse_raw(path.read_bytes())
        expect_eq(failures, "table_metadata_v2.json", "format-version",
                  meta.format_version, 2)
        expect_eq(failures, "table_metadata_v2.json", "table-uuid",
                  str(meta.table_uuid), "550e8400-e29b-41d4-a716-446655440000")
        expect_eq(failures, "table_metadata_v2.json", "location",
                  meta.location, "s3://example/tbl")
        expect_eq(failures, "table_metadata_v2.json", "current-snapshot-id",
                  meta.current_snapshot_id, 1234567890)
        if len(meta.schemas) != 1 or len(meta.schemas[0].fields) != 2:
            failures.add("table_metadata_v2.json",
                         f"schema shape: {meta.schemas}")
        if len(meta.snapshots) != 1:
            failures.add("table_metadata_v2.json",
                         f"snapshots = {len(meta.snapshots)} (want 1)")
        if not any(f[0] == "table_metadata_v2.json" for f in failures):
            print(f"  OK   table_metadata_v2.json (pyiceberg parsed cleanly)")
    except Exception as e:
        failures.add("table_metadata_v2.json", repr(e))

    # ------------------------------------------------------------
    # 4. Delete manifest via fastavro: position + equality entries.
    # ------------------------------------------------------------
    print("\n== manifest_v2_deletes.avro (fastavro)")
    try:
        path = out / "manifest_v2_deletes.avro"
        with path.open("rb") as f:
            entries = list(fastavro.reader(f))
        if len(entries) != 2:
            failures.add("manifest_v2_deletes.avro",
                         f"expected 2 entries, got {len(entries)}")
        else:
            # Both entries have data_file.content = 1 (DeletesContent),
            # but iceberg readers tell position vs equality apart by
            # whether equality_ids is non-empty.
            for i, entry in enumerate(entries):
                df = entry.get("data_file") or {}
                expect_eq(failures, f"manifest_v2_deletes.avro[{i}]", "content",
                          df.get("content"), 1)
            eq_ids = entries[1].get("data_file", {}).get("equality_ids") or []
            if list(eq_ids) != [1]:
                failures.add("manifest_v2_deletes.avro",
                             f"equality entry equality_ids = {eq_ids}, want [1]")
            else:
                print(f"  OK   manifest_v2_deletes.avro (1 position + 1 equality)")
    except Exception as e:
        failures.add("manifest_v2_deletes.avro", repr(e))

    # ------------------------------------------------------------
    # 5. Manifest list with summaries via fastavro: covers
    #    DataContent / DeletesContent + per-partition field
    #    summaries on a partitioned manifest pointer.
    #
    #    We use fastavro (not pyiceberg.manifest.read_manifest_list)
    #    because the latter requires Iceberg's @element-id@
    #    annotation on every Avro array (per the v2 manifest
    #    schema), and the wireform-avro core schema model doesn't
    #    yet thread that property through. Adding it is its own
    #    change to wireform-avro and is tracked separately. The
    #    fastavro read still validates the wire-level structure
    #    pyiceberg consumes once that lands.
    # ------------------------------------------------------------
    print("\n== manifest_list_v2_full.avro (fastavro)")
    try:
        path = out / "manifest_list_v2_full.avro"
        with path.open("rb") as f:
            files = list(fastavro.reader(f))
        if len(files) != 3:
            failures.add("manifest_list_v2_full.avro",
                         f"expected 3 manifest_files, got {len(files)}")
        else:
            data_mf, part_mf, del_mf = files
            expect_eq(failures, "manifest_list_v2_full.avro", "data manifest_path",
                      data_mf.get("manifest_path"), "metadata/manifest_v2.avro")
            expect_eq(failures, "manifest_list_v2_full.avro", "data content",
                      data_mf.get("content"), 0)  # 0 = data

            expect_eq(failures, "manifest_list_v2_full.avro", "partitioned manifest_path",
                      part_mf.get("manifest_path"), "metadata/manifest_v2_partitioned.avro")
            expect_eq(failures, "manifest_list_v2_full.avro", "partitioned partition_spec_id",
                      part_mf.get("partition_spec_id"), 1)
            summaries = part_mf.get("partitions") or []
            if len(summaries) != 1:
                failures.add("manifest_list_v2_full.avro",
                             f"partitioned summaries = {summaries}, want 1 entry")
            else:
                summary = summaries[0]
                expect_eq(failures, "manifest_list_v2_full.avro", "summary lower",
                          summary.get("lower_bound"), b"A")
                expect_eq(failures, "manifest_list_v2_full.avro", "summary upper",
                          summary.get("upper_bound"), b"B")

            expect_eq(failures, "manifest_list_v2_full.avro", "delete manifest_path",
                      del_mf.get("manifest_path"), "metadata/manifest_v2_deletes.avro")
            expect_eq(failures, "manifest_list_v2_full.avro", "delete content",
                      del_mf.get("content"), 1)  # 1 = deletes

            if not any(f[0] == "manifest_list_v2_full.avro" for f in failures):
                print(f"  OK   manifest_list_v2_full.avro ({len(files)} manifest_files)")
    except Exception as e:
        failures.add("manifest_list_v2_full.avro", repr(e))

    # ------------------------------------------------------------
    # 6. Rich table metadata: partition specs, sort orders, refs,
    #    multi-snapshot history.
    # ------------------------------------------------------------
    print("\n== table_metadata_v2_full.json (pyiceberg)")
    try:
        path = out / "table_metadata_v2_full.json"
        meta = TableMetadataUtil.parse_raw(path.read_bytes())
        expect_eq(failures, "table_metadata_v2_full.json", "format-version",
                  meta.format_version, 2)
        expect_eq(failures, "table_metadata_v2_full.json", "current-snapshot-id",
                  meta.current_snapshot_id, 1234567891)
        expect_eq(failures, "table_metadata_v2_full.json", "default-spec-id",
                  meta.default_spec_id, 1)
        expect_eq(failures, "table_metadata_v2_full.json", "default-sort-order-id",
                  meta.default_sort_order_id, 1)
        expect_eq(failures, "table_metadata_v2_full.json", "len(partition_specs)",
                  len(meta.partition_specs), 2)
        expect_eq(failures, "table_metadata_v2_full.json", "len(sort_orders)",
                  len(meta.sort_orders), 2)
        expect_eq(failures, "table_metadata_v2_full.json", "len(snapshots)",
                  len(meta.snapshots), 2)
        expect_eq(failures, "table_metadata_v2_full.json", "len(snapshot-log)",
                  len(meta.snapshot_log), 2)

        # Two snapshots; the second has a parent equal to the first.
        snaps_by_id = {s.snapshot_id: s for s in meta.snapshots}
        if 1234567891 in snaps_by_id:
            expect_eq(failures, "table_metadata_v2_full.json",
                      "snapshot 1234567891 parent",
                      snaps_by_id[1234567891].parent_snapshot_id, 1234567890)

        # Refs: 'main' branch on the latest snapshot, 'snap-v1' tag on the older.
        refs = meta.refs
        if "main" in refs:
            expect_eq(failures, "table_metadata_v2_full.json", "refs.main snapshot",
                      refs["main"].snapshot_id, 1234567891)
            expect_eq(failures, "table_metadata_v2_full.json", "refs.main type",
                      refs["main"].snapshot_ref_type, "branch")
        else:
            failures.add("table_metadata_v2_full.json", "missing main ref")
        if "snap-v1" in refs:
            expect_eq(failures, "table_metadata_v2_full.json", "refs.snap-v1 type",
                      refs["snap-v1"].snapshot_ref_type, "tag")
        else:
            failures.add("table_metadata_v2_full.json", "missing snap-v1 ref")

        # Partition spec[1] is truncate[1] on column 2 ("name").
        spec1 = meta.partition_specs[1]
        if len(spec1.fields) == 1:
            f = spec1.fields[0]
            expect_eq(failures, "table_metadata_v2_full.json",
                      "partition_spec[1] field name", f.name, "name_trunc")
            expect_eq(failures, "table_metadata_v2_full.json",
                      "partition_spec[1] source-id", f.source_id, 2)
            expect_eq(failures, "table_metadata_v2_full.json",
                      "partition_spec[1] transform", str(f.transform), "truncate[1]")
        else:
            failures.add("table_metadata_v2_full.json",
                         f"partition_spec[1] fields = {spec1.fields}")

        if not any(f[0] == "table_metadata_v2_full.json" for f in failures):
            print(f"  OK   table_metadata_v2_full.json (rich metadata round-tripped)")
    except Exception as e:
        failures.add("table_metadata_v2_full.json", repr(e))

    # ------------------------------------------------------------
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
