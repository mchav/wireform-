#!/usr/bin/env python3
"""wireform-hudi ↔ hudi-rs interop driver.

Hand-builds a small Hudi table on disk (with the JSON commit
payload that wireform-hudi parses) and a sibling Parquet base
file per fileId, then:

* runs ``hudi-rs`` (`hudi.HudiTableBuilder`) to enumerate the
  active file slices the *canonical* reader sees;
* runs ``wireform-hudi-interop-probe`` to enumerate the slices
  that wireform-hudi's ``tableStateFromCommits`` derives;
* asserts the two views agree on ``(partition_path, file_id,
  base_file)`` for every active slice.

Hudi-rs is read-only — writing real Hudi tables requires
Spark / hudi-java, which is too heavy to drag in here. The hand-
built layout we produce is exactly what an unpartitioned and a
partitioned Hudi table look like on disk after a couple of
COPY_ON_WRITE commits.

Usage:
    python3 wireform-hudi/scripts/hudi_interop.py
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
from hudi import HudiTableBuilder

ROOT = Path(__file__).resolve().parent.parent

HOODIE_PROPS_BASE = """\
hoodie.table.type=COPY_ON_WRITE
hoodie.table.name={name}
hoodie.table.version=5
hoodie.timeline.layout.version=1
hoodie.archivelog.folder=archived
hoodie.table.recordkey.fields=id
hoodie.table.precombine.field=id
hoodie.datasource.write.hive_style_partitioning=false
hoodie.datasource.write.partitionpath.urlencode=false
hoodie.populate.meta.fields=true
hoodie.compaction.payload.class=org.apache.hudi.common.model.OverwriteWithLatestAvroPayload
hoodie.metadata.enable=false
"""


def write_instant(hoodie: Path, instant_time: str, commit: dict,
                  encoding: str = "json") -> None:
    """Write the (requested, inflight, completed) triple for one
    instant. The completed instant payload can be 'json' (Hudi
    0.x) or 'avro' (Hudi 1.x+); both shapes must round-trip
    through wireform-hudi."""
    for ext in ("commit.requested", "inflight", "commit"):
        fp = hoodie / f"{instant_time}.{ext}"
        if ext != "commit":
            fp.write_text("")
            continue
        if encoding == "json":
            fp.write_text(json.dumps(commit))
        elif encoding == "avro":
            import fastavro
            schema_path = ROOT / "avro" / "HoodieCommitMetadata.avsc"
            schema = fastavro.parse_schema(json.load(schema_path.open()))
            with fp.open("wb") as f:
                fastavro.writer(f, schema, [commit])
        else:
            raise ValueError(f"unsupported encoding {encoding!r}")


def make_stat(file_id: str, rel_path: str, partition: str,
              num_writes: int, prev_commit: str = "null") -> dict:
    return {
        "fileId": file_id,
        "path": rel_path,
        "partitionPath": partition,
        "numWrites": num_writes,
        "totalWriteBytes": 1024,
        "totalLogRecords": 0,
        "totalLogFiles": 0,
        "totalLogBlocks": 0,
        "totalLogFilesCompacted": 0,
        "totalCorruptLogBlock": 0,
        "totalRollbackBlocks": 0,
        "fileSizeInBytes": 1024,
        "baseFile": rel_path.split("/")[-1],
        "prevCommit": prev_commit,
    }


class Failures(list):
    def add(self, where: str, msg: str) -> None:
        self.append((where, msg))


def expect_eq(failures: Failures, where: str, label: str, got, want) -> None:
    if got != want:
        failures.add(where, f"{label} = {got!r}, want {want!r}")


def setup_table(tmp: Path, name: str, partitioned: bool) -> Path:
    base = tmp / name
    hoodie = base / ".hoodie"
    for sub in (".heartbeat", ".aux", "metadata", "archived"):
        (hoodie / sub).mkdir(parents=True, exist_ok=True)

    props = HOODIE_PROPS_BASE.format(name=name)
    if partitioned:
        props += "hoodie.table.partition.fields=region\n"
    (hoodie / "hoodie.properties").write_text(props)
    return base


def run_probe(table_root: Path) -> dict:
    out_json = table_root.parent / (table_root.name + ".probe.json")
    subprocess.run(
        [
            "cabal", "run",
            "wireform-hudi:wireform-hudi-interop-probe",
            "--",
            str(table_root), str(out_json),
        ],
        cwd=ROOT.parent, check=True,
    )
    with out_json.open() as f:
        return json.load(f)


def hudi_rs_slices(table_root: Path) -> list[tuple[str, str, str]]:
    """(partition_path, file_id, base_file_name) tuples per
    hudi-rs's HudiTable.get_file_slices()."""
    t = (
        HudiTableBuilder.from_base_uri(str(table_root))
        .with_hudi_option("hoodie.metadata.enable", "false")
        .build()
    )
    out = []
    for s in t.get_file_slices():
        out.append((s.partition_path, s.file_id, s.base_file_name))
    return sorted(out)


def wireform_slices(summary: dict) -> list[tuple[str, str, str]]:
    out = []
    for s in summary["active_file_slices"]:
        bf = s["base_file"]
        if bf is None:
            continue
        # hudi-rs reports just the file name; wireform sees the
        # commit's `baseFile` (also a name, no path).
        out.append((s["partition_path"], s["file_id"], bf))
    return sorted(out)


# ---------------------------------------------------------------
# Cases
# ---------------------------------------------------------------


def case_unpartitioned(failures: Failures, tmp: Path) -> None:
    base = setup_table(tmp, "unpart", partitioned=False)
    print(f"\n== unpartitioned table at {base}")

    ts1 = "20240101000000000"
    ts2 = "20240102000000000"
    rel1 = f"fid-1_0_{ts1}.parquet"
    rel2 = f"fid-1_0_{ts2}.parquet"

    # Two commits, both touch file id "fid-1": the second supersedes
    # the first as the active base file. Active set after both = 1
    # slice with base_file = rel2.
    commit1 = {
        "partitionToWriteStats": {"": [make_stat("fid-1", rel1, "", 5)]},
        "compacted": False, "operationType": "INSERT", "extraMetadata": {},
    }
    commit2 = {
        "partitionToWriteStats": {"": [make_stat("fid-1", rel2, "", 10, prev_commit=ts1)]},
        "compacted": False, "operationType": "UPSERT", "extraMetadata": {},
    }
    write_instant(base / ".hoodie", ts1, commit1)
    write_instant(base / ".hoodie", ts2, commit2)
    pq.write_table(pa.table({"id": list(range(5))}),  str(base / rel1))
    pq.write_table(pa.table({"id": list(range(10))}), str(base / rel2))

    canonical = hudi_rs_slices(base)
    summary   = run_probe(base)
    ours      = wireform_slices(summary)

    expect_eq(failures, "unpart", "active_file_slice_count",
              summary["active_file_slice_count"], 1)
    expect_eq(failures, "unpart", "completed_commits",
              summary["completed_commits"], [ts1, ts2])
    expect_eq(failures, "unpart", "latest_instant",
              summary["latest_instant"], ts2)
    expect_eq(failures, "unpart", "(partition, file_id, base_file)",
              ours, canonical)

    # The new openHudiTable opener exposes hoodie.properties.
    expect_eq(failures, "unpart", "table_name",  summary["table_name"], "unpart")
    expect_eq(failures, "unpart", "table_type",  summary["table_type"], "COPY_ON_WRITE")
    if not any(f[0] == "unpart" for f in failures):
        print(f"  OK   unpart: {len(canonical)} slice (hudi-rs and wireform agree, "
              f"table_name={summary['table_name']}, table_type={summary['table_type']})")


def case_partitioned(failures: Failures, tmp: Path) -> None:
    base = setup_table(tmp, "part", partitioned=True)
    print(f"\n== partitioned table at {base}")

    ts1 = "20240101000000000"
    ts2 = "20240102000000000"
    rel_us = f"us/fid-us_0_{ts1}.parquet"
    rel_eu1 = f"eu/fid-eu_0_{ts1}.parquet"
    rel_eu2 = f"eu/fid-eu_0_{ts2}.parquet"

    commit1 = {
        "partitionToWriteStats": {
            "us": [make_stat("fid-us", rel_us, "us", 3)],
            "eu": [make_stat("fid-eu", rel_eu1, "eu", 4)],
        },
        "compacted": False, "operationType": "INSERT", "extraMetadata": {},
    }
    commit2 = {
        "partitionToWriteStats": {
            "eu": [make_stat("fid-eu", rel_eu2, "eu", 8, prev_commit=ts1)],
        },
        "compacted": False, "operationType": "UPSERT", "extraMetadata": {},
    }
    write_instant(base / ".hoodie", ts1, commit1)
    write_instant(base / ".hoodie", ts2, commit2)

    (base / "us").mkdir()
    (base / "eu").mkdir()
    pq.write_table(pa.table({"id": [1, 2, 3]}),       str(base / rel_us))
    pq.write_table(pa.table({"id": [10, 11, 12, 13]}),str(base / rel_eu1))
    pq.write_table(pa.table({"id": list(range(8))}),  str(base / rel_eu2))

    canonical = hudi_rs_slices(base)
    summary   = run_probe(base)
    ours      = wireform_slices(summary)

    # 2 active slices: us/fid-us @ ts1, eu/fid-eu @ ts2.
    expect_eq(failures, "part", "active_file_slice_count",
              summary["active_file_slice_count"], 2)
    expect_eq(failures, "part", "(partition, file_id, base_file)",
              ours, canonical)
    if not any(f[0] == "part" for f in failures):
        print(f"  OK   part: {len(canonical)} slices, "
              f"latest_instant={summary['latest_instant']}")


def case_avro_commit(failures: Failures, tmp: Path) -> None:
    """Hudi 1.x writes commit instants as Avro container files
    instead of JSON. Both wire encodings must thread through
    wireform-hudi's reader to the same active-file-slice view."""
    base = setup_table(tmp, "unpart_avro", partitioned=False)
    print(f"\n== unpartitioned table (Avro commit) at {base}")

    ts1 = "20240301000000000"
    rel1 = f"fid-1_0_{ts1}.parquet"

    commit1 = {
        "partitionToWriteStats": {"": [make_stat("fid-1", rel1, "", 7)]},
        "compacted": False,
        "operationType": "INSERT",
        "extraMetadata": {"schema": "{\"type\":\"record\",\"name\":\"X\",\"fields\":[]}"},
        # The avsc has 9 top-level fields beyond the four we care about;
        # fastavro requires every field to be present even if null.
        "totalCreateTime": None, "totalUpsertTime": None, "totalScanTime": None,
        "writePartitionPaths": None, "fileIdAndRelativePaths": None,
    }
    write_instant(base / ".hoodie", ts1, commit1, encoding="avro")
    pq.write_table(pa.table({"id": list(range(7))}), str(base / rel1))

    summary = run_probe(base)

    expect_eq(failures, "unpart_avro", "active_file_slice_count",
              summary["active_file_slice_count"], 1)
    expect_eq(failures, "unpart_avro", "completed_commits",
              summary["completed_commits"], [ts1])
    expect_eq(failures, "unpart_avro", "latest_instant",
              summary["latest_instant"], ts1)

    # The Avro path also surfaces the schema string from
    # extraMetadata, which the JSON path already covered.
    if summary["schema_json"] is None:
        failures.add("unpart_avro", "schema_json missing from Avro commit")

    if not any(f[0] == "unpart_avro" for f in failures):
        print(f"  OK   unpart_avro: 1 slice (Avro commit decoded), "
              f"schema={summary['schema_json'][:40]!r}…")


def case_replacecommit(failures: Failures, tmp: Path) -> None:
    """Replacecommit instants supersede prior file slices for
    the named partitions. Without consuming
    'partitionToReplaceFileIds' a 'TableState' fold over a
    clustered or INSERT_OVERWRITE'd table shows duplicates."""
    base = setup_table(tmp, "rcmt", partitioned=False)
    print(f"\n== unpartitioned table with replacecommit at {base}")

    ts1 = "20240401000000000"
    ts2 = "20240402000000000"
    rel1 = f"fid-old_0_{ts1}.parquet"
    rel2 = f"fid-new_0_{ts2}.parquet"

    commit1 = {
        "partitionToWriteStats": {"": [make_stat("fid-old", rel1, "", 100)]},
        "compacted": False, "operationType": "INSERT", "extraMetadata": {},
    }
    # Replace commit: kills fid-old, writes fid-new.
    replace2 = {
        "partitionToWriteStats": {
            "": [make_stat("fid-new", rel2, "", 50, prev_commit="null")]
        },
        "partitionToReplaceFileIds": {"": ["fid-old"]},
        "compacted": False, "operationType": "INSERT_OVERWRITE",
        "extraMetadata": {},
    }
    write_instant(base / ".hoodie", ts1, commit1)
    # Replacecommit needs an instant filename of <ts>.replacecommit
    # rather than <ts>.commit, plus the same {requested, inflight}
    # placeholders.
    for ext in ("replacecommit.requested", "replacecommit.inflight",
                "replacecommit"):
        fp = base / ".hoodie" / f"{ts2}.{ext}"
        if ext == "replacecommit":
            fp.write_text(json.dumps(replace2))
        else:
            fp.write_text("")

    pq.write_table(pa.table({"id": list(range(100))}), str(base / rel1))
    pq.write_table(pa.table({"id": list(range(50))}),  str(base / rel2))

    summary = run_probe(base)
    expect_eq(failures, "rcmt", "active_file_slice_count",
              summary["active_file_slice_count"], 1)

    # Active slice must be fid-new with rel2 base file.
    if not summary["active_file_slices"]:
        failures.add("rcmt", "no active file slices")
    else:
        slice0 = summary["active_file_slices"][0]
        expect_eq(failures, "rcmt", "file_id",   slice0["file_id"], "fid-new")
        # The base-file path is just the filename (Hudi's HoodieWriteStat
        # 'baseFile' key) — wireform surfaces that.
        expect_eq(failures, "rcmt", "base_file",
                  slice0["base_file"],
                  f"fid-new_0_{ts2}.parquet")

    if not any(f[0] == "rcmt" for f in failures):
        print(f"  OK   replacecommit: 1 slice (fid-old replaced by fid-new)")


def main() -> int:
    failures = Failures()
    tmp = Path(tempfile.mkdtemp(prefix="wireform-hudi-probe-"))

    case_unpartitioned(failures, tmp)
    case_partitioned (failures, tmp)
    case_avro_commit (failures, tmp)
    case_replacecommit(failures, tmp)

    print()
    if failures:
        print(f"{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("All wireform-hudi outputs round-trip with hudi-rs.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
