#!/usr/bin/env python3
"""End-to-end pyarrow <-> wireform-arrow interop driver.

Two phases:

  1. Run the wireform-arrow probe executable (which writes
     candidate .arrows / .arrow files into a temp dir).
     pyarrow reads every file and reports any shape it rejects.

  2. Generate reference .arrows files with pyarrow's
     ipc.new_stream, update the goldens under
     test/golden/ so the Haskell test suite pins the exact
     bytes pyarrow emits today.

Run from the package root (wireform-arrow/). CI can treat the
script as optional; the Haskell test suite already checks the
checked-in goldens without needing pyarrow installed.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.ipc as ipc


ROOT = Path(__file__).resolve().parent.parent
GOLDEN_DIR = ROOT / "test" / "golden"


def run_probe() -> Path:
    """Invoke the cabal-built probe executable; return its output dir."""
    out = Path(tempfile.mkdtemp(prefix="wireform-arrow-probe-"))
    subprocess.run(
        [
            "cabal",
            "run",
            "wireform-arrow:wireform-arrow-pyarrow-probe",
            "--",
            str(out),
        ],
        cwd=ROOT.parent,
        check=True,
    )
    return out


def check_pyarrow_reads(out: Path) -> int:
    """pyarrow must consume every file the probe wrote."""
    failures = 0
    for path in sorted(out.iterdir()):
        data = path.read_bytes()
        try:
            if path.suffix == ".arrows":
                reader = ipc.open_stream(pa.BufferReader(data))
                list(reader)
            elif path.suffix == ".arrow":
                reader = ipc.open_file(pa.BufferReader(data))
                for i in range(reader.num_record_batches):
                    reader.get_batch(i)
            print(f"  OK  {path.name}")
        except Exception as e:
            print(f"  FAIL {path.name}: {e}")
            failures += 1
    return failures


def regen_goldens() -> None:
    """Generate pyarrow reference .arrows files under test/golden/."""
    GOLDEN_DIR.mkdir(parents=True, exist_ok=True)

    # 1. Simple non-nullable int32.
    sch = pa.schema([pa.field("a", pa.int32(), nullable=False)])
    batch = pa.record_batch(
        [pa.array([1, 2, 3, 4, 5], pa.int32())], schema=sch
    )
    with pa.OSFile(str(GOLDEN_DIR / "pa_int32.arrows"), "wb") as f:
        with ipc.new_stream(f, sch) as w:
            w.write_batch(batch)

    # 2. Multi-column mixed with nullable fields.
    sch2 = pa.schema([
        pa.field("i", pa.int64(), nullable=False),
        pa.field("s", pa.string(), nullable=True),
        pa.field("b", pa.bool_(), nullable=True),
    ])
    batch2 = pa.record_batch([
        pa.array([10, 20, 30], pa.int64()),
        pa.array(["alpha", None, "gamma"], pa.string()),
        pa.array([True, False, None], pa.bool_()),
    ], schema=sch2)
    with pa.OSFile(str(GOLDEN_DIR / "pa_mixed.arrows"), "wb") as f:
        with ipc.new_stream(f, sch2) as w:
            w.write_batch(batch2)

    # 3. Dictionary-encoded column.
    dict_arr = pa.DictionaryArray.from_arrays(
        pa.array([0, 1, 0, 2, 1], pa.int32()),
        pa.array(["a", "b", "c"], pa.string()),
    )
    sch3 = pa.schema([pa.field("d", dict_arr.type)])
    batch3 = pa.record_batch([dict_arr], schema=sch3)
    with pa.OSFile(str(GOLDEN_DIR / "pa_dict.arrows"), "wb") as f:
        with ipc.new_stream(f, sch3) as w:
            w.write_batch(batch3)

    print(f"Regenerated goldens under {GOLDEN_DIR.relative_to(ROOT)}/")


def main() -> int:
    if "--regen-goldens" in sys.argv:
        regen_goldens()
        return 0

    out = run_probe()
    try:
        print(f"pyarrow reading wireform output under {out}:")
        n = check_pyarrow_reads(out)
        if n:
            print(f"\n{n} file(s) failed pyarrow decode")
            return 1
        print("\nAll wireform-arrow probe outputs round-trip through pyarrow.")
        return 0
    finally:
        shutil.rmtree(out, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
