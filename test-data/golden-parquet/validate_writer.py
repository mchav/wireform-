#!/usr/bin/env python3
"""Validate that pyarrow can read Parquet files our writer produces.

Reads /tmp/wf-test-*.parquet (created by the wireform-parquet
write side) and prints the schema + first-row-group counts.
"""

import sys
from pathlib import Path

import pyarrow.parquet as pq

paths = sys.argv[1:] or list(Path("/tmp").glob("wf-test-*.parquet"))
for p in paths:
    pf = pq.ParquetFile(str(p))
    schema = pf.schema_arrow
    print(f"== {p}")
    print(f"  rows={pf.metadata.num_rows} groups={pf.num_row_groups}")
    print(f"  schema={schema}")
    rg = pf.metadata.row_group(0)
    for i in range(rg.num_columns):
        c = rg.column(i)
        print(f"  col[{i}] type={c.physical_type} stats=({c.statistics.min} .. {c.statistics.max})"
              f" codec={c.compression}")
