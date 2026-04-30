#!/usr/bin/env python3
"""Generate Parquet golden fixtures used by Test.GoldenParquet.

Run with `python3 generate.py` to (re)build:

  - simple_int.parquet           : single INT64 column, snappy compressed
  - mixed_types.parquet          : id (INT64), name (STRING), val (DOUBLE)
  - bloom_and_index.parquet      : INT64 column with bloom filter +
                                   page index enabled

The fixtures live in this directory and are loaded by the wireform-parquet
test suite, which verifies its reader round-trips the metadata
parquet-mr / arrow-cpp produced.
"""

import os
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq

HERE = Path(__file__).resolve().parent

def write_simple_int():
    table = pa.table({'id': pa.array([1, 2, 3, 4, 5], type=pa.int64())})
    pq.write_table(
        table, HERE / "simple_int.parquet",
        compression='snappy', use_dictionary=False,
    )

def write_mixed_types():
    table = pa.table({
        'id':   pa.array([10, 20, 30], type=pa.int64()),
        'name': pa.array(['alpha', 'beta', 'gamma'], type=pa.string()),
        'val':  pa.array([1.5, 2.5, 3.5], type=pa.float64()),
    })
    pq.write_table(
        table, HERE / "mixed_types.parquet",
        compression='gzip', use_dictionary=False,
    )

def write_bloom_and_index():
    table = pa.table({'id': pa.array(list(range(100)), type=pa.int64())})
    pq.write_table(
        table, HERE / "bloom_and_index.parquet",
        compression='snappy',
        use_dictionary=False,
        write_statistics=True,
        write_page_index=True,
    )

if __name__ == "__main__":
    os.makedirs(HERE, exist_ok=True)
    write_simple_int()
    write_mixed_types()
    write_bloom_and_index()
    print("wrote", *[p.name for p in HERE.glob('*.parquet')])
