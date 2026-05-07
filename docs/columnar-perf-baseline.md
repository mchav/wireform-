# Performance baseline (Parquet, end-to-end)

A 100k-row, 4-column dataset (Int64 id, Float64 score, Utf8
name, Bool active) round-tripped through wireform-parquet and
pyarrow on the same shape.

## Numbers (current main, GHC 9.6.4 -O2, single thread)

|                 | wireform | pyarrow  | ratio       |
|-----------------|---------:|---------:|------------:|
| write none      |  47.9 ms |   9.4 ms |  5.1× slower |
| write snappy    |  48.0 ms |  10.9 ms |  4.4× slower |
| write zstd      |  50.1 ms |  13.1 ms |  3.8× slower |
| read none       |  4522 ms |   2.2 ms | **2086× slower** |
| read snappy     |  4469 ms |   2.4 ms |  1900× slower |
| read zstd       |  4419 ms |   2.0 ms |  2170× slower |

File sizes (bytes): uncompressed=2,157,279 · snappy=1,212,588 · zstd=585,180.

In rows/second:

| workload                   | wireform     | pyarrow         |
|----------------------------|-------------:|----------------:|
| write uncompressed         | 2.1M rows/s  | 10.6M rows/s    |
| read  uncompressed         | 22k rows/s   | 46M rows/s      |

## Read-path observation (worth a follow-up PR)

The read path is **2000× slower than write**. That's the
single most actionable performance finding from this audit.
Likely culprits:

1. The probe's `readBack` function reads one (row group,
   column) at a time. The per-call setup work — parsing each
   column chunk's pages independently — dominates for small
   files. A single-pass row-group reader that decodes every
   column in one walk would be a big win on its own.
2. The Parquet RLE / dictionary decode path inside the
   page-walking loop probably has per-element Haskell
   overhead (intermediate `Vector.snoc`, lazy Maybe, etc.)
   that pyarrow's C++ kernel sidesteps with batched
   bit-unpacking.
3. The `decodeUtf8` / `Text` allocations on the BYTE_ARRAY
   path probably produce one boxed `Text` per value — pyarrow
   keeps strings inline in a Latin-1 / UTF-8 buffer.

Pinpointing which one matters requires profiling
(`+RTS -p`); that's the next concrete step. The benchmark
infra (the `parquet-throughput` benchmark + the
`parquet_bench_compare.py` driver) is in place so a future
"read perf" PR has a regression-testable baseline to point at.

## Reproduce

```bash
# Run the wireform end-to-end Parquet benchmark
cabal bench wireform-parquet:parquet-throughput \
  --benchmark-options='--csv /tmp/wireform_throughput.csv \
                        --time-limit 3'

# Compare against pyarrow on the same shape
python3 wireform-parquet/scripts/parquet_bench_compare.py
```
