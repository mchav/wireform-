# Iceberg SIMD kernel benchmark results

Recorded with `cabal run wireform-iceberg:bench:iceberg-bench --
--time-limit 0.5` on a Linux x86-64 build with `ghc 9.4.7 -O2` and
the C kernels at `-O3 -march=native`.

| Workload                          | Pure (Haskell) | C/SIMDe         | Speedup |
| --------------------------------- | -------------- | --------------- | ------- |
| Murmur3 32 — 8 bytes              | 12.7 ns        | 13.5 ns         | ~1×     |
| Murmur3 32 — 64 bytes             | 58.8 ns        | 16.3 ns         | **3.6×** |
| Murmur3 32 — 1 KiB                | 868 ns         | 277 ns          | **3.1×** |
| Murmur3 32 — 64 KiB               | 57.9 µs        | 18.3 µs         | **3.2×** |
| `bucket[16](long)` (per call)     | 101.6 ns       | 9.0 ns          | **11.3×** |
| XXH64 64 B                        | (n/a)          | 14.8 ns         | —       |
| XXH64 1 KiB                       | (n/a)          | 71.3 ns         | —       |
| XXH64 64 KiB                      | (n/a)          | 4.3 µs          | —       |
| Deletion-vector decode 1001 pos.  | 28.4 µs        | 11.5 µs         | **2.5×** |
| Deletion-vector `containsPosition`| 771 ns         | 14.5 ns         | **53×**  |
| Roaring ARRAY encode 1000 lows    | (n/a)          | 112 ns          | —       |
| Roaring ARRAY decode 1000 lows    | (n/a)          | 176 ns          | —       |
| Roaring ARRAY contains (hit)      | (n/a)          | 13.5 ns         | —       |
| Roaring ARRAY contains (miss)     | (n/a)          | 21.7 ns         | —       |
| Roaring BITSET encode (30 K lows) | (n/a)          | 22.1 µs         | —       |
| Roaring BITSET decode (30 K lows) | (n/a)          | 12.0 µs         | —       |
| Roaring BITSET contains           | (n/a)          | 10.7 ns         | —       |

The 8-byte Murmur3 case is the only one where the FFI overhead is
visible (~1 ns); for the workloads Iceberg actually drives at
runtime (multi-byte string buckets, full `bucket[N](long)`,
deletion-vector reads, position lookups) the SIMD-backed kernels
are 3–53× faster than the pure-Haskell reference, with the largest
absolute wins on the per-row hot paths used during writes
(`bucketLong`) and during scans (`containsPosition`).

The pure references remain available as `_pure`-suffixed exports
in `Iceberg.Murmur3` and `Iceberg.DeletionVector`, so the bench can
re-measure regressions on every branch.
