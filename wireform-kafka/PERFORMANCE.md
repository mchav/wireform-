# Performance

Per-record costs for the hot paths (GHC 9.6.4, -O1):

| Operation | Batch size | Time/record |
|---|---|---|
| RecordBatch encode | 100 | **57 ns** |
| RecordBatch encode | 10 | 81 ns |
| RecordBatch encode | 1 | 191 ns |
| RecordBatch decode | 100 | **45 ns** |
| RecordBatch decode | 10 | 62 ns |
| BatchAccumulator append | 1000 | 245 ns |

**Full producer path:** ~302 ns/record (accumulator and encode, uncompressed)

**Comparison to librdkafka:**
- Encode: within 14% (~50 ns/rec)
- Decode: faster (~70 ns/rec for librdkafka)

The remaining producer gap is mostly STM overhead (~150 ns). Moving to locks would close this but is not yet prioritized.

## What's measured

CPU cost of the client itself: serializing, batching, decoding. Excludes network, compression, and broker latency.

## Key optimizations

- **Single-allocation encoding:** One buffer, direct writes, in-place CRC32C
- **Zero-copy decoding:** Record data slices into receive buffer
- **SIMD:** CRC32C via SSE4.2 or ARM CRC, `memmove` for record shifting
- **Fast Murmur2:** Word32 loads instead of byte assembly
- **Fast varint:** Inline single-byte path

## Run benchmarks

```bash
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 1.0 HotPath'
```

## Live broker comparison

```bash
# Start broker, create topic
export WIREFORM_KAFKA_BROKER=localhost:9092
cabal bench ... --benchmark-options='--time-limit 5.0 HwKafkaComparison'
```

## Tracking regressions

```bash
cabal bench ... --benchmark-options='--csv baseline.csv'
```

Compare with `criterion-compare` or similar tools.
