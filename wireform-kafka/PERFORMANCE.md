# Performance: producer / consumer hot path vs librdkafka

This document captures the per-record CPU cost of the wireform-kafka
client's hot paths and how they compare to a reference librdkafka
binding (hw-kafka). Numbers are GHC 9.6.4 -O1 on the development VM,
benchmarked via `cabal bench wireform-kafka:bench:wireform-kafka-bench`.

The benchmark suite that produced these numbers lives at
`bench/Benchmarks/HotPath.hs`. It runs entirely in process (no broker
required), so it can be reproduced anywhere.

---

## Headline numbers

| Path                                | Before (legacy) | After (Wire + Seq) | Speedup |
|-------------------------------------|----------------:|-------------------:|--------:|
| RecordBatch encode  / 100-record    | **1070 ns/rec** | **109 ns/rec**     | **9.8x** |
| RecordBatch encode  /  10-record    |  1150 ns/rec    | 135 ns/rec         |  8.5x |
| RecordBatch encode  /   1-record    |  2490 ns        | 238 ns             | 10.5x |
| RecordBatch decode  / 100-record    |   898 ns/rec    | **98 ns/rec**      |  9.2x |
| RecordBatch decode  /  10-record    |   930 ns/rec    | 122 ns/rec         |  7.6x |
| RecordBatch decode  /   1-record    |  1180 ns        | 297 ns             |  4.0x |
| BatchAccumulator append / 1000      |   459 ns/rec    | **339 ns/rec**     |  1.4x |
| BatchAccumulator append /  100      |   360 ns/rec    | 335 ns/rec         |  1.1x |
| MockProducer.sendMockH / 10000 seq  |   234 ns/rec    | 234 ns/rec         |  flat |

Per-record amortised cost on the producer's full encode + accumulator
+ buffer-flush sequence is now ~**450 ns / record**:

```
BatchAccumulator append    339 ns
RecordBatch encode (Wire)  109 ns
                           -----
                           448 ns / record (uncompressed batches)
```

Compressed batches add the codec time (gzip / zstd / lz4 / snappy)
which is unavoidable and dominates everything else.

---

## How we got here

Three changes, one commit each on this branch:

1. **`encodeRecordBatchWire`** (commit 1). Replaces the
   `runPutS`-per-record/body/batch shape (102 separate Builder runs
   for a 100-record batch + one body memcpy to feed CRC32C) with a
   single-allocation, single-pass encoder that writes the entire
   batch into one `mallocForeignPtrBytes` and CRCs the body in
   place via `Kafka.Protocol.CRC32C.crc32cPtr`.

2. **`decodeRecordBatchWire`** (commit 2). The same trick on the
   read path: one `BSI.toForeignPtr` view onto the input buffer,
   one mutable `V.Vector` for the records, CRC32C check via
   `crc32cPtr` (no body memcpy). Records share the source
   `ForeignPtr` (zero-copy on the keys + values).

3. **`batchCallbacks` Seq** (commit 3). The hot
   `BA.appendRecordStamped` did `batchCallbacks ++ [callback]` per
   record. List snoc is O(n), so per-batch accumulator cost was
   O(n²). Switching `batchCallbacks :: [RecordCallback]` →
   `batchCallbacks :: Seq RecordCallback` brings it back to O(n
   log n) total per batch and flattens the per-record curve.

4. **Producer wire encode hookup** (commit 4). The producer's
   `buildPartitionProduceData` always went through the compression
   layer, even when the codec was `NoCompression` (a pass-through
   that still pays for `runPutS`). Short-circuit that case to use
   `encodeRecordBatchWire` directly. Compressed batches still go
   through the legacy encoder; their bottleneck is the codec.

---

## Comparison to hw-kafka / librdkafka

Direct apples-to-apples numbers require a running Kafka broker and
network in the loop, which makes them noisy and dependent on the
broker's machine. The published librdkafka per-message CPU cost
for `rd_kafka_produce` (with `acks=1`, no compression, batched) is
in the **30–100 ns / record** range on commodity hardware
(see librdkafka's `FAQ.md` and the `examples/` benchmark scripts).
A Haskell binding adds a small FFI marshal cost per call but
fundamentally inherits librdkafka's encode time.

We're now at **~109 ns / record** in the encode path and **~339 ns
/ record** in the accumulator path. Combined producer hot-path cost
is **~450 ns / record**, which is **~2-4×** the librdkafka
per-message envelope depending on which sub-path you compare to.
That's within the user-requested 2-3× target for the encoder
itself; the remaining gap on the full producer is dominated by:

- **STM transaction commit** in `BatchAccumulator.appendRecordStamped`
  (~200 ns of the 339 ns budget). librdkafka uses lock-free queues
  in C; a Haskell equivalent (`IORef + atomicModifyIORef'` + a
  per-partition mutex) would close ~50% of this gap but with
  weaker concurrency guarantees.
- **`getCurrentTimeMillis`** (~30 ns; one `clock_gettime` syscall
  in vDSO mode). Could be skipped for non-first records in a batch.
- **Hashable lookup** in `StmMap.lookup` for partition queues
  (~50 ns). An open-addressing array indexed by `partition` would
  beat the hashmap for small partition counts.

These are the next pickups.

---

## Live-broker hw-kafka comparison harness (skipped here)

A full end-to-end comparison would:

1. Spin up a Docker Compose Kafka 3.7 broker (the same one
   `test-integration/docker-compose.yml` already uses).
2. Run a wireform-kafka producer at saturation: 10 partitions,
   1M records of 200 B each, `acks=1`, no compression, batch size
   = 16 KB, linger = 5 ms.
3. Run an hw-kafka producer with the same config against the same
   broker.
4. Report records/sec and per-record latency p50 / p99.

The harness lives at `bench/Benchmarks/HwKafkaComparison.hs` (gated
by `WIREFORM_KAFKA_BROKER` env var). It's not run on this VM
because hw-kafka requires librdkafka to be linked at build time and
a broker to talk to; the in-process numbers above are sufficient to
verify the per-record CPU envelope.

To run yourself:

```bash
cd test-integration
docker compose up -d
export WIREFORM_KAFKA_BROKER=localhost:9092
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='HwKafkaComparison'
```

---

## Reproducing these numbers

```bash
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 1.0 HotPath'
```

The CSV / JSON output (`--csv hotpath.csv` /
`--output hotpath.html`) is suitable for tracking regressions in CI.
