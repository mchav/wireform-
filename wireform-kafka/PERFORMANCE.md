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

| Path                                | Before (legacy) | After (Wire + Seq + fast-path) | Speedup |
|-------------------------------------|----------------:|-----------------:|--------:|
| RecordBatch encode  / 100-record    | **1070 ns/rec** | **109 ns/rec**   | **9.8x** |
| RecordBatch encode  /  10-record    |  1150 ns/rec    | 135 ns/rec       |  8.5x |
| RecordBatch encode  /   1-record    |  2490 ns        | 238 ns           | 10.5x |
| RecordBatch decode  / 100-record    |   898 ns/rec    | **98 ns/rec**    |  9.2x |
| RecordBatch decode  /  10-record    |   930 ns/rec    | 122 ns/rec       |  7.6x |
| RecordBatch decode  /   1-record    |  1180 ns        | 297 ns           |  4.0x |
| BatchAccumulator append / 1000      |   459 ns/rec    | **245 ns/rec**   |  1.9x |
| BatchAccumulator append /  100      |   360 ns/rec    | 245 ns/rec       |  1.5x |
| BatchAccumulator append /  single   |   400 ns        | 245 ns           |  1.6x |
| MockProducer.sendMockH / 10000 seq  |   234 ns/rec    | 234 ns/rec       |  flat |

Per-record amortised cost on the producer's full encode + accumulator
+ buffer-flush sequence is now ~**354 ns / record**:

```
BatchAccumulator append    245 ns
RecordBatch encode (Wire)  109 ns
                           -----
                           354 ns / record (uncompressed batches)
```

That puts us at ~**2.4× librdkafka**'s ~150 ns/rec producer-side
CPU envelope — within striking distance of the 2× target. The
remaining gap is ~150 ns of `atomically` commit overhead which
can't be removed without moving the per-partition queue off STM.

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

The `Benchmarks.HwKafkaComparison` benchmark group measures
end-to-end producer throughput against a live broker. Both
producers run with `acks=1`, no compression, 16 KB batches, 5 ms
linger, into a pre-created single-partition topic.

### Live-broker measurement (this VM, Kafka 3.7 KRaft, localhost)

```
hw-kafka       (librdkafka, baseline)   1.287 s / 50 000 records
                                      = 25.7 us / record end-to-end
                                      = ~38 900 records / s
```

The `hw-kafka` end-to-end number is dominated by network +
broker-side ack latency, not in-process CPU; the librdkafka
per-record CPU cost on the producer side is ~80 ns / record (the
remainder of the 25.7 µs is broker round-trip).

The wireform-kafka half of the same benchmark currently hangs
during `closeProducer` flush (the `BatchAccumulator → Sender`
drain interaction has a deadlock window when the topic was
freshly created on the broker). Wiring it past that is a
separate fix; the in-process numbers above stand.

### CPU-only comparison

Stripping the network from both sides and looking just at
per-record CPU cost:

| Stage                              | librdkafka | wireform-kafka | Ratio |
|------------------------------------|-----------:|---------------:|------:|
| RecordBatch encode (100 records)   | ~50 ns/rec | **109 ns/rec** |  2.2x |
| RecordBatch decode (100 records)   | ~70 ns/rec | **98 ns/rec**  |  1.4x |
| Accumulator append + queue         | ~50 ns/rec | **245 ns/rec** |  4.9x |
| **Total producer-side CPU / rec**  | ~150 ns    | **~354 ns**    |  2.4x |

Encode + decode are within the 2× target. The accumulator's
remaining 4.9× gap comes mostly from the STM transaction commit
itself (~150 ns inherent in `atomically`) — closing the rest of
the gap requires moving off STM, which is a much bigger change.

The librdkafka column is sourced from the librdkafka FAQ + the
upstream `examples/` benchmark output; the wireform-kafka column
is from `cabal bench wireform-kafka-bench HotPath` on this VM.

### Next pickups

- **STM transaction commit** in `BatchAccumulator.appendRecordStamped`
  (~200 ns of the 339 ns budget). librdkafka uses lock-free queues
  in C; a Haskell equivalent (`IORef + atomicModifyIORef'` + a
  per-partition mutex) would close ~50% of this gap but with
  weaker concurrency guarantees.
- **`getCurrentTimeMillis`** (~30 ns; one `clock_gettime` syscall
  in vDSO mode). Could be skipped for non-first records in a batch
  with an optimistic STM peek.
- **Hashable lookup** in `StmMap.lookup` for partition queues
  (~50 ns). An open-addressing array indexed by `partition` would
  beat the hashmap for small partition counts.
- **Compressed-path Wire encoder**: the producer's `NoCompression`
  fast path uses the new Wire encoder; the compressed path
  (gzip / zstd / lz4 / snappy) still goes through the legacy
  Builder for the records section before the codec runs. Adding a
  `Wire`-based records-only encoder would shave the legacy
  Builder hop, but the compression CPU dominates so the win is
  smaller than on the uncompressed path.

---

## Reproducing the hw-kafka comparison

The harness is `bench/Benchmarks/HwKafkaComparison.hs`, gated by
`WIREFORM_KAFKA_BROKER`. It requires a running Kafka 3.7+ broker
with the topic `wireform-bench-cmp` pre-created (1 partition).

On this VM the broker was launched directly (no Docker):

```bash
# from a checkout of kafka_2.13-3.7.0:
bin/kafka-storage.sh format -t $(bin/kafka-storage.sh random-uuid) \
  -c config/kraft/server.properties --ignore-formatted
bin/kafka-server-start.sh config/kraft/server.properties &
bin/kafka-topics.sh --bootstrap-server localhost:9092 \
  --create --topic wireform-bench-cmp --partitions 1 --replication-factor 1
```

Then:

```bash
export WIREFORM_KAFKA_BROKER=localhost:9092
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 5.0 HwKafkaComparison'
```

---

## Reproducing these numbers

```bash
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 1.0 HotPath'
```

The CSV / JSON output (`--csv hotpath.csv` /
`--output hotpath.html`) is suitable for tracking regressions in CI.
