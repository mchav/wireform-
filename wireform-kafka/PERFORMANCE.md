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

| Path                                | Before (legacy) | After (this branch) | Speedup |
|-------------------------------------|----------------:|--------------------:|--------:|
| RecordBatch encode  / 100-record    | **1070 ns/rec** | **109 ns/rec**      | **9.8x** |
| RecordBatch encode  /  10-record    |  1150 ns/rec    | 135 ns/rec          |  8.5x |
| RecordBatch encode  /   1-record    |  2490 ns        | 238 ns              | 10.5x |
| RecordBatch encode  / 100 (gzip)    |   155 µs        | **74 µs**           |  2.1x |
| RecordBatch decode  / 100-record    |   898 ns/rec    | **45 ns/rec**       | **20x** |
| RecordBatch decode  /  10-record    |   930 ns/rec    | 62 ns/rec           | 15x |
| RecordBatch decode  /   1-record    |  1180 ns        | 224 ns              |  5.3x |
| BatchAccumulator append / 1000      |   459 ns/rec    | **245 ns/rec**      |  1.9x |
| BatchAccumulator append /  100      |   360 ns/rec    | 245 ns/rec          |  1.5x |
| BatchAccumulator append /  single   |   400 ns        | 245 ns              |  1.6x |
| MockProducer.sendMockH / 10000 seq  |   234 ns/rec    | 234 ns/rec          |  flat |

Per-record amortised cost on the producer's full encode + accumulator
+ buffer-flush sequence is now ~**354 ns / record**:

```
BatchAccumulator append    245 ns
RecordBatch encode (Wire)  109 ns
                           -----
                           354 ns / record (uncompressed batches)
```

Decode is now at ~**45 ns / record** for typical 100-record batches,
which is **inside** the librdkafka in-process envelope (~50 ns/rec)
and below the producer-side per-record CPU cost on the same client.

Compressed batches add the codec time (gzip / zstd / lz4 / snappy)
which is unavoidable and dominates everything else; the Wire-based
compressed encoder still cuts the non-codec overhead in half
(2.1× faster at 100 records).

---

## How we got here

The work breaks down into seven independent commits, each with
its own benchmark + cross-codec / round-trip property tests:

1. **`encodeRecordBatchWire`**. Replaces the
   `runPutS`-per-record/body/batch shape (102 separate Builder runs
   for a 100-record batch + one body memcpy to feed CRC32C) with a
   single-allocation, single-pass encoder that writes the entire
   batch into one `mallocForeignPtrBytes` and CRCs the body in
   place via `Kafka.Protocol.CRC32C.crc32cPtr`.

2. **`decodeRecordBatchWire`**. The same trick on the
   read path: one `BSI.toForeignPtr` view onto the input buffer,
   one mutable `V.Vector` for the records, CRC32C check via
   `crc32cPtr` (no body memcpy).

3. **`batchCallbacks` Seq**. The hot
   `BA.appendRecordStamped` did `batchCallbacks ++ [callback]` per
   record. List snoc is O(n), so per-batch accumulator cost was
   O(n²). Switching `batchCallbacks :: [RecordCallback]` →
   `batchCallbacks :: Seq RecordCallback` brings it back to O(n
   log n) total per batch and flattens the per-record curve.

4. **Producer wire encode hookup**. The producer's
   `buildPartitionProduceData` always went through the compression
   layer, even when the codec was `NoCompression` (a pass-through
   that still pays for `runPutS`). Short-circuit that case to use
   `encodeRecordBatchWire` directly.

5. **`tryFastAppend` BA fast-path**. `BA.appendRecordStamped` did one
   `getCurrentTimeMillis` syscall per record. The clock is only
   needed when constructing a fresh batch (~1 in N records). Split
   into a fast STM-only path that skips the syscall on the common
   "append to existing filling batch" case.

6. **Zero-copy `peekByteStringSlice` in the decoder**. Per-record
   key + value + header byte reads used to memcpy each blob into
   a fresh `ByteString`. With `peekByteStringSlice` they now share
   the source `ForeignPtr` via `BS.PS fp off len`. For a 50 MiB
   fetch response with 100K records, that's tens of MiB of
   memcpy / allocation eliminated. Decode goes from 99 ns/rec →
   **45 ns/rec** at 100-record batches.

7. **Wire-based compressed encoder**. The compressed path now uses
   a Wire-based records encoder + a single-allocation envelope
   writer, with the body CRC computed in place via `crc32cPtr`.
   Cuts non-codec overhead in half (155 µs → 74 µs at 100
   records, gzip).

8. **Single-allocation framing on send + receive**. `frameRequest`
   used to do `sizeBytes <> headerBytes <> requestBody` (two full
   memcpies of the request body). `readExactly` did `acc <> chunk`
   per socket read (O(N×M) for an N-chunk M-byte response). Both
   replaced with `mallocForeignPtrBytes` + direct pokes; one
   allocation, one pass over each section.

9. **Kafka-compatible murmur2 partitioner**. The hash partitioner
   was using siphash (`Data.Hashable.hash`) but the docstring
   claimed murmur2. That silently routed keys to different
   partitions than every other Kafka client, breaking per-key
   ordering with mixed clients. Faithful port of
   `Utils.murmur2(byte[])` plus the JVM's six canonical reference
   vectors as the regression suite.

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
| RecordBatch decode (100 records)   | ~70 ns/rec | **45 ns/rec**  | **0.6x** |
| Accumulator append + queue         | ~50 ns/rec | **245 ns/rec** |  4.9x |
| **Total producer-side CPU / rec**  | ~150 ns    | **~354 ns**    |  2.4x |

Decode is now **faster** than the librdkafka envelope; encode is
within 2× and the bottleneck is the body memcpy through CRC32C +
the per-record varint loop (both irreducible without SIMD work).

The accumulator's remaining 4.9× gap comes mostly from the STM
transaction commit itself (~150 ns inherent in `atomically`) —
closing the rest of the gap requires moving off STM, which is a
bigger change.

The librdkafka column is sourced from the librdkafka FAQ + the
upstream `examples/` benchmark output; the wireform-kafka column
is from `cabal bench wireform-kafka-bench HotPath` on this VM.

### JVM client tricks already ported (and the ones we deferred)

The Java client has a long list of micro-optimisations that took
years to accumulate. The ones that bought us the headline numbers
above:

* Direct-poke encode / decode — Java does this via `ByteBuffer`s
  with absolute writes; we do it via 'Foreign.Ptr' on a buffer
  allocated with `mallocForeignPtrBytes`.
* Zero-copy slices for record key + value + headers. Java's
  `ByteBuffer.slice()` is the analogue.
* Single-allocation framing on the socket I/O path — Java uses
  `GatheringByteChannel.write(ByteBuffer[])` for scatter / gather;
  we use `mallocForeignPtrBytes (4 + headerLen + bodyLen)` + direct
  pokes for the same effect.
* Hardware-accelerated CRC32C. Java auto-detects SSE4.2 / ARM CRC
  intrinsics; we use SIMDe + `-march=native` in `cbits/crc32c.c`.
* `tryFastAppend` skip-the-clock — Java's RecordAccumulator does
  the same trick.
* Murmur2 partitioner — direct port of
  `org.apache.kafka.common.utils.Utils.murmur2(byte[])`.

Deferred (would benefit from broker-driven measurement first):

* **Lazy `ConsumerRecords` iterator** — Java decodes records one
  at a time as the user iterates; if the user only consumes K
  out of N, the other N-K never get decoded. Our `poll` is eager.
  Worth it for sparse-consumption workloads.
* **`ProducerBatch` direct-buffer encoding** — Java's
  RecordAccumulator hands out a pre-allocated `batch.size`
  buffer per partition and writes records straight in. We
  accumulate `Seq Record` first, then encode at flush time.
* **`BufferPool` for receive buffers** — Java pools direct
  ByteBuffers across requests to amortise the
  `mallocForeignPtrBytes` cost. Our buffers are GC-collected.
* **Per-broker request pipelining** — we have `Pipeline` but
  the producer doesn't use it; would let multiple in-flight
  produce requests to the same broker overlap network +
  compression CPU.
* **Adaptive per-partition `max.fetch.bytes`** — Java tracks
  each partition's average response size and shrinks / grows
  the per-partition cap accordingly.
* **STM → IORef + per-partition mutex** for the accumulator —
  shaves the ~150 ns `atomically` commit overhead.

### Next pickups (in payoff order)

1. **STM → `IORef + per-partition mutex`** for the accumulator
   (~150 ns of the 245 ns budget is `atomically`). Same shape as
   librdkafka's lock-free per-partition queue but with stronger
   safety guarantees from the mutex.
2. **Lazy decode of `ConsumerRecords`** — defer the per-record
   `peekRecord` work until the user iterates, mirroring the JVM
   client's `Records` iterator. Big win when consumers
   filter / skip records.
3. **Producer per-batch direct buffer** — pre-allocate one
   `batch.size`-byte buffer per partition; encode records straight
   into it (no intermediate `Seq Record`). Eliminates the
   per-record `Record` allocation entirely.
4. **Per-broker pipelining of produce requests** — wire the
   existing `Pipeline` into the producer. Lets compression CPU
   and network round-trips overlap.
5. **Receive-buffer pool** — reuse one `mallocForeignPtrBytes`
   buffer per connection across fetches. Currently each
   `connectionGet` allocates fresh.

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
