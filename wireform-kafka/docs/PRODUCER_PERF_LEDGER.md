# Producer Hot-Path Performance Ledger

A running record of the producer-side optimisations attempted on the
`cursor/kafka-stm-replacement-7b7c` branch — what landed, what was
reverted with measurements, and what's still on the table for a
future pass. Read top-to-bottom for the chronology, or jump to
[Open levers](#open-levers) for the next things to try.

## Current state

Branch tip: `3b4ad6fa`
("wireform-kafka: per-broker pipe outbox + in-flight on
'unagi-chan' instead of 'TBQueue'").

Bench harness: `wireform-kafka-perf produce`, single producer
thread on `sendMessageDropFastest`, fresh broker, 100 B values,
batch size 16 384, `producerMaxInFlight = 32`,
`+RTS -A64m -N1`.

| | rec/s median | rec/s peak | MUT | GC | productivity |
|---|---|---|---|---|---|
| racy pre-fix baseline | 2.92 M | 3.12 M | — | — | — |
| `3b4ad6fa` (current) | **3.25 M** | **3.92 M** | 3.50 s | 1.22 s | 80.6 % |

That's +11 % median / +26 % peak vs the racy baseline, with a
seal-vs-append regression test (`prop_appendAndSealRaceIsLossless`)
proving correctness of the CAS interlock.

## Landed interventions (in commit order)

Each entry has the commit on the branch, what it changed, and the
measured impact. All measurements are 4–8 consecutive 20 M-record
runs on the same broker after warmup.

### 1. Tier-1/2/3 STM → IORef sweep

Already in `STM_REPLACEMENT_SPEC.md`. Per-record append went from
~262 ns / record (STM) to ~120 ns / record (IORef + mutable hot
state).

### 2. `BatchAck` strict struct + tagged `RecordCallback` (commit `5194e409`)

Replaces the per-record sender ack 4-tuple
`(Text, Int32, Int64, Int64)` with a strict UNPACK'd record. Tags
`RecordCallback` as a sum (`NoRecordCallback | RecordCallback _`)
so the sender's per-record ack-dispatch can skip the strict
`BatchAck` construction entirely when the caller didn't pass a
real callback. Every `*Drop*` send variant now uses
`NoRecordCallback`.

Measured: +14 % median, MUT flat, GC −7 %, alloc rate −9 %.

The two changes had to land together — strict `BatchAck` alone
regressed throughput because it forced field evaluation that the
prior lazy tuple let the no-op callback skip. Tagging the sum
restored the no-op fast path while keeping the strict-construction
win for callers that consume metadata.

### 3. Seal-vs-append CAS interlock (commit `71be3adc`)

Pre-fix: a producer's append CAS could land on a `BatchAccumulating`
that the sender had already swapped out of `queueCurrentBatch` and
read for the snapshot, silently dropping the record. The
`appendRecordStampedUnsafe` shortcut was strictly worse because
its `readIORef + writeIORef` pair could even clobber the sealer's
state writes.

Fix: `BatchHotState.bhSizeRaw` carries the accumulated size in
its non-negative range and a sentinel (`minBound`) in its
negative range to mean "this batch is sealed". Both
`casAppend` and `snapshotBatch` contend on the **same** `IORef`,
so the protocol linearises: any append CAS that lands before the
seal CAS is in the snapshot; any append CAS after sees the
sentinel and routes through `slowAppendIO` to a fresh batch.
`appendRecordStampedUnsafe` becomes an alias for
`appendRecordStamped` (the readIORef+writeIORef shortcut was
unsound).

Property test in `Client.BatchAccumulatorSpec`
(`prop_appendAndSealRaceIsLossless`) produces N records on one
thread while a second thread continuously calls
`drainReadyBatches` to force interleaved seals; the bag of
`recordOffsetDelta` keys observed must equal the bag asked for.
The test fails on the pre-fix code (3999/4000) and passes here.

Measured: −26 % throughput vs the racy code. Fully recovered by
later commits — the regression was *correct*; the previous
numbers were silently lossy.

### 4. `Seq → V.Vector` for `batchRecords` / `batchCallbacks` (commit `cccf381a`)

The wire encoder already wanted `V.Vector RB.Record`, so
`buildRecordBatch` was paying a per-batch
`Seq → List → Vector` shape conversion plus log-N `Seq.index`
lookups for every record's offset-delta stamp. The sender's ack
dispatch was paying for an intermediate
`Seq.mapWithIndex (,) callbacks` to attach the slot index.

`snapshotBatch` builds the vectors via `V.fromListN` on the
reversed cons-list (one allocation of exactly `n` slots).
`buildRecordBatch` uses `V.imap` directly. Sender ack uses
`V.iforM_`. `BatchSplitting.splitBatch` uses `V.splitAt`.

Measured: GC −40 %, throughput within noise. The big win was
clearing `Seq.Internal.Deep` / `.Three` from the heap profile,
making downstream optimisations measurable.

### 5. `Data.Atomics.casIORef` instead of `atomicModifyIORef'` (commit `38d89512`)

Both `casAppend` and `snapshotBatch` are hand-rolled
`readForCAS / casIORef` ticket loops instead of
`atomicModifyIORef'`. Same atomicity guarantee, but `casIORef`
doesn't go through the `(state, result)` tuple closure that
`atomicModifyIORef'` allocates per call. At 3 M records/sec
that closure was ~24 B × 3 M = ~72 MB/s of pure-overhead
allocation.

Measured: GC 4.58 s → 1.02 s (−78 %), productivity 50 % → 83 %,
throughput 2.15 M → 2.78 M (+30 %). Recovered most of the
race-fix cost.

### 6. `vecFromRev` (commit `a2feaf39`)

`snapshotBatch` builds the records / callbacks `V.Vector` by
allocating an `MV.MVector` up-front at the known length and
walking the reversed list with `MV.unsafeWrite` at descending
indices, then `V.unsafeFreeze`. Skips the throwaway reversed
cons spine `V.fromListN n (reverse xs)` would have allocated
(~24 B / record).

Measured: throughput 2.78 M → 3.03 M (+9 %), MUT −13 %, GC
flat (already low). Now at the racy pre-fix throughput baseline
*with* the race-fix correctness.

### 7. Bench harness on `sendMessageDropFastest` (commit `db97dc86`)

The bench used `sendMessageDropUnsafe`; switched to
`sendMessageDropFastest` which uses `producerLastBatch` to cache
the `(topic, partition, queue, ba)` handle on the cache-hit path,
skipping the per-record `BA.TopicPartition` allocation and the
per-record HashMap lookup.

Measured: 3.03 M → 3.18 M median (+5 %), 3.39 M peak (+9 %).

### 8. Per-broker pipe outbox + in-flight on `unagi-chan` (commit `3b4ad6fa`)

The sender's two SPSC hand-offs per pipe (main loop → writer for
`OutboundProduce`, writer → reader for `PendingProduce`) were
both `TBQueue`-backed, paying STM-transaction commit overhead
per enqueue / dequeue. Replaced with `unagi-chan`:

* outbox: `Control.Concurrent.Chan.Unagi.Bounded` (bounded at
  `senderMaxInFlight` for librdkafka-style backpressure).
* in-flight: `Control.Concurrent.Chan.Unagi` (unbounded — depth
  already capped by the outbox bound).

`bpInFlightCount` stays a `TVar` so `flushProducer` can `retry`
on it; bumped before `UB.writeChan` on enqueue and decremented
inside the reader's STM block on ack. The flush poll stays
conservative because the count is only ever transiently *high*
relative to the chan, never low.

Measured: MUT 4.32 s → 3.50 s (−19 %), throughput median
3.18 M → 3.25 M (+5 %), peak 3.39 M → 3.92 M (+16 %). GC up
modestly (unagi's segmented-array internals allocate slightly
more than TBQueue's TVar updates) but productivity-weighted net
positive on every run.

## Reverted / dropped interventions

These were tried and either regressed throughput or were too
invasive for the predicted payoff. Documented so we don't
re-explore them without new information.

### Mutable `IOVector` + `MutablePrimArray` hot state

Replace the `bhRecordsRev`/`bhCallbacksRev` cons-list pair with
a pre-allocated `MV.IOVector RB.Record` + `MutablePrimArray Int`
counter packed (count, sizeBytes, capacity).

Result, 20 M records: throughput 3.02 M → 2.87 M (−5 %),
allocation −5 %. The `MV.unsafeWrite` write-barrier on a boxed
mutable vector (~10 ns / call) plus the larger Gen-1 working
set from the pre-allocated arrays getting promoted out of the
nursery cost more than the cons-cell allocation it was meant to
replace. Reverted.

### Strict UNPACK'd `BatchAck` *without* the `RecordCallback` sum tag

Tried as a standalone change before commit `5194e409`. Regressed
throughput ~10 % because the strict UNPACK forces field
evaluation (partition, offset, timestamp) that the prior lazy
4-tuple let the no-op callback discard. Only viable when paired
with the sum-tagged callback so the sender skips construction
on the no-op branch. See commit `5194e409` for the working
combined form.

### `-funbox-strict-fields` GHC flag

Auto-UNPACK every strict product field (the default
`-funbox-small-strict-fields` only inlines fields ≤ 1 word, so
`TopicPartition !Text !Int32` stays as a separate allocation per
parent). Tried adding it to the library defaults.

Result: throughput −12 %, GC +18 %. Some fields are better left
boxed — re-boxing penalties at use sites outweighed the inlined
struct savings. Reverted.

### Slot-indexed wire encoder (`pokeRecordsIxed` + `encodeRecordBatchWireIxed`)

Skip the per-batch `V.imap` in `buildRecordBatch` (which copies
the whole records vector to stamp `recordOffsetDelta` from the
slot index) by adding a parallel encoder path that reads the
offset from the iteration index instead of the `Record` field.

Result: throughput 3.18 M → 2.96 M (−7 %), GC +20 % even with
`V.ifoldM'` fusion. The per-batch ~14 KiB `V.imap` allocation
the change eliminated was dying in the nursery anyway, and the
new encoder path's `pokeRecordAtOffset` (passing `offsetDelta`
as an explicit arg) didn't fuse as tightly as the original
`pokeRecord` reading directly from `Record{..}`. Reverted.

### `casAppend` fast-path unroll + `NOINLINE` slow-path retry

Inlined the success branch of the `casAppend` retry loop and
moved the recursive collision-retry path to a separate
`NOINLINE casAppendRetry` helper, on the theory that GHC's
`INLINE` on a self-recursive function doesn't deeply inline the
fast path.

Result: throughput −12 %, GC +57 %. The unroll bloated the
inlined fast-path body and broke some downstream optimisation
GHC was doing on the recursive shape. Reverted.

### `bhMaxTsDelta` incremental tracking

Track the running max `recordTimestampDelta` on the append CAS
so `buildRecordBatch` can drop its per-batch `V.foldl'` over
the records vector to compute `batchMaxTimestamp`. Mechanical
change, one new field in `BatchHotState`, exposed as
`batchMaxTimestamp` on `ProducerBatch`.

Result: throughput 3.25 M → 3.12 M median (−4 %). The per-record
`max (bhMaxTsDelta st) tsd` runs 3 M times/sec (~3 ms of MUT),
while the per-batch fold it eliminates costs ~256 boxed-`Int64`
compares × 12 K batches/sec ≈ 3 µs of MUT/sec. The fold is net
cheaper than tracking it incrementally. Reverted.

### Per-batch `MutableArray` / `V.Vector` pool keyed by partition

Pool the `MV.MVector` backing storage that `vecFromRev` allocates
for each sealed batch (records + callbacks vectors). Return them
to the pool from the sender's ack-dispatch path.

**Did not implement.** Re-doing the math: the heap profile's
`MUT_ARR_PTRS_FROZEN_CLEAN` at 78 MiB was *resident*, not
allocated per second. The actual V.Vector backing-array alloc
rate is ~48 MB/s (~2 KiB × 2 vectors × ~12 K batches/sec), about
1.5 % of the ~3 GB/s total. The plumbing required (pool keyed
by size, return-from-ack path via a `batchOnRelease :: IO ()`
callback in `ProducerBatch`, lifetime tracking across the
in-flight window) is substantial for the realistic ~1-2 %
payoff.

## FFI / C audit

Considered moving hot work to C via FFI. Conclusion: no
meaningful headroom because the hot work is already at C-speed
or the FFI boundary cost would dominate. Specifics:

| Area | Status | Why FFI doesn't help |
|---|---|---|
| Wire encoding (`pokeRecord`, `pokeBatch`) | Already raw `Ptr Word8` + GHC primops | Compiles to the same `mov`/`add`/`memcpy` a C compiler would emit |
| CRC32C | Already `CRC.crc32cPtr` FFI to a C impl that uses the hardware `CRC32` instruction | Already as fast as it gets |
| Compression (zlib / zstd / snappy / lz4) | C bindings via existing libraries | Already SIMD-tuned C in the codec inner loops |
| Socket I/O | `Network.Socket` → `send(2)` / `recv(2)` | Already FFI |
| TLS | `tls` via `crypton` | Already C, AES-NI used |
| Atomic CAS | `Data.Atomics.casIORef` → single `LOCK CMPXCHG` primop | An FFI call to a C function would have to do the same instruction with FFI overhead on top |
| `unagi-chan` segmented-array channel | All primops, no FFI | A C MPMC ring buffer on the same algorithm would be the same speed |
| Per-record append | ~50 ns budget per record | FFI prologue alone is ~30-100 ns — would burn the entire budget on the boundary crossing |
| Closure-style callbacks | Haskell trampoline needed regardless | The user's callback is Haskell; C-side dispatch adds boundary crossings for no reason |
| Bulk per-batch encoder in C | Plausible, but… | …same win as the byte-buffer accumulator in pure Haskell. The per-byte work is already at C speed; FFI just adds marshalling. |
| SIMD varint encoding (`vbyte` etc.) | Theoretically 2-4× | Kafka's record format interleaves varints with raw byte payloads; longest run of consecutive varints in a record is ~3 (timestamp delta, offset delta, key length). SIMD's batch-of-N-varints assumption doesn't apply. |
| C hashmap for `accumulatorPartitions` | Marginal | `producerLastBatch` already caches the (`PartitionQueue`, `BatchAccumulating`) handle on the hot path; the HashMap lookup runs only on the cache-miss path (~once per 256 records). |

## Open levers

The remaining gains require structural changes, not single-file
edits. Documented here so they're not lost.

### A. Byte-buffer accumulator (estimated +5-15 % throughput)

Replace `bhRecordsRev :: ![RB.Record]` with a per-batch
`ForeignPtr Word8` buffer that records are encoded into at append
time. `ProducerBatch.batchRecords :: V.Vector RB.Record` becomes
`batchEncodedBody :: ByteString` + `batchRecordCount :: Int`
+ `batchMaxTimestamp :: Int64`.

Wins:
* No per-record `RB.Record` allocation (~48 B + body refs).
* No per-record records cons cell (~24 B).
* Sender's `buildPartitionProduceData` skips the per-batch
  `V.imap` + `pokeBody`/`pokeRecords` walk; just wraps the
  pre-encoded body in the `RecordBatch` envelope (header + CRC).
* Compression path takes the byte slice as input directly (skip
  the `encodeRecordsWire` step).

Concurrency design (the load-bearing detail):
* `BatchHotState` carries `(reserveCursor, publishCursor, count,
  maxTsDelta, callbacksRev, sealed)` — disruptor-style two-cursor
  protocol.
* `casAppend`: CAS `bhReserved` from `oldLen` to `newLen`, encode
  into buffer at `oldLen` (no other writer will touch
  [`oldLen`, `newLen`)), then CAS `bhPublished` from `oldLen` to
  `newLen` — spinning while previous writers haven't published
  yet.
* For single writer per partition (every `*Drop*` send variant):
  `bhReserved == bhPublished` always, no spin, two CAS per
  record.
* `snapshotBatch` reads `bhPublished` (not `bhReserved`) — only
  records with completed buffer writes are in the snapshot.

Cascading edits:
* `BatchSplitting.splitBatch` needs `materialiseRecords ::
  ProducerBatch -> Either String (V.Vector RB.Record)` that
  decodes `batchEncodedBody` via `decodeRecordBatchWire`. Split
  is materialise → split → re-encode each half (the right half
  needs `recordOffsetDelta` adjusted because the encoded varints
  inside the bytes have absolute slot positions baked in).
* `pokeRecordAtOffset` (or equivalent) needs to be a published
  export of `RecordBatchWire` so the accumulator can call it.
* `ProducerSender` gains an `encodeRecordBatchEnvelope ::
  ProducerBatch -> ByteString -> ByteString` that wraps a
  pre-encoded body in the wire envelope (no records walk).
* Tests / benches that pattern on `batchRecords` need to use
  `materialiseRecords` (`ProducerTransactionWiringSpec`,
  `ProducerTimeoutSpec`, `ProducerRetrySpec`,
  `BatchSplittingSpec`, `BatchAccumulatorSpec`,
  `Benchmarks.HotPath`, `Benchmarks.StatsAndStamping`).

Failure modes to guard against:
* Multi-writer publish-cursor race (two appenders racing on
  `bhReserved` could land their byte writes before the publish
  CAS, but a sealer reading `bhPublished` won't see them — that
  loss is *correct* in the sense that the records weren't yet
  acknowledged-as-appended, but the appenders would also need
  to retry on a fresh batch). The disruptor protocol handles
  this; document the property as "every successful
  `appendRecordStamped` call is in *some* sealed batch's
  `batchEncodedBody`".
* Memory ordering on weaker arches (ARM/POWER): the buffer
  write happens-before the publish CAS, and the snapshot's
  `readForCAS` of `bhPublished` is acquire-ordered, so on x86
  TSO and ARM with the explicit barriers in
  `Data.Atomics.casIORef` we're safe — but the property test
  should run under multi-thread stress to catch any bug.
* Retry path: a re-encode of a split batch must produce
  identical wire bytes for the existing records and re-encode
  the new offsets for the right half. Round-trip property test
  required.

Why not done in a previous turn: the cascade of API changes
(ProducerBatch shape, BatchSplitting, sender, tests, benches)
plus the disruptor protocol's correctness proof are not safely
landable in a single iteration. Belongs on a dedicated branch
with its own design doc + multi-thread property tests.

### B. Sender outbox / round-splitter restructure

`ProducerSender.sendBatches` calls `groupBy + sortBy` on the
per-call batch list. For our typical single-topic-single-batch
round those are O(1) but still walk a list. Replacing with an
accumulator-driven structure (groupBy at append time, indexed by
broker handle) could shave per-round overhead.

Estimated: 1-2 % throughput. Low risk, single-file change to
`ProducerSender`. Worth doing if/when other levers are
exhausted.

### C. Compress-at-append for the byte-buffer accumulator

Once (A) lands, compression can also move to append time —
incremental compression into the per-batch buffer (zstd has
streaming APIs that fit this shape). Sender just wraps the
pre-compressed body in the envelope, no compress step at send
time.

Useful only when compression is enabled (the bench uses
`none`). For workloads that compress, this would shift the
codec CPU off the sender thread (where it currently runs in
`encodeRecordBatchWireCompressedWithLevel`) onto the producer
thread. Helpful when sender is the bottleneck, neutral or
slightly negative when producer is.

### D. Per-batch resource pool (estimated +1-2 % throughput, GC −5 %)

The `MV.MVector` storage that `vecFromRev` allocates per batch.
Originally over-estimated as a 30 % GC win; actual ~1.5 % alloc
rate. Plumbing requires `batchOnRelease :: IO ()` on
`ProducerBatch`, called by sender's
`processProduceResponse` after callback dispatch. Tractable but
small.

### E. `Pipeline.allocateAndQueue` / `Metadata.metadataVar` STM →
`IORef` (Tier 4)

Already in `STM_REPLACEMENT_SPEC.md` as out-of-scope for the
initial pass. Not on the per-record hot path, so unlikely to
move benchmarks. Worth doing for consistency / removing the
last STM dependency on infrequent paths.

## How to extend this ledger

When adding an entry:

1. **Run 4-8 consecutive 20 M-record bench iterations** on a
   freshly-formatted broker (kafka logs at `/tmp/kraft-combined-logs`
   should be empty). Use the same RTS flags
   (`+RTS -A64m -N1 -RTS`).
2. **Match consecutive runs** between baseline and intervention
   so broker page-cache state is comparable. The bench is
   broker-bound on the tail, so don't trust single-run deltas.
3. **Quote median + peak** rec/s, MUT, GC, productivity. Use the
   `+RTS -s -RTS` output for the GC numbers.
4. **Note the failure mode** for any reverted intervention so we
   don't re-explore it under the same conditions.
5. **Update [Open levers](#open-levers)** if the intervention
   reveals a new structural opportunity or invalidates an
   existing estimate.
