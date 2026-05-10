# STM → Lighter-Weight Coordination

## Status

**Draft / proposal.** No code changes have landed against this spec yet; it
exists so we can decide which tiers to actually do and in what order. The
analysis below is grounded in the current code (commit ahead of `3b05eb5`
on `cursor/wire-codegen-and-slice-vector-7d97`) and the
`Benchmarks.HotPath` numbers from the most recent run.

## Motivation

`Network.STM` is the default for in-process coordination across the
client, but a lot of the call sites pay STM's transactional overhead for
patterns that need none of its semantics (no `retry`, no `orElse`, no
multi-`TVar` consistency requirement). The producer hot path
(`BatchAccumulator.appendRecordStamped`) is the most visible offender:
the per-append `atomically` block costs ~278 ns / record at the bench, of
which the bulk is the STM commit (CAS + log walk), not the actual work.
Replacing the SPSC-shaped TVars there with `IORef + atomicModifyIORef'`
should land us closer to ~120–150 ns / append while preserving every
externally observable behaviour.

Replacing every `TVar` is the wrong move. Some call sites — most
prominently `Pipeline.allocateAndQueue` — genuinely want STM's
compositional `check` semantics for backpressure. The spec partitions
the codebase into four buckets and only proposes touching the first
three.

## Inventory of STM use

Counted across `wireform-kafka/src` + `wireform-kafka/streams/src`. Top
files by STM density:

| File | TVar+STM ops |
|---|---:|
| `Client/Consumer.hs` | 90 |
| `Client/Mock/Cluster.hs` | 82 |
| `Client/Producer.hs` | 54 |
| `Client/Pipeline.hs` | 53 |
| `Client/Internal/BatchAccumulator.hs` | 46 |
| `Client/Mock/Fault.hs` | 33 |
| `Client/Internal/TransactionCoordinator.hs` | 31 |
| `Client/Internal/Heartbeat.hs` | 30 |
| `Client/Transaction.hs` | 27 |
| `Client/AdminClient.hs` | 23 |

Distribution of STM primitives in the producer hot-path files:

```
86 TVar
 7 TMVar
 6 TQueue
 4 StmMap.listT
 2 StmMap.lookup
 2 StmMap.Map
 1 StmMap.newIO
 1 StmMap.insert
```

## Categorisation

### Category A — counters / standalone scalars

`pipelineNextId :: TVar RequestId`,
`consumerCorrelationId :: TVar Int32`,
`senderCorrelationId :: TVar Int32`,
`hbCorrelationId :: TVar Int32`,
`txnCorrelationId :: TVar Int32`,
`accumulatorClosed :: TVar Bool`,
`senderRunning :: TVar Bool`,
`pipelineClosed :: TVar Bool`,
`hbRunning :: TVar Bool`,
`consumerSubscription :: TVar (Maybe [Text])`.

Touched on every request as
`atomically $ do { n <- readTVar v; writeTVar v (n+1); pure n }`. Compose
with nothing. Pure overhead vs. `IORef` + `atomicModifyIORef'`. Per-call
saving on the order of 50–80 ns each at modern GHC.

### Category B — single-step swap on an SPSC structure

`PartitionQueue.queueCurrentBatch :: TVar (Maybe ProducerBatch)` and
`PartitionQueue.queueBatches :: TVar (Seq ProducerBatch)`. Per partition
this is a single-producer (the user thread that calls `sendMessage`)
single-consumer (the sender thread that calls `drainReadyBatches`)
arrangement. Today every `appendRecordStamped` runs an `atomically` block
that touches 3–4 TVars; bench measures 250–350 ns / append, dominated by
the STM commit. None of the steps need `retry` / `orElse`.

`consumerAssignment :: StmMap.Map TopicPartition Int64` and
`consumerPaused :: StmMap.Map TopicPartition Int64` follow the same
SPSC pattern at the consumer side: read on every `poll`, written only
at rebalance + post-fetch offset advance.

### Category C — single-writer multi-reader hand-off

`hbMemberId :: TVar Text`, `hbGenerationId :: TVar Int32`,
`hbCoordinatorAddr :: TVar (Maybe BrokerAddress)`,
`hbDedicatedConn :: TVar (Maybe (BrokerAddress, Connection))`,
`txnProducerId :: TVar (Maybe ProducerId)`,
`txnProducerEpoch :: TVar (Maybe ProducerEpoch)`,
`txnState :: TVar TransactionState`,
`txnCoordinator :: TVar (Maybe TransactionCoordinator)`,
`metadataVar :: TVar (Maybe ClusterMetadata)`,
`senderTransactionalId :: TVar (Maybe Text)`.

Subscribe / JoinGroup / metadata refresh / transaction init writes these
at most once per group rebalance / metadata refresh / transaction. The
heartbeat / poll / send loop just reads. `IORef` is sufficient; the GHC
IORef model already gives total ordering visible to
`atomicModifyIORef'`.

### Category D — actually wants STM

Keep these.

* `Pipeline.allocateAndQueue` — six-step transaction with two `check`
  blocks for backpressure that compose into proper "block until queue +
  in-flight have headroom" semantics. Replacing this means reinventing
  condition-variable waits, and the saving is per-request
  (microseconds), not per-record.
* `pipelinePending :: TVar (IntMap PendingRequest)` — atomically
  inserted in the same transaction as the send-queue write.
* `pipelineSendQueue :: TQueue` — typical Kafka producer is
  single-threaded into the queue, but TQueue's STM compositionality is
  what `allocateAndQueue` consumes via `queueSize`. Could be swapped for
  `unagi-chan` later if the queue becomes the wall (see "Future work").
* `pendingResponse :: TMVar` — one-shot promise slot. STM gives nothing
  here over `MVar`, but the diff is negligible and replacing it is more
  churn than it's worth.
* `closeBatchAccumulator` and `flushPendingBatches` — flip
  `accumulatorClosed` and walk every partition to mark batches ready in
  one transaction. STM keeps the whole drain consistent.

## Proposed work, in order

### Tier 1 — counters (low risk, mechanical)

Convert every Category A counter:

| Field | Current type | Proposed type |
|---|---|---|
| `pipelineNextId` | `TVar RequestId` | `IORef RequestId` |
| `consumerCorrelationId` | `TVar Int32` | `IORef Int32` |
| `senderCorrelationId` | `TVar Int32` | `IORef Int32` |
| `hbCorrelationId` | `TVar Int32` | `IORef Int32` |
| `txnCorrelationId` | `TVar Int32` | `IORef Int32` |
| `consumerSubscription` | `TVar (Maybe [Text])` | `IORef (Maybe [Text])` |

Pattern at the call site:

```haskell
nextCorrId :: IORef Int32 -> IO Int32
nextCorrId ref = atomicModifyIORef' ref (\n -> (n + 1, n))
```

The Pipeline collision-loop `nextFreeCorrelationId` becomes a CAS-loop
that reads the pending IntMap snapshot atomically (Tier 2 below) and
falls through to `atomicModifyIORef'` on the counter.

Risk: zero behavioural change. The `accumulatorClosed`,
`senderRunning`, `hbRunning`, `pipelineClosed` flags need slightly more
care because some compositions today rely on them being read inside an
`atomically` block alongside a structural write. Two options:

* Leave those four `TVar Bool` alone. They're read once per call, not
  the bottleneck.
* Or keep them as `TVar Bool` for the compositions that need them and
  add a parallel `IORef Bool` mirror for the per-append fast path. The
  mirror is updated last; producers may briefly observe `closed=False`
  after the canonical close, but `closeBatchAccumulator` already drains
  on close so a stray late append is safe.

Recommendation: leave the booleans alone in Tier 1, revisit if
profiling indicates it. Counters are pure win.

Estimated cost: ~half a day. Estimated saving: 50–80 ns per request on
each affected hot path (per-record correlation-ID alloc on the producer
sender, per-poll on the consumer, per-tick on the heartbeat).

### Tier 2 — `BatchAccumulator.PartitionQueue` (high return, medium risk)

This is the headline change.

Current shape:

```haskell
data PartitionQueue = PartitionQueue
  { queueBatches      :: !(TVar (Seq ProducerBatch))
  , queueCurrentBatch :: !(TVar (Maybe ProducerBatch))
  }
```

Proposed:

```haskell
data PartitionQueue = PartitionQueue
  { queueBatches      :: !(IORef (Seq ProducerBatch))
  , queueCurrentBatch :: !(IORef (Maybe ProducerBatch))
  }
```

Per-partition the writer is single (the user thread that produces into
the partition); the sender thread reads via `drainReadyBatches`. The
SPSC pattern means `atomicModifyIORef'` for the current-batch swap and
for the ready-queue append is sufficient.

The `appendRecordStamped` rewrite, sketched:

```haskell
appendRecordStamped acc tp record callback stamp = do
  closed <- readIORef (accumulatorClosed acc)
  if closed
    then pure False
    else do
      mq <- HashMap.lookup tp <$> readIORef (accumulatorPartitions acc)
      case mq of
        Just q -> do
          fast <- atomicModifyIORef' (queueCurrentBatch q) $ \mb ->
            case mb of
              Just b  -> let (b', readyM) = appendInto cfg record callback b
                         in (Just b', Just readyM)
              Nothing -> (Nothing, Nothing)
          case fast of
            Just (Just readyBatch) -> do
              atomicModifyIORef' (queueBatches q) $ \s -> (s |> readyBatch, ())
              pure True
            Just Nothing -> pure True
            Nothing -> slowAppend ... -- new partition / new batch
        Nothing -> slowAppend ...
```

Where `appendInto cfg record callback b` returns the updated batch and
optionally a `Ready` batch to push into the per-partition ready queue.

`drainReadyBatches` becomes a per-partition `atomicModifyIORef'` swap of
the ready queue with `Seq.empty`, then a snapshot of the current batch
to honour the `linger.ms` deadline.

Trade-offs / risks:

* `accumulatorPartitions` switches from `StmMap` to
  `IORef (HashMap TopicPartition PartitionQueue)`. Inserting a new
  partition becomes an `atomicModifyIORef'` CAS-loop (HashMap insert is
  cheap). Same correctness story as `Map.insertWith` racing two
  inserters: only one wins, the other walks away.
* The closed-flag race: a producer may `appendRecord` after
  `closeBatchAccumulator` has flipped `accumulatorClosed` but before
  the producer has seen it. This is also possible today under STM
  whenever the producer's transaction has already started reading the
  closed TVar before close commits — the difference is the window is
  larger without STM. `closeBatchAccumulator` drains every partition
  on close, so a stray late append goes into a ready queue that nobody
  reads, which is the same outcome as today.
* The `flushPendingBatches` operation today uses one STM transaction
  to walk every partition. After the rewrite it becomes a fold over
  the partitions IORef snapshot, doing one `atomicModifyIORef'` per
  partition. Loses cross-partition atomicity, which is fine — flush is
  a checkpoint, not a barrier; mid-flush appends to other partitions
  were always allowed.

Estimated cost: 1 day, including a fresh `Benchmarks.HotPath`
appendRecordStamped run and an integration test pass.

Estimated saving: `appendRecordStamped` from ~278 ns to ~120–150 ns
per record (~50–55% reduction on the hottest STM path). At the
`HwKafkaComparison` workload (50 000 records / iteration) that's
~6–8 ms shaved off the producer iteration time, lifting the
producer-vs-`librdkafka` ratio from ~2.3× to roughly ~2.7×.

### Tier 3 — consumer poll path (medium return, medium risk)

Convert `consumerAssignment` and `consumerPaused` from `StmMap` to
`IORef (HashMap TopicPartition Int64)`. The current snapshot pattern
in `poll`:

```haskell
assignment <- atomically $ do
  asgn       <- ListT.toList $ StmMap.listT consumerAssignment
  pausedList <- ListT.toList $ StmMap.listT consumerPaused
  let pausedSet = Map.fromList pausedList
  pure [(tp, off) | (tp, off) <- asgn, not (Map.member tp pausedSet)]
```

becomes

```haskell
assignment <- do
  asgn   <- readIORef consumerAssignment
  paused <- readIORef consumerPaused
  pure (HashMap.toList (HashMap.difference asgn paused))
```

Reads are now branch-free; the rebalance / commit writes use
`atomicModifyIORef'`. Convert the `consumerHeartbeat` `TVar` fields
(Category C) at the same time since the consumer is the only writer.

Estimated cost: 1 day.

Estimated saving: ~200–400 ns per poll on the assignment snapshot
(currently dominated by the `ListT.toList` walk inside `atomically`).
Mostly invisible per-record but adds up at high poll rates.

### Tier 4 — leave as STM (no work)

Documented for completeness:

* `Pipeline.allocateAndQueue` and `pipelinePending` (composes
  backpressure check with insert + queue write).
* `Pipeline.pipelineSendQueue :: TQueue` (the `queueSize` peek inside
  `allocateAndQueue` consumes `TQueue`'s STM interface).
* `pendingResponse :: TMVar` slots.
* `closeBatchAccumulator` and `flushPendingBatches`.
* `Mock/Cluster.hs` and `Mock/Fault.hs` (test-only state machines that
  trade per-op CPU for clarity; not on any hot path).

## Cross-cutting concerns

### Memory model

`atomicModifyIORef'` provides sequential consistency for the single
ref it modifies, plus a release store visible to subsequent
`readIORef` on other cores. That's exactly the same guarantee the
single-step STM transactions provide for one TVar. For Category A and
the SPSC parts of Category B, the swap is one IORef so the model
matches.

For Category C single-writer-multi-reader fields, plain `readIORef` is
sufficient: the writer's `writeIORef` is a release store, the reader's
`readIORef` is an acquire load on x86 (and uses a fence on weaker
architectures).

### Test impact

`closeBatchAccumulator` semantics — close-flag flip plus per-partition
drain — needs an explicit unit test to lock in the (already-existing)
behaviour that any append after `accumulatorClosed = True` returns
`False`. There's a test for this in `test/Client/`, double-check it's
still correct under the IORef model.

The `Pipeline` integration tests already exercise the
`allocateAndQueue` backpressure path; those don't change since Tier 4
keeps STM.

The `Streams` runtime uses STM via `Streams/Runtime.hs` and
`Streams/Runtime/WorkerPool.hs`. Those are out of scope for this spec
— they're the worker pool's job-dispatch fabric, which is different
from the per-record producer accumulator.

### Benchmark before / after

For each tier, run before / after of:

* `Benchmarks.HotPath / Producer / BatchAccumulator.appendRecordStamped`
  (the most STM-sensitive microbench).
* `Benchmarks.HotPath / Producer / RecordBatch encode` (sanity — should
  not change).
* `Benchmarks.HwKafkaComparison / Producer 16K-batch sendAsync`
  (end-to-end; expects ~10% improvement after Tier 2).

## Future work, out of scope

* Replace `Pipeline.pipelineSendQueue :: TQueue` with `unagi-chan` for
  multi-producer single-consumer throughput. Worth ~3–4× on the
  send-queue throughput in a contended environment, but typical Kafka
  producers are single-threaded into the queue.
* Replace `pendingResponse :: TMVar` with `MVar`. Marginal saving;
  not worth the diff churn.
* Investigate `unboxed-ref` for the correlation-ID counters if profiling
  shows the boxed `IORef Int32` allocation matters at very high call
  rates.

## Decision required

Pick one of:

1. **Tier 1 only.** Mechanical, no behavioural risk, modest saving.
2. **Tier 1 + Tier 2.** Recommended path. Tier 2 is where the bulk of
   the win lives; Tier 1 is a safe warm-up that verifies the
   IORef-conversion mechanics work cleanly.
3. **Tier 1 + 2 + 3.** Full sweep. Adds the consumer-poll improvement
   on top.

Tier 4 is "do nothing" by design — those call sites need STM and we
keep them.
