# wireform-kafka-streams Riffle — Design Spec

> **Riffle** (n.) — a stretch of fast-flowing water in a stream;
> here, the codename for the additive extension tier of
> `wireform-kafka-streams` that sits beyond Apache Kafka Streams
> parity. Riffle is not an Apache Kafka project; it's a
> wireform-flavour roadmap layered on top of the parity port,
> picked up at compile time per topology.

Status: **draft / proposal**. This document is the design contract
for the post-parity evolution of `wireform-kafka-streams`. It does
not change anything shipped today; it lays out the structural and
breadth additions that would lift the library from "faithful KS
port" to "KS-shape, Flink-class behaviour where it matters".

The single overriding constraint is **additivity**: every Riffle change
is opt-in, ships as a new module or a new constructor, and does not
break the operator-for-operator parity claim that the README leads
with. Existing topologies keep compiling unchanged. Existing runtime
backends keep working. Wherever a Riffle feature has a legacy
equivalent, the legacy path stays — selecting Riffle is a config
toggle, a different smart constructor, or a different builder shape,
never a forced migration.

Riffle has no version bump until every section below has landed,
documented, and tested end-to-end. Sections marked **Phase 1** are
the first wave; sections marked **Phase 2** depend on Phase 1
plumbing.

---

## 0. Where Riffle plugs in

The current code has two layers, and Riffle respects both:

| Layer | Module | Riffle changes integrate as… |
| ----- | ------ | ------------------------- |
| **Typed AST** | `Kafka.Streams.Topology.Free` (`FreeArrow Prim`) | new `Prim` constructors, new smart constructors, new fusion rules. |
| **Imperative graph** | `Kafka.Streams.Topology` (`Topology` / `AnyStoreBuilder` / `SourceSpec` / `ProcessorSpec`) | new `AnyStoreBuilder` shapes, new `SourceSpec`/`ProcessorSpec` fields (kept `Maybe` for optionality), new `topo*` indices. |
| **Runtime** | `Kafka.Streams.Runtime.*` | new modules under `Kafka.Streams.Runtime.{Snapshot, KeyGroup, AsyncIO, Watermark, Sink2PC}`. Existing `WorkerPool` / `EOS` / `StandbyTask` keep their current entry points. |

The single `compile :: Topology Void o -> IO (o, Topo.Topology)`
remains the bridge. New `Prim` constructors compile to either
existing `ProcessorSpec` shapes (for Riffle features that reuse the
single-task model — e.g. async I/O lives inside one task) or to new
spec shapes added alongside the existing ones (for features that
need a new runtime concept — e.g. snapshot-aware stores).

---

## 1. State durability decoupled from the changelog (Phase 1)

### Problem

`KeyValueStore` / `WindowStore` / `SessionStore` recovery today is a
linear scan of the changelog topic. A 1 TB store on a restarted
instance reads the entire changelog and rebuilds the local store
before the task serves traffic. `Kafka.Streams.Runtime.StandbyTask`
halves the recovery wall-clock cost at 2× storage and 2× write
amplification. There is no ceiling on recovery time.

### Fix

State becomes a first-class durable artifact. The changelog becomes
a write-ahead log between snapshots — not the source of truth.

Concrete changes:

1. **New `AnyStoreBuilder` shapes** alongside the existing four
   (`AsKeyValueBuilder` / `AsWindowBuilder` / `AsSessionBuilder` /
   `AsRawBuilder`):

   ```haskell
   data AnyStoreBuilder where
     AsKeyValueBuilder :: !(StoreBuilderKV k v)     -> AnyStoreBuilder
     AsWindowBuilder   :: !(StoreBuilderW   k v)    -> AnyStoreBuilder
     AsSessionBuilder  :: !(StoreBuilderS   k v)    -> AnyStoreBuilder
     AsRawBuilder      :: !StoreBuilder              -> AnyStoreBuilder
     -- new in Riffle:
     AsSnapshotKV      :: !(StoreBuilderSnapKV k v) -> AnyStoreBuilder
     AsTieredKV        :: !(StoreBuilderTieredKV k v) -> AnyStoreBuilder
     AsRemoteKV        :: !(StoreBuilderRemoteKV k v) -> AnyStoreBuilder
   ```

   - `StoreBuilderSnapKV` is a local store (RocksDB / Persistent /
     in-memory) that incrementally snapshots to an
     `ObjectStore` (S3 / GCS / MinIO / local FS).
   - `StoreBuilderTieredKV` is a hot-tier-on-disk + cold-tier-on-S3
     store. Reads check hot first, fall through to cold. Writes
     go hot; a background compactor migrates cold pages.
   - `StoreBuilderRemoteKV` keeps no local state at all — every
     get/put is a network call against FoundationDB / TiKV /
     DynamoDB. Node restart is a metadata operation.

   Every new shape projects to a `KeyValueStore k v` through
   `txnStore`-style overlays so processor code is unchanged. The
   user picks the backend at builder time and the rest of the
   topology is oblivious.

2. **New module `Kafka.Streams.Runtime.Snapshot`** holds the
   snapshot lifecycle:

   ```haskell
   data SnapshotPlan = SnapshotPlan
     { spInterval        :: !Duration         -- e.g. every 5 min
     , spMaxRecordsBetween :: !(Maybe Int64)  -- or every N records
     , spObjectStore     :: !ObjectStoreClient
     , spRetention       :: !Int              -- snapshots to keep
     }

   class SnapshottableStore s where
     storeSnapshot :: s -> ObjectStoreClient -> IO SnapshotId
     storeRestore  :: ObjectStoreClient -> SnapshotId -> IO s
     storeAdvancedTo :: s -> IO ChangelogOffset
       -- last changelog offset baked into the snapshot;
       -- recovery replays from this offset, not from 0.
   ```

   Snapshots happen on a separate worker thread (driven by
   `nowMillis` + `spInterval`), not on the stream thread. The EOS
   coordinator (existing `EOSCoordinator.storeCommit`) is the
   hook: snapshot writes are committed in lock-step with the
   producer transaction, then the snapshot manifest is published
   to the object store.

3. **Recovery contract.** Today recovery is
   `replay(0, end-of-changelog)`. With snapshots:

   - Boot: read the latest snapshot manifest. Pull the
     snapshot files into the local store directory.
   - Replay only `[snapshot.advancedTo, end-of-changelog)`.
   - Recovery time becomes `O(time-since-last-snapshot)`, not
     `O(state-size)`.

4. **Standby tasks under Riffle backends.** `StandbyTask` and
   `StandbyManager` are unchanged for the legacy local-store path.
   For an `AsRemoteKV` store the runtime registers no standby — the
   "standby" is the remote KV cluster itself. For `AsSnapshotKV` /
   `AsTieredKV`, standbys become *pointers to remote state*:
   `StandbyTask` records the snapshot ID and the changelog offset
   it's caught up to, but holds no local replica. Promotion =
   fetch the latest snapshot + replay the tail.

### Optionality

- Default backend stays the same (local in-memory or RocksDB-via-
  `+rocksdb`). The Riffle backends are picked explicitly via new
  smart constructors in `Kafka.Streams.State.KeyValue.Snapshot` /
  `Kafka.Streams.State.KeyValue.Tiered` /
  `Kafka.Streams.State.KeyValue.Remote`.
- `LoggingConfig` is unchanged. A user that wants the old
  changelog-only behaviour gets it by not selecting a snapshot-
  capable builder.
- `EOSCoordinator.storeCommit` already exists as the integration
  point; Riffle wires the snapshot publisher into it via
  `withTransactionalStores` (existing function) so the noop path
  is unaffected.

---

## 2. Parallelism decoupled from partition count (Phase 1 → Phase 2)

### Problem

Today `WorkerPool` dispatches by `hash (topic, partition) mod N`
(`submitRecordHashed`) or by explicit `(topic, partition) -> Int`
routing (`submitRecord`). Either way the unit of parallelism is the
partition. Want more workers than partitions? You can't get more
work in parallel. Want fewer? Some workers idle. Rescaling without
repartitioning Kafka topics is impossible.

### Fix

Introduce a **key-group model** — the runtime sharding unit becomes
a key-group rather than a (topic, partition).

Concrete changes:

1. **New module `Kafka.Streams.Runtime.KeyGroup`** defines the
   model:

   ```haskell
   newtype KeyGroupId = KeyGroupId { unKeyGroupId :: Int }
     deriving stock (Eq, Ord, Show)

   data KeyGroupConfig = KeyGroupConfig
     { kgTotal   :: !Int    -- e.g. 128, must be >= max partitions
     , kgHash    :: !(ByteString -> Int)  -- defaults to xxHash
     }

   -- Pure mapping: which key-group does a byte-key belong to?
   keyGroupOf :: KeyGroupConfig -> ByteString -> KeyGroupId

   -- Per-instance assignment: which key-groups does this
   -- runtime own right now?
   data KeyGroupAssignment = KeyGroupAssignment
     { kgaOwned   :: !(Set KeyGroupId)
     , kgaWarming :: !(Map KeyGroupId WarmupProgress)
     }
   ```

2. **New worker pool entry point** alongside the existing two:

   ```haskell
   -- Existing:
   newWorkerPool        :: TopologyValid -> Text
                        -> [HashSet (TopicName, Int32)] -> IO WorkerPool
   newWorkerPoolHashed  :: TopologyValid -> Text -> Int -> IO WorkerPool

   -- New:
   newWorkerPoolKeyGrouped
     :: TopologyValid -> Text
     -> KeyGroupConfig -> KeyGroupAssignment
     -> IO WorkerPool
   ```

   The key-grouped pool routes records by hashing the record key
   to a key-group, then mapping the key-group to a worker via the
   current `KeyGroupAssignment`. The existing two entry points are
   untouched.

3. **State sharded by key-group, not by partition.** With snapshot
   stores (§1), the snapshot key in object storage is keyed by
   `(store, keyGroupId, snapshotId)`. Rebalance = update
   `KeyGroupAssignment` + fetch the snapshot for any newly-owned
   key-groups + replay the tail. No changelog repartition.

4. **Rescaling becomes a key-group migration.** Adding an instance:
   the cluster recomputes `KeyGroupAssignment` (existing
   `Kafka.Streams.Runtime.Assignor` plus a new key-group-aware
   strategy), the new instance fetches snapshots for its assigned
   key-groups, warms via the changelog tail, and starts serving.
   Removing an instance: its key-groups redistribute the same way.
   Partition count stays whatever it was when the topic was
   created.

5. **Existing partition-based dispatch remains the default.** A
   topology compiled without an explicit key-group config keeps
   using `newWorkerPoolHashed` exactly as today. The key-group
   mode is opt-in per `KafkaStreams.start` call.

### Optionality

Three coexisting dispatch modes, picked at runtime startup:

- **`Partition`** — current behaviour, `newWorkerPool`.
- **`Hashed`** — current behaviour, `newWorkerPoolHashed`.
- **`KeyGroup`** — new Riffle behaviour, `newWorkerPoolKeyGrouped`.

`StreamsConfig` gains a `dispatchMode :: DispatchMode` field with a
default that preserves today's behaviour.

The Phase-1 deliverable is the `KeyGroup` module + the new pool
entry point + a hand-driven `KeyGroupAssignment`. The Phase-2
deliverable is the assignor integration: a key-group-aware variant
in `Kafka.Streams.Runtime.Assignor` that participates in the
rebalance protocol.

---

## 3. Async I/O as a first-class operator (Phase 1)

### Problem

Already enumerated in the existing `Kafka.Streams.Topology.Free`
haddock that justifies refusing `foreachAsync`: no backpressure,
silent errors, EOS-incompatibility, lost ordering. The recommended
patterns work but force users to hand-roll bounded pools inside
custom processors. For I/O-bound enrichment topologies (the single
loudest "we left KS for Flink" complaint), this is the highest-
leverage gap to close.

### Fix

A first-class `AsyncMap` family with bounded in-flight,
backpressure, ordered/unordered output, timeout + retry, dead-letter
routing, and EOS-correct offset semantics.

Concrete changes:

1. **New `Prim` constructors** in `Kafka.Streams.Topology.Free`:

   ```haskell
   data Prim i o where
     -- existing constructors omitted...

     AsyncMapValues
       :: !AsyncIOConfig
       -> (v -> IO v')
       -> Prim (KStream k v) (KStream k v')

     AsyncMapKeyValue
       :: !AsyncIOConfig
       -> (k -> v -> IO (k', v'))
       -> Prim (KStream k v) (KStream k' v')

     AsyncConcatMapValues
       :: !AsyncIOConfig
       -> (v -> IO [v'])
       -> Prim (KStream k v) (KStream k v')
   ```

   Smart constructors `asyncMapValues` / `asyncMapKeyValue` /
   `asyncConcatMapValues` ship in the same module.

2. **`AsyncIOConfig`** as a new module
   `Kafka.Streams.AsyncIO.Config`:

   ```haskell
   data AsyncIOConfig = AsyncIOConfig
     { aioBufferCapacity :: !Int                -- max in-flight
     , aioWorkers        :: !Int                -- async worker pool size
     , aioOutputMode     :: !AsyncOutputMode    -- Ordered | Unordered
     , aioTimeout        :: !Duration
     , aioRetry          :: !AsyncRetryStrategy
     , aioOnFailure      :: !AsyncFailurePolicy
     , aioPunctuator     :: !AsyncPunctuator
     }

   data AsyncOutputMode = OrderedOutput | UnorderedOutput

   data AsyncFailurePolicy
     = FailTask                       -- shut the task down (default)
     | DropAndContinue
     | DeadLetter !TopicName !(Produced ByteString ByteString)
     | LogAndContinue                 -- mirrors logAndContinue handler

   data AsyncRetryStrategy
     = NoRetry
     | RetryFixed   !Int !Duration
     | RetryBackoff !Int !Duration !Int     -- attempts, initial, multiplier

   data AsyncPunctuator
     = NoTimeoutSweep
     | TimeoutSweepEvery !Duration
   ```

3. **Runtime: new module `Kafka.Streams.Runtime.AsyncIO`.** The
   compiled `ProcessorSpec` for an `AsyncMap*` is a processor that
   owns:

   - a bounded `TBQueue` of `(InFlightId, k, v, IO v')`,
   - a worker pool of `aioWorkers` async threads draining the
     queue,
   - a reorder buffer (`ordered` mode) or direct forwarding
     (`unordered` mode),
   - a per-record deadline tracker driven by a stream-time
     punctuator,
   - a retry-state map keyed by `InFlightId`.

4. **Backpressure ties into the consumer poll loop.** When the
   bounded `TBQueue` is full, the processor's input is blocked,
   which back-propagates through the `WorkerPool.workerInbox`
   `TQueue`. The runtime's outer poll loop already pauses
   `submitRecord*` when the inbox can't accept more work
   (existing semantics — the `STM` retry on a full inbox is the
   natural backpressure signal). Riffle adds a metric and a
   `pauseFetch` hint to the consumer for the chronic-backpressure
   case.

5. **Offset commit semantics.** The async operator integrates with
   the existing `EOSCoordinator` cycle through a new
   `ProcessorContext.ctxRegisterPreCommitDrain` hook + an
   engine-level registry (`engineAsyncDrains`) + a public
   `drainPreCommit :: Engine -> IO ()` invoked as the first step
   of `commitEngine` (before stores and the record collector are
   flushed). Every async operator registers in `procInit`; the
   registered drain blocks on the stream thread until every
   submitted request has been deposited by the worker pool and
   then forwards everything downstream. Result: in the EOS-v2
   commit cycle (`beginTxn → flushBody → commitOffsets → commitTxn`),
   `flushBody` already includes the drain — async output and
   source offsets land in the same transaction. **Landed in
   Phase 1.**

6. **AST-level fusion.** In `Kafka.Streams.Topology.Free.Optimize`:

   - `MapValues f >>> AsyncMapValues cfg g` ⇒
     `AsyncMapValues cfg (g . f)` when `cfg` permits — `f` is pure
     so it can be lifted into the async worker. Toggle:
     `optFuseSyncIntoAsync :: Bool`, default `True`.
   - `AsyncMapValues cfgA f >>> AsyncMapValues cfgB g` does
     *not* fuse — different `AsyncIOConfig` semantics, different
     backpressure budgets. The user that wants fusion uses
     `mapValuesM` and accepts the synchronous-on-stream-thread
     contract.

### Optionality

- `mapValuesM` / `mapKeyValueM` / `mapRecordM` are unchanged.
  Users that want today's synchronous-on-stream-thread semantics
  pick those.
- `foreachStreamAsync` (the imperative-DSL `IO ()` escape hatch
  flagged in the existing haddock) is still available, still not
  promoted to a first-class `Prim`, still documented as the
  best-effort fire-and-forget option.
- `AsyncMap*` is the new first-class non-blocking operator
  family. It is opt-in per call site.

This is the **highest-leverage Phase-1 change.** It's a
self-contained operator, the surface is small, and it closes the
single most-cited gap with Flink for the workload class that drove
the post-KS conversation.

---

## 4. Two-phase commit sink interface (Phase 2)

### Problem

EOS today is internal to the Kafka transaction: transactional
producer + `TxnOffsetCommit` in one go. Once the sink is anything
other than a Kafka topic — JDBC, Iceberg, Postgres, S3, an HTTP
endpoint — EOS evaporates. `Kafka.Streams.Runtime.EOS.EOSCoordinator`
already has the right shape (`initTxn` / `beginTxn` / `commitTxn` /
`abortTxn` / `storeCommit` / `storeAbort`); what's missing is the
user-extensible external-sink layer.

### Fix

Expose `TwoPhaseCommitSink` as a user-implementable interface and
ship four reference implementations.

Concrete changes:

1. **New module `Kafka.Streams.Sink.TwoPhase`:**

   ```haskell
   data TwoPhaseCommitSink k v = TwoPhaseCommitSink
     { tpcsName         :: !Text
     , tpcsBeginTxn     :: !(IO TxnHandle)
     , tpcsWrite        :: !(TxnHandle -> Record k v -> IO ())
     , tpcsPreCommit    :: !(TxnHandle -> IO PreCommitToken)
       -- ^ Flushes to the external system but does not finalise.
       -- The returned token is logged to the durable state
       -- backend (§1) so the framework can recover the in-flight
       -- transaction across restarts.
     , tpcsCommit       :: !(PreCommitToken -> IO (Either Text ()))
     , tpcsAbort        :: !(TxnHandle -> IO ())
     , tpcsRecover      :: !(PreCommitToken -> IO RecoveryDecision)
       -- ^ Called on restart for any pre-commit token found in
       -- the state backend. The sink decides: 'CommitFromToken',
       -- 'AbortFromToken', or 'UnknownLeaveAsIs'.
     }

   data RecoveryDecision
     = CommitFromToken     -- finish the half-committed transaction
     | AbortFromToken      -- roll back the half-committed transaction
     | UnknownLeaveAsIs    -- log + bail (operator must intervene)
   ```

2. **New `Prim` constructor:**

   ```haskell
   data Prim i o where
     -- existing constructors omitted...
     SinkTwoPhase :: !(TwoPhaseCommitSink k v)
                  -> Prim (KStream k v) ()
   ```

3. **`EOSCoordinator` extension.** A new helper
   `withTwoPhaseCommitSinks` (sibling of the existing
   `withTransactionalStores`) takes a list of registered sinks
   and threads their `preCommit` / `commit` / `abort` into the
   commit cycle. The existing four steps become five:

   ```
   beginTxn → flush → commitOffsets → preCommit2PC → commitTxn → commit2PC → storeCommit
   ```

   - Failure at `preCommit2PC` → abort the producer txn + abort
     all 2PC sinks.
   - Failure at `commitTxn` → abort 2PC sinks + abort store.
   - Failure at `commit2PC` (after producer txn committed) →
     `CommitFatal`; the sink tokens are persisted, recovery on
     restart runs `tpcsRecover`.

4. **Reference implementations** ship as their own modules so the
   core has no extra dependencies:

   - `Kafka.Streams.Sink.TwoPhase.JDBC` — uses `postgresql-simple`
     transactions, commits a per-task txn marker row alongside
     the writes.
   - `Kafka.Streams.Sink.TwoPhase.Iceberg` — leverages
     `wireform-iceberg` (already in this repo) for the
     write-then-commit-manifest two-step.
   - `Kafka.Streams.Sink.TwoPhase.S3` — staged uploads to a
     `__pending__/` prefix, atomic rename on commit.
   - `Kafka.Streams.Sink.TwoPhase.HTTP` — idempotency-key based
     pre-commit + finalize endpoints.

### Optionality

- Plain `Sink` / `SinkExtracted` / `Through` are unchanged.
  Topologies that only sink to Kafka behave exactly as today.
- `SinkTwoPhase` is a new constructor used only when the user
  reaches for it.
- The reference 2PC sinks live in their own modules; the core
  package gets no new dependencies.

---

## 5. Cross-source watermarks (Phase 2)

### Problem

Today watermarks are per-source and per-task:
`SourceSpec.sourceExtractor` is a single `TimestampExtractor`, and
`Kafka.Streams.Time.StreamTime` is a per-task monotonic max of
extracted timestamps. There's no cross-source coordinator, no
idle-source detection, no alignment between fast and slow sources.
For CDC-style topologies (Debezium snapshot vs streaming phase) and
for any topology with mixed-rate sources, this is a non-starter.

### Fix

A Flink/Beam-shaped watermark model — per-source `WatermarkStrategy`
+ cross-source coordinator + alignment groups + idle-source
detection.

Concrete changes:

1. **New module `Kafka.Streams.Watermark`:**

   ```haskell
   data WatermarkStrategy k v = WatermarkStrategy
     { wsExtract   :: !(TimestampExtractor k v)
     , wsGenerator :: !WatermarkGenerator
     , wsIdleness  :: !IdlenessConfig
     , wsAlignment :: !(Maybe AlignmentGroupId)
     }

   data WatermarkGenerator
     = BoundedOutOfOrderness !Duration
       -- watermark = max-seen-timestamp - outOfOrderness
     | MonotonicTimestamps
       -- watermark = max-seen-timestamp (no out-of-order)
     | Periodic !Duration !(Timestamp -> StreamTime -> Timestamp)
       -- user-supplied periodic generator
     | OnEvent !(Record ByteString ByteString -> StreamTime -> Maybe Timestamp)
       -- per-event punctuated watermark

   data IdlenessConfig
     = NeverIdle
     | IdleAfter !Duration
       -- if no record arrives for Duration, advance watermark by
       -- forwarding wall-clock time

   newtype AlignmentGroupId = AlignmentGroupId Text

   data AlignmentGroup = AlignmentGroup
     { agId        :: !AlignmentGroupId
     , agMaxDrift  :: !Duration
       -- fast sources within the group pause when they drift more
       -- than agMaxDrift ahead of the group's slowest source
     }
   ```

2. **`SourceSpec` field — additive, optional:**

   ```haskell
   data SourceSpec = SourceSpec
     { sourceName        :: !NodeName
     , sourceTopics      :: ![TopicName]
     , sourceKeySerde    :: !AnySerde
     , sourceValueSerde  :: !AnySerde
     , sourceExtractor   :: !AnyTimestampExtractor
     , sourceOffsetReset :: !Consumed.AutoOffsetReset
     , sourcePattern     :: !(Maybe Text)
     , sourceWatermarkStrategy :: !(Maybe AnyWatermarkStrategy)
       -- ^ NEW. 'Nothing' (the default) means: legacy per-task
       -- 'StreamTime' behaviour, exactly as today. 'Just' opts
       -- the source into the Riffle watermark coordinator.
     }
   ```

   Code that builds `SourceSpec` via the existing `addSource` /
   `addSourceWith` keeps working; the new field defaults to
   `Nothing`.

3. **New `Prim` constructors / smart constructors:**

   ```haskell
   withWatermarkStrategy
     :: WatermarkStrategy k v
     -> Topology Void (KStream k v)
     -> Topology Void (KStream k v)

   withAlignmentGroup
     :: AlignmentGroupId
     -> Duration            -- max drift
     -> Topology Void (KStream k v)
     -> Topology Void (KStream k v)
   ```

   These wrap an existing `Source` / `SourceMulti` / `TableSource`
   primitive without changing the wire type.

4. **New runtime module `Kafka.Streams.Runtime.Watermark`:**

   - `WatermarkCoordinator` is per-`StreamsApp` (not per-task):
     it tracks the *minimum* watermark across every source that
     feeds a given downstream operator, plus the per-source idle
     state, plus alignment-group membership.
   - The coordinator publishes "current watermark for input X to
     operator Y" via a `TVar`; downstream operators that care
     about event-time (windowed aggregates, stream-stream joins,
     `suppress(untilWindowCloses)`) read it instead of consulting
     per-task `StreamTime`.
   - Idle-source detection: when a source's last record is older
     than `IdleAfter d`, the coordinator marks it idle and
     excludes it from the min — so a quiet partition no longer
     stalls downstream windows.
   - Alignment: when a source's watermark drifts more than the
     group's `agMaxDrift` ahead of the group min, the runtime
     pauses fetching from that source (consumer `pauseFetch`).

5. **Event-time joins across sources.** The existing
   `StreamStreamJoin` / `StreamStreamLeftJoin` / `StreamStreamOuterJoin`
   primitives are unchanged. The runtime, when both sides have
   `Just` watermark strategies, drives the join's window expiry
   off the coordinated watermark instead of per-task `StreamTime`.
   When both sides are `Nothing`, behaviour is exactly as today.

### Optionality

- All existing `TimestampExtractor` machinery
  (`wallClockTimestampExtractor` / `recordTimestampExtractor` /
  `failOnNoTimestampExtractor` / `logAndSkipOnNoTimestamp` /
  `usePartitionTimeOnInvalidTimestamp`) is unchanged.
- `StreamTime` and per-task watermark math is unchanged.
- A source without a `WatermarkStrategy` runs on the legacy
  per-task model. A topology with no `WatermarkStrategy` on any
  source skips the coordinator entirely; there is no runtime cost
  for unused functionality.

---

## 6. Operator-level fixes (Phase 1 / Phase 2)

Smaller-scope changes. Each is additive; each lists the existing
behaviour that survives unchanged.

| Pain | Riffle fix | Phase | Optionality |
| ---- | ------- | ----- | ----------- |
| `getStateStore "name"` is stringly-typed and returns `AnyStateStore` you cast. | Typed `StoreRef k v` with phantom types. `processStream` overload takes `[StoreRef]` instead of `[StoreName]`. Compile error if you reference a store you didn't declare. The current `Topic k v` machinery in this repo already proves the pattern works. | 1 | The stringly-typed `processStream` / `withStateStoreKV` calls remain. `StoreRef` is the new typed alternative. |
| `suppress(untilWindowCloses)` has unbounded-buffer pathologies. | Mandatory explicit memory budget on `suppressUntilWindowCloses` Riffle variant: `(BufferConfig, OverrunPolicy)`. Policies: `DropOldest` / `ShedToDLQ` / `Fail` / `SpillToSnapshotStore`. | 1 | Existing `suppressUntilTimeLimit` / `suppressWindowed` / `suppressWindowedWith` keep their current contracts. |
| State-store schema evolution is on the user. | Stores carry a `schemaVersion :: Int`. New `StoreBuilder*Versioned` writes both old and new during a configurable burn-in window. The runtime drains the old store after burn-in. | 2 | Existing builders are version-pinned; opting in is a different builder call. |
| State TTL is wall-clock only. | Add `EventTimeTTL` to `StoreBuilderKV` / `StoreBuilderW` / `StoreBuilderS`. Expiry driven off the coordinated watermark (§5). | 2 | The wall-clock TTL is unchanged; `EventTimeTTL` is a new field with default `NoEventTimeTTL`. |
| Trigger / emit policy is hard-coded per operator. | Promote the existing `EmitStrategy` (KIP-825) to a first-class `EmitPolicy` reused by every windowed / stateful operator. Plus `OnCount n` and a user-supplied `EmitPolicy`. | 2 | Existing `withEmitStrategy` calls are unchanged. |
| CDC integration is "Kafka topic, hope you read it right". | New `Kafka.Streams.Sources.CDC` primitive: `cdcSource :: CdcConfig -> Topology Void (KStream k v)`. Knows about snapshot-vs-streaming phases, drives a watermark idleness handler (§5), emits schema-change events as side records, and applies key-aware compaction. | 2 | Plain `streamFromTopic` against a Debezium topic still works. |
| Internal topics (repartition / changelog) leak across deploys. | The framework owns their lifecycle: deterministic names (already true via `StableNames`), declared in the `Topology`, cleaned up on `cleanUp` (already true), and the runtime detects orphaned ones at startup and surfaces them. | 1 | Today's behaviour is the default; orphan detection is a warning-level diagnostic, not a hard failure. |
| Rebalance pauses. | KIP-848 (new consumer-group protocol) end-to-end + hot standby + key-group migration (§2) → partition movement is metadata-only when standby is caught up. | 2 | The classic-protocol code path stays; KIP-848 is selected via `StreamsConfig`. |
| Observability is metrics-soup. | Per-operator structured lag, queue depths, time-spent. New module `Kafka.Streams.Observability.Topology` ships a DAG-shaped JSON snapshot per task, suitable for a web UI overlay. | 1 | The existing `Kafka.Streams.Metrics` surface is unchanged. The new module is opt-in. |
| Multi-instance rebalance fragile. | KIP-848 + key-groups + remote state (§§1–2 + the above). | 2 | Today's classic-protocol rebalance stays default. |

---

## 7. Architectural sketches

### 7.1 Async I/O processor — runtime shape

```
Engine (single task thread)
   │
   │  forward record k v
   ▼
AsyncMapValues processor
   ┌──────────────────────────────────────────────────────┐
   │  inFlight :: TBQueue (InFlightId, k, v, IO v')       │
   │  reorder  :: TVar (Map InFlightId (Either e v'))     │
   │  nextOut  :: TVar InFlightId                         │
   │  workers  :: Vector (Async ())                       │
   └──────────────────────────────────────────────────────┘
                    │       ▲
                    │       │ result
                    ▼       │
              ┌─────────────────────┐
              │  async worker pool  │  ──→ external API
              └─────────────────────┘
                    │
                    ▼
   forward(record k v') to downstream
   (Ordered: drains reorder in nextOut order;
    Unordered: forwards as results arrive.)
```

Pre-commit drain: when the engine enters `runCommitCycle.flushBody`,
the processor blocks on `inFlight` empty + `reorder` empty.
Offsets are then safe to commit because every async result is
either forwarded or dead-lettered.

### 7.2 Snapshot-aware KV store — recovery flow

```
on task start:
  manifest <- objectStore.fetchLatest(store, keyGroup)
  case manifest of
    Nothing -> behave like today (full changelog replay)
    Just m  -> do
      objectStore.downloadFiles(m, localDir)
      restoreFromLocal(localDir)
      replayChangelog(from = m.advancedTo)

on commit (post-producer-txn):
  if elapsed >= spInterval || recordsSince >= spMaxRecordsBetween:
    snap <- store.snapshot()
    objectStore.upload(snap)
    objectStore.publishManifest(store, keyGroup, snap)
```

### 7.3 Watermark coordinator — data flow

```
Source A (Strategy WA) ───►  watermark W_A
Source B (Strategy WB) ───►  watermark W_B   ──►  Coordinator
Source C (no strategy) ─/                    ──►  min over { W_A, W_B }
                                                 (excluding idle sources)
                                              │
                                              ▼
                  downstream op (windowed agg / SSJ / suppress)
                       reads coordinated watermark
                       (sources without strategy still see
                        per-task StreamTime as today)
```

---

## 8. Compatibility surface

This section is the contract for *what existing code keeps doing*.

- Every existing `addSource` / `addSourceWith` / `addProcessor` /
  `addStateStore*` / `addSink*` call compiles unchanged. Any new
  `SourceSpec` / `ProcessorSpec` / `AnyStoreBuilder` field is
  optional with a default that reproduces today's behaviour.
- Every existing `Prim` constructor stays. New `Prim` constructors
  are added; no existing constructor is removed or renamed.
- `compile :: Topology Void o -> IO (o, Topo.Topology)` stays the
  one entry point. `compileNoOptimize` / `compileWith` /
  `compileWithOptimization` / `compileInBuilder` stay.
- `EOSCoordinator` keeps its existing fields. New fields
  (`twoPhaseSinkPreCommit`, `twoPhaseSinkCommit`,
  `twoPhaseSinkAbort`) default to no-op via `noopEOSCoordinator`.
- `WorkerPool`'s `newWorkerPool` / `newWorkerPoolHashed` stay.
  `newWorkerPoolKeyGrouped` is additive.
- `StandbyTask` / `StandbyManager` keep their current API. The
  Riffle backend wiring uses them through their existing surface;
  no struct changes are required.
- `TimestampExtractor` and all five shipped extractors are
  unchanged. `WatermarkStrategy` is a wrapper around an extractor.
- All KIP-295 / KIP-307 / KIP-825 / KIP-418 / KIP-892 work that
  recently landed is preserved as-is. None of it overlaps with
  the Riffle additions; both can coexist.

A topology that selects no Riffle features should produce byte-for-
byte identical compiled graphs to today's compiler, modulo
diagnostics. Riffle features only kick in when the topology
explicitly opts in.

---

## 9. Land order

**Phase 1** (lands first, in roughly this order):

1. Async I/O operator family — biggest user-visible leverage,
   most self-contained, no new modules touched outside
   `Topology.Free` + `AsyncIO` + `Engine`. Validated by the
   `Streams.AsyncIOSpec` suite (36 cases including 250+
   Hedgehog-randomised executions across permutation,
   failure-injection, retry/timeout, concurrency, lifecycle,
   and stress groups). **Landed.**

4. Bounded `suppressUntilWindowCloses` with overrun policies:
   `DropOldestSilently` extends `BufferOverflowPolicy`;
   `suppressWindowedShed` + `DeadLetterShelf` route over-cap
   entries to a side topic. The four spec policies (fail / drop /
   shed / spill) map to `ShutdownWhenFull` / `DropOldestSilently`
   / `suppressWindowedShed` / *deferred until §2*. **Landed.**

5. Orphan internal-topic detection: pure
   `detectOrphans :: Topology -> Text -> [TopicName] ->
   [OrphanInternalTopic]` in
   `Kafka.Streams.Observability.OrphanTopics` plus the
   conventional `changelogTopic` / `repartitionTopic` naming
   helpers. The runtime can wire this into startup as a
   diagnostic; the detector itself is pure and CI-friendly.
   **Landed.**

6. Topology DAG JSON observability: `topologyDescription` /
   `topologyDescriptionWith` / `liveTopologyDescription` in
   `Kafka.Streams.Observability.Topology` emit a versioned JSON
   document suitable for a web-UI overlay; the live variant
   layers in counter / gauge / DurationStats snapshots from the
   engine's `MetricsRegistry`. **Landed.**

7. Library-wide property / chaos suite —
   `Streams.Properties.*` covers the cross-cutting correctness
   invariants the unit tests cannot. (The original modules were
   namespaced "Antithesis" / "Jepsen-style", but we never ran
   those tools against the suite, so the name now reflects what
   the tests actually are: Hedgehog properties, state-machine
   models, and fault-injection harnesses driven from in-process
   mocks.) The suite covers:

   * `KVStoreSMSpec` — state-machine vs `Data.Map` model
     (in-memory + KIP-892 transactional store).
   * `OptimizerEqSpec` — optimised vs unoptimised topology output.
   * `WindowMathSpec` — 17 properties on tumbling / hopping /
     sliding / unlimited / session windows.
   * `EOSChaosSpec` — `runCommitCycle` schedule against a
     pure model, plus extensions for `getOffsets` throwing,
     `abortTxn` returning Left, and `storeAbort` returning Left.
   * `WorkerPoolSMSpec` — sequential pool dynamics.
   * `WorkerPoolConcurrentSpec` — concurrent submit + add / remove
     conservation; sticky routing under concurrency.
   * `ObservabilityTopologySpec` — DAG JSON renderer round-trips.
   * `OrphanTopicsSpec` — internal-topic detector edge cases.
   * `ChangelogReplaySpec` — active/standby replication
     properties: interleaved replay equivalence, multi-replica
     convergence, promote-on-failover via 2nd-gen standby
     replay, per-store isolation on shared changelog.
   * `WatermarkSpec` — stream-time = running-max under
     out-of-order input; backwards `advanceDriverStreamTime`
     is a no-op.
   * `AtLeastOnceRedeliverySpec` — induced redelivery via
     `seekMC`; output multiset is a superset of the input
     multiset; per-value redelivery is bounded by the rewind
     distance.

   Found bugs along the way: `TransactionalStore` iterator
   bypassing buffered writes (`kvsRange`/`kvsAll`),
   `hoppingWindows` mis-alignment when `size < advance`,
   `WorkerPool.removePoolWorker` deadlock when the inbox wasn't
   fully drained, and an unwrapped `getOffsets` exception in
   `runCommitCycle` that bypassed the abort path. All fixed in
   the same PR as their tests. **Landed.**
2. Snapshot-aware `KeyValueStore` shape + the
   `Kafka.Streams.Runtime.Snapshot` module + an in-memory and an
   FS-backed `ObjectStoreClient` (S3 wire ships in Phase 2 once
   `wireform-s3` lands or via an external dependency).
3. Typed `StoreRef k v` + the typed `processStream` overload.
4. `suppressUntilWindowCloses` bounded-buffer variant. **Landed.**
5. Orphan-internal-topic detection diagnostic. **Landed.**
6. Topology DAG JSON observability snapshot. **Landed.**
7. Key-group module + `newWorkerPoolKeyGrouped` entry point.
   Hand-driven assignment lives in
   `Kafka.Streams.Runtime.KeyGroup` (defining `KeyGroupConfig`,
   `KeyGroupAssignment`, `WarmupProgress`,
   `assignedToKeyGroupRange`); `newWorkerPoolKeyGrouped` /
   `submitRecordKeyGrouped` /
   `updateKeyGroupAssignment` in `WorkerPool` route records by
   the live assignment, and `StreamsConfig.dispatchMode`
   (`DispatchPartition | DispatchHashed | DispatchKeyGroup`)
   picks the constructor at startup. **Landed.**

In addition to the original Phase-1 line items, the following
Phase-1 \xc2\xa71 / \xc2\xa76 items landed in the same series:

* **Snapshot-aware KV store + `Runtime.Snapshot` + `ObjectStoreClient`**:
  `Kafka.Streams.Runtime.ObjectStore` defines the contract +
  ships `inMemoryObjectStore` and `filesystemObjectStore`
  references; `Kafka.Streams.State.KeyValue.Snapshot` defines
  `SnapshotPlan`, `SnapshotManifest`, `snapshotStore`,
  `restoreFromSnapshot`, `listSnapshots`;
  `Kafka.Streams.Runtime.Snapshot` owns the lifecycle (`SnapshotState`,
  `shouldSnapshot`, `publishIfDue`, `recoverStore`,
  `pruneOldSnapshots`). Recovery is now O(time-since-last-snapshot)
  rather than O(state-size). The S3 / GCS / Azure adapters
  ship in their respective packages. **Landed.**
* **Typed `StoreRef k v`**: `Kafka.Streams.State.Ref` adds
  `StoreKind`, `StoreRef (kind :: StoreKind) k v`,
  `SomeStoreRef`, and `kvRefOfBuilder` / `windowRefOfBuilder`
  / `sessionRefOfBuilder` smart constructors. Lookup helpers
  `getKVStoreRef` / `getWindowStoreRef` / `getSessionStoreRef`
  type-check the kind at compile time. The stringly-typed
  `getStateStore` / `processStream` calls remain. **Landed.**
* **`EmitPolicy` ADT** (\xc2\xa76 table row):
  `Kafka.Streams.EmitPolicy` promotes the KIP-825
  `EmitStrategy` enum to a first-class policy any windowed /
  stateful operator can consume. Adds `EmitOnCount n` and a
  user-supplied `EmitCustom` arm alongside the original
  `EmitOnUpdate` / `EmitOnWindowClose`; `decideEmit` /
  `EmitContext` are the consumer-side API. **Landed.**
* **Standby snapshot-pointer mode** (\xc2\xa71 follow-up):
  `Kafka.Streams.Runtime.StandbyTask` learns
  `StandbyMode = ReplayBytes | SnapshotPointer`.
  `newSnapshotPointerStandby` allocates a pointer-mode standby
  that holds @(snapshotId, advancedTo)@ without a local
  replica. `bumpSnapshotPointer` is the runtime hook the
  active calls when it publishes a fresh snapshot.
  Promotion = fetch the snapshot blob + replay the changelog
  tail. **Landed.**
* **Event-time TTL tied to the coordinated watermark** (\xc2\xa76
  table row): `ttlClockFromCoordinator :: WatermarkCoordinator
  -> IO Timestamp` builds the TTL wrapper's clock from the
  cross-source effective watermark. The wrapper itself still
  takes any @IO Timestamp@, so tests + wall-clock callers keep
  their existing surface. **Landed.**
* **Richer CDC source** (\xc2\xa76 table row):
  `Kafka.Streams.Sources.CDC` gains
  `CDCPhase = SnapshotPhase | StreamingPhase`, a `SchemaChange`
  side channel (`pushSchemaChange` / `setPhase` / the new
  `CDCPoll` return type), and `compactCDCBatch`: key-aware
  compaction that keeps only the last event per key in
  source order. `cdcToKTableStep` applies compaction
  automatically and surfaces schema changes + phase as part
  of its return. **Landed.**
* **Per-operator watermark consumption** (\xc2\xa75 last mile):
  `ProcessorContext` gains
  `ctxCoordinatedWatermark :: IO (Maybe Timestamp)` and a
  convenience helper `effectiveTime` that returns the
  coordinated watermark when wired, falling back to
  `ctxStreamTime`. The suppress operator already consumes it;
  the time-windowed aggregator / stream-stream join wiring
  is mechanically the same and a future small commit.
  **Landed.**
* **KIP-848 bridge** (\xc2\xa76 / \xc2\xa72 last mile):
  `Kafka.Streams.Runtime.RebalanceBridge` translates the
  broker-protocol `Kafka.Client.ConsumerGroupV2.AssignmentDelta`
  into the streams runtime's `RP.Reconciliation` shape so the
  same reconciler logic services both real-broker and
  in-process topologies. `applyAssignmentDelta` is the
  per-heartbeat hook. **Landed.**

**Phase 2** (depends on Phase 1 plumbing):

8. Two-phase commit sink interface + JDBC / Iceberg / S3 / HTTP
   reference sinks. The contract lives in
   `Kafka.Streams.Sinks.TwoPhase`: `TwoPhaseSink` with
   `tpsStage` / `tpsPrepare` / `tpsCommit` / `tpsAbort` /
   `tpsRecover` + `RecoveryDecision`, the `withTwoPhaseSinks`
   extension on `EOSCoordinator`, three in-process reference
   sinks (in-memory, filesystem atomic-rename, HTTP-echo), the
   `Topology.Free.SinkTwoPhase` Prim + `sinkTwoPhase` smart
   constructor + a foreach-style `compileSinkTwoPhase` that
   stages per-record into the sink's internal buffer. The 5-step
   commit cycle (`beginTxn \xe2\x86\x92 flushBody \xe2\x86\x92 commitOffsets \xe2\x86\x92
   preCommit2PC \xe2\x86\x92 commitTxn \xe2\x86\x92 commit2PC \xe2\x86\x92 storeCommit`) is
   wired into `runCommitCycle` so the producer transaction
   straddles the 2PC. The real JDBC / Iceberg / S3 / HTTP
   adapters live in separate packages because each pulls in its
   own driver. **Landed.**
9. Cross-source watermark coordinator + `WatermarkStrategy` +
   alignment groups + idle-source detection.
   `Kafka.Streams.Watermark` ships the full rich strategy
   record `{ wsName, wsGenerator, wsIdleness, wsAlignment }`
   with the `WatermarkGenerator` ADT (`MonotonicTimestamps`,
   `BoundedOutOfOrderness`, `NoWatermarkGen`,
   `CustomGenerator`), `IdlenessConfig`, and `AlignmentGroupId`.
   The runtime side wires through
   `Consumed.withWatermarkStrategy` \xe2\x86\x92 `SourceSpec.sourceWatermarkStrategy`
   \xe2\x86\x92 `Engine.attachWatermarkCoordinator`, so every record on a
   strategy-attached source reports its timestamp to the
   coordinator via `reportRecord`. `WatermarkCoordinator`
   publishes the min-of-live-sources effective watermark with
   idle-timeout skipping and alignment-group backpressure
   (`shouldPauseSource` / `alignmentBacklog`). Engine-side
   consumption (windows reading `currentEffectiveWatermark`
   instead of per-task `engineStreamTime`) is the remaining
   per-operator change. **Landed.**
10. Event-time TTL on state stores. The
    `Kafka.Streams.State.KeyValue.TTL` wrapper takes
    `TTLConfig { ttlDuration, ttlClock }` (clock is
    event-time, from `ctxStreamTime`) and lazily filters
    expired entries on every read while exposing
    `expireBefore now` for active sweeping from a
    punctuator. **Landed.**
11. Schema-versioned stores + burn-in migration.
    `Kafka.Streams.State.KeyValue.SchemaVersioned` adds
    `SchemaVersion` + `SchemaMigration` chains, a wrapper that
    stamps every write with the current version and migrates
    reads forward, and `burnInMigrate` to rewrite older entries
    onto the current version with resumable
    `BurnInProgress`. **Landed.**
12. CDC source primitive. `Kafka.Streams.Sources.CDC` defines
    the `CDCEvent` ADT (matches Debezium / DMS wire schema:
    Insert / Update / Delete with before- and after-image),
    `CDCSource` for the poll loop, `inMemoryCDCSource` for
    tests, and `applyCDCToKVStore` / `cdcToKTableStep` for the
    canonical CDC-to-KTable mapping. **Landed.**
13. Key-group-aware assignor + KIP-848 rebalance protocol.
    `Kafka.Streams.Runtime.KeyGroup` defines the routing
    primitive (decouples parallelism from partition count);
    `Kafka.Streams.Runtime.RebalanceProtocol` defines the
    KIP-848 wire types + incremental `reconcile` /
    `applyReconciliation` (guarantees no task is double-owned
    during a transfer); `Assignor.assignKeyGroups` is the
    sticky, balanced key-group assigner used by the runtime
    once it switches off the old protocol. **Landed.**
14. Hot-tier + cold-tier (S3) store backend.
    `Kafka.Streams.State.KeyValue.Tiered` wraps a hot
    `KeyValueStore` + a `ColdTier` (point get/put/delete + bulk
    scan — the API S3 / GCS satisfy in practice). Reads probe
    hot, fall through to cold and promote; eviction policies
    decide which entries to demote when the hot tier exceeds
    its budget. The cold tier ships an in-process reference;
    the S3 adapter lands in `wireform-s3`. **Landed.**
15. Remote-KV store backend (FoundationDB / TiKV / DynamoDB).
    `Kafka.Streams.State.KeyValue.Remote` defines the
    `RemoteKVClient` contract and an in-process mock with a
    per-call fault policy so the chaos suite can drive
    arbitrary error schedules. The wrapper exposes the
    `KeyValueStore` interface and surfaces `RemoteRetryable` /
    `RemoteFatal` as exceptions the runtime's standard handler
    decides on. The real FDB / TiKV / DynamoDB adapters live
    in separate packages. **Landed.**

---

## 10. What this gets you

Stacking everything above on top of today's library produces a
"Kafka Streams Riffle" with:

- Flink-class state durability and recovery — bounded by
  `time-since-last-snapshot`, not by state size.
- Flink-class async I/O with bounded backpressure, EOS-correct
  offset semantics, and ordered / unordered output modes.
- Flink-class cross-source watermarks with idleness handling and
  alignment groups.
- Flink-class two-phase commit to external sinks (JDBC, Iceberg,
  S3, HTTP) via a single user-extensible interface.
- Parallelism decoupled from partition count via key-groups.
- Typed, composable topology surface that JVM Kafka Streams'
  Java API can't express.

Compared to running an actual Flink cluster, the two things this
library keeps are the **library deployment model** (your service
contains the topology — no separate JobManager / TaskManager
fleet, no JAR submission) and **typed integration with
application code** (the `Prim` GADT is a first-class Haskell
value, not a serialised JobGraph).

Compared to staying on Kafka Streams classic, every documented
pain point that drove the post-KS Flink conversation is closed
without breaking the Kafka-bus-at-the-center mental model that
made KS attractive in the first place.

Every change above is additive. Adopt them one at a time. Skip
the ones you don't need.
