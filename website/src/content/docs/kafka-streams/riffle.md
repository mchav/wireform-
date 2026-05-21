---
title: "Riffle: Extended features"
description: "Optional extensions that add Flink-class capabilities: async I/O, snapshot recovery, two-phase commit sinks, and more."
sidebar:
  order: 1
  label: Extended features
---

Riffle adds optional capabilities to the base Kafka Streams implementation. Everything here is opt-in: your existing topologies continue working unchanged, and you can adopt features one at a time.

**When to use Riffle:**
- External enrichment calls that would block the stream thread (use async I/O)
- Large state stores where changelog replay slows deploys (use snapshot recovery)
- Writing to external systems that need exactly-once semantics (use 2PC sinks)
- Scaling past your partition count (use key-group routing)
- Joining streams with very different data rates (use watermark coordination)

:::tip[Need definitions?]
Kafka and Streams terminology is in the [Glossary](./glossary/).
:::

:::note[Quick reference]
- **Async I/O**: For calling external APIs without blocking the stream thread
- **Snapshot stores**: For fast recovery from large state stores
- **2PC sinks**: For exactly-once writes to databases, object stores, or HTTP endpoints
- **Watermark coordinator**: For handling idle partitions and mixed-rate joins
- **Key-groups**: For scaling past partition count
:::

For design details, see [`RIFFLE_SPEC.md`](https://github.com/iand675/wireform-/blob/main/wireform-kafka/streams/RIFFLE_SPEC.md).

## What Riffle adds

| Feature | Solves this problem |
| ------- | ------------------- |
| **Async I/O** | Calling slow external APIs (HTTP, gRPC, SQL) without blocking the stream thread |
| **Snapshot stores** | Multi-hour changelog replay when restarting with large state |
| **2PC sinks** | Exactly-once writes to Postgres, Iceberg, S3, or HTTP endpoints |
| **Watermark coordinator** | Windows that stall on idle partitions or mixed-rate joins |
| **Key-group routing** | Needing more parallelism than your topic has partitions |

Riffle keeps Kafka Streams' library model (no separate cluster, no JAR submission) while adding these capabilities.


## What's available now

Riffle features ship in production-ready modules. Import the module listed to use each feature.

### State durability beyond the changelog

Standard Kafka Streams recovers by replaying the changelog from offset 0. Riffle adds snapshot-based recovery: state becomes a first-class durable artifact, and the changelog acts as a write-ahead log between snapshots.

| Feature | Import this module | Use when |
| ------- | ------------------ | -------- |
| Snapshot-aware KV store | `Kafka.Streams.State.KeyValue.Snapshot` | You need recovery time bounded by snapshot frequency, not state size |
| Snapshot lifecycle | `Kafka.Streams.Runtime.Snapshot` | You want the runtime to decide when to snapshot and recover at boot time |
| Object-store contract | `Kafka.Streams.Runtime.ObjectStore` | You need S3/GCS/Azure backends (core ships in-memory and filesystem backends; cloud adapters live in separate packages) |
| Hot + cold tiered KV | `Kafka.Streams.State.KeyValue.Tiered` | Your state is too large for local disk; reads probe hot tier, fall through to cold, and promote |
| Remote KV (FoundationDB, TiKV, DynamoDB shape) | `Kafka.Streams.State.KeyValue.Remote` | You want no local state; node restart becomes a metadata operation |
| Pointer-mode standby | `Kafka.Streams.Runtime.StandbyTask` | Your standby state is too large to replicate; the standby tracks `(snapshotId, offset)` and fetches the snapshot at promotion |

With a snapshot backend, boot time becomes `O(time-since-last-snapshot)` instead of `O(state-size)`. See [Topology evolution](./operating/topology-evolution/) for how this affects rolling deploys.

### Async I/O as a first-class operator

The most important Riffle feature for I/O-bound workloads. The async operator provides bounded backpressure, EOS-correct offset semantics, ordered or unordered output, per-request timeout and retry, and explicit failure policies.

| Feature | Import this module |
| ------- | ------------------ |
| `AsyncMapValues` / `AsyncMapKeyValue` / `AsyncConcatMapValues` | `Kafka.Streams.Topology.Free` |
| Configuration (`AsyncIOConfig`, `AsyncOutputMode`, `AsyncFailurePolicy`, etc.) | `Kafka.Streams.AsyncIO.Config` |
| Runtime processor | `Kafka.Streams.Runtime.AsyncIO` |
| Sync-to-async fusion optimizer | `Kafka.Streams.Topology.Free.Optimize` |

EOS correctness works via `ProcessorContext.ctxRegisterPreCommitDrain`. The drain blocks the stream thread until every in-flight request completes; only then are offsets safe to commit. This keeps async output and source offsets in the same transaction.

Full walkthrough with capacity sizing: [Enrichment via external systems](./guides/enrichment/).

### Two-phase commit sinks

Standard EOS only works for Kafka-to-Kafka pipelines. Once you write to JDBC, Iceberg, Postgres, S3, or HTTP, you need two-phase commit to maintain exactly-once semantics. Riffle provides a 2PC sink interface.

| Feature | Import this module |
| ------- | ------------------ |
| Contract (`TwoPhaseSink`, `SinkTxnId`, `SinkOutcome`, `RecoveryDecision`) | `Kafka.Streams.Sinks.TwoPhase` |
| Reference sinks (in-memory, filesystem, HTTP echo) | `Kafka.Streams.Sinks.TwoPhase` |
| Coordinator wiring | `withTwoPhaseSinks` extends `EOSCoordinator` |
| Commit cycle implementation | `Kafka.Streams.Runtime.EOS` |
| Topology constructor | `Kafka.Streams.Topology.Free` (smart constructor `sinkTwoPhase`) |

The producer transaction straddles the sink's 2PC. A failure at `preCommit2PC` or `commitTxn` aborts both sides. A failure at `commit2PC` (after Kafka commits) leaves the sink transaction in prepared state; `tpsRecover` resolves it on next boot.

Real adapters (JDBC, Iceberg, S3, HTTP) live in separate packages. The contract and reference sinks ship in core.

Operator walkthrough: [Exactly-once across Kafka and other systems](./operating/exactly-once/).

### Cross-source watermark coordinator

Standard `engineStreamTime` is per-task and per-source. This breaks for cross-source joins, mixed-rate sources, and idle partitions. Riffle adds a per-application coordinator that tracks the minimum watermark across all live sources, excludes idle sources after timeout, and optionally backpressures fast sources via alignment groups.

| Feature | Import this module |
| ------- | ------------------ |
| Strategy types (`WatermarkStrategy`, `WatermarkGenerator`, `IdlenessConfig`, etc.) | `Kafka.Streams.Watermark` |
| Smart constructors (`monotonicAscending`, `boundedOutOfOrderness`, `withIdleness`, etc.) | `Kafka.Streams.Watermark` |
| Source registration | `Consumed.withWatermarkStrategy` |
| Coordinator operations | `Kafka.Streams.Watermark` (module contains `WatermarkCoordinator`) |
| Event-time TTL | `Kafka.Streams.State.KeyValue.TTL` with `ttlClockFromCoordinator` |

The `suppress` operator already uses coordinated watermarks. Time-windowed aggregators and stream-stream joins need wiring updates (in progress).

Sources without a `WatermarkStrategy` keep the legacy per-task model. No runtime cost for unused functionality.

### Key-group routing

Standard Kafka Streams parallelism is capped at your partition count. Key-groups decouple parallelism from partitions. The runtime hashes each record onto one of 128 key-groups (configurable), and the assignor moves key-groups between workers.

| Feature | Import this module |
| ------- | ------------------ |
| Types and config (`KeyGroupId`, `KeyGroupCount`, `KeyGroupConfig`) | `Kafka.Streams.Runtime.KeyGroup` |
| Assignment logic | `Kafka.Streams.Runtime.KeyGroup` |
| Routing helpers | `Kafka.Streams.Runtime.KeyGroup` |
| Worker-pool integration | `Kafka.Streams.Runtime.WorkerPool` |
| Startup selector | Set `StreamsConfig.dispatchMode = DispatchKeyGroup` |
| Assignor implementation | `Kafka.Streams.Runtime.Assignor` |

Standard dispatch modes (`DispatchPartition`, `DispatchHashed`) remain available and stay the default.

See [Scaling and rebalancing](./operating/scaling/).

### KIP-848 rebalance protocol

The new broker-side rebalance protocol moves assignment to the group coordinator. Members exchange subscriptions and member epochs. Reconciliation is incremental: a task moving from member A to B first appears in A's removal set, and only appears in B's addition set after A acknowledges release.

| Feature | Import this module |
| ------- | ------------------ |
| Wire types (`Subscription`, `Assignment`, `MemberEpoch`, etc.) | `Kafka.Streams.Runtime.RebalanceProtocol` |
| Reconciler | `Kafka.Streams.Runtime.RebalanceProtocol` |
| Group-state machine | `Kafka.Streams.Runtime.RebalanceProtocol` |
| Broker-protocol bridge | `Kafka.Streams.Runtime.RebalanceBridge` |

The classic protocol remains available. Select KIP-848 via `StreamsConfig`.

### Operator-level upgrades

Smaller features that solve specific pain points. All are additive-standard APIs continue working unchanged.

| Problem | Solution | Import this module |
| ------- | -------- | ------------------ |
| Stringly-typed `getStateStore` returns `AnyStateStore` | Typed `StoreRef k v` with phantom types; `getKVStoreRef`, `getWindowStoreRef`, etc. type-check at compile time | `Kafka.Streams.State.Ref` |
| `suppress(untilWindowCloses)` has unbounded buffers | Bounded variant with explicit `BufferOverflowPolicy`: drop oldest, shutdown when full, or shed to DLQ | `Kafka.Streams.Suppress` |
| Schema evolution is manual | `SchemaVersioned` wraps stores with `SchemaVersion` + `SchemaMigration` chains; `burnInMigrate` rewrites entries with resumable progress | `Kafka.Streams.State.KeyValue.SchemaVersioned` |
| TTL only supports wall-clock | `Kafka.Streams.State.KeyValue.TTL` accepts any `IO Timestamp` clock; pair with `ttlClockFromCoordinator` for event-time TTL | `Kafka.Streams.State.KeyValue.TTL` |
| Emit policy is hard-coded per operator | `EmitPolicy` type gives windowed operators configurable triggers: on update, on window close, on count, or custom predicate | `Kafka.Streams.EmitPolicy` |
| CDC requires manual topic handling | `cdcSource` understands snapshot vs streaming phases, surfaces `SchemaChange` records, applies key-aware compaction | `Kafka.Streams.Sources.CDC` |
| Internal topics leak across deploys | `detectOrphans` flags drift between expected and actual topics; runtime surfaces it as startup diagnostic | `Kafka.Streams.Observability.OrphanTopics` |
| Observability is unstructured | Per-operator lag, queue depths, time-spent. `topologyDescription` emits versioned JSON for UI overlays | `Kafka.Streams.Observability.Topology` |

## Mapping problems to Riffle pieces

A decision table for "I have X problem; which Riffle piece is the
answer?":

| Problem | Riffle piece | Operator doc |
| ------- | ------------ | ------------ |
| External enrichment via HTTP / gRPC / SQL; sync `mapValuesM` is too slow | Async I/O | [Enrichment](./guides/enrichment/) |
| Writes to Postgres / Iceberg / S3 / HTTP need to commit atomically with Kafka offsets | Two-phase commit sink | [Exactly-once](./operating/exactly-once/) |
| Boot-time changelog replay is the long pole on rolling deploys | Snapshot-aware KV + filesystem / S3 object store | [Topology evolution](./operating/topology-evolution/) |
| State is too large to keep on local disk | Tiered (hot + cold S3) KV, or Remote KV | (see store backend modules) |
| Need to scale a stateful topology past partition count | Key-group dispatch + assignor | [Scaling](./operating/scaling/) |
| Joined sources advance at different rates; windows stall on the laggard | Cross-source watermark coordinator | [Visibility](./operating/visibility/) |
| Joined sources advance at different rates; fast side blows up state | Watermark alignment group | Same |
| One partition stops receiving records; downstream windows stall | `IdleAfter` in `IdlenessConfig` | Same |
| State store needs an event-time TTL, not wall-clock | `EventTimeTTL` via `ttlClockFromCoordinator` | (see KV.TTL) |
| Store serde needs to evolve without a full repartition | `SchemaVersioned` + `burnInMigrate` | [Topology evolution](./operating/topology-evolution/) |
| Renames keep leaking internal topics on the broker | Orphan-topic detector | [Observability](./operating/observability/) |
| You need to render the topology in a UI with live metric overlays | `liveTopologyDescription` | Same |
| Stringly-typed store lookups have caused production bugs | Typed `StoreRef k v` | (see State.Ref) |
| Suppress under load occasionally OOMs | Bounded `suppressUntilWindowCloses` with explicit `BufferOverflowPolicy` | (see Suppress) |
| Need different per-operator emit triggers (count, custom predicate) | `EmitPolicy` (incl. `EmitOnCount n`, `EmitCustom`) | (see EmitPolicy) |
| Materialising a CDC feed (Debezium / DMS) into a KTable, with schema-change awareness | `cdcSource` + `cdcToKTableStep` | (see Sources.CDC) |

## Compatibility contract

**Why this matters:** Riffle is designed for incremental adoption. You don't need
to rewrite your topology or risk breaking existing behavior to use one feature.
This section explains exactly what stability guarantees you get when mixing
standard and Riffle features.

A topology that uses no Riffle features compiles to the same
imperative graph as standard Kafka Streams, modulo diagnostics.
Riffle features only activate when explicitly imported.

Specifically:

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
  (`preCommit2PC`, `commit2PC`, `abort2PC`) default to no-op via
  `noopEOSCoordinator`.
- `WorkerPool`'s `newWorkerPool` / `newWorkerPoolHashed` stay.
  `newWorkerPoolKeyGrouped` is additive.
- `StandbyTask` / `StandbyManager` keep their current API. The
  Riffle pointer-mode backend uses them through the same surface.
- `TimestampExtractor` and all five shipped extractors are
  unchanged. `WatermarkStrategy` is a wrapper around an extractor.
- All KIP-295 / KIP-307 / KIP-825 / KIP-418 / KIP-892 work is
  preserved as-is. None of it overlaps with the Riffle additions;
  both can coexist.

This matters operationally because adopting Riffle is incremental.
A team that runs a working parity-port topology can pick one
feature at a time: for example, swap a problematic
`mapValuesM`-on-HTTP for `asyncMapValues`: without touching the
rest of the topology, and without committing to the entire
extension tier.

## Recommended adoption path

If you're starting from a parity-port topology, adopt Riffle in
roughly this order:

1. **Observability first.** Topology JSON in CI; orphan-topic
   detector on startup; metrics-registry-to-push pipeline.
   Zero behaviour change; sets you up to monitor everything that
   follows.
2. **Async I/O for any I/O-bound enrichment.** Swap one
   `mapValuesM` at a time; verify the throughput and EOS
   behaviour; expand.
3. **Bounded `suppress` and `EmitPolicy`** where the
   unbounded-suppress pathologies bite.
4. **Typed `StoreRef`** during any refactor that touches store
   access; the old stringly-typed calls keep working.
5. **Snapshot-aware stores + pointer-mode standby** when
   changelog-replay time has become the rolling-deploy gate.
6. **Watermark coordinator** when you start joining mixed-rate
   sources or hit idle-partition stalls.
7. **Two-phase commit sinks** when you have external writes that
   need atomic-with-Kafka semantics.
8. **Key-group dispatch** when you hit the partition-count
   parallelism ceiling and can't (or don't want to) repartition
   topics.

You can stop at any step. Every step is an additive deploy.

## Related reading

- [`RIFFLE_SPEC.md`](https://github.com/iand675/wireform-/blob/main/wireform-kafka/streams/RIFFLE_SPEC.md)
 : the canonical design contract with rationale per section.
- [Topology evolution](./operating/topology-evolution/): how
  Riffle snapshot stores change rolling-deploy windows.
- [Scaling and rebalancing](./operating/scaling/): how key-groups
  and KIP-848 change the rebalance story.
- [Exactly-once across Kafka and other systems](./operating/exactly-once/)
 : the 2PC sink contract in operator terms.
- [Enrichment via external systems](./guides/enrichment/): the
  async I/O walkthrough.
- [Observability](./operating/observability/): topology JSON,
  orphan detection, and the live overlay.
- [Visibility versus ACID databases](./operating/visibility/) -
  where the watermark coordinator and event-time TTL fit in the
  visibility story.
- [Topology optimization](./concepts/topology-optimization/) -
  including the `optFuseSyncIntoAsync` Riffle fusion rule.
