---
title: "Riffle: Extended features"
description: "Optional extensions that add Flink-class capabilities: async I/O, snapshot recovery, two-phase commit sinks, and more."
sidebar:
  order: 1
  label: Extended features
---

Kafka Streams is deliberately minimal. It gives you stateful stream processing as a library -- no cluster to deploy, no job submission system, no resource manager. That simplicity is its strength, but it also means the base model has hard edges that surface as your application grows.

Riffle is a set of opt-in extensions that address those edges without abandoning the library model. Each feature targets a specific operational pain point that the base Kafka Streams design doesn't solve.

## The problems Riffle solves

### Your stream threads block on external calls

A common pattern is enriching records by calling an HTTP API, querying a database, or invoking a gRPC service. In base Kafka Streams, that call happens synchronously on the stream thread. While the thread waits for a 50ms HTTP response, it processes zero records. Multiply that across thousands of records per second and your throughput collapses.

**Riffle's async I/O** operators (`asyncMapValues`, `asyncMapKeyValue`, `asyncConcatMapValues`) move external calls off the stream thread. You configure concurrency bounds, timeout, retry, and failure policy. The runtime manages backpressure and drains all in-flight requests before committing offsets, so exactly-once semantics are preserved.

```haskell
asyncMapValues
  (asyncIOConfig { aioMaxConcurrency = 64, aioTimeout = 5_000_000 })
  (\record -> callEnrichmentAPI (rvValue record))
```

Full walkthrough: [Enrichment via external systems](./guides/enrichment/).

### Restarts take too long because of changelog replay

Base Kafka Streams recovers state by replaying the changelog topic from the beginning. If your state store holds 50 GB, recovery replays 50 GB of changelog records. On a rolling deploy with 20 instances, that's 20 sequential multi-minute waits.

**Riffle's snapshot stores** periodically checkpoint state to an object store (local filesystem, S3, GCS, Azure). On restart, the runtime loads the latest snapshot and only replays changelog records written after it. Recovery time becomes proportional to time-since-last-snapshot instead of total state size.

| Store variant | Use when |
| ------------- | -------- |
| Snapshot-aware KV | Recovery time needs to be bounded by snapshot frequency, not state size |
| Hot + cold tiered KV | State exceeds local disk; reads probe a hot tier and fall through to cold storage |
| Remote KV | No local state at all; node restart is a metadata operation |
| Pointer-mode standby | Standby replicas are too large to replicate; standbys track a snapshot reference and fetch on promotion |

See [Topology evolution](./operating/topology-evolution/) for how this changes rolling deploys.

### Exactly-once only works for Kafka-to-Kafka

Kafka's transactional producer gives you exactly-once semantics when the output goes to another Kafka topic. The moment you write to Postgres, Iceberg, S3, or an HTTP endpoint, you're on your own. A crash between the external write and the Kafka offset commit means duplicates or data loss.

**Riffle's two-phase commit sinks** coordinate the external system's transaction with Kafka's. The producer transaction straddles the sink's 2PC: if either side fails, both abort. If a crash happens after Kafka commits but before the sink confirms, `tpsRecover` resolves the prepared transaction on next boot.

```haskell
sinkTwoPhase "postgres-sink" postgresSink produced
```

The contract and reference sinks ship in core. Production adapters (JDBC, Iceberg, S3, HTTP) live in separate packages.

Operator walkthrough: [Exactly-once across Kafka and other systems](./operating/exactly-once/).

### Windows stall on idle partitions or mixed-rate joins

Base Kafka Streams tracks stream time per task. When you join two streams that produce records at very different rates, the slow side holds back window closures on the fast side. Worse: if a partition goes completely idle, any window waiting for that partition's watermark to advance will wait forever.

**Riffle's watermark coordinator** tracks the minimum watermark across all live sources, automatically excludes idle sources after a configurable timeout, and optionally backpressures fast sources via alignment groups.

```haskell
let strategy = boundedOutOfOrderness (seconds 5)
                 & withIdleness (idleAfter (seconds 30))

addSourceWith topic (consumed & withWatermarkStrategy strategy)
```

See [Visibility versus ACID databases](./operating/visibility/).

### You need more parallelism than your partition count allows

Kafka Streams ties parallelism to partition count: one task per partition. If your topic has 12 partitions and your stateful operator needs 64-way parallelism, you're stuck -- repartitioning an existing topic is disruptive.

**Key-group routing** decouples parallelism from partitions. The runtime hashes each record onto one of N key-groups (default 128), and the assignor distributes key-groups across workers independently of the partition layout.

```haskell
let config = defaultStreamsConfig
      { dispatchMode = DispatchKeyGroup (keyGroupConfig 128) }
```

See [Scaling and rebalancing](./operating/scaling/).

### Smaller operational improvements

| Problem | Riffle solution | Module |
| ------- | --------------- | ------ |
| `getStateStore` returns an untyped `AnyStateStore`, causing runtime cast errors | Typed `StoreRef k v` with phantom types; type-checked at compile time | `Kafka.Streams.State.Ref` |
| `suppress(untilWindowCloses)` buffers grow without bound under load | Bounded variant with explicit overflow policy: drop oldest, shutdown, or shed to DLQ | `Kafka.Streams.Suppress` |
| Schema evolution in state stores requires a full repartition | `SchemaVersioned` wraps stores with migration chains; `burnInMigrate` rewrites entries with resumable progress | `Kafka.Streams.State.KeyValue.SchemaVersioned` |
| TTL expiry only uses wall-clock time | TTL accepts any clock; pair with the watermark coordinator for event-time TTL | `Kafka.Streams.State.KeyValue.TTL` |
| Windowed operators have a fixed emit policy | `EmitPolicy` gives configurable triggers: on update, on window close, on count, or custom predicate | `Kafka.Streams.EmitPolicy` |
| CDC sources require manual topic management | `cdcSource` handles snapshot vs streaming phases, surfaces schema changes, applies key-aware compaction | `Kafka.Streams.Sources.CDC` |
| Internal topics leak when topologies are renamed or removed | `detectOrphans` flags drift between expected and actual topics at startup | `Kafka.Streams.Observability.OrphanTopics` |
| Observability is ad-hoc | Per-operator lag, queue depths, time-spent metrics; `topologyDescription` emits versioned JSON for UI overlays | `Kafka.Streams.Observability.Topology` |

## KIP-848 rebalance protocol

Riffle also includes the new broker-side rebalance protocol (KIP-848). The classic eager-rebalance protocol stops the world on every group membership change. KIP-848 moves assignment to the group coordinator, making reconciliation incremental: a task moving from member A to B first appears in A's removal set, and only shows up in B's addition set after A acknowledges release.

The classic protocol remains available. Select KIP-848 via `StreamsConfig`.

## Adoption path

Every feature is independent. Adopt them one at a time in whatever order matches your pain:

1. **Observability** -- topology JSON and orphan-topic detection. Zero behavior change; sets up monitoring for everything that follows.
2. **Async I/O** -- swap one blocking `mapValuesM` at a time. Biggest throughput win for I/O-bound workloads.
3. **Bounded suppress and EmitPolicy** -- where unbounded suppress buffers are causing memory pressure.
4. **Typed StoreRef** -- during any refactor that touches store access. The old stringly-typed calls keep working.
5. **Snapshot stores** -- when changelog replay is the gate on rolling deploys.
6. **Watermark coordinator** -- when mixed-rate joins or idle partitions stall your windows.
7. **2PC sinks** -- when external writes need atomic-with-Kafka semantics.
8. **Key-group dispatch** -- when you hit the partition-count parallelism ceiling.

A topology that uses no Riffle features compiles to the same imperative graph as base Kafka Streams. Riffle features only activate when explicitly imported, and every existing API call continues working unchanged.

## Related reading

- [Enrichment via external systems](./guides/enrichment/) -- async I/O walkthrough with capacity sizing
- [Topology evolution](./operating/topology-evolution/) -- how snapshot stores change rolling deploys
- [Scaling and rebalancing](./operating/scaling/) -- key-groups and KIP-848
- [Exactly-once across Kafka and other systems](./operating/exactly-once/) -- the 2PC sink contract
- [Observability](./operating/observability/) -- topology JSON, orphan detection, live overlays
- [Visibility versus ACID databases](./operating/visibility/) -- watermarks and event-time TTL
- [Topology optimization](./concepts/topology-optimization/) -- including the `optFuseSyncIntoAsync` fusion rule
- [`RIFFLE_SPEC.md`](https://github.com/iand675/wireform-/blob/main/wireform-kafka/streams/RIFFLE_SPEC.md) -- design contract with per-section rationale
