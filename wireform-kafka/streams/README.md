# wireform-kafka-streams

A Haskell port of [Apache Kafka Streams](https://kafka.apache.org/documentation/streams/).
Mirrors the Java DSL operator-for-operator and the runtime
contract semantics-for-semantics; differences from the JVM are
spelled out below rather than being implicit.

This document is the canonical scope-of-the-library reference.
It tells you what works the same as the JVM, what works
differently, and what isn't there yet — so you can decide
whether the library fits your use case before writing code.

> **Status**: feature-complete relative to Apache Kafka 4.0
> Streams; ports JVM topologies one-to-one. The runtime drives
> a real broker through `Kafka.Streams.Runtime` *and* an
> in-process `TopologyTestDriver` for deterministic tests.
> See **What's not yet there** below for the honest list of
> remaining gaps.

---

## TL;DR

| Area                          | Status                                                                                                     |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------- |
| Stateless DSL                 | **Full parity.** `filter` / `map` / `flatMap` / `selectKey` / `peek` / `branch` (`Branched.withFunction` / `withConsumer`) / `merge` / `to` / `print`. |
| Stateful DSL                  | **Full parity.** `groupBy` / `count` / `reduce` / `aggregate` / `cogroup` (incl. `KGroupedTable` with subtractor — KIP-150). |
| Windowing                     | **Full parity.** Tumbling / hopping / sliding (KIP-450) / session windows; `suppress` (KIP-328 — incl. `shutDownWhenFull` / `emitEarlyWhenFull`); `EmitStrategy` (KIP-825). |
| Joins                         | **Full parity.** Stream-stream (windowed) with `StreamJoined` (KIP-479); stream-table; table-table; foreign-key (KIP-213) with `TableJoined` (KIP-545); GlobalKTable; modern `JoinWindows.ofTimeDifferenceWithNoGrace` / `ofTimeDifferenceAndGrace` (KIP-633). |
| Processor API                 | **Full parity.** `Processor`, `FixedKeyProcessor` (KIP-820), `ProcessorSupplier` with declared stores, `ProcessorContext` (incl. `appendHeader` / `requestCommit`), `Punctuator` (wall-clock + stream-time). |
| State stores                  | **Full parity.** KV (in-mem + RocksDB + LRU — `Stores.lruMap`), Window (incl. `TimestampedWindowStore`), Session, Timestamped (KIP-258), Versioned (KIP-889/960; `vkvDelete` included). |
| Side effects                  | **Full parity, plus typed IO variants.** Blocking `foreachStream` + non-blocking `foreachStreamAsync`. See the [SideEffects](#side-effects) section. |
| Interactive Queries           | **Full parity.** KIP-67 + KIP-796 typed `Query` API; KIP-805 `WindowKeyQuery` / `WindowRangeQuery`; KIP-889/960 `VersionedKeyQuery` / `MultiVersionedKeyQuery`; KIP-535 `StreamsMetadata` / `KeyQueryMetadata` + `StoreQueryParameters`. |
| Exception handlers            | **Full parity.** KIP-280 production, KIP-671 uncaught (REPLACE_THREAD / SHUTDOWN_CLIENT / SHUTDOWN_APPLICATION), KIP-1033 processing. |
| Dynamic runtime               | **Full parity.** KIP-663 `addStreamThread` / `removeStreamThread`; KIP-812 `CloseOptions`; KIP-988 standby + global-restore listeners; `cleanUp()`; `metadataForLocalThreads` / `metricsAndState`. |
| EOS-V2                        | **Wire-level path is in place.** Bound transactional producer + `EOSCoordinator` + KIP-892 buffer.          |
| Schema Registry serdes        | **In place.** Avro / JSON-Schema / Protobuf payload serdes + Confluent envelope + transport-agnostic HTTP.  |
| Standby tasks                 | **Live.** `Kafka.Streams.Runtime.StandbyTask` + `StandbyDriver`: the second-consumer changelog poll-loop dispatches into per-task replay; lag flows into the KIP-441 warmup map. |
| KIP-441 probing rebalance     | **Live.** The event loop calls `Consumer.requestRejoin` on the configured cadence when warmups are ready.   |
| Multi-thread runtime          | **Yes.** `numStreamThreads > 1` spins up an N-worker pool; one consumer dispatches by `hash (topic, partition) mod N` so per-partition state stays coherent. |
| Multi-instance rebalance      | **Yes.** `setRebalanceListener` + `ownedPartitions` + `standbyTasks` on `KafkaStreams` track partition transitions via the driver's `RebalanceEvent` channel. The native driver wires those events from `Kafka.Client.Consumer.setRebalanceListener`, which fires on every `subscribe` / re-subscribe / fenced-heartbeat (UNKNOWN_MEMBER_ID, FENCED_INSTANCE_ID = lost; everything else graceful = revoked). KIP-869 standby-grace state machine runs end-to-end. |
| Live-broker integration tests | `.github/workflows/wireform-kafka-integration.yml` spins up an `apache/kafka` KRaft broker (3.7 + 4.0) via docker compose and runs the live suite on every PR. |
| GHC                           | **9.6.4 / 9.8.4 / 9.10.1 / 9.12.1** in CI.                                                                  |

What follows is the user-facing scope: a section per operator
family + a worked example, the runtime model, and an honest
list of what isn't there yet.

---

## What works (DSL surface)

### Sources / sinks

| JVM                                         | Haskell                                                              |
| ------------------------------------------- | -------------------------------------------------------------------- |
| `StreamsBuilder.stream(topic)`              | `streamFromTopic`                                                    |
| `StreamsBuilder.table(topic, Materialized)` | `tableFromTopic`                                                     |
| `StreamsBuilder.globalTable(topic, Materialized)` | `globalTable`                                                  |
| `KStream.to(topic, Produced)`               | `toTopic`                                                            |
| `KStream.to(TopicNameExtractor, Produced)`  | `toExtracted` + `TopicNameExtractor` (KIP-303)                       |
| `KStream.through(topic)`                    | `throughTopic`                                                       |
| `KStream.repartition(...)`                  | `repartition`                                                        |
| `KStream.toTable(...)`                      | `toTable` (KIP-523)                                                  |
| `KTable.toStream(...)`                      | `toKStreamFromKTable`                                                |
| `KTable.suppress(...)` (KIP-328)            | `suppressKStream` / `suppressWindowed` / `suppressUntilTimeLimit`    |

### Stateless transforms

| JVM                          | Haskell                                            |
| ---------------------------- | -------------------------------------------------- |
| `KStream.filter` / `filterNot` | `filterStream` / `filterNotStream` (+ `*Named`)  |
| `KStream.map` / `mapValues`  | `mapKeyValue` / `mapValues` (+ `*M` IO variants, `*Named`) |
| `KStream.flatMap` / `flatMapValues` | `flatMapKeyValue` / `flatMapValues`         |
| `KStream.selectKey`          | `selectKey` (+ `selectKeyNamed`)                   |
| `KStream.peek`               | `peekStream` (+ `peekStreamNamed`)                 |
| `KStream.foreach`            | `foreachStream`                                    |
| `KStream.print(Printed)`     | `printStream` / `printToHandle`                    |
| `KStream.merge`              | `mergeStreams` / `mergeStreamsN`                   |
| `KStream.split` (KIP-418)    | `splitStream` + `Branched` / `branchedFrom`         |
| `KStream.transformValues`    | `transformValuesStream` / `processValuesStream`    |
| `KStream.process`            | `processStream` (returns `IO ()`; for typed downstream use `processValuesStream`) |
| `KStream.values`             | `valuesStream`                                     |

### KTable surface

| JVM                                     | Haskell                                  |
| --------------------------------------- | ---------------------------------------- |
| `KTable.filter` / `filterNot`           | `filterTable`                            |
| `KTable.mapValues`                      | `mapValuesTable`                         |
| `KTable.suppress(Suppressed)` (KIP-328) | `suppressKStream` etc.                   |
| `KTable.join` / `leftJoin` / `outerJoin` | `joinKTableKTable` / `leftJoinKTableKTable` / `outerJoinKTableKTable` |
| `KTable.join(KTable, fkExtractor, joiner, Materialized)` (KIP-213) | `foreignKeyJoinKTable` / `leftForeignKeyJoinKTable`. Subscription-token verification baked in. |
| `KTable.groupBy(KeyValueMapper)`        | `groupByKTable`                          |
| `KTable.toStream`                       | `toKStreamFromKTable`                    |

### Aggregations

| JVM                                         | Haskell                                |
| ------------------------------------------- | -------------------------------------- |
| `KGroupedStream.count` / `reduce` / `aggregate` | `countStream` / `reduceStream` / `aggregateStream` |
| `KGroupedStream.windowedBy(TimeWindows)`    | `windowedByTime`                       |
| `KGroupedStream.windowedBy(SessionWindows)` | `windowedBySession`                    |
| `TimeWindowedKStream.count` / `reduce` / `aggregate` | `countWindowed` / `reduceWindowed` / `aggregateWindowed` |
| `SessionWindowedKStream.count` / `aggregate` | `countSessionWindowed` / `aggregateSessionWindowed` |
| `KGroupedStream.cogroup(Aggregator)` (KIP-150) | `cogroup` / `addCogrouped` / `aggregateCogrouped` |

### Joins

| JVM                                                       | Haskell                                                                  |
| --------------------------------------------------------- | ------------------------------------------------------------------------ |
| `KStream.join(KStream, ValueJoiner, JoinWindows, StreamJoined)` | `joinKStreamKStream` (+ `leftJoinKStreamKStream`, `outerJoinKStreamKStream`) |
| `KStream.join(KTable, ValueJoiner, Joined)`               | `joinKStreamKTable` (+ `leftJoinKStreamKTable`)                          |
| `KTable.join(KTable, ValueJoiner, Materialized)`          | `joinKTableKTable` etc.                                                  |
| `KStream.join(GlobalKTable, KeyValueMapper, ValueJoiner)` | `joinKStreamGlobalKTable` (+ `leftJoinKStreamGlobalKTable`)              |
| `KTable.join(KTable, fkExtractor, joiner, Materialized)`  | `foreignKeyJoinKTable` (KIP-213, with token verification)                |

### Windowing

| JVM                                | Haskell                            |
| ---------------------------------- | ---------------------------------- |
| `TimeWindows.of(Duration)`         | `tumblingWindows`                  |
| `TimeWindows.of(...).advanceBy(...)` | `hoppingWindows`                 |
| `SlidingWindows.ofTimeDifferenceWithNoGrace(...)` (KIP-450) | `slidingWindows` |
| `SessionWindows.with(...)`         | `sessionWindows`                   |
| `Windows.grace(...)`               | `withGracePeriod` / `withSessionGracePeriod` |
| `Suppressed.untilWindowCloses(BufferConfig)` (KIP-328) | `suppressWindowed` / `suppressWindowedHandle` |
| `Suppressed.untilTimeLimit(...)`   | `suppressUntilTimeLimit`           |

### State stores

All four shapes the JVM ships, plus the modern KIP additions:

| JVM                                        | Haskell                                |
| ------------------------------------------ | -------------------------------------- |
| `KeyValueStore` (in-memory + RocksDB)      | `Kafka.Streams.State.KeyValue.{InMemory, RocksDB, Persistent}` |
| `WindowStore`                              | `Kafka.Streams.State.Window.InMemory`  |
| `SessionStore`                             | `Kafka.Streams.State.Session.InMemory` |
| `TimestampedKeyValueStore` (KIP-258)       | `Kafka.Streams.State.KeyValue.Timestamped` |
| `VersionedKeyValueStore` (KIP-889 / 960)   | `Kafka.Streams.State.KeyValue.Versioned` |
| `CachingKeyValueStore`                     | `Kafka.Streams.State.KeyValue.Caching` |
| Transactional store (KIP-892, EOS-V3)      | `Kafka.Streams.State.Transactional`    |
| `DslStoreSuppliers` (KIP-1247)             | `Kafka.Streams.DSL.DslStoreSuppliers`  |
| `addReadOnlyStateStore` (KIP-813)          | `addReadOnlyStateStore`                |

The RocksDB backend is gated behind the `rocksdb` cabal flag
and uses `rocksdb-haskell-kadena`.

### Processor API

Everything that's reachable via `org.apache.kafka.streams.processor.api`:

- `Processor`, `ProcessorContext`, `processorName`, `noopProcessor`,
  `statelessProcessor`.
- `forwardRecord` / `forwardTo`, `currentRecordMetadata`,
  `getStateStore`, `taskId`, `applicationIdC`, `streamTimeC`,
  `wallClockTimeC`.
- `schedule` with `WallClockTimePunctuation` /
  `StreamTimePunctuation` and `Cancellable`.
- Custom store builders via `StoreBuilder` / `StoreBuilderKV` /
  `StoreBuilderW` / `StoreBuilderS`.
- `addStateStore` / `connectProcessorAndStateStores`.
- `RestoreListener`-style hooks for changelog replay.

### Topology / configuration

- `StreamsBuilder` mutable builder mirroring Java's fluent API.
- `Topology` data type with full description (`TopologyDescription`,
  pretty-printer matches the JVM output shape).
- KIP-295 optimisation toggles (`OptimizationConfig`,
  `optimizeTopology`).
- KIP-307 stable processor names.
- `StreamsConfig` covering `application.id`, `bootstrap.servers`,
  `num.stream.threads`, `commit.interval.ms`, `cache.max.bytes.buffering`,
  `processing.guarantee`, `default.deserialization.exception.handler`,
  `task.timeout.ms`, `acceptable.recovery.lag`,
  `max.warmup.replicas`, `probing.rebalance.interval.ms`,
  `task.assignor.class`, `poll.ms`.
- `StreamsConfigKey` + `streamsConfigFromMap` for `Properties`-style
  configuration.

### Runtime

- `Kafka.Streams.Runtime.KafkaStreams` mirrors the JVM
  `KafkaStreams` lifecycle: `newKafkaStreams`, `startKafkaStreams`,
  `closeKafkaStreams`, `streamsStatus`, `setStateListener`,
  `awaitState`.
- `pauseKafkaStreams` / `resumeKafkaStreams` / `isPausedKafkaStreams`
  (KIP-834).
- `LagListener` / `LagInfo` / `publishLag` (KIP-647).
- `applyEOSCoordinator` to plug in a transactional commit
  driver (`newRealEOSCoordinator` wraps a real
  `Kafka.Client.Transaction`).
- `startKafkaStreamsWith :: KafkaStreams -> StreamDriver -> IO ()`
  injection seam — you can hand the runtime any
  `Kafka.Streams.Runtime.NativeDriver.StreamDriver`. Two
  constructors ship: `newNativeDriver` (production,
  `Producer` + `Consumer` + optional bound `Transaction`) and
  `newMockDriver` (deterministic, in-process, used by the
  test suite).

### Interactive Queries

- KIP-67: `queryEngineStore`, `ReadOnlyKeyValueStore`,
  `roKvGet`, `roKvAll`, `roKvRange`.
- KIP-796: typed `Query` API (key-range, count, all).
- Works against every store backend (in-memory + RocksDB).

### Schema Registry serdes

- `Kafka.Streams.Serde.SchemaRegistry`: client interface,
  Confluent envelope (`magicByte` + `int32 BE id` + payload),
  in-memory + mock-HTTP clients for tests.
- `Kafka.Streams.Serde.SchemaRegistry.Http`: transport-agnostic
  HTTP-backed registry (`HttpRequester` record-of-IO so users
  pin their own `http-client` / `wreq` / `req`).
- Payload-format serdes:
  - `Kafka.Streams.Serde.Avro` — pluggable Avro encoder /
    decoder; works with any Avro library.
  - `Kafka.Streams.Serde.JsonSchema` — JSON-Schema-typed
    payloads, same Confluent envelope.
  - `Kafka.Streams.Serde.Protobuf` — Protobuf with the
    Confluent message-index varint (zigzag-encoded; index 0 is
    `0x00`).

### Time

`Kafka.Streams.Time` ships `Timestamp`, `Duration`, the
`millis` / `seconds` / `minutes` / `hours` / `days` builders,
and the `TimestampExtractor` types `recordTimestampExtractor`,
`wallClockTimestampExtractor`, `extractFromValue`.

### Metrics

`Kafka.Streams.Metrics` is the in-process registry — counters,
gauges, duration stats. Plus the librdkafka-shaped
`Kafka.Telemetry.StatsJson` and the OTel push state machine
(KIP-714).

---

## Side effects

The DSL has four seams for `IO` inside a topology (covered
end-to-end by [`Kafka.Streams.Examples.SideEffects`](examples/Kafka/Streams/Examples/SideEffects.hs)):

| Seam                         | JVM equivalent             | Notes                                                            |
| ---------------------------- | -------------------------- | ---------------------------------------------------------------- |
| `peekStream`                 | `KStream.peek`             | Observe records non-destructively.                               |
| `foreachStream`              | `KStream.foreach`          | Terminal IO sink.                                                |
| `mapValuesM` / `mapKeyValueM` | `KStream.mapValues` / `map` | Explicit IO variants. JVM `mapValues` is nominally pure but in practice people sneak side effects in; the typed variant makes the IO contract visible. |
| Processor API + `Punctuator` | `ProcessorContext.schedule` | Wall-clock + stream-time scheduled effects, full `IO`.          |

**Two caveats apply to all of them:**

- **EOS-V2 doesn't extend to side effects.** Under
  `ExactlyOnceV2` the producer side is transactional and offsets
  commit through `sendOffsetsToTransaction`, but external
  effects in `peek` / `foreach` / `mapValuesM` / a `Punctuator`
  are not part of that transaction. A topology rewind on
  rebalance will replay them. If you need exactly-once for
  the side effect, gate it on a state-store-backed idempotency
  token. **Same as the JVM.**
- **Order is per-task, not global.** On a single task, effects
  fire in topology order. With `numStreamThreads > 1` different
  keys may be processed concurrently across tasks. To force a
  global order, route through `repartition` to one partition
  or sink to a topic and consume separately. **Same as the JVM.**

---

## Semantic differences from the JVM

These are the things that look the same on paper but behave
slightly differently. None are deal-breakers; they just exist.

1. **`mapValues` / `map` are pure by default; `*M` variants are
   explicit IO.** The JVM types `mapValues` as `(V) -> V'` and
   leaves IO implicit. We type the pure variant as `(v -> v')`
   and ship `mapValuesM :: (v -> IO v')` for the IO case. If
   you have IO inside a transform, use the `M` variant; the
   pure one runs at type-check time.

2. **List comprehensions / `do`-notation in topology code is
   normal Haskell.** Building topologies imperatively with
   `IO` inside `StreamsBuilder` is the idiomatic style — it
   mirrors the JVM fluent API. Don't try to make it pure;
   the JVM doesn't either.

3. **No reflection.** The JVM uses `Class<?>` for store-type
   discovery (`QueryableStoreTypes.keyValueStore()`); we use a
   typed `Query` API plus `unsafeCoerce` inside the DSL when
   the user-supplied serde guarantees the type. If you bypass
   the DSL and pin store types incorrectly, you'll get a
   crash at the use site; don't bypass the DSL.

4. **`KTable.suppress` returns a `KStream`, not a `KTable`.**
   The JVM allows `KTable.suppress(Suppressed.untilWindowCloses(...)).toStream()`
   to chain. Here `suppressWindowed` already returns
   `KStream (WindowedKey k) v` because we don't carry the
   compound `KTable<Windowed<K>, V>` type — the windowed key is
   a separate type. To strip the window envelope use
   `selectKey` after `suppress`.

5. **The runtime is "one consumer, N workers", not "N
   stream-threads, each its own consumer".** Java's
   `StreamThread` model is one consumer + one producer per
   thread joining the same group; the broker's coordinator
   reassigns partitions across threads. We use a
   simpler-but-effective design:
   - One `StreamDriver` (one consumer, one producer) per
     `KafkaStreams` instance.
   - With `numStreamThreads = N > 1`, the runtime spins up a
     'WorkerPool' of N workers, each with its own engine and
     state stores. Records dispatch by `hash (topic,
     partition) mod N` (sticky within a process), so the
     same partition consistently lands on the same worker.
   - At commit time the runtime drains every worker's
     in-memory collector through the shared producer.
   - Interactive Queries federate across worker engines the
     way Java's `CompositeReadOnlyKeyValueStore` federates
     across local tasks.

   The tradeoffs vs. Java's per-thread model:
   - **Less network overhead** — one consumer connection.
   - **No store rebalance across workers within a process** —
     a partition's state stays on whichever worker it first
     hashed to. Multi-instance (multiple OS processes joining
     the same group) is what you'd use to redistribute work
     across a cluster, and that's the next milestone.

6. **`processStream` returns `IO ()`, not `KStream`.** The
   JVM `KStream.process(ProcessorSupplier, ...)` returns a
   typed `KStream<K, V'>`. Encoding the type change at the
   record-of-callbacks boundary requires a typed value-serde
   coming back from the processor; we instead expose
   `processValuesStream` which takes the output value serde
   explicitly and returns the typed `KStream`. Use it when
   your processor changes the value type.

7. **In-process driver vs. real broker.** The JVM
   `TopologyTestDriver` requires the same `StreamsConfig`
   you would use against a real broker, just without the
   network. Our `Kafka.Streams.Driver.TopologyTestDriver`
   doesn't need any config: `newDriver topology applicationId`
   is enough. The runtime path uses the same `StreamsConfig`
   the JVM does.

---

## What's not yet there

Honest list. Items here aren't unsupported in principle — they
just haven't landed yet.

- **Multi-process rebalance verification under a live broker.**
  Single-process multi-thread works (`numStreamThreads > 1`),
  the cross-instance rebalance code path is wired
  (`setRebalanceListener` + `ownedPartitions` + `standbyTasks`
  + the KIP-869 grace state-machine + KIP-535 JoinGroup
  metadata exchange). What's still pending is a multi-process
  CI test that joins two instances to the same consumer group
  and observes assignment migration. The single-instance Docker
  fixture in `.github/workflows/wireform-kafka-integration.yml`
  covers the protocol path; the multi-instance variant is
  follow-up work.
- **Schema Registry compatibility-mode probing.** The serde
  wraps the Confluent envelope and trusts the registry's
  schema id; we don't proactively check the registry's
  `compatibility-mode` setting before publishing a new schema
  version.
- **`Printed.toFile` with log rotation.** We have
  `printStream` + `printToHandle`; rotating-file sinks are
  the user's responsibility (give us a `Handle` you manage).

Otherwise the streams runtime + DSL are **feature-complete
relative to Apache Kafka 4.0 Streams**; tests cover every
shipped operator end-to-end (375 cases in
`wireform-kafka-streams-test`).

---

## When to use this library

**Good fit:**

- You're building a streaming app in Haskell and don't want to
  shell out to JVM Streams.
- You want exactly the JVM Kafka Streams operator surface but
  in Haskell (DSL parity is one-to-one).
- Your topology is unit-testable through the in-process
  driver, and you don't strictly need a multi-thread live
  runtime today.
- You want typed state stores (in-memory or RocksDB) with the
  full KIP-258 / KIP-889 / KIP-960 surface.
- You want Confluent Schema Registry interop without forcing
  an `http-client` dep on the world.

**Consider alternatives:**

- You need to scale a single consumer group across many OS
  processes today — the within-process multi-thread runtime
  works, but cross-process rebalance hasn't landed; for a
  multi-instance deployment JVM Streams is still the answer.
- You need bug-for-bug compatibility with Streams-the-product
  including its quirks. We aim for spec compliance, not
  bug-for-bug.
- Your app is fundamentally pull-based microservices with
  occasional async work — a plain
  `Kafka.Client.{Producer, Consumer}` setup is simpler.

---

## Examples

`wireform-kafka/streams/examples/` ships fourteen demos
mirroring the canonical Apache Kafka Streams examples plus a
dedicated [SideEffects](examples/Kafka/Streams/Examples/SideEffects.hs)
walk-through. Run any of them from the in-process driver
without a broker:

```
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- word-count
cabal run wireform-kafka-streams-examples -- side-effects
cabal run wireform-kafka-streams-examples -- all
```

See [`examples/README.md`](examples/README.md) for the full
index with one-liner descriptions.

---

## Related documents

- [`../TUTORIAL.md`](../TUTORIAL.md) — runnable walkthrough that
  goes from producer/consumer hello-world to a transactional
  Streams pipeline including the multi-instance + IQ + standby
  story.
- [`../CONFIG_PARITY.md`](../CONFIG_PARITY.md) — `librdkafka` knob
  parity for the client layer; streams-specific config knobs
  are listed in `Kafka.Streams.Config.StreamsConfig` and
  spelled out in the §StreamsConfig section above.
- [`examples/README.md`](examples/README.md) — operator-by-operator
  example index with the JVM equivalent of each demo.
- [`bench/results/README.md`](bench/results/README.md) — runtime
  benchmark numbers + reproduction recipe.
