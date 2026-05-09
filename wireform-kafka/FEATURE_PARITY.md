# Feature Parity Spec

This document is the running plan for bringing `wireform-kafka` (core
client) and `wireform-kafka-streams` (Streams DSL + runtime) to
production-grade parity with the upstream references:

| Library                     | Reference target            | Tracked version |
|-----------------------------|-----------------------------|-----------------|
| `wireform-kafka`            | Apache Kafka client wire    | Kafka 4.0       |
| `wireform-kafka` (UX side)  | `librdkafka`                | 2.x             |
| `wireform-kafka-streams`    | Apache Kafka Streams (Java) | Kafka 4.0       |

The spec is intentionally bias-toward-test: every "Done" item below has at
least one targeted test (unit, property, or against the in-process mock
broker); every "Outstanding" item lists the test surface that has to land
with the implementation.

---

## 1. Goals & non-goals

### Goals

1. **Wire compatibility** — a real Kafka 4.0 broker can speak to our
   client, and a real Kafka 4.0 client can speak to our streams
   implementation, end-to-end without the other side noticing it isn't
   talking to a Java peer.
2. **Semantic compatibility** — the public API behaves the way the JVM
   client / Streams library does for the same input. Especially:
   exactly-once-V2, sticky / cooperative-sticky rebalancing, transactional
   offset commit, suppress (KIP-328), versioned stores (KIP-889/KIP-960).
3. **Operational compatibility** — every `librdkafka` knob that maps to
   client behaviour has a corresponding `wireform-kafka` config field
   with the same name, default, and semantics. See
   [`CONFIG_PARITY.md`](./CONFIG_PARITY.md).
4. **Testability** — an in-process `MockCluster` covers every failure
   mode that `librdkafka`'s mock-cluster suite exercises, so we can land
   regressions with deterministic tests.

### Non-goals

- Bug-for-bug compatibility with quirks the JVM client itself documents
  as deprecated.
- The pre-KIP-848 ZooKeeper coordinator path. KRaft only.
- Compatibility with brokers older than Kafka 2.5 (we negotiate down via
  `ApiVersions` to the lowest version we still implement, not arbitrarily
  far back).
- Schema-Registry-the-product (Confluent's HTTP service). We will ship
  the *client-side* Avro / JSON-Schema / Protobuf serdes interfaces, but
  spinning up a registry is out of scope.

---

## 2. Done — current state inventory

### 2.1 `wireform-kafka` (core client)

#### Wire protocol

- API key coverage: Produce, Fetch, ListOffsets, Metadata, OffsetCommit,
  OffsetFetch, FindCoordinator, JoinGroup, SyncGroup, Heartbeat,
  LeaveGroup, DescribeGroups, ListGroups, DeleteGroups, CreateTopics,
  DeleteTopics, ApiVersions, InitProducerId, AddPartitionsToTxn,
  AddOffsetsToTxn, EndTxn, TxnOffsetCommit, DescribeConfigs, AlterConfigs.
- Versioned (incl. flexible / tagged-fields) request/response
  encoders + decoders, generated from the Kafka schema. Comprehensive
  round-trip property tests (`test/Protocol/Generated/ComprehensiveSpec.hs`).
- Record batch v2 (KIP-98), variable-length records, headers,
  CRC32-C, idempotent producer sequencing, transactional records.
- Compression: gzip, snappy, lz4, zstd. Per-batch + per-record codecs,
  property-tested.

#### Client behaviour

- `Producer`:
  - Idempotent producer (KIP-98) with per-partition sequence numbers.
  - `BatchAccumulator` with linger.ms, batch.size, sticky-partitioning
    (KIP-480 inspired), per-batch retry counter + exponential backoff
    with jitter, structured `Logger` callback for sender errors.
  - Transaction *coordination protocol* (`Kafka.Client.Transaction`):
    `initTransactions`, `beginTransaction`, `sendOffsetsToTransaction`,
    `commitTransaction`, `abortTransaction`. State machine, coordinator
    discovery, `InitProducerId`, `AddPartitionsToTxn`,
    `AddOffsetsToTxn`, `TxnOffsetCommit`, `EndTxn` are wired against
    the `TransactionCoordinator`. Per-stamp `(TxnId, Epoch)`
    visibility tracking in the mock means aborted records *stay*
    aborted across epoch reuse.
  - **Important caveat**: the `Transaction` value and the `Producer`
    value are still separate. The send path doesn't yet automatically
    `addPartitionsToTxn` for partitions touched inside an open
    transaction, doesn't stamp the transactional producer-id/epoch
    onto outgoing batches (it uses the idempotent producer-id), and
    doesn't fence sends after commit/abort. End-to-end transactional
    use therefore still requires manual orchestration. See §3.1.
- `Consumer`:
  - `subscribeFlow` with FindCoordinator → JoinGroup → SyncGroup →
    OffsetFetch.
  - Range / round-robin / sticky / cooperative-sticky assignors. Sticky
    consumes previous-generation `ownedPartitions` (consumer-protocol
    v1, KIP-341/KIP-429); fixed in this branch.
  - Per-partition pause/resume, manual offset store, isolation level
    (read-committed / read-uncommitted), commit-with-metadata.
  - LeaveGroup RPC on close (no more silent-timeout-out members).
  - Heartbeat thread.
- `AdminClient`:
  - createTopics / deleteTopics / listTopics / describeTopics /
    listConsumerGroups / describeConsumerGroups / deleteConsumerGroups /
    describeConfigs.
  - DescribeConfigs decodes KIP-226 `ConfigSource` correctly to
    distinguish DEFAULT_CONFIG from any override; the unwrap helpers are
    public + unit-tested.
- `TransactionCoordinator`:
  - findCoordinator, initProducerId, addPartitionsToTxn, endTransaction,
    addOffsetsToTxn, txnOffsetCommitWith. Pure request builders are
    public + tested.
- `Pipeline`:
  - Send / receive / timeout threads, correlation-id routing into
    caller-held `TMVar` slots, in-flight + queue-size backpressure,
    statistics counters, clean shutdown. Tested against a localhost echo
    broker.

#### Networking

- `Network.Connection`:
  - Per-broker connection pool (`ConnectionManager`).
  - DNS resolution modes (host / `use_all_dns_ips` /
    `resolve_canonical_bootstrap_servers_only` per KIP-235), broker
    address family selection (any / v4 / v6).
  - Socket buffer / Nagle / keepalive / max-idle / max-fails knobs.
  - Best-effort `isConnected` liveness probe (alive-idle case fixed in
    this branch).
- API version negotiation:
  - `ApiVersionsRequest` on connect, `ApiVersionCache` per broker,
    `selectVersion` picks the highest version both sides understand.
- SASL:
  - PLAIN, SCRAM-SHA-256, SCRAM-SHA-512, OAUTHBEARER, AWS_MSK_IAM,
    GSSAPI handshakes. `wireName` round-trips.

#### Mock broker

- `Kafka.Client.Mock.*`:
  - `MockCluster`: topics, partitions, logs, HWM/LSO, brokers (up/down),
    groups (offsets, members, generation id), transactions (state, epoch,
    committed/aborted stamps, pending offsets), KRaft roles, re-auth
    deadlines, manual clock.
  - `FaultPolicy`: per-op / per-topic / per-group / per-txn injectable
    errors (retriable, fatal, timeout, leader-change, txn-fenced).
  - `MockProducer`, `MockConsumer`, `MockAdmin` with the same surface as
    the real clients, used by both the streams `MockStreamsDriver` and
    the core-client failure-mode tests.
  - `IdempotentState`, `BackoffPolicy`, `TelemetryCounters`,
    `ProducerStamp`.
  - Failure-mode coverage parity with selected `librdkafka` mock-cluster
    tests (0121, 0125, 0127, 0130, 0137, 0142, 0143, 0146, 0147, 0148,
    0150).

### 2.2 `wireform-kafka-streams`

- KStream DSL: filter, map, mapValues, flatMap, flatMapValues,
  selectKey, peek, branch / split, merge, repartition, through, toTable
  (KIP-523), `processValuesStream` (low-level), named variants for every
  operator (KIP-307), `printToHandle`, dynamic-topic sink
  (`TopicNameExtractor`, KIP-303).
- KTable DSL: filter, mapValues, suppress (KIP-328), join, leftJoin,
  outerJoin, transformValues, toStream, groupBy.
- GlobalKTable + global join.
- Aggregations: `count`, `reduce`, `aggregate` over KGroupedStream and
  KGroupedTable.
- Cogroup (KIP-150): multiple grouped streams of distinct value types
  feeding one aggregate.
- Windowed processing: tumbling, hopping, sliding (KIP-450), session
  windows. `TimeWindowedKStream`, `SessionWindowedKStream`.
- Joins: stream-stream, stream-table, table-table, foreign-key
  (rudimentary), windowed stream-stream.
- State stores:
  - `KeyValueStore` (in-memory + RocksDB-backed via
    `rocksdb-haskell-kadena`).
  - `WindowStore`, `SessionStore`, `TimestampedKeyValueStore`
    (KIP-258), `VersionedKeyValueStore` (KIP-889/KIP-960).
- Processor API: `ProcessorContext`, `Punctuator`, schedule
  wall-clock + stream-time punctuations.
- Topology:
  - `Topology`, `StreamsBuilder`, sub-topology computation by
    repartition boundary.
  - `TopologyDescription` with pretty-printer mirroring the JVM
    representation.
- `KafkaStreams` runtime (engine-only):
  - Worker pool (`numStreamThreads`), per-task processing, deterministic
    quiescence via STM.
  - `Engine` integrates with mock or real client; `MockStreamsDriver`
    hooks the engine to the in-process broker.
- Exactly-once V2 *scaffolding*: idempotent producer wired to the
  engine's commit boundaries, plus a pluggable `EOSCoordinator`
  recording-stub the runtime tests assert against. The actual
  transactional producer integration (records bracketed by
  `BeginTxn` / `EndTxn` markers; `sendOffsetsToTransaction` for
  consumer offsets) is still pending — tracked in §3.1 below.
- Standby task scaffolding (the `MockCluster` already understands
  changelog topics and warmup reads).
- `StreamsConfig` covering processing.guarantee, num.stream.threads,
  commit.interval.ms, cache.max.bytes.buffering,
  default.deserialization.exception.handler, etc.
- `StreamsMetrics`: in-process registry, counters / gauges / duration
  stats.
- Interactive Queries v1 (KIP-67) + KIP-796 typed Query API (key range,
  count, all).
- `DslStoreSuppliers` (KIP-1247) so users can swap default backends.
- `RestoreListener`-style hooks for changelog replay.

### 2.3 Tests

| Suite                              | Count |
|------------------------------------|-------|
| `wireform-kafka:wireform-kafka-test`         | 416 |
| `wireform-kafka:wireform-kafka-streams-test` | 258 |

Both green on every commit on this branch.

---

## 3. Outstanding work — gap analysis

For each item: severity (S0 = parity-blocker, S1 = important for real
production use, S2 = nice-to-have), rough invasiveness, and the test
surface that must land.

### 3.1 Core client gaps

#### S0 — Wire `Transaction` into `Producer` (end-to-end transactional support)

- **What's missing.** The transaction coordinator protocol
  (`Kafka.Client.Transaction`) and the producer (`Kafka.Client.Producer`)
  are independent values today. Concretely the producer's send path:
  - does **not** stamp the transactional producer-id / epoch onto
    outgoing record batches — it uses `producerIdempotentId` /
    `producerIdempotentEpoch` regardless of whether a transaction is
    open;
  - does **not** call `AddPartitionsToTxn` the first time a transaction
    touches a new (topic, partition); the broker therefore rejects the
    eventual `EndTxn` with `INVALID_TXN_STATE`;
  - does **not** block sends issued outside `InTransaction` state once a
    `Transaction` has been initialised, so callers can accidentally
    produce non-transactional records on a transactional producer;
  - does **not** stamp the `isTransactional` bit on the record-batch
    header even when one is open.
  Net effect: the API surface (`initTransactions` / `beginTransaction`
  / `commitTransaction` / `abortTransaction` /
  `sendOffsetsToTransaction`) exists, but a user calling them and
  `produce` cannot get exactly-once semantics — the records simply
  aren't part of the transaction the broker sees.
- **Why this matters.** This is the entire reason
  exactly-once (KIP-98 / KIP-447) exists. Until this lands, every
  Streams EOS-V2 path is also blocked (§3.2), and KIP-892 (S0,
  EOS-V3 store transactions) cannot be implemented at all.
- **Invasiveness.** Medium. The cleanest model is to bind a
  `Transaction` to the `Producer` at creation (so
  `Producer.producerTransactional` carries an `Maybe Transaction`
  alongside its config), then thread that `TVar TransactionState`
  through:
  - the send path: stamp `(producerId, epoch)` from the txn state
    into the batch header instead of from `producerIdempotentId`;
  - `BatchAccumulator.addRecord`: enqueue an
    `AddPartitionsToTxn` request via the transaction coordinator
    when a (topic, partition) is first observed in this txn;
  - send-time checks: refuse to enqueue when state is not
    `InTransaction` (or `Ready` for non-transactional producers);
  - `closeProducer`: if state is `InTransaction`, abort.
- **Tests.**
  - End-to-end against the mock broker: `initTransactions →
    beginTransaction → produce → commitTransaction` results in the
    records being visible to a read-committed consumer; the same
    sequence with `abortTransaction` results in records *not* being
    visible (we already have per-stamp visibility in the mock; it
    just isn't being driven from the producer).
  - Producer-fence test: a second producer with the same
    `transactional.id` fences the first; the first's next produce
    fails with `ProducerFenced` instead of writing.
  - `sendOffsetsToTransaction` round-trip: a producer-consumer
    consume-process-produce loop commits in one atomic step.
  - Producer with `transactional.id` set but no `beginTransaction`
    rejects `produce`.
- **Dependencies.** None new. All the protocol pieces are already
  implemented; this is integration / wiring work.

#### S0 — TLS/SSL

- **What's missing.** `connectionUseSecure` is plumbed through to
  `Network.Connection`, but we have no integration test that talks to a
  TLS-enabled broker. Certificate validation, hostname verification,
  client-cert auth, and SNI are passed through to the underlying
  `crypton-connection` package but not exercised.
- **Invasiveness.** Low. A test broker built on `Network.TLS` plus
  fixture certificates checked into `test/Network/TLS/`.
- **Tests.**
  - Successful handshake against a self-signed cert with explicit trust
    store.
  - `endpoint.identification.algorithm = https` (KIP-235) hostname
    mismatch failure.
  - Client-cert auth (mutual TLS).
  - SNI is forwarded for shared multi-tenant hostnames.
- **Dependencies.** None.

#### S0 — SASL re-authentication (KIP-368)

- **What's missing.** We track `connMaxReauthMs` in the mock cluster and
  expose the config field, but the client itself doesn't actually perform
  the re-auth handshake mid-session. A long-lived OAUTHBEARER session
  will silently fail on the broker's deadline.
- **Invasiveness.** Medium. `Connection` needs a per-broker reauth
  timer; it has to drive a fresh `SaslHandshake` + `SaslAuthenticate`
  exchange without dropping in-flight requests.
- **Tests.**
  - `MockCluster.markReauthDeadline` is already there; add a flow test
    where the client refreshes credentials mid-stream and the next
    request is accepted.
  - Reauth fails → connection is torn down with a typed error, not a
    generic timeout.

#### S1 — Producer / consumer interceptor APIs

- **What's missing.** No analogue to
  `org.apache.kafka.clients.producer.ProducerInterceptor` or
  `ConsumerInterceptor`. Tracing / metrics tools written against the JVM
  client cannot be ported without rewrite.
- **Invasiveness.** Low. Two callback types in `ProducerConfig` /
  `ConsumerConfig`:
  - `producerInterceptor :: ProducerRecord k v -> IO (ProducerRecord k v)`
  - `producerOnAcknowledgement :: ProducerRecord k v -> Either Error RecordMetadata -> IO ()`
  - `consumerInterceptor :: ConsumerRecords k v -> IO (ConsumerRecords k v)`
  - `consumerOnCommit     :: Map TopicPartition OffsetAndMetadata -> IO ()`
- **Tests.** Property tests showing the chain is invoked in registration
  order and exceptions in interceptors don't break the producer/consumer
  loop.

#### S1 — Quota / throttling response handling

- **What's missing.** Broker `ThrottleTimeMs` headers in responses are
  decoded but ignored. The producer doesn't pause; the consumer doesn't
  back off.
- **Invasiveness.** Medium. Each response decoder can carry a
  `responseThrottleTimeMs` value; the sender / fetcher honour it before
  the next request.
- **Tests.** Mock broker returns throttle, client honours the delay
  before sending the next request to that broker.

#### S1 — Static membership generation persistence (KIP-345)

- **What's missing.** `consumerGroupInstanceId` is wired through, but we
  don't persist any local state across restarts so a restarting consumer
  always rejoins as a fresh member from the broker's perspective.
- **Invasiveness.** Low. Optional callback to persist
  `(memberId, generationId)` pre-shutdown; on startup pass it back into
  the JoinGroup.

#### S1 — OpenTelemetry / `librdkafka` JSON stats output

- **What's missing.** `Kafka.Telemetry.OpenTelemetry` is currently a
  stub (functions log warnings). We have a `MetricsRegistry` and per-op
  `TelemetryCounters` in the mock; we don't ship them as either OTel
  spans or `librdkafka`-style stats JSON.
- **Invasiveness.** Medium. Wire the existing counters to OTLP; emit a
  `librdkafka` stats JSON document on a timer.
- **Tests.** Snapshot test for the JSON document; OTLP exporter
  in-memory test that asserts span attributes line up with the JVM
  conventions.

#### S2 — Compression dictionary support (zstd dict)

- **What's missing.** We pass straight zstd; no dict negotiation.
  `librdkafka` supports a static dict.

#### S2 — KIP-466 client-side leader rebalance

- **What's missing.** The client treats `NOT_LEADER_FOR_PARTITION` as
  generic-retry; it doesn't update its leader cache from
  `MetadataResponse.preferredLeader`.

#### S2 — Pluggable network transport

- The `Network.Connection` type is hard-coded to `crypton-connection`.
  A custom `Transport` typeclass would let users plug in a unix-socket or
  testing transport without TCP.

### 3.2 Streams gaps

#### S0 — Full `KafkaStreams` runtime against the real client

- **What's missing.** The `KafkaStreams` runtime currently drives the
  `Engine` directly. To run against a real broker we need to wire the
  engine through `Kafka.Client.Producer` / `Consumer`, including:
  - `ConsumerRebalanceListener` callbacks
    (`onPartitionsAssigned` / `onPartitionsRevoked`) so tasks suspend +
    resume properly on rebalance.
  - Source-topic offset reset policies translated into
    `auto.offset.reset` on the underlying consumer.
  - Producer-per-task vs producer-per-thread vs producer-per-instance
    selection (KIP-447 vs KIP-892 in EOS-V2 vs EOS-V3).
  - **Genuine EOS commit boundaries:** the engine's commit ticks must
    drive the producer's `beginTransaction` / `produce` /
    `sendOffsetsToTransaction` / `commitTransaction` cycle. Today the
    EOSCoordinator is recording-only. Blocked by §3.1 "Wire
    `Transaction` into `Producer`".
- **Invasiveness.** High. The engine's `IO` interface needs an
  abstraction over the producer/consumer (either a class-based one or a
  driver record). The mock-driver path stays as-is; a new
  `Kafka.Streams.Runtime.NativeDriver` drives the real client.
- **Tests.**
  - End-to-end against a real local broker (requires Docker fixture).
  - End-to-end against the mock broker by routing the *real-client* code
    paths through the mock — i.e. the abstraction has two
    implementations and we test the same scenarios against both.

#### S0 — KIP-892 store transactions (EOS-V3)

- **What's missing.** Today's EOS commits the producer transaction and
  the changelog write atomically, but state-store puts happen
  pre-transaction. KIP-892 introduces a *store transaction* layer so the
  store is rolled forward only when the changelog commit lands. The
  versioned-store + changelog interplay needs to honour this.
- **Invasiveness.** High. Touches every `KeyValueStore` /
  `WindowStore` impl: introduce a transactional buffer in front of
  the actual put. The RocksDB implementation can use a write-batch.
- **Tests.**
  - Property: any abort after a buffered put leaves the store in its
    pre-transaction state.
  - Crash test: `MockCluster` injects a producer fence between the
    changelog write and the commit; after restore from changelog, the
    store equals the last committed state.

#### S0 — Schema-Registry serdes interface

- **What's missing.** No analogue to
  `io.confluent.kafka.serializers.KafkaAvroSerializer`. We can't
  interoperate with Schema-Registry-using producers/consumers.
- **Invasiveness.** Medium. Define `Serde` typeclass hierarchy +
  a `SchemaRegistryClient` interface; provide pluggable Avro / JSON
  Schema / Protobuf serdes that fetch schemas through that client.
  Don't ship a registry.
- **Tests.**
  - Round-trip an Avro record through wire format, mock the
    SchemaRegistry HTTP exchange.
  - Compatibility-mode tests for forward / backward / full evolution.

#### S1 — Probing rebalance (KIP-441)

- **What's missing.** We have warmup-replica wiring in the mock
  but no client-side logic to issue probing rebalances when warmup tasks
  catch up. Without it, standby promotion is slow on real clusters.
- **Invasiveness.** Medium. New piece of the assignor logic; the
  cooperative-sticky assignor variant needs a "warmup ready" hook from
  the runtime.

#### S1 — KIP-869 high-availability task revocation

- **What's missing.** When a partition is revoked, we currently drop
  the task immediately. KIP-869 keeps the task running as a
  read-only standby until the new owner has caught up, smoothing over
  rebalance pauses.

#### S1 — Foreign-key joins (KIP-213) — full

- **What's there.** Basic foreign-key join is wired but uses naive
  re-keying; we don't ship the dual-changelog + versioned-store
  structure the JVM client uses, so out-of-order updates can produce
  stale results under specific timing.
- **What's missing.** The subscription-store + responder topic + correct
  FK-tombstone handling.
- **Tests.** Property test: for any sequence of left + right updates,
  the join output equals the result of replaying the same sequence
  serially.

#### S1 — Multi-instance liveness simulation

- **What's there.** The `MockCluster` supports multiple
  `MockStreamsDriver` instances joining the same group.
- **What's missing.** A canonical scenario harness that injects
  instance failures (process crash, partition isolation, slow GC) and
  asserts the surviving instances still make progress.
- **Tests.** Hedgehog property test that enumerates failure orderings
  over a small input fixture and verifies output stream is identical to
  the no-failure run.

#### S1 — KIP-892 / Streams configuration full surface

- `task.timeout.ms`, `acceptable.recovery.lag`, `max.warmup.replicas`,
  `probing.rebalance.interval.ms`, `task.assignor.class` (custom assignor
  registration). Most are accepted in the config record but not actually
  honoured by the runtime.

#### S2 — Custom `TopologyOptimization` toggles

- The JVM client's `Topology.optimize(REUSE_KTABLE_SOURCE_TOPICS, …)`
  rewrites the topology to fold redundant repartitions. We don't have
  this rewriter pass.

#### S2 — Topology naming auto-rewrite for backwards compatibility

- KIP-307's `Named` operator is exposed on every DSL combinator, but we
  don't have a "stable name" auto-generator that the JVM client uses
  when no explicit name is given (so two builds of the same topology
  produce identical changelog topic names). For users that rely on
  `application.id` portability across builds, this matters.

### 3.3 Cross-cutting / infrastructure

| Item                                    | Severity | Notes                                                                 |
|-----------------------------------------|----------|-----------------------------------------------------------------------|
| Live-broker integration test harness    | S1       | Currently mock-only. A `Test.Tasty` group that boots Docker would let us regress against a real Kafka 4.0. |
| GHC 9.10 / 9.12 build matrix            | S2       | Currently 9.6.4 only.                                                 |
| Benchmark suite (vs `librdkafka` numbers) | S2     | Have anecdotal local numbers; no committed benchmark targets.         |
| Documentation: tutorial-grade README    | S2       | Module haddocks are good; a tutorial walkthrough is missing.          |

---

## 4. Prioritisation

In approximate order:

1. **Wire `Transaction` into `Producer`** (S0). Strict prerequisite
   for everything EOS-related: Streams EOS-V2, KIP-892 EOS-V3,
   `sendOffsetsToTransaction` consume-process-produce loops. This is
   wiring, not new protocol work.
2. **`KafkaStreams` runtime against the real client** (S0). Unblocks
   end-to-end tests and is a prerequisite for KIP-892 store
   transactions; also depends on (1) for genuine EOS commit boundaries.
3. **TLS handshake test fixture + reauth** (S0). Needed for any
   production deploy.
4. **KIP-892 store transactions** (S0). Brings EOS into Kafka 4.0
   parity; without it, exactly-once is only "exactly-once-V2". Depends
   on (1) and (2).
5. **Schema-Registry serdes interface** (S0). Most enterprise users
   won't adopt the lib without it.
5. **Probing rebalance + KIP-869 revocation** (S1). Needed for smooth
   rolling restarts at scale.
6. **Quota/throttle handling, interceptors, OpenTelemetry/stats output**
   (S1). Polish for production.
7. **Custom `TopologyOptimization`, naming auto-rewrite, FK-join
   correctness** (S1/S2).
8. **Live-broker harness + benchmarks** (cross-cutting).

Difficulty notes (no calendar estimates):

- 1 (Producer ↔ Transaction wiring) is integration work. The
  protocol pieces all exist and are unit-tested; the change is
  threading the `Transaction` state through the producer's send
  path and `BatchAccumulator`. Expect to touch `Producer`, the
  send loop in `Internal.ProducerSender`, the batch header
  stamping in `Internal.BatchAccumulator`, and add new
  end-to-end specs against the mock.
- 2, 4 are deep refactors of internal abstractions (engine driver,
  store transactional buffer). They cascade into existing tests; expect
  to update most of `Streams/MockDriverModesSpec` and
  `Streams/EngineSpec`.
- 3 (TLS) is largely fixture work + new test files.
- 5 is additive — new module hierarchy, no existing code changes.

---

## 5. Test strategy

Every implementation step lands with the following tiers:

1. **Unit / property tests** in `test/Client/*Spec.hs` or
   `test/Streams/*Spec.hs`. Pure-function tests for codec, builders,
   assignors, store invariants.
2. **Mock-broker integration tests** in `test/Client/MockBroker*Spec.hs`
   or `test/Streams/Mock*Spec.hs`. Drive end-to-end flows through
   `MockCluster` with injected faults.
3. **Live-broker smoke tests** (planned), gated behind
   `-fwith-live-broker` so CI without Docker still runs.
4. **Benchmark targets** (planned) using `tasty-bench`, guarded by
   `-fwith-benchmarks`.

For each item in §3, the listed "Tests" subsection becomes the spec for
the test work; a feature isn't considered done until those tests are
landed and green.

---

## 6. Risks & open questions

- **Engine driver abstraction shape.** Class-based vs record-of-IO. The
  former is more idiomatic but harder to mock without exposing instance
  internals; the latter is simpler but uglier. Decision deferred to the
  start of the runtime-rewire work.
- **State store transactional buffer.** RocksDB's `WriteBatch` is the
  natural fit, but we'd need to thread a batch identity through every
  store operation. The in-memory implementations need a parallel
  layer.
- **Schema Registry HTTP client.** We don't currently depend on `http-client`
  or `wreq`. Picking the dep is a one-way door; pin it to `http-client` +
  `tls` since we already pull `crypton-connection`.
- **Multi-broker test ergonomics.** Spinning N `MockCluster` instances
  and joining them is currently per-test boilerplate. A shared
  `withMockEnsemble` helper would reduce friction.

---

## 7. Tracking

Outstanding work is tracked in this document, not in GitHub issues, until
the project graduates beyond personal-fork status. When an item moves
from "Outstanding" to "Done" the corresponding section in §2 grows and
its mention in §3 is deleted, with a one-line PR reference left in the
commit message.

The current snapshot of in-progress / pending work is:

- **Top of the queue:** wire `Kafka.Client.Transaction` into
  `Kafka.Client.Producer` so that `produce` calls inside an
  `InTransaction` window actually become part of the broker-side
  transaction (§3.1). The protocol coordinator already exists and
  is unit-tested; this is integration work in the producer's send
  path. Until it lands, exactly-once is API-shaped only — not
  semantically real — and every Streams EOS-V2 / KIP-892 EOS-V3
  step is blocked.
- All other in-flight items from prior chats are landed; this branch
  brings the Pipeline implementation, AdminClient/TC/Subscribe codec
  exposure, the consumer-protocol-v0→v1 sticky-assignor fix, and the
  alive-idle Connection probe in line with the spec above.
