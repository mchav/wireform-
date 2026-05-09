# Changelog for `kafka-native`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

#### Producer / Transaction wiring (KIP-98 / KIP-447)

- `Kafka.Client.Producer.bindTransaction` lets a
  `Kafka.Client.Transaction` value be attached to a `Producer` so
  that `sendMessage` participates in the broker-side transaction
  lifecycle: the gate rejects sends outside `InTransaction`,
  the first send to a new (topic, partition) issues
  `AddPartitionsToTxn`, and outgoing record batches are stamped
  with the transactional producer-id / epoch / sequence and have
  their `attrIsTransactional` bit set.
- `producerTxnGate` exposes the pure transactional-state gate.
- `Kafka.Client.Internal.BatchAccumulator.appendRecordStamped` +
  `BatchStamp` for explicit per-batch (producer-id, epoch,
  base-sequence, isTransactional) stamping.
- `ProducerBatch.batchIsTransactional`; consumed by
  `Kafka.Client.Internal.ProducerSender.buildRecordBatch` (now
  exported).
- `closeProducer` aborts an open transaction before shutdown.
- `test-integration/Integration/TransactionalSpec.hs`: live-broker
  end-to-end suite for commit / abort / fence /
  `sendOffsetsToTransaction` (gated by `WIREFORM_KAFKA_BROKER`).

#### Producer / consumer interceptors + observability

- `producerInterceptor` / `producerOnAcknowledgement` callbacks on
  `ProducerConfig` (KIP-388 / JVM `ProducerInterceptor` analogue).
- `consumerInterceptor` / `consumerOnCommit` callbacks on
  `ConsumerConfig`.
- `Kafka.Telemetry.StatsJson`: librdkafka-shaped statistics JSON
  (`StatsSnapshot`, `TopicStats`, `renderStats`).

#### Network / SASL / TLS

- `Kafka.Network.Transport`: pluggable byte-stream transport
  interface plus `mkTcpTransport` and an in-memory
  `mkPipeTransport` for tests.
- `effectiveReauthDeadlineMs` / `reauthRequiredAtMs` (KIP-368
  session re-authentication helpers).
- `test/Network/TlsHandshakeSpec.hs`: in-process `Network.TLS`
  server covering happy-path / KIP-235 hostname mismatch / mTLS
  / SNI forwarding, with self-signed fixtures under
  `test/Network/TLS/`.

#### Producer / Metadata / KIP-219 + KIP-466

- `processProduceResponse` honours the broker's `ThrottleTimeMs`
  (KIP-219) by sleeping the sender thread before the next batch.
- `Kafka.Client.Metadata.updatePartitionLeader` (KIP-466)
  patches the cached leader id from `ProduceResponse.CurrentLeader`
  in-place; the producer wires that automatically.

#### KIP-345 static membership

- `Consumer.StaticMembershipState`,
  `consumerStaticMembershipPersist`, `consumerStaticMembershipResume`,
  `currentStaticMembershipState`. The persist callback fires on
  `closeConsumer`; the resume hook seeds the heartbeat state.

#### Streams

- `StreamsConfig` gains `taskTimeoutMs`, `acceptableRecoveryLag`,
  `maxWarmupReplicas`, `probingRebalanceIntervalMs`,
  `taskAssignorClass` (KIP-892 + KIP-441 family); parsed by
  `streamsConfigFromMap`.
- `Kafka.Streams.Runtime.ProbingRebalance` (KIP-441 decision
  layer): `WarmupProgress`, `classifyWarmups`, `shouldProbe`.
- `Kafka.Streams.Runtime.RevocationGrace` (KIP-869):
  `classifyRevocation`, `planRevocation` returning
  `RevokeImmediate` or `KeepAsStandby deadline`.
- `Kafka.Streams.State.Transactional` (KIP-892 EOS-V3): a
  `TransactionalStore` overlay on top of any `KeyValueStore` with
  buffered puts / deletes, read-your-writes, `txnCommit` /
  `txnAbort`.
- `Kafka.Streams.Runtime.NativeDriver`: `StreamDriver`
  record-of-IO + `newNativeDriver` constructor that wires
  `Producer` + `Consumer` + bound `Transaction` for the
  KafkaStreams runtime.
- `Kafka.Streams.Runtime.MultiInstanceHarness`: pure scenario
  interpreter for failure-ordering Hedgehog tests.
- `Kafka.Streams.DSL.ForeignKeyJoinV2` (KIP-213): subscription
  message + responder data layer + pure FK-join state machine.
- `Kafka.Streams.Topology.Optimization` (KIP-295):
  `TopologyOptimizationLevel` toggles + `optimizationFlags`.
- `Kafka.Streams.Topology.StableNames` (KIP-307):
  per-`OperatorClass` deterministic name generator matching the
  JVM's `KSTREAM-FILTER-0000000000` shape.
- `Kafka.Streams.Serde.SchemaRegistry` (Confluent serdes):
  `SchemaRegistryClient` interface, `inMemoryRegistry`,
  `mockHttpRegistry`, `encodeEnvelope` /
  `decodeEnvelope`, `registrySerde` wrapper.

#### Tooling

- `TUTORIAL.md` walkthrough (mock cluster → producer → transaction
  → Streams DSL → KIP-892 → Schema Registry → stats JSON).
- `bench/Benchmarks/StatsAndStamping.hs`: Criterion benchmarks
  for the new stats JSON renderer + record-batch encoder hot path.

### Added (KIP audit batch)

In addition to everything above, this branch lands the high-impact
items from a top-to-bottom audit of accepted client KIPs against
the JVM client + librdkafka:

- **KIP-848 next-gen consumer protocol** —
  `Kafka.Client.ConsumerGroupV2` ships the `GroupMemberState`
  transition table + the pure `planHeartbeat` decision layer
  used by the new `ConsumerGroupHeartbeat` RPC.
- **KIP-932 share groups** — `Kafka.Client.ShareConsumer` is
  the high-level surface for queue-semantic consumption with
  `Accept` / `Release` / `Reject` per-record acks.
- **KIP-714 client telemetry push** — `Kafka.Telemetry.Push`
  carries the subscription state machine + the
  `planTelemetryStep` decision (refresh / push / sleep / done)
  that drives `GetTelemetrySubscriptions` + `PushTelemetry`.
- **Metrics framework (KIP-92 / 295 / 361 / 363 / 377 / 386 /
  522 / 565 / 597 / 613 / 700 / 959 / 1107 / 1178)** —
  `Kafka.Telemetry.Metrics` is the in-process registry +
  histogram backend, plus pre-named metric strings for every
  documented producer / consumer / admin path.
- **`statistics.interval.ms` emitter** — `Kafka.Client.StatsEmitter`
  forks a background thread that scrapes the metrics registry
  and renders the librdkafka-shaped JSON snapshot on the
  configured cadence.
- **KIP-247 / KIP-944 Future API** — `Kafka.Client.Future`
  carries `KafkaFuture` / `Promise` / `awaitFuture` / `thenFuture`
  on top of `TMVar`.
- **KIP-516 topic identifiers** — `Kafka.Client.TopicId` carries
  the UUID newtype + the bidirectional `TopicIdTable` resolution
  map.
- **KIP-415 / 429 rebalance listener** —
  `Kafka.Client.RebalanceListener` ships `RebalanceListener` with
  `onAssigned` / `onRevoked` / `onLost` callbacks + a
  combinator + an exception-swallowing dispatcher.
- **KIP-906 record filter** — `Kafka.Client.Filter` ships
  `RecordFilter` newtype + `byKeyEquals` / `byHeaderEquals` /
  `byTopicIn` / `<&&>` / `<||>` / `negateFilter`.
- **KIP-359 / 597 / 843 / 1054 / 1166 / 1218 record metadata** —
  `Kafka.Client.RecordMetadata` ships `HeaderSerde` (utf8 / bytes
  / double), `EnrichedRecord` with leaderEpoch, typed
  `ProducerError` ADT with `producerErrorMessage`.
- **KIP-540 / 918 / 919 admin timeouts + routing** —
  `Kafka.Client.AdminTimeouts` ships `AdminCallTimeout` /
  `AdminRouting` / `routeOperation` + the `AdminOperationKind`
  classifier so the AdminClient sends mutating ops to the
  controller / KRaft quorum and reads to any broker.
- **KIP-768 / 1169 OAuth/OIDC + PKCE** — `Kafka.Network.Auth.OAuthOidc`
  ships `OidcClientConfig`, `PkceVerifier` / `pkceChallenge`
  with `PkceS256` + `PkcePlain`, `OidcToken` with
  `tokenRefreshDeadlineMs` (75% rule), `TokenCache`, pluggable
  `OidcTokenFetcher`.
- **KIP-126 split oversized batches** —
  `Kafka.Client.Internal.BatchSplitting` ships `splitBatch` that
  halves a multi-record batch on `MESSAGE_TOO_LARGE` while
  preserving every metadata field.
- **KIP-487 / 1054 retry classifier** —
  `Kafka.Client.RetryClassifier` ships the canonical Kafka
  error-code → `Retriable` / `Abortable` / `Fatal` table + the
  human-readable `errorMessage` lookup.
- **KIP-107 DeleteRecords** — `Kafka.Client.DeleteRecords`
  ships the high-level helper.

### Changed

- `closeProducer` aborts an open transaction before shutdown
  (KIP-98 hygiene).
- `Kafka.Client.Internal.ProducerSender.buildRecordBatch` reads
  `batchIsTransactional` from the batch (previously hardcoded to
  `False`) and is exported.
- `KIP_TRACKING.md` — implemented-count climbed from ~40 to ~110
  on this branch; unimplemented count dropped from ~291 to ~140.

## 0.1.0.0 - YYYY-MM-DD
