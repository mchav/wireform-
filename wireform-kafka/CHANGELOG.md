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

### Changed

- `closeProducer` aborts an open transaction before shutdown
  (KIP-98 hygiene).
- `Kafka.Client.Internal.ProducerSender.buildRecordBatch` reads
  `batchIsTransactional` from the batch (previously hardcoded to
  `False`) and is exported.

## 0.1.0.0 - YYYY-MM-DD
