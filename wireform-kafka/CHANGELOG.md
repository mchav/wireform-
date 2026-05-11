# Changelog for `wireform-kafka`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- **`Kafka.Client.Producer.withProducer`** and `withProducer'` —
  `Control.Exception.bracket` wrappers that open a producer, run a
  body, and flush + close on exit even when the body throws. Setup
  failures surface as `IOError` so they participate in the usual
  bracket / catch / restart idioms instead of being a returned
  `Either`.
- **`Kafka.Client.Consumer.withConsumer`** and `withConsumer'` — the
  consumer analogue. Optionally subscribes to a topic list as part
  of the bracket; on exit calls `closeConsumerWithTimeout` (or a
  user-supplied shutdown).
- New **`CONCEPTS.md`** — a five-minute, plain-language Kafka
  primer covering topics, partitions, brokers, producers,
  consumers, consumer groups, offsets, transactions, and streams,
  each mapped to the type / function in this library.
- The `Kafka` umbrella module now re-exports `Kafka.Client.Group`
  so `runConsumer` / `runBatchedConsumer` / `withGroupConsumer` /
  `defaultGroupConfig` are reachable as `Kafka.*` without a
  separate import.

### Removed

- `Kafka.Telemetry.OpenTelemetry` no longer exports the seven
  span/metric stubs (`createProducerSpan`, `createConsumerSpan`,
  `createTransactionSpan`, `recordMessageSent`,
  `recordMessageReceived`, `recordRequestDuration`,
  `recordBatchSize`). They were `return ()` placeholders with no
  implementation and zero callers anywhere in the repo; the
  README and module docstring previously claimed
  "OpenTelemetry spans + metrics" support that wasn't actually
  there. The module's real purpose — SDK-independent W3C Trace
  Context propagation over Kafka record headers — is unchanged
  and now documented honestly.

### Changed

- `Kafka.Client.Group.GroupConfig` and `GroupConsumer` field
  selectors lose the `gc` prefix — `gcBootstrapBrokers` →
  `bootstrapBrokers`, `gcGroupId` → `groupId`, `gcTopics` →
  `topics`, and so on through every field. This is a breaking
  change for code that constructs `GroupConfig` by record-update
  syntax; the fix is a straight find-and-replace.
- `Kafka.Client.Producer` and `Kafka.Client.Consumer` module
  docstrings have been rewritten as a friendly orientation:
  what's in the module, the recommended starting point, how to
  pick between near-duplicate variants, where the config knobs
  live. Their export lists are reorganised into labelled sections
  so the everyday calls float above the more advanced surface.
- `Kafka.Client.Simple`'s docstring now opens with an explicit
  "this is not the module beginners should reach for, despite the
  name" callout — that module is a single-broker single-record
  reference for conformance / protocol tooling, not a
  high-throughput client.
- README is rewritten as a beginner-aimed on-ramp: what the package
  does in two sentences, a "pick the layer you need" table, and
  concrete recipes for produce / consume (high level) / consume
  (custom poll loop) / streams.
- TUTORIAL.md is re-ordered to start from `withProducer` +
  `runConsumer` against a live broker and progressively reveal the
  mock cluster, transactions, streams, state stores, multi-instance
  routing, schema registry, and observability.

- **TLS offload support** for the broker connection layer
  (`Kafka.Network.TlsOffload`, new field
  `Kafka.Network.Connection.connTlsOffload`). When set, the
  client skips its own `crypton-connection` TLS handshake and
  routes every broker socket through a sidecar / Unix-domain
  socket / kTLS-style endpoint, regardless of the value of
  `connUseTls`. Four deployment shapes are supported out of the
  box:
  - `transparentTlsOffload` (kTLS / NLB / TPROXY — connect to
    the broker's advertised address, cipher work happens
    out-of-band);
  - `staticTlsOffload` (every broker connection routes to one
    fixed TCP or Unix-domain endpoint — the standard Envoy /
    `kafka-proxy` shape);
  - `perBrokerTlsOffload` (per-broker endpoint map — the
    standard stunnel-with-port-per-broker shape used against
    MSK);
  - `customTlsOffload` (arbitrary `OffloadBrokerKey -> IO
    (Maybe TlsOffloadEndpoint)` resolver — for service-mesh /
    control-plane-discovered sidecars).
  The connection pool is still keyed by the logical broker
  address so per-broker SASL state and request pipelining are
  preserved when several brokers fan in to the same sidecar
  socket. New test suite `Network.TlsOffloadSpec` covers each
  mode end-to-end with in-process TCP and UDS sidecars.
- `Kafka.Network.Connection.connectOffload`: public,
  retry-aware single-broker form of the offload path for
  callers that don't need a `ConnectionManager`.

### Changed

- Vendored Apache Kafka **4.0.0** protocol JSON schemas
  (`data/kafka-protocol-schemas/`) and regenerated every
  `Kafka.Protocol.Generated.*` module. Removes 4.1+-only message
  types (`AlterShareGroupOffsets*`, `DeleteShareGroupOffsets*`,
  `DescribeShareGroupOffsets*`, `StreamsGroupHeartbeat*`,
  `StreamsGroupDescribe*`) and renames `ListConfigResources*` back
  to `ListClientMetricsResources*` (its 4.0.0 spelling — same
  apiKey 74).
- `Kafka.Protocol.Codegen.Types.FieldSpec` parses `tag` from either
  a JSON number (post-4.0 trunk style) or a numeric string (the
  4.0.0 release ships `"tag": "0"`), so the codegen is tolerant of
  either upstream encoding.
- High-level client call sites that hand-built request records lost
  the post-4.0 fields they no longer need to populate
  (`apiVersionsRequestClusterId`/`NodeId` (KIP-1242),
  `topicProduceDataTopicId`/`offsetCommitRequestTopicTopicId`/
  `offsetFetchRequestTopicsTopicId`/`txnOffsetCommitRequestTopicTopicId`
  (KIP-516 / KIP-848 v6/v10 topic-id additions),
  `fetchPartitionHighWatermark` (KIP-405),
  `initProducerIdRequestEnable2Pc`/`KeepPreparedTxn` (KIP-939),
  `txnOffsetCommitRequestGenerationIdOrMemberEpoch` rename).
- Integration `docker-compose.yml` and the bench / tutorial docs
  bumped from `apache/kafka:3.7.0` / `kafka_2.13-3.9.2` references
  to `apache/kafka:4.0.0` / `kafka_2.13-4.0.0`.
