# Changelog for `wireform-kafka`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

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
