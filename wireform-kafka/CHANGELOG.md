# Changelog for `wireform-kafka`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- **`Kafka.Errors`** — the exception hierarchy every public Kafka
  operation throws on failure. `KafkaException` carries a
  structured `KafkaErrorKind` (`ConnectError`,
  `AuthenticationError`, `TimeoutError`, `RecordTooLargeError !Int
  !Int`, `ProducerFencedError`, `ConfigurationError ![Text]`,
  `OffsetOutOfRangeError !Text !Int32 !Int64`, …) plus an optional
  underlying `SomeException` cause. Includes `isRetriable` /
  `isFatal` classifiers (KIP-487 shape) and an `orThrow :: IO
  (Either String a) -> IO a` bridge for migrating internal
  helpers. Mirrors `org.apache.kafka.common.errors.*` on the JVM.
- **`Kafka.Headers`** — typed wrapper around `Vector (Text,
  ByteString)`. Headers are a list on the wire but at the
  application level you almost always want random-access lookup
  and structured construction. The new `Headers` newtype preserves
  insertion order, supports O(n) lookup / O(1) append / O(n)
  replace, and has a `Semigroup`/`Monoid` instance.
- **`Kafka.Serde`** — `Serde a` plus the standard built-ins
  (`textSerde`, `int32Serde`, `int64Serde`, `doubleSerde`,
  `voidSerde`, `uuidSerde`, `jsonSerde`, …). Moved here from
  `Kafka.Streams.Serde`, which is now a thin re-export so
  existing `import Kafka.Streams.Serde` call sites keep working.
- **`Kafka.Topic`** — typed topic reference. `Topic k v` bundles
  a name + key serde + value serde. Smart constructors `topic`,
  `topicAny`, `bytesTopic`, `textTopic`.
- **`Kafka.Client.Producer.publish` / `publish_`** — typed sends
  that take a `Topic k v` and apply the topic's serdes
  automatically. Saves the typical `encodeUtf8` boilerplate at
  every send site.
- **`Kafka.Client.Producer.producerHealthy`** and
  **`Kafka.Client.Consumer.consumerHealthy`** — cheap in-process
  health probes suitable for a Kubernetes `livenessProbe`.
  `producerHealthy` returns `False` if the sender thread died;
  `consumerHealthy` returns `False` if the group's heartbeat
  thread died. Neither call contacts the broker.
- **`Kafka.Client.AdminClient.ensureTopic`** — idempotent topic
  creation. Calls `createTopics` for the supplied topic and
  treats the broker-side `TOPIC_ALREADY_EXISTS` (error code 36)
  as success.
- Runnable client examples under **`examples/Kafka/Client/Examples/`**:
  `Produce`, `ProduceTyped`, `Consume`, `Group`, `Transaction`.
  All wired to a `wireform-kafka-client-examples` cabal
  executable: `cabal run wireform-kafka-client-examples produce`.
- `OverloadedRecordDot` is now in the cabal common-defaults, with
  hand-written `HasField` instances on `ConsumerRecord`,
  `ProducerRecord`, `RecordMetadata`, `TopicPartition` that map
  the prefixed selectors (`crKey`, `recordValue`, `metadataTopic`,
  `tpTopic`, …) to bare-name dot accessors. Callers can now write
  `rec.key` / `rec.value` / `rec.partition` / `md.offset` without
  any new imports; the original prefixed selectors continue to
  work unchanged.

### Changed

- **`defaultProducerConfig` now matches the Java 3.x producer
  defaults out of the box: idempotent producer is **ON**, acks
  are **all** (`producerDelivery = ExactlyOnce`), and
  `max.in.flight.requests.per.connection` is 5. Strongest
  single-producer delivery guarantees (no duplicates, no
  reordering) are the default; callers who specifically need
  lower latency can downgrade to `AtLeastOnce` / `AtMostOnce`
  explicitly. **Behavioural caveat:** creating a producer now
  triggers an additional `InitProducerId` round-trip with the
  coordinator.
- **`withProducer` / `withConsumer` / `withAdminClient` /
  `withGroupConsumer`** throw `Kafka.Errors.KafkaException` on
  setup failure instead of the previous generic `IOError`. The
  structured error kind lets callers pattern-match on
  `ConnectError` / `ConfigurationError` / etc. instead of
  grepping error strings.
- **`Kafka.Client.Producer.sendMessageAsync`** now returns `IO
  (MVar (Either String RecordMetadata))` instead of `IO (Either
  String ())`. Callers wait on the broker result by taking the
  `MVar` whenever they're ready:

  ```haskell
  handle <- sendMessageAsync p "events" Nothing "hello"
  ... do other work ...
  md <- takeMVar handle
  ```

  The producer's interceptor + onAcknowledgement hooks still fire
  on the sender thread.
- **`Kafka.Client.Producer` fire-and-forget rename**: the `Drop`
  family now uses the trailing-underscore convention idiomatic
  with `Control.Monad.forM_`:
  `sendMessageDrop` → `sendMessage_`,
  `sendMessageDropUnsafe` → `sendMessageUnsafe_`,
  `sendMessageDropFastest` → `sendMessageFastest_`,
  `sendMessagesDrop` → `sendMessages_`.
- **`Kafka.Client.Producer.withProducer`** and `withProducer'` —
  `Control.Exception.bracket` wrappers that open a producer, run a
  body, and flush + close on exit even when the body throws.
  Setup failures surface as `KafkaException` so they participate
  in the usual bracket / catch / restart idioms instead of being
  a returned `Either`.
- **`Kafka.Client.Consumer.withConsumer`** and `withConsumer'` — the
  consumer analogue. Optionally subscribes to a topic list as part
  of the bracket; on exit calls `closeConsumerWithTimeout` (or a
  user-supplied shutdown). Setup failures surface as
  `KafkaException`.
- New **`CONCEPTS.md`** — a five-minute, plain-language Kafka
  primer covering topics, partitions, brokers, producers,
  consumers, consumer groups, offsets, transactions, and streams,
  each mapped to the type / function in this library.
- The `Kafka` umbrella module now re-exports `Kafka.Client.Group`
  so `runConsumer` / `runBatchedConsumer` / `withGroupConsumer` /
  `defaultGroupConfig` are reachable as `Kafka.*` without a
  separate import.

### Removed

- The five `Kafka.Client.*Extras` modules are gone. Every utility
  they exposed is consolidated into the module it naturally
  belongs in. No functional change; just a cleaner module
  structure.

  | from | to |
  |---|---|
  | `Kafka.Client.AdminExtras.defaultAdminApiTimeoutMs` | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.TopicCreateDefaults` (+ `defaultTopicCreateDefaults`) | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.NullKeyCompactionPolicy` (+ `defaultNullKeyCompactionPolicy`) | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.admin*LatencyMs` constants | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.HostResolver` | `Kafka.Network.Connection` |
  | `Kafka.Client.AdminExtras.PerPartitionFetchKnob` | `Kafka.Client.Consumer` |
  | `Kafka.Client.AdminExtras.SslEngineFactory` | dropped (was a degenerate `newtype` wrapping `IO ()` with no callers) |
  | `Kafka.Client.ConsumerExtras` (all of it) | `Kafka.Client.Consumer` |
  | `Kafka.Client.ConnectionExtras` (all of it) | `Kafka.Network.Connection` |
  | `Kafka.Client.ProducerExtras.EnhancedCallback` (+ `noopEnhancedCallback` / `dispatchEnhanced`) | `Kafka.Client.Producer` |
  | `Kafka.Client.ProducerExtras.transactionalIdOptional` | `Kafka.Client.Transaction` |
  | `Kafka.Client.ProducerExtras.TxnErrorRecovery` (+ `classifyTxnError`) | `Kafka.Client.Transaction` |
  | `Kafka.Client.ProducerExtras.TxnDeadline` (+ `effectiveTxnDeadlineMs`) | `Kafka.Client.Transaction` |
  | `Kafka.Client.ShareGroupExtras` (all of it) | `Kafka.Client.ShareConsumer` |

  The five unit-test specs that exercised them are renamed to
  reflect what they're actually testing:
  `Client.AdminExtrasSpec` → `Client.AdminClientConfigSpec`,
  `Client.ConsumerExtrasSpec` → `Client.ConsumerSnapshotsSpec`,
  `Client.ConnectionExtrasSpec` → `Network.ConnectionHelpersSpec`,
  `Client.ProducerExtrasSpec` → `Client.TransactionHelpersSpec`,
  `Client.ShareGroupExtrasSpec` → `Client.ShareConsumerHelpersSpec`.

- `Kafka.Client.Simple` is gone. It was a single-broker,
  single-record helper used as a low-level reference for the
  protocol; everything it did is already covered by the
  higher-level modules:
  - `Simple.createSimpleClient` / `closeSimpleClient` —
    use `Kafka.Network.Connection.connect` /
    `Kafka.Network.Connection.disconnect` (or
    `Conn.withConnection`).
  - `Simple.getMetadata` — use `Kafka.Client.AdminClient.listTopics`
    or `Kafka.Client.AdminClient.describeTopics` (and the
    `MetadataCache` is automatically populated by
    `createProducer` / `createConsumer`).
  - `Simple.produceSimple` — use `Kafka.Client.Producer.sendMessage`.
  - `Simple.fetchSimple` — use `Kafka.Client.Consumer.assign` +
    `Kafka.Client.Consumer.poll`.
  The integration suite has been migrated; only one orphan unit
  test in the old `test/Integration/BasicSpec.hs` referenced it,
  and that file wasn't wired into any test target — it has been
  removed too.
- `Kafka.Telemetry.TraceContext` is gone. It was a hand-rolled
  W3C Trace Context codec that existed because the previous
  `Kafka.Telemetry.OpenTelemetry` module didn't actually use the
  OpenTelemetry SDK; that's been replaced by real
  `hs-opentelemetry-api` usage (see *Changed* below), so the
  hand-rolled codec is now redundant.
- The earlier seven dead span / metric stubs on
  `Kafka.Telemetry.OpenTelemetry` (`createProducerSpan`,
  `createConsumerSpan`, `createTransactionSpan`,
  `recordMessageSent`, `recordMessageReceived`,
  `recordRequestDuration`, `recordBatchSize`) are gone with this
  rewrite.

### Changed

- `Kafka.Telemetry.OpenTelemetry` is rewritten on top of
  [`hs-opentelemetry-api`](https://hackage.haskell.org/package/hs-opentelemetry-api).
  `Tracer`s are real `OTel.Tracer`s, spans are real `OTel.Span`s,
  and the propagator that ships with the configured
  `TracerProvider` (W3C Trace Context by default in the SDK) is
  the one we delegate to for header inject / extract. New
  exports: `kafkaTracer`, `kafkaInstrumentationLibrary`,
  `inProducerSpan`, `inConsumerSpan`, `inTransactionSpan`,
  `producerSpanArguments`, `consumerSpanArguments`,
  `transactionSpanArguments`, `injectIntoProducerHeaders`,
  `extractFromConsumerHeaders`, `tracingProducerInterceptor`.

### Changed

- The 20 `Kafka.Streams.DSL.*` modules are renamed to
  `Kafka.Streams.*` — `Kafka.Streams.DSL.KStream` →
  `Kafka.Streams.KStream`, etc. The `DSL` segment was a
  category, not a domain object, and sat between
  `Kafka.Streams` and the actual user-facing types
  (`KStream`, `KTable`, `StreamsBuilder`, `Consumed`,
  `Produced`, …). Imports of the umbrella `Kafka.Streams`
  module are unchanged; direct imports of
  `Kafka.Streams.DSL.X` must be rewritten to
  `Kafka.Streams.X`.
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
