# Java SDK Parity

Mapping Apache Kafka 4.0 Java SDK to wireform-kafka Haskell equivalents.

**Legend:** Direct equivalent, Partial or different shape, Not implemented

## Producer

| Java | Status | Haskell |
|---|---|---|
| `KafkaProducer` | Direct | `Producer`, `createProducer`, `withProducer` |
| `MockProducer` | Direct | `MockProducer` |
| `ProducerRecord` | Direct | `ProducerRecord` |
| `RecordMetadata` | Direct | `RecordMetadata` |
| `send()` | Direct | `sendMessage`, `sendMessageAsync` |
| `flush()` | Direct | `flushProducer` |
| `purge()` | Direct | `purgeProducer` |
| `partitionsFor()` | Direct | `partitionsFor` |
| `metrics` | Direct | `Kafka.Telemetry.Metrics` |
| `initTransactions` | Direct | `initTransactions` |
| `beginTransaction` | Direct | `beginTransaction` |
| `commitTransaction` | Direct | `commitTransaction` |
| `abortTransaction` | Direct | `abortTransaction` |
| `sendOffsetsToTransaction` | Direct | `commitOffsetsInTransaction` |

## Consumer

| Java | Status | Haskell |
|---|---|---|
| `KafkaConsumer` | Direct | `Consumer`, `withConsumer` |
| `MockConsumer` | Direct | `MockConsumer` |
| `ConsumerRecord` | Direct | `ConsumerRecord` |
| `subscribe()` | Direct | `subscribe` |
| `assign()` | Direct | `assign` |
| `poll()` | Direct | `poll`, `pollRecords` |
| `commitSync`, `commitAsync` | Direct | `commitSync`, `commitAsync`, `commitSyncOffsets`, `commitAsyncCallback` |
| `seek`, `seekToBeginning`, `seekToEnd` | Direct | Same names |
| `position`, `committed` | Direct | Same names |
| `pause`, `resume` | Direct | Same names |
| `wakeup()` | Direct | `wakeupConsumer` |
| `group.id` | Direct | `consumerGroupId` |
| `auto.offset.reset` | Direct | `consumerAutoOffsetReset` |
| `enable.auto.commit` | Direct | `consumerAutoCommit` |
| `max.poll.records` | Direct | `consumerMaxPollRecords` |
| `partition.assignment.strategy` | Direct | `consumerAssignmentStrategy` |

## Admin Client

| Java | Status | Haskell |
|---|---|---|
| `createTopics` | Direct | `createTopics`, `ensureTopic` |
| `deleteTopics` | Direct | `deleteTopics` |
| `listTopics` | Direct | `listTopics` |
| `describeTopics` | Direct | `describeTopics` |
| `describeCluster` | Direct | `describeCluster` |
| `describeConfigs`, `alterConfigs` | Direct | Same names |
| `listConsumerGroups`, `describeConsumerGroups` | Direct | Same names |
| `createAcls`, `describeAcls`, `deleteAcls` | Direct | Same names |
| `createPartitions` | Direct | `createPartitions` |
| `alterPartitionReassignments` | Direct | Same name |
| `DelegationToken` ops | Direct | Same names |
| `addRaftVoter`, `removeRaftVoter` | Direct | Same names |

## Streams

| Java | Haskell |
|---|---|
| `KafkaStreams` | `KafkaStreams` / `newKafkaStreams` |
| `StreamsBuilder` | `StreamsBuilder` |
| `Topology` | `Topology` |
| `KStream` | `KStream` |
| `KTable` | `KTable` |
| `GlobalKTable` | `GlobalKTable` |
| `filter`, `filterNot` | `filterStream`, `filterNotStream` |
| `map`, `mapValues` | `mapKeyValue`, `mapValues` |
| `flatMap`, `flatMapValues` | `flatMapKeyValue`, `flatMapValues` |
| `selectKey()` | `selectKey` |
| `peek()` | `peekStream` |
| `foreach()` | `foreachStream` |
| `merge()` | `mergeStreams` |
| `join()` variants | `joinKStreamKStream`, `joinKStreamKTable`, `joinKTableKTable`, etc. |
| `groupBy()` / `groupByKey()` | `groupByStream` / `groupByKey` |
| `count()` / `reduce()` / `aggregate()` | `countStream` / `reduceStream` / `aggregateStream` |
| `windowedBy()` | `windowedByTime` / `windowedBySession` |
| `suppress()` | `suppressKStream` / `suppressWindowed` |
| `Stores.*` | `Stores` module |
| `Processor` / `ProcessorContext` | Same names |
| `Punctuator` | `Punctuator` |

## Serialization

| Java | Haskell |
|---|---|
| `Serde<T>` | `Serde` |
| `Serdes.String` | `textSerde`, `utf8Serde` |
| `Serdes.Integer`, `Long`, `Short` | `int16Serde`, `int32Serde`, `int64Serde` |
| `Serdes.Float`, `Double` | `floatSerde`, `doubleSerde` |
| `Serdes.ByteArray` | `byteArraySerde` |
| `Serdes.UUID` | `uuidSerde` |
| `Serdes.Void` | `voidSerde` |
| `HasSerde` (implicit) | Automatic serde resolution via `HasSerde` typeclass |

## Errors

Java has many exception classes; Haskell uses `KafkaException` with `KafkaErrorKind`:

| Java | Haskell |
|---|---|
| `TimeoutException` | `TimeoutError` |
| `AuthorizationException` | `AuthorizationError` |
| `AuthenticationException` | `AuthenticationError` |
| `ProducerFencedException` | `ProducerFencedError` |
| `OffsetOutOfRangeException` | `OffsetOutOfRangeError` |

## Not implemented

1. Some niche Admin operations (mostly at protocol layer but not wrapped)
2. KIP-714 telemetry IDs (local deterministic IDs; broker-assigned when RPC lands)
3. Full Java metrics framework (we use smaller registry + OpenTelemetry)
4. Some Consumer overload timeout variants; use async and STM
5. Some specific exception classes (covered by `KafkaErrorKind` constructors)

Core producer/consumer/streams DSL is at full parity.
