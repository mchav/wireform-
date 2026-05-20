# Kafka Concepts

A five-minute guide to the ideas behind Kafka. Read this before diving into the API.

## Topics are logs

A Kafka topic is an append-only log with a name. Producers write to the end; consumers read from wherever they last left off. Each record has a key, value, headers, and a broker-assigned offset.

Records stay on the broker according to your retention policy: hours, days, or forever.

## Partitions scale and order

Topics split into partitions. A 12-partition topic is 12 independent logs that share a name.

- **Scale**: More partitions means more parallel readers and writers.
- **Order**: Records within one partition are ordered. Records across partitions are not.

If you need a key's history ordered, use a key. The default partitioner sends all records with that key to the same partition.

## Brokers and clusters

A **broker** is a server storing partitions. A **cluster** is brokers working together. Partitions replicate across brokers (the *replication factor*) so one failure doesn't lose data.

Your client needs only one broker address to start: the **bootstrap broker**. The cluster tells it where the others are.

## Producers and consumers

- **Producers** write records. Long-lived: create at startup, use for the process lifetime.
- **Consumers** read records. Also long-lived; they maintain a session with the cluster.

These are independent. You can produce without ever consuming, and vice versa.

## Consumer groups share the work

Multiple consumers can split a topic's partitions among themselves using a **consumer group**.

- Two consumers in group `analytics`: each gets half the partitions.
- Two consumers in different groups: each gets all records (fan-out).
- One consumer crashes: after a timeout, its partitions move to survivors.

Pick a stable `groupId` per application.

## Offsets track progress

Each record has an **offset** within its partition. Consumers **commit** offsets to remember their position. On restart, they resume from the committed offset.

| Commit mode | Behavior | Use when |
|---|---|---|
| `CommitSync` | Commit after each handler | Smallest duplicate window |
| `CommitAsync` | Fire-and-forget | Better throughput, may reprocess |
| `CommitManual` | You call commit | Precise control needed |

Default is at-least-once: every record is processed at least once, but crashes may cause reprocessing.

## Transactions for exactly-once

Two related mechanisms:

**Idempotent producer**: Sequence numbers detect duplicates from retries. Enable with `producerIdempotent = True`.

**Transactional producer**: Groups multiple sends into an atomic unit. Combined with `commitOffsetsInTransaction`, gives exactly-once semantics. The consumer offset and producer output commit together.

## Batching and compression

Producers batch records targeting the same partition. Batches compress (`Zstd`, `Gzip`, `Snappy`, `Lz4`). On retriable errors, the producer retries with exponential backoff.

All tunable via `defaultProducerConfig`.

## Kafka Streams

Stateful processing on top of Kafka:

- **KStream**: Every record, in order (events).
- **KTable**: Latest value per key (materialized view).
- **Windowing**: Bucket records by time.
- **Joins**: Combine streams or tables on key.

The DSL uses the `HasSerde` typeclass to automatically resolve serdes for common types like `Text`, `Int64`, `Double`, `UUID`. You typically don't need to manually wire serdes unless using custom encodings. See [`streams/README.md`](./streams/README.md).

## Concept to code

| Concept | Haskell type | Module |
|---|---|---|
| Topic | `Text` | — |
| Producer | `Producer` | `Kafka.Client.Producer` |
| Consumer | `Consumer` | `Kafka.Client.Consumer` |
| Consumer group | `groupId` field | `Kafka.Client.Group` |
| Record | `ProducerRecord`, `ConsumerRecord` | `Kafka.Client.Producer/Consumer` |
| Partition and offset | `TopicPartition`, `Int64` | `Kafka.Client.Consumer` |
| Transaction | `Transaction` | `Kafka.Client.Transaction` |
| KStream/KTable | `KStream`, `KTable` | `Kafka.Streams` |

Read [`TUTORIAL.md`](./TUTORIAL.md) next for working code.
