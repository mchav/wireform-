# Kafka concepts — plain-language primer

You don't need to know how Kafka works internally to use
`wireform-kafka`, but a handful of terms come up everywhere in
the API and the docs. This is a five-minute tour.

## A topic is a durable append-only log

Think of a Kafka topic as a log file with a name. Producers
append records to the end; consumers read records out from
wherever they last were. Records are bytes (key, value,
headers) plus a broker-assigned offset and timestamp. Kafka
remembers everything you've appended until your retention
policy says otherwise — most clusters keep records for hours,
days, or weeks; some keep them forever.

Because the log is append-only and the broker remembers
where every record landed, two consumers can read the same
topic from different positions completely independently.

## A topic is sharded into partitions

A topic with one partition is a single log. A topic with three
partitions is three independent logs that happen to share a
name. Each record goes into exactly one partition — either the
one you specify, the one chosen by hashing the record's key,
or the one the current "sticky" run is using.

Two reasons partitions matter:

  * **Throughput.** More partitions means more parallel
    producers writing and more parallel consumers reading. A
    single partition is the unit of parallelism.
  * **Ordering.** Records inside one partition are ordered by
    offset; records across partitions are not. If you need the
    same key's history to always be ordered, give it a key —
    the default partitioner will put every record with that
    key on the same partition.

In this library, the broker assigns the offset; you mostly
don't pick partitions explicitly — you let the partitioner do
its thing.

## Brokers are the servers; the cluster is the set of them

A broker is a process that holds partitions on disk and serves
read / write requests. A Kafka cluster is one or more brokers
working together. Every topic's partitions are replicated
across some number of brokers (the *replication factor*) so a
single broker dying doesn't lose data.

Your client only needs to know one broker to start with — the
*bootstrap broker* (the `bootstrapBrokers` / `connectAddrs`
field in this library). The cluster will tell the client where
every other broker lives.

## Producers append; consumers read

  * A **producer** is the writer side. You open one
    (`withProducer`), hand it records, and it batches them up
    and ships them to the right broker for the right partition.
    Producers are long-lived: a typical service opens one at
    startup and uses it for the lifetime of the process.

  * A **consumer** is the reader side. You open one, tell it
    which topics you want, and it asks the broker for the
    next batch of records. Consumers are also long-lived;
    they keep a session with the broker so they don't have to
    re-discover their partitions on every poll.

The two are completely independent: you can run a producer
without ever running a consumer, and vice versa.

## Consumer groups: how N consumers share a topic

If you ran two copies of your consumer service against the
same topic, you'd get every record twice — once per copy.
That's almost never what you want.

A **consumer group** is a way for multiple consumer instances
to share the partitions of a topic among themselves. Each
member of the group gets some subset of the partitions; the
broker rebalances when a member joins or leaves. Two members
of the same group never see the same record.

  * Two consumers in group `my-service` → each gets half the
    partitions.
  * Two consumers in different groups → each gets every
    record (parallel pipelines).
  * One consumer in `my-service`, but the consumer crashes →
    the broker waits out the session timeout and reassigns
    its partitions to whoever's left.

In this library, "consumer group" appears as the `groupId`
parameter / field. Pick a stable string per logical
application.

## Offsets: where you got to

Every record has an offset within its partition (just a
counter). When a consumer reads records, it eventually
**commits** the offset of the last record it processed. On a
crash + restart, the consumer resumes from the committed
offset rather than from the start (or end) of the partition.

You control when offsets are committed:

  * `CommitSync` (default in `Kafka.Client.Group`) — commit
    after each successful handler call. Smallest possible
    duplicate window on a crash.
  * `CommitAsync` — fire-and-forget commit, higher
    throughput, may reprocess a few seconds of records if
    you crash before the broker acks the commit.
  * `CommitManual` — you call `commitSync` / `commitAsync`
    yourself when you want.

"At least once" delivery is the default: a record is
guaranteed to be handled at least once, but a crash between
the handler returning and the commit may cause it to be
handled again. "At most once" is the inverse (commit first,
then process). "Exactly once" requires transactions — see
below.

## Transactions and idempotence

Two related ideas:

  * **Idempotent producer** — a producer that stamps every
    record with a sequence number so the broker can detect
    duplicates from retries. Enables "at least once" without
    duplicates on the producer side. In this library, set
    `producerIdempotent = True`.

  * **Transactional producer** — a producer that can group
    multiple sends across multiple partitions into one
    atomic write. Either all of them land or none of them
    do. Combined with `commitOffsetsInTransaction`, this
    gives end-to-end "exactly once": the consumer's offset
    commit and the producer's output are atomic. See
    "Kafka.Client.Transaction".

## Compression, batching, retries

A producer doesn't ship each record over the wire on its own;
it batches records that target the same partition and ships
the batch when it's full or `linger.ms` elapses. The batch
can be compressed (`Zstd` / `Gzip` / `Snappy` / `Lz4`). On a
retriable error (e.g. broker temporarily not the leader for
that partition) the producer retries with exponential
backoff up to `retries` attempts or `deliveryTimeoutMs`
elapses, whichever comes first.

All of this is on by default with sensible knobs; you tune
it from `defaultProducerConfig`.

## Streams: stateful processing on top

Reading records, doing some logic, and writing the result
back is what most Kafka apps are; doing it with windowed
aggregations, joins, and exactly-once side effects is what
Kafka Streams is for. The `Kafka.Streams` module ports the
Java DSL one combinator at a time:

  * `KStream` — a stream of records.
  * `KTable` — a stream of latest-value-per-key.
  * `groupByKey` + `aggregate` / `reduce` / `count` —
    rolling aggregations.
  * `windowedBy` — bucket records by time.
  * `join` — combine two streams or two tables on key.

The Streams runtime is built on top of the same client. See
[`streams/README.md`](./streams/README.md).

## Mapping to this library

| Kafka concept | Type / function | Module |
|---|---|---|
| Topic | `Text` | n/a (use the string name) |
| Producer | `Producer` | `Kafka.Client.Producer` |
| Consumer | `Consumer` | `Kafka.Client.Consumer` |
| Consumer group | `groupId` field on `GroupConfig` / `ConsumerConfig` | `Kafka.Client.Group` / `Kafka.Client.Consumer` |
| Group runner | `runConsumer` / `runBatchedConsumer` | `Kafka.Client.Group` |
| Record (key / value / headers) | `ProducerRecord`, `ConsumerRecord` | `Kafka.Client.Producer`, `Kafka.Client.Consumer` |
| Partition + offset | `TopicPartition`, `Int64` offset | `Kafka.Client.Consumer` |
| Transaction | `Transaction` | `Kafka.Client.Transaction` |
| Stream topology | `KStream`, `KTable`, `Topology` | `Kafka.Streams` |
| Cluster admin | `AdminClient` | `Kafka.Client.AdminClient` |

Read [`TUTORIAL.md`](./TUTORIAL.md) next for an end-to-end
walkthrough that exercises each of these.
