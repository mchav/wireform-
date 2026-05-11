# Config parity with librdkafka

This is the mapping between librdkafka's
[`CONFIGURATION.md`](https://github.com/confluentinc/librdkafka/blob/master/CONFIGURATION.md)
entries and the corresponding `wireform-kafka` config fields. Every
field has its librdkafka name in a comment in the source as well.

The defaults track librdkafka's where the JVM client agrees with
them; where they diverge (most notably `session.timeout.ms`, which
the JVM client widened to 45000 in Kafka 3.0 via KIP-735), we
follow the JVM-Kafka 3.x default so application behaviour matches
what users see in `kafka-console-consumer` / `kafka-console-producer`.

## Global / connection (`Kafka.Network.Connection.ConnectionConfig`)

| librdkafka                                  | Field                                      | Default              |
|----------------------------------------------|--------------------------------------------|----------------------|
| `client.id`                                  | `connClientId`                             | `"wireform-kafka"`   |
| `bootstrap.servers`                          | passed to `createProducer` / `createConsumer` | (none)            |
| `socket.timeout.ms`                          | `connReadTimeout`, `connWriteTimeout`      | 30s each            |
| `socket.connection.setup.timeout.ms`         | `connTimeout`                              | 10s                  |
| `request.timeout.ms`                         | `connRequestTimeoutMs`                     | 30000                |
| `socket.send.buffer.bytes`                   | `connSocketSendBuffer`                     | 0 (OS default)       |
| `socket.receive.buffer.bytes`                | `connSocketReceiveBuffer`                  | 0 (OS default)       |
| `socket.keepalive.enable`                    | `connSocketKeepalive`                      | False                |
| `socket.nagle.disable`                       | `connSocketNagleDisable`                   | False                |
| `socket.max.fails`                           | `connSocketMaxFails`                       | 1                    |
| `connections.max.idle.ms`                    | `connMaxIdleMs`                            | 540000 (9 min)       |
| `connections.max.reauth.ms`                  | `connMaxReauthMs`                          | 0 (disabled)         |
| `reconnect.backoff.ms`                       | `connRetryDelay`                           | 100                  |
| `reconnect.backoff.max.ms`                   | `connBackoffMaxMs`                         | 10000                |
| `message.max.bytes`                          | `connMessageMaxBytes`                      | 1000000              |
| `receive.message.max.bytes`                  | `connReceiveMessageMaxBytes`               | 100000000 (100 MiB)  |
| `topic.metadata.refresh.interval.ms`         | `connMetadataMaxAgeMs`                     | 900000 (15 min)      |
| `topic.metadata.refresh.fast.interval.ms`    | `connTopicMetadataRefreshFastIntervalMs`   | 250                  |
| `topic.metadata.refresh.sparse`              | `connTopicMetadataRefreshSparse`           | True                 |
| `broker.address.ttl`                         | `connBrokerAddressTtl`                     | 1000                 |
| `broker.address.family`                      | `connBrokerAddressFamily`                  | `BrokerAddressAny`   |
| `client.dns.lookup`                          | `connDnsLookup`                            | `DnsResolveCanonicalBootstrapServersOnly` |
| `security.protocol` / `ssl.*`                | `connUseTls`, `connTlsSettings`            | False                |
| `sasl.*`                                     | `connSasl`                                 | Nothing              |

## Producer (`Kafka.Client.Producer.ProducerConfig`)

| librdkafka                                  | Field                                      | Default              |
|----------------------------------------------|--------------------------------------------|----------------------|
| `client.id`                                  | `producerClientId`                         | `"kafka-native-producer"` |
| `acks` / `request.required.acks`             | `producerDelivery`                         | `AtLeastOnce` (1)    |
| `compression.type`                           | `producerCompression`                      | None                 |
| `compression.level`                          | `producerCompressionLevel`                 | Nothing              |
| `batch.size`                                 | `producerBatchSize`                        | 16384                |
| `linger.ms`                                  | `producerLingerMs`                         | 0                    |
| `max.in.flight.requests.per.connection`      | `producerMaxInFlight`                      | 5                    |
| `retries`                                    | `producerRetries`                          | 2147483647           |
| `retry.backoff.ms`                           | `producerRetryBackoffMs`                   | 100                  |
| `retry.backoff.max.ms`                       | `producerRetryBackoffMaxMs`                | 1000                 |
| (multiplier; librdkafka-internal)            | `producerRetryBackoffMultiplier`           | 2.0                  |
| (jitter; librdkafka-internal)                | `producerRetryBackoffJitter`               | 0.2                  |
| `delivery.timeout.ms` (KIP-91)               | `producerDeliveryTimeoutMs`                | 120000 (2 min)       |
| `request.timeout.ms`                         | `producerRequestTimeoutMs`                 | 30000                |
| `max.request.size` / `message.max.bytes`     | `producerMaxRequestSize`                   | 1048576 (1 MiB)      |
| `queue.buffering.max.messages`               | `producerQueueBufferingMaxMessages`        | 100000               |
| `queue.buffering.max.kbytes`                 | `producerQueueBufferingMaxKbytes`          | 1048576 (1 GiB)      |
| `transaction.timeout.ms`                     | `producerTransactionTimeoutMs`             | 60000                |
| `enable.gapless.guarantee`                   | `producerEnableGaplessGuarantee`           | False                |
| `sticky.partitioning.linger.ms`              | `producerStickyPartitioningLingerMs`       | 10                   |
| `partitioner`                                | `producerPartitioner`                      | `defaultPartitioner` (KIP-480 sticky) |
| `enable.idempotence`                         | `producerIdempotent`                       | False                |
| `transactional.id`                           | `producerTransactional`                    | Nothing              |

## Consumer (`Kafka.Client.Consumer.ConsumerConfig`)

| librdkafka                                  | Field                                      | Default              |
|----------------------------------------------|--------------------------------------------|----------------------|
| `client.id`                                  | `consumerClientId`                         | `"kafka-native-consumer"` |
| `group.id`                                   | `consumerGroupId`                          | `"default-group"`    |
| `group.instance.id` (KIP-345)                | `consumerGroupInstanceId`                  | Nothing              |
| `partition.assignment.strategy`              | `consumerAssignmentStrategy`               | `RangeAssignment`    |
| `session.timeout.ms`                         | `consumerSessionTimeoutMs`                 | 45000 (KIP-735)      |
| `heartbeat.interval.ms`                      | `consumerHeartbeatIntervalMs`              | 3000                 |
| `enable.auto.commit`                         | `consumerAutoCommit`                       | True                 |
| `auto.commit.interval.ms`                    | `consumerAutoCommitIntervalMs`             | 5000                 |
| `enable.auto.offset.store`                   | `consumerEnableAutoOffsetStore`            | True                 |
| `auto.offset.reset`                          | `consumerAutoOffsetReset`                  | `Latest`             |
| `max.poll.records`                           | `consumerMaxPollRecords`                   | 500                  |
| `max.poll.interval.ms` (KIP-256)             | `consumerMaxPollIntervalMs`                | 300000 (5 min)       |
| `isolation.level`                            | `consumerIsolationLevel`                   | `ReadUncommitted`    |
| `enable.partition.eof`                       | `consumerEnablePartitionEof`               | False                |
| `check.crcs`                                 | `consumerCheckCrcs`                        | True                 |
| `fetch.min.bytes`                            | `consumerFetchMinBytes`                    | 1                    |
| `fetch.max.bytes`                            | `consumerFetchMaxBytes`                    | 52428800 (50 MiB)    |
| `fetch.wait.max.ms`                          | `consumerFetchMaxWaitMs`                   | 500                  |
| `max.partition.fetch.bytes` / `fetch.message.max.bytes` | `consumerFetchMessageMaxBytes` | 1048576              |
| `fetch.error.backoff.ms`                     | `consumerFetchErrorBackoffMs`              | 500                  |
| `queued.max.messages.kbytes`                 | `consumerQueuedMaxMessagesKbytes`          | 65536                |
| `client.rack` (KIP-392)                      | `consumerRackId`                           | Nothing              |
| (connection-level knobs)                     | `consumerConnectionConfig`                 | `defaultConnectionConfig` |

## Retry / backoff curve

The producer threads its `producerRetry*` knobs into
`Kafka.Client.Internal.ProducerSender.RetryConfig`. The exposed
helper `nextRetryBackoffMs :: RetryConfig -> Int -> Int` computes
`min(retryBackoffMaxMs, retryBackoffMs * retryBackoffMultiplier^attempt)`
with deterministic (sin-based) jitter — the same shape the in-memory
mock uses (`Kafka.Client.Mock.Backoff.nextBackoffMs`). Tests can
reproduce the curve exactly because there's no PRNG.

| Field                            | librdkafka match           | Notes                       |
|----------------------------------|----------------------------|-----------------------------|
| `retryMaxAttempts`               | `retries`                  | 2147483647 by default       |
| `retryBackoffMs`                 | `retry.backoff.ms`         | 100                         |
| `retryBackoffMaxMs`              | `retry.backoff.max.ms`     | 1000                        |
| `retryBackoffMultiplier`         | (internal)                 | 2.0                         |
| `retryBackoffJitter`             | (internal)                 | 0.2 (deterministic)         |

## What's deliberately not exposed

These librdkafka knobs target wire/protocol behaviour we don't
yet implement, or are aliases:

- `client.dns.lookup=resolve_all_bootstrap_servers` (we always
  resolve all bootstrap servers).
- `socket.connection.setup.timeout.max.ms` (overridden by
  `connTimeout`).
- `socket.connection.setup.timeout.ms.alpha.factor` and friends
  (deprecated.).
- `internal.termination.signal` (process-level signal handling).
- `log_level` / `log.queue` / `log_cb` (logger plumbing).
- `metric.reporters` / `metrics.recording.level` (use
  `Kafka.Streams.Metrics` or your own counters).
- `auto.create.topics.enable` (broker-side; we expose
  `setAutoCreateTopics` on the mock for tests).
- `oauthbearer.method` / `oauthbearer.config` (wired through the
  `SASL.SaslOAuthBearer` config value rather than as flat strings).

If a missing knob matters for your workload, open an issue (or a
PR adding it to the relevant config record).
