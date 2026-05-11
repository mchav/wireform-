# Config parity with librdkafka

This is the mapping between librdkafka's
[`CONFIGURATION.md`](https://github.com/confluentinc/librdkafka/blob/master/CONFIGURATION.md)
entries and the corresponding `wireform-kafka` config fields. Every
field has its librdkafka name in a comment in the source as well.

The defaults track librdkafka's where the JVM client agrees with
them; where they diverge (most notably `session.timeout.ms`, which
the JVM client widened to 45000 in Kafka 3.0), we
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
| `delivery.timeout.ms`                        | `producerDeliveryTimeoutMs`                | 120000 (2 min)       |
| `request.timeout.ms`                         | `producerRequestTimeoutMs`                 | 30000                |
| `max.request.size` / `message.max.bytes`     | `producerMaxRequestSize`                   | 1048576 (1 MiB)      |
| `queue.buffering.max.messages`               | `producerQueueBufferingMaxMessages`        | 100000               |
| `queue.buffering.max.kbytes`                 | `producerQueueBufferingMaxKbytes`          | 1048576 (1 GiB)      |
| `transaction.timeout.ms`                     | `producerTransactionTimeoutMs`             | 60000                |
| `enable.gapless.guarantee`                   | `producerEnableGaplessGuarantee`           | False                |
| `sticky.partitioning.linger.ms`              | `producerStickyPartitioningLingerMs`       | 10                   |
| `partitioner`                                | `producerPartitioner`                      | `defaultPartitioner` (sticky)        |
| `enable.idempotence`                         | `producerIdempotent`                       | False                |
| `transactional.id`                           | `producerTransactional`                    | Nothing              |

## Consumer (`Kafka.Client.Consumer.ConsumerConfig`)

| librdkafka                                  | Field                                      | Default              |
|----------------------------------------------|--------------------------------------------|----------------------|
| `client.id`                                  | `consumerClientId`                         | `"kafka-native-consumer"` |
| `group.id`                                   | `consumerGroupId`                          | `"default-group"`    |
| `group.instance.id`                          | `consumerGroupInstanceId`                  | Nothing              |
| `partition.assignment.strategy`              | `consumerAssignmentStrategy`               | `RangeAssignment`    |
| `session.timeout.ms`                         | `consumerSessionTimeoutMs`                 | 45000                |
| `heartbeat.interval.ms`                      | `consumerHeartbeatIntervalMs`              | 3000                 |
| `enable.auto.commit`                         | `consumerAutoCommit`                       | True                 |
| `auto.commit.interval.ms`                    | `consumerAutoCommitIntervalMs`             | 5000                 |
| `enable.auto.offset.store`                   | `consumerEnableAutoOffsetStore`            | True                 |
| `auto.offset.reset`                          | `consumerAutoOffsetReset`                  | `Latest`             |
| `max.poll.records`                           | `consumerMaxPollRecords`                   | 500                  |
| `max.poll.interval.ms`                       | `consumerMaxPollIntervalMs`                | 300000 (5 min)       |
| `isolation.level`                            | `consumerIsolationLevel`                   | `ReadUncommitted`    |
| `enable.partition.eof`                       | `consumerEnablePartitionEof`               | False                |
| `check.crcs`                                 | `consumerCheckCrcs`                        | True                 |
| `fetch.min.bytes`                            | `consumerFetchMinBytes`                    | 1                    |
| `fetch.max.bytes`                            | `consumerFetchMaxBytes`                    | 52428800 (50 MiB)    |
| `fetch.wait.max.ms`                          | `consumerFetchMaxWaitMs`                   | 500                  |
| `max.partition.fetch.bytes` / `fetch.message.max.bytes` | `consumerFetchMessageMaxBytes` | 1048576              |
| `fetch.error.backoff.ms`                     | `consumerFetchErrorBackoffMs`              | 500                  |
| `queued.max.messages.kbytes`                 | `consumerQueuedMaxMessagesKbytes`          | 65536                |
| `client.rack`                                | `consumerRackId`                           | Nothing              |
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

## Environment-variable overrides

`createProducer` and `createConsumer` automatically read the
standard `KAFKA_*` environment variables that the broader Kafka
ecosystem (Confluent's Docker images, `kcat`, the various
librdkafka-based language bindings, the JVM client when launched
from the `kafka-console-*` scripts, …) has converged on. Anything
the env supplies wins over the corresponding field on the config
record you passed in; anything the env leaves unset keeps the
code-supplied value. To opt out, ensure no `KAFKA_*` variables
are set in the process environment.

The convention is: take the librdkafka / JVM `CONFIGURATION.md`
property name, uppercase it, replace each `.` with `_`, and prefix
with `KAFKA_`:

| librdkafka                             | Env var                                          |
|----------------------------------------|--------------------------------------------------|
| `bootstrap.servers`                    | `KAFKA_BOOTSTRAP_SERVERS`                        |
| `client.id`                            | `KAFKA_CLIENT_ID`                                |
| `security.protocol`                    | `KAFKA_SECURITY_PROTOCOL`                        |
| `sasl.mechanism`                       | `KAFKA_SASL_MECHANISM`                           |
| `sasl.username` / `sasl.password`      | `KAFKA_SASL_USERNAME` / `KAFKA_SASL_PASSWORD`    |
| (JVM-style alias)                      | `KAFKA_SASL_PLAIN_USERNAME` / `_PASSWORD`        |
| `request.timeout.ms`                   | `KAFKA_REQUEST_TIMEOUT_MS`                       |
| `socket.timeout.ms`                    | `KAFKA_SOCKET_TIMEOUT_MS`                        |
| `socket.keepalive.enable`              | `KAFKA_SOCKET_KEEPALIVE_ENABLE`                  |
| `reconnect.backoff.ms`                 | `KAFKA_RECONNECT_BACKOFF_MS`                     |
| `reconnect.backoff.max.ms`             | `KAFKA_RECONNECT_BACKOFF_MAX_MS`                 |
| `connections.max.idle.ms`              | `KAFKA_CONNECTIONS_MAX_IDLE_MS`                  |
| `message.max.bytes`                    | `KAFKA_MESSAGE_MAX_BYTES`                        |
| `receive.message.max.bytes`            | `KAFKA_RECEIVE_MESSAGE_MAX_BYTES`                |
| `broker.address.family`                | `KAFKA_BROKER_ADDRESS_FAMILY`                    |
| `client.dns.lookup`                    | `KAFKA_CLIENT_DNS_LOOKUP`                        |
| `acks`                                 | `KAFKA_ACKS`                                     |
| `compression.type` / `level`           | `KAFKA_COMPRESSION_TYPE` / `KAFKA_COMPRESSION_LEVEL` |
| `batch.size`                           | `KAFKA_BATCH_SIZE`                               |
| `linger.ms`                            | `KAFKA_LINGER_MS`                                |
| `max.in.flight.requests.per.connection`| `KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION`    |
| `retries` / `retry.backoff.{ms,max.ms}`| `KAFKA_RETRIES` / `KAFKA_RETRY_BACKOFF_{MS,MAX_MS}` |
| `delivery.timeout.ms`                  | `KAFKA_DELIVERY_TIMEOUT_MS`                      |
| `max.request.size`                     | `KAFKA_MAX_REQUEST_SIZE`                         |
| `enable.idempotence`                   | `KAFKA_ENABLE_IDEMPOTENCE`                       |
| `transactional.id`                     | `KAFKA_TRANSACTIONAL_ID`                         |
| `transaction.timeout.ms`               | `KAFKA_TRANSACTION_TIMEOUT_MS`                   |
| `group.id` / `group.instance.id`       | `KAFKA_GROUP_ID` / `KAFKA_GROUP_INSTANCE_ID`     |
| `enable.auto.commit`                   | `KAFKA_ENABLE_AUTO_COMMIT`                       |
| `auto.commit.interval.ms`              | `KAFKA_AUTO_COMMIT_INTERVAL_MS`                  |
| `auto.offset.reset`                    | `KAFKA_AUTO_OFFSET_RESET`                        |
| `session.timeout.ms`                   | `KAFKA_SESSION_TIMEOUT_MS`                       |
| `heartbeat.interval.ms`                | `KAFKA_HEARTBEAT_INTERVAL_MS`                    |
| `max.poll.records`                     | `KAFKA_MAX_POLL_RECORDS`                         |
| `max.poll.interval.ms`                 | `KAFKA_MAX_POLL_INTERVAL_MS`                     |
| `isolation.level`                      | `KAFKA_ISOLATION_LEVEL`                          |
| `fetch.min.bytes` / `max.bytes`        | `KAFKA_FETCH_MIN_BYTES` / `KAFKA_FETCH_MAX_BYTES` |
| `fetch.wait.max.ms`                    | `KAFKA_FETCH_MAX_WAIT_MS` (also `KAFKA_FETCH_WAIT_MAX_MS`) |
| `max.partition.fetch.bytes`            | `KAFKA_MAX_PARTITION_FETCH_BYTES`                |
| `client.rack`                          | `KAFKA_CLIENT_RACK`                              |
| `partition.assignment.strategy`        | `KAFKA_PARTITION_ASSIGNMENT_STRATEGY`            |
| `check.crcs`                           | `KAFKA_CHECK_CRCS`                               |

The simplest path is to let `createProducer` / `createConsumer`
read the env automatically:

```haskell
import qualified Kafka as Kafka

main = do
  -- Picks up KAFKA_BOOTSTRAP_SERVERS / KAFKA_CLIENT_ID /
  -- KAFKA_SECURITY_PROTOCOL / KAFKA_SASL_* / KAFKA_ACKS / etc.
  Right p <- Kafka.createProducer [] Kafka.defaultProducerConfig
  ...
```

If the bootstrap-broker positional argument is empty and
`KAFKA_BOOTSTRAP_SERVERS` is set, the env value is used; if both
are set, the explicit positional value wins.

For callers that want to inspect or pre-apply the overlay
manually (e.g. to log the effective config), the same logic is
exposed through:

```haskell
import qualified Kafka.Client.Env as Env

Env.bootstrapServersFromEnv :: IO (Maybe [Text])
Env.loadKafkaEnv            :: IO (Either [ConfigError] KafkaEnv)
Env.applyKafkaEnvToConnectionConfig
                            :: KafkaEnv -> ConnectionConfig
                            -> Either [ConfigError] ConnectionConfig

-- Producer-/consumer-specific overlays live alongside their
-- config types to keep Env import-cycle-free:
Producer.applyKafkaEnvToProducerConfig
                            :: KafkaEnv -> ProducerConfig
                            -> Either [ConfigError] ProducerConfig
Consumer.applyKafkaEnvToConsumerConfig
                            :: KafkaEnv -> ConsumerConfig
                            -> Either [ConfigError] ConsumerConfig
```

`Env.parseKafkaEnvList` exposes the same parser against an
explicit `[(Text, Text)]` table for testing or for callers that
sniff the environment in a non-standard way.

Notes on the credential mechanisms:

* `KAFKA_SECURITY_PROTOCOL=SASL_PLAINTEXT` or `SASL_SSL` combined
  with `KAFKA_SASL_MECHANISM=PLAIN` / `SCRAM-SHA-256` /
  `SCRAM-SHA-512` plus `KAFKA_SASL_USERNAME` /
  `KAFKA_SASL_PASSWORD` constructs a `Conn.connSasl` for you.
* `OAUTHBEARER`, `AWS_MSK_IAM`, and `GSSAPI` need callbacks or
  out-of-band credentials, so we reject those env-var combos
  with a clear error and ask the caller to populate
  `Conn.connSasl` programmatically.
* When `KAFKA_SECURITY_PROTOCOL` requests TLS and the caller has
  not pre-populated `Conn.connTlsSettings`, the loader installs
  `Conn.defaultTlsSettings` keyed off the first
  `KAFKA_BOOTSTRAP_SERVERS` host so SNI / hostname verification
  has the right name to check.

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
