# Configuration

Mapping librdkafka settings to wireform-kafka Haskell fields.

## Connection

`Kafka.Network.Connection.ConnectionConfig`:

| librdkafka | Haskell | Default |
|---|---|---|
| `client.id` | `connClientId` | `"wireform-kafka"` |
| `bootstrap.servers` | Passed to `createProducer` and `createConsumer` | Required |
| `socket.timeout.ms` | `connReadTimeout`, `connWriteTimeout` | 30s |
| `socket.connection.setup.timeout.ms` | `connTimeout` | 10s |
| `request.timeout.ms` | `connRequestTimeoutMs` | 30000 |
| `socket.*.buffer.bytes` | `connSocketSendBuffer`, `connSocketReceiveBuffer` | 0 (OS) |
| `socket.keepalive.enable` | `connSocketKeepalive` | False |
| `reconnect.backoff.ms` | `connRetryDelay` | 100 |
| `reconnect.backoff.max.ms` | `connBackoffMaxMs` | 10000 |
| `topic.metadata.refresh.interval.ms` | `connMetadataMaxAgeMs` | 15 min |
| `security.protocol`, `ssl.*` | `connUseTls`, `connTlsSettings` | False |
| `sasl.*` | `connSasl` | Nothing |

## Producer

`Kafka.Client.Producer.ProducerConfig`:

| librdkafka | Haskell | Default | Purpose |
|---|---|---|---|
| `acks` | `producerDelivery` | `AtLeastOnce` (1) | Durability vs latency |
| `compression.type` | `producerCompression` | None | `None`, `Gzip`, `Snappy`, `Lz4`, `Zstd` |
| `compression.level` | `producerCompressionLevel` | Nothing | Codec-specific |
| `batch.size` | `producerBatchSize` | 16384 | Target batch bytes |
| `linger.ms` | `producerLingerMs` | 0 | Wait time for batching |
| `max.in.flight.requests.per.connection` | `producerMaxInFlight` | 5 | Pipelined requests |
| `retries` | `producerRetries` | 2147483647 | Retry attempts |
| `retry.backoff.ms` | `producerRetryBackoffMs` | 100 | Initial retry delay |
| `delivery.timeout.ms` | `producerDeliveryTimeoutMs` | 2 min | Max wait for delivery |
| `enable.idempotence` | `producerIdempotent` | False | Exactly-once per partition |
| `transactional.id` | `producerTransactional` | Nothing | Enable transactions |
| `transaction.timeout.ms` | `producerTransactionTimeoutMs` | 60000 | Coordinator timeout |
| `partitioner` | `producerPartitioner` | `defaultPartitioner` | Partition selection |

## Consumer

`Kafka.Client.Consumer.ConsumerConfig`:

| librdkafka | Haskell | Default | Purpose |
|---|---|---|---|
| `group.id` | `consumerGroupId` | `"default-group"` | Group membership |
| `session.timeout.ms` | `consumerSessionTimeoutMs` | 45000 | Eviction timeout |
| `heartbeat.interval.ms` | `consumerHeartbeatIntervalMs` | 3000 | Heartbeat frequency |
| `enable.auto.commit` | `consumerAutoCommit` | True | Auto commits |
| `auto.commit.interval.ms` | `consumerAutoCommitIntervalMs` | 5000 | Commit frequency |
| `auto.offset.reset` | `consumerAutoOffsetReset` | `Latest` | Start position |
| `max.poll.records` | `consumerMaxPollRecords` | 500 | Records per poll |
| `max.poll.interval.ms` | `consumerMaxPollIntervalMs` | 5 min | Poll timeout |
| `isolation.level` | `consumerIsolationLevel` | `ReadUncommitted` | Transactional reads |
| `fetch.min.bytes` | `consumerFetchMinBytes` | 1 | Minimum fetch data |
| `fetch.max.bytes` | `consumerFetchMaxBytes` | 50 MiB | Maximum fetch data |
| `partition.assignment.strategy` | `consumerAssignmentStrategy` | `RangeAssignment` | Partition assignment |

## Retry backoff

Exponential backoff formula:
```
min(retryBackoffMaxMs, retryBackoffMs * multiplier^n)
```

With deterministic jitter (sine-based for reproducibility).

| Field | Default |
|---|---|
| `retryMaxAttempts` | 2147483647 |
| `retryBackoffMs` | 100 |
| `retryBackoffMaxMs` | 1000 |
| `retryBackoffMultiplier` | 2.0 |
| `retryBackoffJitter` | 0.2 |

## Environment variables

Any `KAFKA_*` variable overrides the corresponding config field:

| librdkafka | Environment variable |
|---|---|
| `bootstrap.servers` | `KAFKA_BOOTSTRAP_SERVERS` |
| `client.id` | `KAFKA_CLIENT_ID` |
| `acks` | `KAFKA_ACKS` |
| `compression.type` | `KAFKA_COMPRESSION_TYPE` |
| `group.id` | `KAFKA_GROUP_ID` |
| `session.timeout.ms` | `KAFKA_SESSION_TIMEOUT_MS` |

Explicit config in code takes precedence over environment.

## Programmatic access

```haskell
import qualified Kafka.Client.Env as Env

mbootstrap <- Env.bootstrapServersFromEnv
env <- Env.loadKafkaEnv
let config' = Env.applyKafkaEnvToConnectionConfig env defaultConnectionConfig
```

## Not exposed

Some librdkafka settings aren't available. Either handled differently or not yet implemented:

- `socket.connection.setup.timeout.max.ms` - overridden by `connTimeout`
- `log_level`, `log.queue` - use your app's logging
- `metric.reporters` - use `Kafka.Telemetry.Metrics`

Request additions via issue or PR.
