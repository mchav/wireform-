# Mock Broker

In-memory Kafka cluster for fast, deterministic tests.

## Why use it

- **Fast:** No network, no Docker
- **Deterministic:** Controlled clock and randomness
- **Fault injection:** Inject specific errors
- **Parallel-safe:** Each test gets its own cluster

## Core modules

| Module | Provides |
|---|---|
| `Cluster` | Topics, partitions, groups, transactions, KRaft |
| `Fault` | Error injection |
| `Producer` | Transaction-aware producer with epoch fencing |
| `Consumer` | Round-robin assignment, commits, pause/resume |
| `Admin` | Create/delete topics, describe cluster |
| `Idempotent` | KIP-98 idempotent producer state |
| `Backoff` | Deterministic exponential backoff |
| `Telemetry` | Operation counters |

## Example

```haskell
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Producer
import Kafka.Client.Mock.Consumer
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  cluster <- newMockCluster 1
  createTopic cluster "events" 3

  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  sendMock producer "events" 0 (Just "key") "value" 0

  consumer <- newMockConsumer cluster faults (GroupId "g") ReadUncommitted 100
  subscribeMC consumer ["events"]
  result <- pollMC consumer
  print (length (pollRecords result))
```

## Fault injection

```haskell
import Kafka.Client.Mock.Fault

-- Queue error for specific partition
queueProduceErrors cluster "events" 0 [LeaderNotAvailable, None]

-- Sticky error until cleared
setStickyProduce cluster NotEnoughReplicas

-- Per-group commit failures
addCommitFault cluster (GroupId "my-group") RebalanceInProgress
```

See `Fault` module for all 19 error types.

## Coverage

The mock powers 768+ tests without external dependencies.

## vs librdkafka mock

| librdkafka | This mock |
|---|---|
| `rd_kafka_mock_cluster_t` | `MockCluster` |
| `rd_kafka_mock_topic_create` | `createTopic` |
| `rd_kafka_mock_push_request_errors` | `queueProduceErrors`, `queueFetchErrors` |
| `rd_kafka_mock_set_request_error` | `setStickyProduce` |
| Virtual clock | `tickClock` |

Tradeoff: We give up socket-level testing for speed and determinism. Wire protocol tests use the integration suite.
