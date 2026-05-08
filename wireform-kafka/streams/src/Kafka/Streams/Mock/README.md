# Mock broker

In-process emulation of a Kafka cluster for testing the Streams runtime
end-to-end, including failure modes. The design is modelled directly on
[librdkafka's `rd_kafka_mock_cluster_t`](https://github.com/confluentinc/librdkafka/blob/master/src/rdkafka_mock.h),
adapted for Haskell idiom.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│ MockCluster                                          │
│  topics: Map TopicName MockTopic                     │
│  groups: Map GroupId  (Map (Topic, Part) Offset)     │
│  txns:   Map TxnId    TxnState                       │
│  brokers / downBrokers / clock                       │
└──┬───────────────────────────────────────────────┬───┘
   │  reads + writes                               │
┌──▼─────────────┐                       ┌─────────▼──────┐
│ MockProducer   │   FaultPolicy ────►   │ MockConsumer    │
│  send / flush  │     queues + sticky   │ poll / commit   │
│  txn begin /   │                       │ subscribe / seek│
│  commit / abort│                       │  (R/U or R/C)   │
└────────────────┘                       └─────────────────┘
   ▲                                              ▲
   │ emit via collector                           │ feed to engine
┌──┴──────────────────────────────────────────────┴──┐
│ MockStreamsDriver                                  │
│   wires Engine ↔ MockProducer + MockConsumer       │
│   tickDriver  / runUntilQuiet  / externalSend      │
└────────────────────────────────────────────────────┘
```

## Comparison with `librdkafka`'s mock cluster

| Concept                              | librdkafka                             | Here                              |
|--------------------------------------|----------------------------------------|-----------------------------------|
| Cluster object                       | `rd_kafka_mock_cluster_t`              | `MockCluster`                     |
| Topic creation                       | `rd_kafka_mock_topic_create`           | `createTopic`                     |
| Per-partition log                    | hidden                                 | `dumpPartition`, `partitionLogSize` |
| HWM / LSO                            | wired to fetch responses               | `partitionHWM`, `partitionLastStableOffset` |
| Broker outage                        | `rd_kafka_mock_broker_set_down`        | `markBrokerDown`/`markBrokerUp`   |
| Push request errors                  | `rd_kafka_mock_push_request_errors`    | `queueProduceErrors`/`queueFetchErrors`/`queueCommitErrors`/`queueTxnErrors` |
| Sticky errors                        | `rd_kafka_mock_set_request_error`      | `setStickyProduce`/`setStickyFetch` |
| Coordinator errors                   | `rd_kafka_mock_coordinator_set_error`  | `addCommitFault` (group) / `addTxnFault` (txn) |
| Time control                         | per-broker virtual clock               | `tickClock` on the cluster        |
| Driving the engine                   | client connects via TCP                | `MockStreamsDriver` calls `feedSource` directly |

The crucial difference: librdkafka's mock cluster speaks the wire
protocol over a TCP socket, so any Kafka client can connect to it.
Ours doesn't — it's a Haskell-level facade. That trade-off is
deliberate: we get deterministic STM-coordinated tests without
needing a port allocator, and the `MockStreamsDriver` exercises the
streams engine in isolation from the (separately-tested)
`Kafka.Client.Producer` / `Kafka.Client.Consumer` plumbing.

For an end-to-end socket-level test that includes the wire encoders
and the `wireform-kafka` client itself, the existing
`test-integration` suite spins up a real `kafka-native` broker.

## Failure modes covered

The two test specs (`MockClusterSpec`, `MockFailureModesSpec`)
exercise:

* Producer:
    * basic round-trip
    * single + queued retriable faults (drain in FIFO order)
    * fatal faults
    * sticky faults block all writes; clearing resumes them
    * per-partition fault queues are isolated
    * send to a non-existent partition surfaces the right error
* Consumer:
    * subscribe + poll + commit
    * fetch faults isolated to the affected partition
    * `seekMC` overrides the committed offset
    * `OffsetOutOfRange` classified as retriable
    * subscribing to a non-existent topic yields no assignment
    * commit fault returns `Left` without mutating the offset store
* Transactions:
    * read-uncommitted vs read-committed isolation
    * `commitTxn` advances LSO; `abortTxn` permanently hides records
    * interleaved txns: committed visible, aborted hidden
    * `beginTxn` fault keeps the producer non-transactional
    * `commitTxn` fault leaves the txn `TxnOpen` (next attempt
      re-runs)
* Cluster:
    * broker-down state observable via `isBrokerUp` / `downedBrokers`
    * monotonic logical clock via `tickClock`
* Streams driver end-to-end:
    * pass-through topology
    * filter topology
    * fetch-fault-then-recover
    * sticky fetch fault blocks output indefinitely
    * clearing a sticky fetch fault drains buffered records
    * three queued fetch faults consume three ticks
    * LSO and HWM track non-transactional sink writes

## Adding new failure scenarios

The pattern is uniform:

```haskell
-- 1. Spin up the cluster + faults.
c  <- newMockCluster 1
createTopic c (topicName "t") 1
fp <- noFaults

-- 2. Pre-load failures.
queueProduceErrors fp (topicName "t") 0 [ErrLeaderNotAvailable]
setStickyFetch     fp (topicName "t") 0 ErrCoordinatorLoadInProgress

-- 3. Build the producer / consumer / driver.
p  <- newMockProducer c fp Nothing

-- 4. Drive operations and assert on cluster state.
r  <- sendMock p (topicName "t") 0 Nothing "v" (Timestamp 0)
n  <- partitionLogSize c (topicName "t") 0
```

The fault APIs accept lists so you can stage a sequence of
errors-then-success without rewriting the test fixture mid-flight.
