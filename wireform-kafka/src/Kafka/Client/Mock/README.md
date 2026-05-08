# Mock broker

In-process emulation of a Kafka cluster for testing the
`wireform-kafka` core client (and the streams runtime built on
top of it) including failure modes. Modelled directly on
[librdkafka's `rd_kafka_mock_cluster_t`](https://github.com/confluentinc/librdkafka/blob/master/src/rdkafka_mock.h)
and adapted for Haskell idiom.

The mock lives in the core `wireform-kafka` library so that both
the streams runtime and the core client tests can share it.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│ Kafka.Client.Mock.Cluster                            │
│  topics: Map Text MockTopic                          │
│  groups: Map GroupId  (Map (Topic, Part) Offset)     │
│  members: Map GroupId [(MemberId, Set Topic)]        │
│  txns:   Map TxnId    TxnState                       │
│  txnEpoch + committedStamps + abortedStamps          │
│  brokers / downBrokers / clock                       │
└──┬───────────────────────────────────────────────┬───┘
   │  reads + writes                               │
┌──▼─────────────┐                       ┌─────────▼──────┐
│ MockProducer   │   FaultPolicy ────►   │ MockConsumer    │
│  send / flush  │     queues + sticky   │ poll / commit   │
│  txn begin /   │     per (topic,part)  │ subscribe /     │
│  commit / abort│     per group / txn   │   seek / seek   │
│  fence on stale│                       │  (R/U or R/C)   │
│   epoch        │                       │  member id +    │
│                │                       │  group assignor │
└────────────────┘                       └─────────────────┘
                       (used directly from
                        wireform-kafka/test/Client/MockBroker*Spec.hs)

      ┌──────────────────────────────────────────────────┐
      │ Kafka.Streams.Mock.* (thin wrappers, streams)    │
      │   adapter for TopicName ↔ Text                   │
      │                  Timestamp ↔ Int64               │
      └─┬────────────────────────────────────────────────┘
        │ wraps for use from the streams engine
┌───────▼──────────────────────────────────────────┐
│ Kafka.Streams.Mock.StreamsDriver                 │
│   wires the streams Engine ↔ MockProducer +      │
│   MockConsumer; tickDriver / runUntilQuiet       │
│   At-least-once and EOS-V2 modes                 │
└──────────────────────────────────────────────────┘
```

## Comparison with `librdkafka`'s mock cluster

| Concept                              | librdkafka                             | Here                                      |
|--------------------------------------|----------------------------------------|-------------------------------------------|
| Cluster object                       | `rd_kafka_mock_cluster_t`              | `MockCluster`                             |
| Topic creation                       | `rd_kafka_mock_topic_create`           | `createTopic`                             |
| Per-partition log                    | hidden                                 | `dumpPartition`, `partitionLogSize`       |
| HWM / LSO                            | wired to fetch responses               | `partitionHWM`, `partitionLastStableOffset` |
| Broker outage                        | `rd_kafka_mock_broker_set_down`        | `markBrokerDown` / `markBrokerUp`         |
| Push request errors                  | `rd_kafka_mock_push_request_errors`    | `queueProduceErrors` / `queueFetchErrors` / `queueCommitErrors` / `queueTxn{Begin,Commit,Abort}Errors` |
| Sticky errors                        | `rd_kafka_mock_set_request_error`      | `setStickyProduce` / `setStickyFetch`     |
| Coordinator errors                   | `rd_kafka_mock_coordinator_set_error`  | `addCommitFault` / `addTxn{Begin,Commit,Abort}Fault` |
| Idempotent producer epoch fence      | wire-level `INVALID_PRODUCER_EPOCH`    | per-`(TxnId, Epoch)` fence in `appendToPartition` |
| Group rebalance                      | join/leave + assignor                  | `joinGroup` / `leaveGroup` / `assignmentFor` (round-robin) |
| Time control                         | per-broker virtual clock               | `tickClock` on the cluster                |
| Transport                            | TCP socket; any client connects        | Haskell-level façade                      |

The crucial difference: librdkafka's mock cluster speaks the wire
protocol over a TCP socket, so any Kafka client can connect to it.
Ours doesn't — it's a Haskell-level facade. The trade-off is
deliberate: we get deterministic STM-coordinated tests without
needing a port allocator, and tests can drive the producer /
consumer / cluster as ordinary IO actions.

For an end-to-end socket-level test that includes the wire encoders
and the actual `wireform-kafka` `Kafka.Client.Producer` /
`Kafka.Client.Consumer` plumbing, the existing `test-integration`
suite spins up a real broker.

## Failure modes covered (82 tests across two test suites)

### Core client (`wireform-kafka/test/Client/MockBroker*Spec.hs`)

`Client.MockBrokerSpec` (11):
* topology, append + fetch, group offsets
* producer round-trip, retriable + fatal faults
* per-partition fetch fault isolation
* read-committed isolation, `commitTxn` advances LSO, `abortTxn` hides records
* broker up / down state

`Client.MockBrokerFailureModesSpec` (15):
* producer queued FIFO drain, sticky blocks all, clear resumes, per-partition isolation, non-existent partition error
* consumer `seekMC` overrides commit, `OffsetOutOfRange` retriable, unknown topic = empty assignment
* commit fault returns `Left` without committing
* txn interleaved committed + aborted, `beginTxn`/`commitTxn` op-targeted faults
* `markBrokerDown`, `tickClock`

`Client.MockBrokerAdvancedSpec` (9):
* headers round-trip
* epoch bump on commit / abort
* **stale-epoch producer fenced after sibling commit**
* multi-partition txn commit advances LSO on every touched partition
* **two consumers split partitions round-robin**
* **three consumers, one leaves → partitions redistribute**
* coordinator retry then success
* multi-topic single-consumer assignment

### Streams (`wireform-kafka/streams/test/Streams/Mock*Spec.hs`)

`MockClusterSpec` (14) + `MockFailureModesSpec` (18) +
`MockAdvancedSpec` (9) + `MockDriverModesSpec` (7): the same coverage
above, exercised via the `TopicName` / `Timestamp` adapter layer,
plus end-to-end streams-driver scenarios:
* multi-partition input, key-hashed routing across N output partitions
* EOS round-trip read-committed visible
* EOS commit fault aborts the tick
* EOS recovers after an aborted tick
* two drivers in same group split partitions
* sibling driver leaves → survivor gets all partitions

## Adding new failure scenarios

The pattern is uniform:

```haskell
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer
import Kafka.Client.Mock.Consumer

-- 1. Spin up the cluster + faults.
c  <- newMockCluster 1
createTopic c "t" 1
fp <- noFaults

-- 2. Pre-load failures.
queueProduceErrors fp "t" 0 [ErrLeaderNotAvailable]
setStickyFetch     fp "t" 0 ErrCoordinatorLoadInProgress

-- 3. Build the producer / consumer.
p <- newMockProducer c fp Nothing

-- 4. Drive operations and assert on cluster state.
r <- sendMock p "t" 0 Nothing "v" 0
n <- partitionLogSize c "t" 0
```

The fault APIs accept lists so you can stage a sequence of
errors-then-success without rewriting the test fixture mid-flight.
The `TxnOp = TxnBegin | TxnCommit | TxnAbort` granularity lets txn
tests target an exact operation without bleeding faults onto
sibling calls.
