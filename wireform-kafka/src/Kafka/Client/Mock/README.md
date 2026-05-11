# Mock broker

In-process emulation of a Kafka cluster for testing the
`wireform-kafka` core client (and the streams runtime built on
top of it) including failure modes. Modelled directly on
[librdkafka's `rd_kafka_mock_cluster_t`](https://github.com/confluentinc/librdkafka/blob/master/src/rdkafka_mock.h)
and adapted for Haskell idiom.

The mock lives in the core `wireform-kafka` library so that both
the streams runtime and the core client tests can share it.

## Modules

```
Kafka.Client.Mock.Cluster      -- topics, partitions, logs, HWM, LSO,
                                  brokers + leader, consumer-group offsets +
                                  members + generation, txn state +
                                  per-(TxnId, Epoch) snapshot sets,
                                  KRaft role + controller broker,
                                  re-auth deadline, manual clock
Kafka.Client.Mock.Fault        -- 19 MockError variants + per-(topic,
                                  partition) / per-GroupId / per-(TxnId,
                                  TxnOp) error queues + sticky overrides
Kafka.Client.Mock.Producer     -- txn-aware producer view; epoch fencing
                                  on stale-epoch sends; sendBatch (barrier
                                  batch); flushSync; sendMockH for headers
Kafka.Client.Mock.Consumer     -- ReadUncommitted / ReadCommitted; per-member
                                  round-robin assignor; pause/resume per
                                  partition; manual offset store; commit
                                  with metadata + leader epoch
Kafka.Client.Mock.Admin        -- createTopicsAdmin / deleteTopicsAdmin /
                                  describeTopicAdmin / listConsumerGroupsAdmin /
                                  describeConsumerGroupAdmin /
                                  describeClusterAdmin
Kafka.Client.Mock.Idempotent   -- KIP-98 idempotent state: ProducerId +
                                  epoch + per-(topic, partition) sequence
                                  + dedup table
Kafka.Client.Mock.Backoff      -- BackoffPolicy + nextBackoffMs / backoffSeries
                                  with deterministic jitter (no PRNG, so tests
                                  are reproducible)
Kafka.Client.Mock.Telemetry    -- KIP-714 telemetry counters (produce / fetch /
                                  commit / txn begin / txn commit / txn abort)
                                  with snapshot reads
```

## librdkafka mock-test ports

| librdkafka test | Subject | Spec |
|---|---|---|
| `0070_null_empty.c` | null vs empty key/value | `MockBrokerExtSpec` |
| `0085_headers.c` | record headers (multi, dedup, ordering) | `MockBrokerExtSpec` |
| `0105_transactions_mock.c` | sendOffsetsToTxn + txn op-targeted faults | `MockBrokerCoopSpec`, `MockBrokerFailureModesSpec` |
| `0109_auto_create_topics.c` | auto-create topics on first send | `MockBrokerExtSpec` |
| `0113_cooperative_rebalance.c` | added / revoked deltas | `MockBrokerCoopSpec` |
| `0118_commit_rebalance.c` | offset commit racing rebalance | `MockBrokerCoopSpec` |
| `0120_asymmetric_subscription.c` | members subscribe to disjoint topic sets | `MockBrokerCoopSpec` |
| `0121_clusterid.c` | cluster id + metadata | `MockBrokerNetSpec` |
| `0125_immediate_flush.c` | flushSync semantics | `MockBrokerStoreSpec` |
| `0127_fetch_queue_backoff.c`, `0143_exponential_backoff_mock.c` | exponential backoff curve | `MockBrokerNetSpec` |
| `0130_store_offsets.c` | manual offset store + commit | `MockBrokerStoreSpec` |
| `0137_barrier_batch_consume.c` | sendBatch produces contiguous offsets | `MockBrokerStoreSpec` |
| `0138_admin_mock.c` | createTopics / deleteTopics / describe | `MockBrokerAdminSpec` |
| `0139_offset_validation_mock.c` | leader-epoch + offset validation (KIP-320) | `MockBrokerCoopSpec` |
| `0140_commit_metadata.c` | per-commit metadata bytes | `MockBrokerExtSpec` |
| `0142_reauthentication.c` | re-auth deadline | `MockBrokerProtoSpec` |
| `0144_idempotence_mock.c` | sequence numbers + dedup | `MockBrokerIdempotentSpec` |
| `0145_pause_resume_mock.c` | per-partition pause/resume | `MockBrokerExtSpec` |
| `0146_metadata_mock.c` | metadata refresh | `MockBrokerNetSpec` |
| `0147_consumer_group_consumer_mock.c` | generation id stability | `MockBrokerProtoSpec` |
| `0148_kraft_modes.c` | KRaft role + controller broker | `MockBrokerProtoSpec` |
| `0150_telemetry_mock.c` | per-op telemetry counters | `MockBrokerProtoSpec` |
| broker-down propagation | leader = down → not_leader_for_partition | `MockBrokerNetSpec`, `MockBrokerFailureModesSpec` |

## Comparison with librdkafka

| Concept                  | librdkafka                              | Here                                |
|--------------------------|-----------------------------------------|-------------------------------------|
| Cluster                  | `rd_kafka_mock_cluster_t`               | `Kafka.Client.Mock.Cluster`         |
| Topic creation           | `rd_kafka_mock_topic_create`            | `createTopic`                       |
| Push errors              | `rd_kafka_mock_push_request_errors`     | `queueProduceErrors` / `queueFetchErrors` / `queueCommitErrors` / `queueTxn{Begin,Commit,Abort}Errors` |
| Sticky errors            | `rd_kafka_mock_set_request_error`       | `setStickyProduce` / `setStickyFetch` |
| Coordinator errors       | `rd_kafka_mock_coordinator_set_error`   | `addCommitFault` / `addTxn{Begin,Commit,Abort}Fault` |
| Idempotent producer fence | wire-level `INVALID_PRODUCER_EPOCH`     | per-`(TxnId, Epoch)` fence in `appendToPartition` |
| Group rebalance          | join/leave + assignor                   | `joinGroup` / `leaveGroup` / `assignmentFor` (per-member round-robin) |
| Cooperative rebalance    | KIP-429 incremental cooperative         | `cooperativeRebalance` returns `RebalanceDelta` |
| Leader epoch (KIP-320)   | `OffsetForLeaderEpoch`                  | `currentLeaderEpoch` / `bumpLeaderEpoch` / `validateOffsetEpoch` |
| Auto-create topics       | broker-side flag                        | `setAutoCreateTopics`               |
| Idempotent producer      | `enable.idempotence=true`               | `Kafka.Client.Mock.Idempotent`      |
| sendOffsetsToTxn         | wire-level `TxnOffsetCommit`            | `sendOffsetsToTxn` + `pendingTxnOffsets` |
| Pause / resume           | `rd_kafka_pause_partitions`             | `pausePartitions`                   |
| Manual offset store      | `rd_kafka_offset_store`                 | `storeOffsetMC` + `commitStoredOffsetsMC` |
| Commit metadata          | `OffsetAndMetadata`                     | `OffsetAndMetadata` + `commitOffsetsWithMetadataMC` |
| Backoff                  | wired in `rd_kafka_buf_retry`           | `BackoffPolicy` + `nextBackoffMs`   |
| Cluster metadata         | `MetadataResponse`                      | `describeClusterMetadata`           |
| Admin client             | `rd_kafka_AdminClient_*`                | `Kafka.Client.Mock.Admin`           |
| Telemetry                | `rd_kafka_telemetry_*` (KIP-714)        | `Kafka.Client.Mock.Telemetry`       |
| KRaft mode               | `rd_kafka_set_kraft_*` knobs            | `KRaftRole` + `setKRaftRole`        |
| Re-auth                  | `connections.max.reauth.ms`             | `setReauthDeadline` / `isReauthExpired` |
| Time control             | per-broker virtual clock                | `tickClock`                         |
| Transport                | TCP socket; any client connects         | Haskell-level façade                |

The trade-off is deliberate: a Haskell façade gives deterministic
STM-coordinated tests without needing a port allocator, and it
lets tests drive the producer / consumer / cluster as ordinary IO
actions. For an end-to-end socket-level test that includes the
wire encoders and the actual `Kafka.Client.Producer` /
`Kafka.Client.Consumer` plumbing, the existing `test-integration`
suite spins up a real broker.

## Numbers

* 9 modules under `Kafka.Client.Mock.*` + 4 streams adapter shims
* 110 mock-broker tests in the core client suite (across 8 specs)
* 48 mock-broker tests in the streams suite
* Total: **626 tests** passing across both suites
