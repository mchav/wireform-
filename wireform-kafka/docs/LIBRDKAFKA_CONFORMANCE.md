# librdkafka conformance catalog

The librdkafka project ships [a test suite](https://github.com/confluentinc/librdkafka/tree/master/tests)
that is the de-facto conformance reference for any Kafka client. It
contains **168 registered tests** (114 `.c` + 33 `.cpp` files plus a
shared 8.8K-line C test framework in `tests/test.{c,h}`), grouped by
the purpose of each numbered file.

This document maps every one of those tests onto either a Haskell
port (`test-conformance/Conformance/TNNNN/`), a duplicate of an
existing test in our repo, or one of three explicit *blockers*:

  - **NEEDS_MOCK_CLUSTER** — depends on librdkafka's
    `rd_kafka_mock_cluster_new`, an in-process Kafka broker mock built
    into librdkafka. We do not have an equivalent yet. Reproducing it
    is its own substantial subproject (a stateful in-process broker
    that speaks the JoinGroup / Fetch / Produce / OffsetCommit
    protocols server-side).
  - **NEEDS_BROKER_CONFIG** — needs a real broker booted with a
    specific configuration (auto-create off, transactional id allowed,
    SASL/SSL listener, ACL grants) that our local KRaft broker does
    not have without `trivup`-style orchestration.
  - **NOT_APPLICABLE_API** — tests librdkafka-specific C API surface
    (`rd_kafka_conf_*`'s string-DSL parser, `rd_kafka_event_*`'s
    polling loop, `rd_kafka_interceptor_*` callback hooks,
    `rd_kafka_topic_t` opaque-handle cache semantics, …) that has no
    analogue in a typed Haskell library. Where the *intent* of the
    test is meaningful for us (e.g., "configuration validation"), we
    port the intent rather than the C API surface and link it here.

## Counts

| Status | Count | Where |
|--------|------:|-------|
| Ported (passing in `test:wireform-kafka-conformance`) | 12 files / 47 cases | `test-conformance/Conformance/T*/` |
| Already covered by existing tests | 6 | see "duplicates" below |
| NEEDS_MOCK_CLUSTER | 23 | blocked on building an in-process broker mock |
| NEEDS_BROKER_CONFIG | ~85 | blocked on `trivup`-equivalent broker orchestration |
| NOT_APPLICABLE_API | ~42 | librdkafka-internal; no analogue |

Total: 168.

## Ported tests (passing today)

| librdkafka                   | wireform-kafka                                                 | Notes |
|------------------------------|---------------------------------------------------------------|-------|
| `0000-unittests.c`           | `Conformance.T0000.Unittests`                                  | Smoke: default configs are sane, every umbrella module imports. |
| `0004-conf.c`                | `Conformance.T0004.Conf`                                       | Validation: empty bootstrap brokers / group id / topic list rejected. |
| `0006-symbols.c`             | `Conformance.T0006.Symbols`                                    | Build-time check disguised as a runtime test: every public namespace imports. |
| `0017-compression.c`         | `Conformance.T0017.Compression`                                | Round-trips every compression codec on representative payloads (no broker). |
| `0043-no_connection.c`       | `Conformance.T0043.NoConnection`                               | Bounded-retry connect to an unreachable host; backoff cap honoured. |
| `0072-headers_ut.c`          | `Conformance.T0072.Headers`                                    | RecordHeader present / absent / multi-header order. |
| `0080-admin_ut.c`            | `Conformance.T0080.AdminUt`                                    | NewTopic / AdminClientConfig / ConfigResourceType ADT smoke. |
| `0086-purge_local.c`         | `Conformance.T0086.PurgeLocal`                                 | BatchAccumulator: enqueue / close / drain semantics. |
| `0095-all_brokers_down.c`    | `Conformance.T0095.AllBrokersDown`                             | Bounded-time failure when every broker is unreachable. |
| `0103-transactions_local.c`  | `Conformance.T0103.TransactionsLocal`                          | Transaction state machine: legal/illegal transitions without a coordinator. |
| `0142-reauthentication.c`    | `Conformance.T0142.Reauthentication`                           | Per-mechanism re-auth payload determinism (KIP-368 session-re-auth scheduling is a TODO; documented). |
| `0144-idempotence_mock.c`    | `Conformance.T0144.IdempotenceMock` (partial)                  | Per-partition sequence-number bookkeeping; mock-cluster half is gated on the missing infra. |

Run with:

```bash
cabal test wireform-kafka:wireform-kafka-conformance
```

## Duplicates of existing coverage

These librdkafka tests cover the same intent as a test we already had
under `test/`. Re-porting them adds no signal; we list them here so
the catalog is complete.

| librdkafka                       | Existing wireform-kafka test |
|----------------------------------|------------------------------|
| `0033-regex_subscribe_local.c`   | `Client.GroupSpec` (assignor selection + topic filtering) |
| `0034-offset_reset_mock.c`       | `Client.ConsumerConfigSpec` (auto-offset-reset enum) |
| `0046-rkt_cache.c`               | `Kafka.Client.Metadata` `MetadataCache` lookup tests |
| `0079-fork.c`                    | N/A (no fork-after-init concept in our pure-Haskell stack) |
| `0102-static_group_rebalance_mock.c` | `Client.GroupSpec` + the auto-rebalance path in `Client.Consumer.poll` |
| `0125-immediate_flush.c`         | `Client.BatchAccumulatorSpec` |

## NEEDS_MOCK_CLUSTER (23)

These all depend on `rd_kafka_mock_cluster_new` — librdkafka's
in-process Kafka broker mock that speaks just enough of the wire
protocol (Produce / Fetch / Metadata / JoinGroup / SyncGroup /
Heartbeat / OffsetCommit / OffsetFetch / FindCoordinator /
ListOffsets / SaslHandshake / SaslAuthenticate / DescribeConfigs)
to satisfy a real client without spinning up Java + ZK/KRaft.
Building an equivalent in Haskell is a substantial subproject — it
needs to reuse our `Kafka.Protocol.Generated.*` decoders/encoders on
the *server* side and maintain partition logs, consumer-group state
machines, and txn-coordinator state.

Until then, these are skipped:

```
0009-mock_cluster.c                  0143-exponential_backoff_mock.c
0031-get_offsets_mock.c              0144-idempotence_mock.c           (partial port above)
0034-offset_reset_mock.c             0145-pause_resume_mock.c
0045-subscribe_update_mock.c         0146-metadata_mock.c
0045-subscribe_update_racks_mock.c   0147-consumer_group_consumer_mock.c
0055-producer_latency_mock.c         0148-offset_fetch_commit_error_mock.c
0076-produce_retry_mock.c            0150-telemetry_mock.c
0102-static_group_rebalance_mock.c   8001-fetch_from_follower_mock_manual.c
0105-transactions_mock.c             …plus ~7 others ending in `_mock`
0107-fetch_from_follower_mock.c
0109-auto_create_topics_mock.c
0125-immediate_flush.c
0136-resolve_cb.c
0137-barrier_batch_consume.c
0139-offset_validation_mock.c
```

Building the mock is the **biggest single thing standing between us
and the rest of the catalog passing**. Plan:

1. New library `wireform-kafka-mock` exposing
   `Kafka.Mock.Cluster.{newCluster, withCluster, broker, partition}`.
2. Implement only the subset of API keys above; use the existing
   `Kafka.Protocol.Generated.*` decoders to parse incoming requests
   and the same modules' encoders to emit responses.
3. Per-partition log = `IORef (Vector RecordBatch)`; consumer-group
   state = an STM map keyed by group id.
4. Wire it into the conformance suite so the `_mock` ports become
   real tests (no broker still needed at run time).

## NEEDS_BROKER_CONFIG (~85)

Tests in this category depend on a real broker booted with a specific
configuration. librdkafka uses a Python tool called `trivup` to
spin up brokers in matrices of versions × auth modes × SSL/PLAINTEXT.
Examples:

| File | Required broker setup |
|------|----------------------|
| `0007-autotopic.c` | `auto.create.topics.enable=true` (default) plus our existing producer bug — see integration suite. |
| `0011-produce_batch.c` | Real broker; transactional id allowed. |
| `0017-compression.c` (broker variant) | Real broker; we ported the codec round-trip half above. |
| `0030-offset_commit.c` | Real broker; consumer-group enabled. |
| `0050-subscribe_adds.c` | Real broker; pre-created topics. |
| `0054-offset_time.c` | Real broker; ListOffsets-by-timestamp supported. |
| `0075-retry.c` | Real broker; injected mid-flight failure (needs trivup hooks). |
| `0083-cb_event.c` | Real broker; event callback wiring (NOT_APPLICABLE_API for us). |
| `0099-commit_metadata.c` | Real broker; consumer offsets topic. |
| `0104-fetch_from_follower.c` | Real broker; rack-aware-fetch enabled (KIP-392). |
| `0113-cooperative_rebalance.c` | Real broker; cooperative-sticky support. |
| `0119-consumer_auth.c` | Real broker; ACLs configured. |
| `0123-connections_max_idle.c` | Real broker; `connections.max.idle.ms` short. |
| `0140-commit_metadata.cpp` | Real broker; consumer-coordinator commit metadata round-trip. |
| `0146-metadata_mock.c` | (also mock — already counted above) |
| `0149-broker-same-host-port.c` | Multi-broker cluster on the same host with distinct ports. |
| `0151-purge-brokers.c` | Real broker; metadata-driven broker rotation. |
| `0152-rebootstrap.c` | Real broker; controlled restart. |
| `0153-memberid.c` | Real broker; consumer-group rejoin. |

We have a working KRaft single-node broker setup
(`docs/PERFORMANCE.md`) that runs the integration suite. Going beyond
that to the full librdkafka matrix is a `trivup`-equivalent project
in Python or `docker compose`.

We also have **6 known producer / consumer bugs** that block tests in
this category — even with the broker booted correctly, our producer
returns `offset=-1` for successful sends and our consumer's manual
`assign` reports zero partitions assigned. Those bugs predate this
catalog; see the integration suite output for the specific failures.

## NOT_APPLICABLE_API (~42)

These test librdkafka-specific C API surfaces with no Haskell analogue.
Skipping them is correct, not a gap.

| Theme | Examples |
|-------|----------|
| `rd_kafka_event_*` polling loop | `0039-event.c`, `0039-event_log.c`, `0058-log.c`, `0062-stats_event.c`, `0083-cb_event.c` |
| `rd_kafka_interceptor_*` plugin hooks | `0100-thread_interceptors.c`, `0094-idempotence_msg_timeout.c`, `0098-consumer-txn.cpp` |
| `rd_kafka_topic_t` opaque-handle cache | `0021-rkt_destroy.c`, `0046-rkt_cache.c` |
| `rd_kafka_conf_*` string-DSL parser | `0004-conf.c` (port: typed-config validation), `0044-partition_cnt.c` |
| C API symbol export check | `0006-symbols.c` (port: importable-namespace smoke) |
| `rd_kafka_producev` variadic API | `0074-producev.c` |
| `rd_kafka_destroy_flags` | `0084-destroy_flags_local.c`, `0037-destroy_hang_local.c`, `0020-destroy_hang.c` |
| C++ wrapper | every `.cpp` that just re-exercises a `.c` test |
| Stats JSON shape (librdkafka-defined) | `0053-stats_timing.c`, `0062-stats_event.c` |
| OS thread interaction | `0078-c_from_cpp.c`, `0100-thread_interceptors.c` |

## Process

1. New conformance ports go in `test-conformance/Conformance/TNNNN/`,
   one module per librdkafka test number. Module docstring says
   exactly what the librdkafka file did and what we ported.
2. Each new module is added to `test-conformance/Main.hs` and to
   `wireform-kafka.cabal`'s `wireform-kafka-conformance.other-modules`.
3. Update this document's "Counts" table and the corresponding row in
   the "Ported / Duplicates / NEEDS_*" tables.

`cabal test wireform-kafka:wireform-kafka-conformance` should always
be green; if a port can't pass without the mock cluster or specific
broker config, that's a sign the test belongs in NEEDS_MOCK_CLUSTER /
NEEDS_BROKER_CONFIG, not in `test-conformance/`.
