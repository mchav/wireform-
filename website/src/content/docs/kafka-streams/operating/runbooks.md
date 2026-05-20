---
title: Runbooks
description: Common incidents in a wireform-kafka-streams deployment and the procedures for resolving them.
sidebar:
  order: 8
---

This page contains procedures for common incidents. Each runbook is designed to be followed during an active incident. Start with the alert you are seeing, work through diagnosis, then follow the resolution steps.

Keep this page bookmarked. When an alert fires, open it and follow the relevant procedure.

## What a runbook is (and isn't)

A runbook is a **checklist for your brain during an outage**. When production
is down, you don't want to be figuring out which metrics matter or what the
recovery command is. You want a procedure you can follow that leads to a known
good state.

Each runbook in this page follows the same structure:
- **Alert:** What you see in your monitoring (the trigger)
- **Diagnosis:** How to confirm the root cause (the investigation)
- **Resolve:** The steps to fix it (the action)
- **Prevent:** How to stop it happening again (the learning)

A runbook is not a troubleshooting guide for development. If you're debugging
why your topology behaves unexpectedly in the test driver, that's a different
process. Runbooks are for when the service is already deployed and something
has gone wrong in production.

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

:::note[TL;DR]
- Most-common incidents covered: rebalance storm, `CommitFatal` after `commit2PC`, standby that never promotes, orphan internal topics, producer fenced, async-I/O stall, deserialisation flood, unbounded state-dir growth, slow commit cycle, IQ during rebalance.
- Each runbook is structured the same way: alert → diagnosis → resolve → prevent.
- Use the quick metric-reference at the bottom when you don't know what to look at first.
:::

## Rebalance storm

**Alert:** `task-restart-total` rate spike across multiple
instances, throughput drops, `state-transition: RUNNING ->
REBALANCING -> RUNNING` flapping in logs.

### Diagnosis

A "storm" is [rebalances](../glossary/#rebalance) triggered faster than the group can
settle, usually within minutes of each other. Common causes:

1. **Liveness flaps.** One instance is hitting GC pauses, network
   blips, or saturated CPU. Its heartbeats time out, the group
   kicks it, it rejoins, repeat.
2. **Probing-rebalance loop.** `probingRebalanceIntervalMs` is
   set short and `acceptableRecoveryLag` is set tight; warmups
   pass the threshold, the probing rebalance fires, the new
   ownership is fragile and triggers another probe immediately.
3. **Memory pressure.** RocksDB compaction stalling the instance
   long enough to miss heartbeats.
4. **Misconfigured `session.timeout.ms` on the consumer side**
   relative to `heartbeat.interval.ms`.

### Resolve

1. **Identify the flapping instance(s).** `setRebalanceListener`
   logs should show which instances repeatedly join and leave.
2. **For liveness flaps:** look at OS-level metrics for that
   instance (CPU, GC, network). Fix the underlying cause; the
   rebalance loop stops itself.
3. **For probing-rebalance loops:** temporarily raise
   `probingRebalanceIntervalMs` to 600_000 (10 min default).
   Restart the affected instances with the new config.
4. **For RocksDB compaction pressure:** check
   `storePutTotal` versus underlying disk write throughput; the
   ratio tells you whether you're write-throttled. Mitigations
   include moving the state directory to faster storage,
   reducing `numStandbyReplicas` temporarily, or shedding load
   via `pauseKafkaStreams` on the worst-affected instance until
   compaction catches up.
5. **For consumer-config skew:** confirm
   `session.timeout.ms > 3 × heartbeat.interval.ms` and that
   neither has changed recently.

### Prevent

- Standardise `probingRebalanceIntervalMs` and
  `acceptableRecoveryLag` so all instances agree.
- Monitor GC pauses; alert at p99 > 200 ms.
- Provision local disk IOPS for RocksDB's compaction worst case,
  not its steady state. See [Running in containers](./containers/#3-memory-accounting)
  for how compaction interacts with container memory limits and the
  OOM-killer.

---

## `CommitFatal` after `commit2PC`

**Alert:** `commit-cycle-fatal` counter > 0 with reason
`commit2PC: …`.

### Diagnosis

The producer transaction committed (records are durable on
Kafka), but the external 2PC sink failed to finalise. The
in-flight `SinkTxnId` is stranded in the prepared state on the
external system. The runtime is killed on this outcome.

### Resolve

1. **Confirm the runtime is dead.** The supervisor should already
   be restarting it. Don't `unpause` or otherwise interfere
   before the restart.
2. **On restart, the runtime calls `tpsRecover`** for every
   configured sink. The sink returns a list of `SinkTxnId`s
   currently in the prepared state.
3. **For each token, the runtime calls the sink's recovery logic**
  : `CommitFromToken` (finish the half-committed txn),
   `AbortFromToken` (roll it back), or `UnknownLeaveAsIs` (log
   and leave).
4. **Verify the recovery decision is correct.** Cross-reference
   the consumer offsets at `applicationId-<task>` with the
   prepared-txn list on the external system. If a prepared txn's
   producer cycle did commit (offsets advanced past it), the
   correct action is `CommitFromToken`.
5. **If `UnknownLeaveAsIs` came back, escalate.** Manual review:
   inspect the external system's prepared state, confirm what
   was supposed to happen, finish or roll back by hand.

### Prevent

- Ensure `tpsRecover` is implemented correctly for every
  production sink: not just returning `[]`.
- Make `tpsCommit` and `tpsAbort` strictly idempotent so a
  duplicate call after a partial recovery is a no-op.
- Alert on `commit-cycle-fatal` separately from
  `commit-cycle-aborted`; the latter is normal noise, the
  former is operator-required.

See [Exactly-once across Kafka and other systems](./exactly-once/#the-four-reference-sinks)
for the contract details.

---

## Standby never promotes; lag stays high

**Alert:** Per-task warmup lag for a standby has been above
`acceptableRecoveryLag` for longer than
`2 × probingRebalanceIntervalMs`.

### Diagnosis

The standby's changelog-replay loop can't keep up with the active's
production rate. Common causes:

1. **Network bandwidth saturation.** The standby is reading the
   changelog at a rate lower than the active is writing it.
2. **Local disk throughput.** State writes during replay are
   slower than the changelog read.
3. **`numStandbyReplicas` is too high** for the cluster's
   write capacity. Each replica triples the changelog read fan-
   out.
4. **The active's commit-cycle is faster than the standby can
   catch up between cycles.** Symptom: lag oscillates around a
   floor but never crosses it.

### Resolve

1. **Identify the affected standby.** `LagListener` snapshots
   tell you which task and which store.
2. **Compare standby-replay throughput to active-write
   throughput.** If the gap is structural (active produces faster
   than standby can drain even at idle), the standby will never
   catch up.
3. **For network / disk saturation:** move the standby to an
   instance with more headroom, or reduce `numStandbyReplicas`
   for this task. Both require config changes + restart.
4. **For commit-cycle skew:** raise `commitIntervalMs` on the
   active so batches are larger; the standby's replay cost per
   commit goes down.
5. **As a last resort:** use Riffle's `SnapshotPointer` standby
   mode. The standby stops replicating bytes and instead tracks
   `(snapshotId, advancedTo)`. Promotion fetches the snapshot
   blob + replays the tail. Trade-off: promotion takes longer
   (snapshot fetch time) but steady-state cost is zero.

### Prevent

- Provision the standby instances with at least equal disk and
  network capacity to the active.
- Right-size `numStandbyReplicas` for your write capacity, not
  just your storage capacity.

---

## Orphan internal topics detected

**Alert:** Startup log line `orphan internal topic: <name>` or
the orphan-detector metric > 0.

### Diagnosis

A previous deploy renamed an operator or removed a store. The
broker is keeping the old internal topic (changelog or
repartition) and its disk usage continues.

### Resolve

1. **Confirm it is genuinely orphaned.** Run
   `Kafka.Streams.Observability.OrphanTopics.detectOrphans`
   against the current production topology and the current
   broker topic list. The output should match the alert.
2. **Check for in-flight rolling deploys.** If a v1 instance is
   still running and v2 introduced the rename, the "orphan" is
   actually still in use by v1. Don't delete until v1 is fully
   drained.
3. **Settle.** Wait at least one full commit-cycle multiple
   beyond the slowest instance's drain time.
4. **Delete via the AdminClient.** Use a real broker-side
   `deleteTopics` call. The runtime will not do this for you.
5. **Confirm clean.** Re-run the detector; expect zero output.

### Prevent

- Pin every stateful operator's name with `Named`.
- Run the orphan detector in CI against the deployment-shape
  golden file.
- Treat any topology change that affects internal-topic names as
  a stateful migration (see
  [Topology evolution](./topology-evolution/)).

---

## Producer fenced / `INVALID_PRODUCER_EPOCH`

**Alert:** Runtime log line containing
`InvalidProducerEpochException` or `ProducerFencedException`.

### Diagnosis

Under [EOS-V2](../glossary/#eos--eos-v2--eos-v3) the broker [fences a producer](../glossary/#fenced-producer) when a newer producer
with the same `transactional.id` has been observed. This usually
means:

1. **Two instances of the same `(applicationId, taskId)` are
   somehow alive.** Almost always a misconfigured rollout or a
   zombie from a previous deploy.
2. **A network partition resolved with both sides thinking they
   own the task.** KIP-848's incremental reconciliation makes
   this very rare but not impossible.
3. **`transactional.id.expiration.ms` on the broker has elapsed**
   for an idle producer; the broker forgets it; the instance
   tries to commit and gets fenced.

### Resolve

1. The runtime will already have transitioned the affected task
   to `ERROR` and (depending on
   `setUncaughtExceptionHandler`) either restarted the thread,
   shut down the client, or shut down the application. Default
   per the runtime is to log and try to recover the task.
2. **Identify the zombie.** `metadataForLocalThreads` on every
   instance shows what they think they own. The instance whose
   ownership doesn't match the rebalance log is the zombie.
3. **Kill the zombie.** SIGTERM the OS process. The group
   rebalance will reassign cleanly.
4. **If the cause was broker-side TXN expiration:** raise
   `transactional.id.expiration.ms` on the broker for this
   workload, or shorten the EOS commit interval so the producer
   isn't ever idle long enough to expire.

### Prevent

- Use process supervisors that kill old generations before
  starting new ones (no overlapping lifecycle).
- Set sensible `instance.id` for `static membership` so
  rebalances don't churn during normal restarts.

---

## Async I/O backpressure causes stream-thread stall

**Alert:** Async-operator `aio-deposit-rate` near zero while
`aio-enqueue-rate` also near zero (the queue is full and the
stream thread is blocked).

### Diagnosis

The external system the async operator is calling has slowed
down or is failing. The in-flight queue
(`aioBufferCapacity`) fills, the stream thread blocks on
enqueue, and the entire downstream pipeline stalls.

This is **working as intended**: it's the backpressure signal -
but it should not last long.

### Resolve

1. **Check the external system.** If it's down, the right
   action is at the external system, not at the streams app.
2. **If the external system is slow but up:** check the async
   operator's `aioRetry` and `aioTimeout`. A long timeout with
   retries means each in-flight slot is occupied for
   `(attempts + 1) × timeout` worst case. The queue can never
   drain faster than that.
3. **If the queue is structurally too small:** raise
   `aioBufferCapacity` (requires restart). The trade-off is
   more in-flight memory and a longer pre-commit drain.
4. **If the failures are partial:** consider switching
   `aioOnFailure` from `FailTask` to `LogAndContinue` so a
   minority of failures don't shed the whole pipeline.
5. **If the external system is overwhelmed:** the async
   operator is doing what you asked: flooding it. Drop
   `aioWorkers` (restart required) to reduce concurrent calls.

### Prevent

- Size `aioBufferCapacity ≈ 4 × aioWorkers` so brief stalls
  don't immediately propagate.
- Set `aioTimeout` to the external system's p99 + a buffer,
  not its average.
- Monitor `aio-failure-rate` separately; sustained high
  failure rate is its own incident.

---

## Schema deserialisation flood

**Alert:** `droppedRecordsTotal` rate spike, paired with a
matching rate on the `DeserializationException` log channel.

### Diagnosis

An upstream producer started writing records that the current
deserialiser can't parse. (The [railway-oriented programming](../concepts/railway-oriented-programming/) page explains where this routing decision lives and how a DLQ wired through the `DeserializationHandler` gives you reprocessability.) Three usual causes:

1. **Schema Registry compat policy was bypassed**: a new schema
   was published without compatibility checks, and your
   `registrySerdeChecked` wrapper rejects every record. (This
   means the wrapper is doing its job; the producer is the
   problem.)
2. **A new producer service rolled out** with a different wire
   format and skipped Schema Registry entirely.
3. **A new field with a default value was added correctly**,
   but your generated decoder doesn't have it.

### Resolve

1. **Identify the offending source.** Look at the
   `DeserializationException` payload to see the bad record;
   trace it to its producer.
2. **Stop the bleeding.** If you're losing important records,
   switch the deserialiser handler from `logAndContinue` to
   `failFast` so the stream stops processing. The records remain
   on the topic and can be re-processed once the bug is fixed.
   (Be careful: with `failFast`, your consumer group stops
   making progress until the bad records are dealt with.)
3. **Fix the producer.** Either roll the producer back, or
   re-publish through Schema Registry with the correct
   compatibility check.
4. **Re-process the dropped records.** Use the
   `processingException.handler` `DEAD_LETTER` policy
   (KIP-1033) if you have one configured; otherwise re-consume
   from the offending offset range with a one-shot consumer.

### Prevent

- Use `registrySerdeChecked` for every Schema Registry-backed
  serde. It probes the per-subject compatibility mode and fails
  fast at construction.
- Alert on `droppedRecordsTotal` rate, not just absolute count.
- Enforce Schema Registry compatibility at the producer side
  too: don't rely on the consumer being the only check.

---

## Local state directory grows without bound

**Alert:** `stateDir` disk usage exceeds expected ceiling.

### Diagnosis

State stores grow with the number of unique keys. Common causes
of unbounded growth:

1. **The source topic is not compacted** (or has very long
   retention) and you're materialising every key ever seen.
2. **A windowed store has `withGracePeriod`** longer than your
   retention budget assumes.
3. **A KTable is built off a topic with very high unique-key
   cardinality** and you didn't realise.
4. **Old standby task directories** from a prior deploy were
   never cleaned up.

### Resolve

1. **Identify which store** is growing. RocksDB has per-store
   subdirectories under `stateDir`; sizing them is direct.
2. **For (1) and (3):** add a TTL via
   `Kafka.Streams.State.KeyValue.TTL` (wall-clock) or
   `EventTimeTTL` (driven off the coordinated watermark). This
   actively expires entries on every read; pair with a punctuator
   that calls `expireBefore` for active sweeping.
3. **For (2):** confirm the grace period matches the window
   retention. A 1-hour window with a 7-day grace materialises 7
   days of state.
4. **For (4):** `cleanUp` wipes the local directory; the runtime
   re-warms from the changelog or snapshot on next start. Safe
   when standbys exist; loses work otherwise.

### Prevent

- Always pick a `KeyValueStore` backend that fits the cardinality
  budget. In-memory is fine for ≤10⁶ keys; for more, use
  RocksDB (`+rocksdb` flag), the Riffle tiered (hot + cold S3)
  backend, or the remote-KV backend.
- Apply TTLs proactively for any topology where the key
  cardinality is unbounded.
- Monitor `stateDir` size as a first-class metric, not just disk
  usage. [Running in containers → disk sizing](./containers/#4-disk-sizing)
  has the budgeting formula.

---

## EOS commit cycle taking longer than `commitIntervalMs`

**Alert:** `commit-duration` p99 approaches `commitIntervalMs`.

### Diagnosis

The commit cycle (`runCommitCycle`) walks six steps:
`beginTxn → flush → commitOffsets → preCommit2PC → commitTxn →
commit2PC → storeCommit`. Any of them can take time. Most likely:

1. **`flush` is slow** because there are many records in the
   transactional buffer (high commit interval, large per-record
   processing cost).
2. **`commitTxn` round-trip to the broker is slow** (network
   latency, broker load).
3. **`commit2PC` is slow** because the external sink's commit
   operation is expensive (e.g. Iceberg manifest commit on a
   large dataset).
4. **`storeCommit` is slow** because the KIP-892 transactional
   buffer has accumulated many writes.

### Resolve

1. **Per-step timing.** `Kafka.Streams.Metrics` exposes per-step
   counters. Identify which step dominates.
2. **For (1):** consider lowering `commitIntervalMs` so each
   cycle has fewer records. Trade-off: more commit overhead per
   record, but better tail latency.
3. **For (2):** check broker health; this is rarely the
   bottleneck if the broker is healthy.
4. **For (3):** raise `commitIntervalMs` so 2PC commits are
   amortised over more records. Be aware this widens the
   reprocessing window on a fault.
5. **For (4):** tune cache size (`cacheMaxBytesBuffering`) so
   writes coalesce more aggressively before they hit the store.

### Prevent

- Benchmark the commit cycle under your expected throughput at
  load-test time; size `commitIntervalMs` accordingly.
- For 2PC sinks, the commit is structurally per-cycle; size
  cycles for the sink's commit cost, not the per-record cost.

---

## Interactive query returns `StoreNotFound` during a rebalance

**Alert:** Spikes of 404s on an IQ-fronted endpoint during a
rolling deploy.

### Diagnosis

During a rebalance, a partition (and its state store) is
transiently unowned. Queries routed to the previous owner get
`StoreNotFound`; queries routed to the new owner get the same
until the store has been re-bound (instant with standby + KIP-848,
slow without).

### Resolve

1. **In the query layer, retry with backoff and refresh
   metadata.** `Kafka.Streams.Discovery.StreamsMetadata` updates
   as the rebalance completes; a retry after `~1s` usually
   succeeds.
2. **Optionally fall through to a standby** for the duration of
   the rebalance. `KeyQueryMetadata.standbyHosts` returns every
   live standby; a stale read is usually better than a 404.
3. **If the rebalance is taking minutes**, see "Standby never
   promotes; lag stays high" above: that's actually the
   underlying issue.

### Prevent

- Build the query layer with the rebalance-window assumption
  baked in. A naïve "one shot, fail on 404" client will be
  brittle.
- Use `numStandbyReplicas >= 1` so the rebalance is
  metadata-only and the unavailability window is sub-second.

---

## Reading the metrics during an incident

**When you don't know where to start:** Every incident produces symptoms, but
the same symptom ("things are slow") can have different root causes (CPU
saturation, disk I/O, network latency, or a poison-pill record). This table
gives you a diagnostic path. Start with the symptom you're seeing, check the
"First metric" column, and use that reading to decide what to check next.

Quick reference for which metric to look at first:

| Symptom | First metric | Then |
| ------- | ------------ | ---- |
| "Things are slow" | `process-latency` (p50, p99) per node | Drill into the slowest node |
| "Throughput dropped" | `processTotal` rate per node | If a single node, check its `process-latency`; if global, check `commit-cycle-aborted` |
| "Records seem to disappear" | `droppedRecordsTotal` | Check `DeserializationException` log channel |
| "Rebalance loop" | `task-restart-total` | Check `setRebalanceListener` log |
| "EOS issues" | `commit-cycle-aborted` and `-fatal` | The reason field tells you which step failed |
| "Standby not promoting" | per-task warmup lag from `LagListener` | Check standby replay throughput vs active write rate |

---

## Related reading

- [Observability](./observability/): the metric surface this
  page leans on.
- [Topology evolution](./topology-evolution/): the deployment
  procedures these runbooks reference.
- [Exactly-once across Kafka and other systems](./exactly-once/) -
  the EOS internals behind the commit-cycle runbooks.
