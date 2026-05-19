---
title: Dynamic topology changes
description: What you can change about a running KafkaStreams instance, what you can change only by restarting, and what you can't change at all without an applicationId migration.
sidebar:
  order: 3
---

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

Kafka Streams' [DSL](../glossary/#dsl) builds a [`Topology`](../glossary/#topology) value once, at compile time.
The runtime takes that value, validates it, walks the graph, and
binds it to [consumer-group](../glossary/#consumer-group) state, [internal topics](../glossary/#internal-topic), and local [state stores](../glossary/#state-store).
After that point the topology shape is frozen for the life of the
`KafkaStreams` instance — but a handful of things around the
topology can still change. This page is the map of what's mutable
where.

## The four tiers

| Tier | Example | How to change |
| ---- | ------- | ------------- |
| Hot (live, no rebalance) | Worker count, pause/resume, metrics, IQ routing, EOS coordinator swap | Function call on the running `KafkaStreams` |
| Warm (live, one rebalance) | Group membership, standby replica count (effectively next rebalance), KIP-848 reconciliation | Add/remove an instance |
| Restart-required | `processingGuarantee`, `dispatchMode`, `numStandbyReplicas` materially, every `StreamsConfig` field except those above | Restart the process |
| Migration-required | Topology shape (operators, stores, serdes), `applicationId`, key-group count | Re-deploy with a topology-change procedure |

## Hot tier — change without a rebalance

These operations modify the running instance without disturbing the
consumer group.

### Worker count

```haskell
import qualified Kafka.Streams.Runtime as R

R.addStreamThread streams        -- mirrors Java's addStreamThread
R.removeStreamThread streams     -- mirrors Java's removeStreamThread
R.numStreamThreadsRunning streams
```

Worker pool is reshaped in-process; no broker-side rebalance. Use
for autoscaling within a process. The worker that's added /
removed reshuffles its share of the local partition assignment
across the surviving workers; state stays put for partitions whose
owning worker doesn't change.

### Pause / resume

```haskell
R.pauseKafkaStreams  streams
R.resumeKafkaStreams streams
R.isPausedKafkaStreams streams
```

Stops record processing. The consumer keeps heartbeating so the
group doesn't rebalance. Use for:

- Quiescing an instance before an in-place state migration.
- A maintenance window that shouldn't shed work.
- Coordinating with an out-of-band job that needs exclusive
  state access.

### State listener

```haskell
R.setStateListener streams (\old new -> publishLog ("transition: " <> show (old, new)))
```

Fires on every `StreamsStatus` transition. Idempotent — replacing
the listener doesn't re-fire prior transitions.

### Rebalance listener

```haskell
R.setRebalanceListener streams handler
```

Fires on every `assign` / `revoke` event. Use to drain external
resources keyed by partition (per-partition file handles, etc.)
before the partition moves.

### Production / processing / uncaught exception handlers

```haskell
R.setProductionExceptionHandler streams handler
R.setProcessingExceptionHandler streams handler
R.setUncaughtExceptionHandler   streams handler
```

All three are hot-swappable. The runtime calls the current handler
on the next eligible exception. Use to escalate behaviour
mid-incident (e.g. tighten from `LogAndContinue` to `FailTask`
when a downstream system is sick).

### EOS coordinator

```haskell
R.applyEOSCoordinator streams coordinator
```

Replace the coordinator the commit cycle drives. Production swap
is rare; tests use this to inject a recording coordinator. If you
swap mid-cycle, the new coordinator takes over on the next
commit.

### Lag listener

```haskell
R.setLagListener streams (\lags -> publishMetrics lags)
```

Receives a snapshot of every task's lag on each publish.
Hot-swappable.

### Standby update / global restore listeners

```haskell
R.setStandbyUpdateListener streams ...
R.setGlobalUpdateListener  streams ...
```

For deeper hooks into the standby and global-store restore
lifecycle. Useful when you want a UI to show "currently restoring
store X, N records left".

## Warm tier — change with one rebalance

These operations require coordination with the consumer group.
The change takes effect on the next rebalance, which is
incremental under KIP-848 so the disruption is bounded.

### Group membership

Adding a new OS process that joins the same `applicationId`
triggers a rebalance. Under KIP-848 the reconciliation is per-task
incremental:

1. New instance heartbeats with its `Subscription`
   (`subscribedTopics`, empty `currentlyOwned`, fresh
   `memberEpoch`).
2. Coordinator computes new `TargetAssignment`.
3. Existing instances see tasks in their `rRemove` and release
   them on the next heartbeat.
4. New instance sees those tasks in its `rAdd` and picks them up.

No double ownership at any point. See
[Scaling](../operating/scaling/#processes-across-the-group) for
the full reconciler shape.

### `application.server` advertisement

```haskell
R.setApplicationServer streams "host:port"
```

Changes the host:port this instance advertises via JoinGroup
subscription metadata. Peers learn the new value at the next
rebalance. Useful when an instance moves to a different network
address (e.g. across a load balancer cutover).

### Standby effective replica count

`numStandbyReplicas` in `StreamsConfig` is read at startup, but
the standby state machine doesn't aggressively tear down replicas
on a live reduction — it just lets them lag out. To meaningfully
change the number, restart the instance; the change takes effect
on the next assignment.

### `setRackId` for rack-aware assignment

Not all rack-aware behaviour is hot — but the rack tag the
instance advertises is.

## Restart-required tier

Restart the instance. The runtime re-reads `StreamsConfig` and
re-wires.

| Knob | Why restart |
| ---- | ----------- |
| `processingGuarantee` | Producer / consumer wiring (transactional vs idempotent) is set at construction |
| `dispatchMode` | Worker pool is built around it; routing function bakes in at startup |
| `numStreamThreads` initial value | Use `addStreamThread` for live change instead |
| `commitIntervalMs` | Read once at startup by the commit-cycle scheduler |
| `pollMs` | Same |
| `cacheMaxBytesBuffering` | Each store's caching layer sizes itself at construction |
| `maxTaskIdleMs` | Read at construction |
| `replicationFactor` | Determines internal-topic creation at startup |
| `stateDir` | Local-store paths bake in |
| `taskAssignor` / `taskAssignorClass` | Assignor is selected at startup |
| `rackAwareAssignmentStrategy` and costs | Same |
| `acceptableRecoveryLag`, `maxWarmupReplicas`, `probingRebalanceIntervalMs` | The probing-rebalance scheduler is built around them |
| `applicationServer` initial value | Hot-changeable via `setApplicationServer`, but the *first* read is at startup |
| `taskTimeoutMs` | Per-task supervisor reads at construction |
| `defaultDeserHandler`, `defaultProductionHandler` | The hot-swap handlers cover the production / processing / uncaught axes, not these defaults |
| `windowSizeMs` | Default windowed-serde auto-resolver consults at construction |
| `upgradeFrom` | Read once at startup by the assignor |

Procedure: drain, restart, rejoin the group. With standbys, the
restart is metadata-only.

## Migration-required tier

Some changes need more than a restart. They require an explicit
data migration, a topic rename, or a fresh `applicationId`.

### Topology shape

The compiled `Topology` value is built at compile time. To change
it:

1. Edit the topology source.
2. Recompile.
3. Roll out per [Topology evolution](../operating/topology-evolution/).

The deploy procedure depends on the diff (rename, add, remove,
schema change) — see that page.

### `applicationId`

The `applicationId` is the consumer-group identity and the prefix
for every internal topic. Changing it means:

1. A new consumer group starts from scratch (no preserved offset).
2. New internal topics are created; old ones become orphans.
3. Active queries are temporarily empty until the new state is
   re-built from upstream.

Treat this as a fresh-start deploy, not a rollout. Two cases
where you might do this deliberately:

- **Migrating to a different broker cluster** without taking the
  app down — run old and new in parallel until the new catches
  up, then cut over.
- **Resetting a corrupted application state** — wipe the old
  `applicationId`'s internal topics, change the id, restart from
  scratch.

In either case, the orphan-topic detector on the **new**
`applicationId` will not flag the old topics (different prefix);
clean them up manually.

### Key-group count

`KeyGroupConfig.kgcTotal` is treated as immutable for the life of
the application under `DispatchKeyGroup`. Changing it requires
the same kind of state-store migration you'd run for a topic
repartition: drain, wipe local state, restart with the new count,
warm from changelog or snapshot.

This is one reason the default key-group count (128) is generous:
you almost never need to change it. If you do, plan a maintenance
window or a parallel-cluster cutover.

### Serde change on a stateful store

Two options, depending on whether the old and new serdes can
round-trip each other:

- **Yes** — schema-compatible evolution. Deploy normally.
- **No** — use `Kafka.Streams.State.KeyValue.SchemaVersioned` to
  version-tag every write and migrate reads forward. The wrapper
  takes a `SchemaMigration` chain that the runtime applies
  transparently on read. `burnInMigrate` rewrites older entries
  in-place with resumable `BurnInProgress`.

The full story is in [Topology evolution](../operating/topology-evolution/#5-changing-the-key-value-or-window-of-an-existing-store).

## Compile-only API patterns to avoid

Two idioms produce a "dynamic-looking" topology that is actually
recompiled and redeployed:

1. **"Plugin" processors.** Tempting design: dispatch records by a
   field to one of N user-supplied processors, registered at
   runtime. Reality: every "registration" is part of the topology
   shape, and adding one requires a recompile. Use a single
   processor with a runtime-loaded function table inside — the
   topology stays fixed; the function table can be hot-reloaded
   from a config source.
2. **"Schema-driven" routes.** Tempting: read a routing config
   from Kafka, build new sinks at runtime. Reality: sinks are
   part of the compiled topology. Use one sink with a
   `TopicNameExtractor` (KIP-303, `Kafka.Streams.KStream.TopicNameExtractor`)
   that derives the target topic from the record + a runtime
   config. The DSL knows about this pattern and supports it via
   `toExtracted`.

## What a "dynamic-topology" feature would look like (and why it isn't shipped)

A truly dynamic topology — where you add a new branch to a running
graph without restarting — would need:

- A way to introduce a new processor node into the validated
  topology in place.
- A way to coordinate the new node's state-store creation across
  the consumer group atomically.
- A way to advance the topology version on every peer so the
  rebalance protocol doesn't reject the new shape.

That feature isn't shipped. The parity port mirrors the JVM
client, which also doesn't support it; the Riffle work has
explicitly stayed inside the "topology is a value" model because
the alternative undermines the AST-as-truth invariant that the
optimiser, the topology JSON, and the orphan detector all rely
on.

The closest equivalents the library does ship:

- `TopicNameExtractor` for dynamic sinks (one sink, many target
  topics).
- `Branched.withConsumer` for dynamic processing (one stream,
  many runtime-registered consumers — within the bounds of the
  compiled topology).
- `addReadOnlyStateStore` and a punctuator-driven loader for
  dynamic reference data (one fixed store, content loaded at
  runtime).

These get you most of the operational benefit of "dynamic
topology" without the runtime-consistency mess.

## A summary table

| Want to change… | Layer | Procedure |
| --------------- | ----- | --------- |
| Number of workers in this process | Hot | `addStreamThread` / `removeStreamThread` |
| Pause processing | Hot | `pauseKafkaStreams` |
| State / rebalance / exception listeners | Hot | `set*Listener` |
| EOS coordinator | Hot | `applyEOSCoordinator` |
| Application server host:port | Hot | `setApplicationServer` |
| Add an instance | Warm | Start a new process with same `applicationId` |
| Remove an instance | Warm | Drain + close |
| Commit cadence, cache size, etc. | Restart | Update `StreamsConfig`, restart |
| Processing guarantee | Restart | Drain → bring up new generation with new guarantee |
| Dispatch mode | Restart | `cleanUp` recommended |
| Topology shape | Migration | Follow [Topology evolution](../operating/topology-evolution/) |
| Application id | Migration | Treat as fresh-start deploy |
| Key-group count | Migration | Drain + wipe + restart |
| Stateful-store serde | Migration | `SchemaVersioned` or double-write rename |

## Related reading

- [Topology evolution](../operating/topology-evolution/) — the
  migration-required tier in detail.
- [Scaling](../operating/scaling/) — the warm tier in detail.
- [Topology optimization](./topology-optimization/) — what the
  AST rewrites do once you decide to recompile.
