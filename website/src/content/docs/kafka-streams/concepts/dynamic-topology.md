---
title: Dynamic topology changes
description: What you can change about a running KafkaStreams instance, what requires a restart, and what requires a full migration.
sidebar:
  order: 3
---

When you deploy a Kafka Streams application, you might need to change its configuration or behavior. Some changes are instant. Others require a restart. Some require careful migration procedures.

This guide explains the four tiers of change mutability and how to handle each one safely.

## Why changes have different requirements

Kafka Streams ties your topology to persistent state:
- **Internal topics** (changelog, repartition) are created based on your topology shape
- **State stores** are written to local disk with specific formats
- **Consumer group membership** determines who processes which partitions
- **Task assignments** pin specific work to specific instances

Changing something that affects these requires care. The framework enforces safety by making expensive changes explicit.

## The four tiers of change

| Tier | What you can change | How to change it | Examples |
| ---- | ------------------- | ---------------- | ---------- |
| **Hot** | Runtime configuration | Function call on running instance | Worker threads, pause/resume, listeners |
| **Warm** | Group membership | Add/remove instances | Scaling horizontally, rolling deploys |
| **Restart** | Startup configuration | Restart the process | Processing guarantee, dispatch mode, most config |
| **Migration** | Topology fundamentals | Follow a procedure | Change topology shape, application ID, serde |

## Hot tier: change without stopping

These changes take effect immediately without disturbing processing.

### Adjust worker thread count

Add or remove processing threads within your application:

```haskell
import qualified Kafka.Streams.Runtime as R

R.addStreamThread streams     -- Add a worker
R.removeStreamThread streams  -- Remove a worker
```

**Why this is hot:** Workers share the same consumer connection. Adding one just reshuffles partition assignments within the process. No broker coordination needed.

**Use when:** Your CPU usage is low and you want more parallelism, or you want to reduce threads during quiet periods.

### Pause and resume processing

Temporarily stop processing without leaving the consumer group:

```haskell
R.pauseKafkaStreams streams   -- Stop processing
R.resumeKafkaStreams streams  -- Resume processing
```

**Why this works:** The consumer keeps heartbeating, so the broker knows the instance is alive. But no records are polled or processed.

**Use when:**
- You need exclusive access to state stores for maintenance
- You're coordinating with an external batch job
- You want to pause during a dependency outage without triggering rebalances

### Register event listeners

Add or replace listeners for observability:

```haskell
-- State transitions (RUNNING, REBALANCING, ERROR)
R.setStateListener streams $ \old new ->
  publishLog ("State: " <> show old <> " -> " <> show new)

-- Rebalance events (assignments, revocations)
R.setRebalanceListener streams $ \event ->
  case event of
    Assigned partitions -> warmCaches partitions
    Revoked partitions  -> flushBuffers partitions
```

**Why this is hot:** Listeners observe; they don't change processing. Replacing a listener just changes who gets notified.

**Use when:** You need to add monitoring, change alert destinations, or adjust debug logging.

## Warm tier: change with one rebalance

These changes require coordination with the consumer group but don't interrupt processing for long.

### Scale by adding instances

Start a new process with the same `applicationId`:

```haskell
-- In a new terminal or pod
my-streams-app  -- same config, same applicationId
```

**What happens:**
1. New instance joins the consumer group
2. Group coordinator computes new partition assignments
3. Existing instances release some partitions (incremental rebalance)
4. New instance picks up its share
5. Processing resumes on all instances

**Time to resume:** Typically seconds. The new instance must replay changelog tails for assigned state stores.

### Scale by removing instances

Stop a process cleanly:

```haskell
R.closeKafkaStreams streams  -- Clean shutdown
```

**What happens:**
1. Instance leaves the consumer group
2. Remaining instances get reassigned its partitions
3. If standbys exist, they become active (fast)
4. If no standbys, replay from changelog (slower)

**Why warm, not hot:** The group must rebalance to redistribute work. Other instances are affected.

## Restart tier: change configuration

These settings are read once at startup and baked into the runtime:

| Setting | Why it requires restart |
| ------- | ----------------------- |
| `processingGuarantee` | Determines whether producer is transactional |
| `dispatchMode` | Worker pool routing function baked in |
| `numStandbyReplicas` | Standby state machine initialized at startup |
| `commitIntervalMs` | Commit cycle scheduler configured at start |
| `stateDir` | Local paths opened at initialization |

**How to change:**
1. Update configuration
2. Drain the instance (stop processing cleanly)
3. Restart with new config
4. Rejoin the consumer group

**Best practice for rolling restarts:**
- Update one instance at a time
- Wait for it to reach RUNNING state before proceeding
- With standbys configured, each restart is fast (metadata-only promotion)

## Migration tier: change fundamentals

These changes affect data layout or identity and require explicit procedures.

### Change topology shape

Adding, removing, or renaming operators affects internal topics and state stores.

**The problem:** Your old changelog topic contains state in the old format. The new topology expects a different format or different stores entirely.

**Procedure:**
1. Review the change in [topology evolution](../operating/topology-evolution/)
2. Run golden-file diff in CI to see what changes
3. For stateful operator changes: drain, deploy, accept warmup cost
4. For renames: old topics become orphans, clean up after verification

### Change application ID

The `applicationId` is your consumer group identity and internal topic prefix.

**The problem:** Changing it means:
- New consumer group (starts from scratch, no offsets)
- New internal topics created (old ones become orphans)
- State rebuilt from upstream (queries return empty until catch-up)

**Procedure:**
- Treat as a fresh-start deployment
- Run old and new in parallel during transition if possible
- Clean up old internal topics after confirming new app is stable

### Change state store serde

Changing how state is serialized affects changelog compatibility.

**Options:**
- **Schema-compatible:** Both old and new can read each other's data. Deploy normally.
- **Schema-incompatible:** Use `SchemaVersioned` store to migrate reads forward. Or double-write and cut over.

See [topology evolution](../operating/topology-evolution/#changing-serde) for full procedure.

## What you cannot change (and why)

Some requested features aren't supported because they would undermine correctness:

### Truly dynamic topology

**Request:** Add a new processing branch to a running topology without restarting.

**Why not supported:**
- New nodes need state stores created atomically across the group
- Rebalance protocol would need to version topology shapes
- Optimizations assume static topology for correctness

**Alternatives:**
- Use `TopicNameExtractor` for dynamic sink topics (one topology, many destinations)
- Use `Branched.withConsumer` for runtime-registered handlers (within fixed topology)
- Deploy new topology version as a separate application

### Change key-group count live

**Request:** Scale `kgcTotal` without restart.

**Why not supported:** Key-group routing is baked into state store sharding. Changing it requires re-partitioning state.

**Alternative:** Drain, wipe local state, restart with new count, warm from changelog.

## Summary table

| Change | Tier | Downtime | Procedure |
| ------ | ---- | ---------- | --------- |
| Add worker threads | Hot | None | Function call |
| Pause/resume | Hot | None | Function call |
| Add listener | Hot | None | Function call |
| Add instance | Warm | Seconds | Start new process |
| Remove instance | Warm | Seconds | `closeKafkaStreams` |
| Change processing guarantee | Restart | Minutes | Rolling restart |
| Change commit interval | Restart | Minutes | Rolling restart |
| Rename operator | Migration | Minutes | Drain + deploy |
| Change serde | Migration | Minutes | Schema migration or double-write |
| Change application ID | Migration | Hours | Fresh deploy, parallel run |

## Related reading

- [Topology evolution](../operating/topology-evolution/): Detailed procedures for migration-tier changes
- [Scaling](../operating/scaling/): Details on warm-tier scaling
- [Running in containers](../operating/containers/): Configuring for Kubernetes and similar environments
