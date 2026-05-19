---
title: Kafka Streams overview
description: Mental model for wireform-kafka-streams — the library, the Riffle extensions, and how the runtime fits into your service.
sidebar:
  order: 1
---

:::caution[Not yet on Hackage]
wireform is in heavy development and has not been published to Hackage yet.
APIs may change. Use it today by adding the repo as a path dependency.
:::

`wireform-kafka-streams` is a Haskell port of [Apache Kafka
Streams](https://kafka.apache.org/documentation/streams/). It mirrors the
JVM DSL operator-for-operator and the runtime contracts
semantics-for-semantics, then layers an additive extension tier on top —
called **Riffle** — that closes the operational gaps that historically
drove teams off Kafka Streams onto Flink.

This section assumes you already know what Kafka is, what a partition is,
what a consumer group does, and roughly what a streaming topology looks
like. It does **not** assume you have run Kafka Streams in production
before. The pages here exist because the differences from a normal
request/response service or from an ACID database are not obvious until
they hurt.

## The library, not a cluster

The most important fact about wireform-kafka-streams is that it is a
**library**. Your service `import`s it, builds a topology value, and
runs the runtime inside the same OS process that serves the rest of
your app. There is no JobManager, no TaskManager fleet, no JAR
submission step. A "deploy" is a normal binary rollout.

That has consequences:

| Concern                | wireform-kafka-streams | Flink-style cluster |
| ---------------------- | ---------------------- | ------------------- |
| Deploy unit            | Your service binary    | A job graph submitted to a cluster |
| Scale-out unit         | More OS processes joining the consumer group | More TaskManagers |
| State location         | Local disk + (optionally) S3 / object store | Cluster-managed checkpointing |
| Rolling deploys        | Normal blue/green / canary on your own service | Job-level savepoint + restart |
| Cross-service coupling | The Kafka topics, period | The cluster + the topics |

The runtime gives you the same semantics as the JVM Streams client
because it speaks the same wire protocol to the broker. Kafka does most
of the heavy lifting around coordination (consumer groups, transactional
producers, log compaction). The library wires those primitives into a
declarative DSL and adds local state stores, watermarks, and the rest.

## Two layers: parity + Riffle

The codebase has two layers, and you can pick which one a given
topology uses on a per-call basis.

| Layer | What it gives you | When to reach for it |
| ----- | ------------------ | -------------------- |
| **Parity** — `Kafka.Streams.*` minus the Riffle modules | Operator-for-operator port of Apache Kafka Streams 4.0 | You want the well-understood JVM Streams contract, just in Haskell |
| **Riffle** — `Kafka.Streams.AsyncIO`, `Kafka.Streams.Sinks.TwoPhase`, `Kafka.Streams.Watermark`, `Kafka.Streams.Runtime.KeyGroup`, `Kafka.Streams.State.KeyValue.{Snapshot, Tiered, Remote, Versioned, TTL, SchemaVersioned}`, `Kafka.Streams.Sources.CDC`, `Kafka.Streams.EmitPolicy`, `Kafka.Streams.Observability.*` | Async enrichment with backpressure + EOS, snapshot-based state recovery, two-phase commit to non-Kafka sinks, cross-source watermarks, key-group rescaling, structured topology observability | You hit any of the operational walls the JVM-parity surface cannot get past |

Riffle is **strictly additive**. Every feature is opt-in via a new
smart constructor, a new field defaulting to the legacy value, or a
new module. A topology that uses no Riffle features compiles to the
same imperative graph the parity-only compiler would emit.

If you read [`RIFFLE_SPEC.md`](https://github.com/iand675/wireform-/blob/main/wireform-kafka/streams/RIFFLE_SPEC.md)
you will see the design contract for the post-parity work. Everything
in that document is either landed (look for **Landed.** markers) or
explicitly deferred. The pages in this section describe the **landed**
surface and how to operate it.

## What a topology actually is

In wireform-kafka-streams a topology is a first-class Haskell value of
type `Topology i o`:

```haskell
import Control.Category ((>>>))
import Kafka.Streams

topology :: Topology Void ()
topology =
  source "in" textSerde textSerde
    >>> mapValues T.toUpper
    >>> filter   (\r -> recordValue r /= "")
    >>> sink "out" textSerde textSerde
```

The DSL is a `FreeArrow Prim` in `Kafka.Streams.Topology.Free`. You
compose with `Control.Category.(>>>)` and the `Arrow` operators; joins
pair their legs with `Control.Arrow.(&&&)` or the dedicated `join*`
helpers. The value is inspected, optimised, and only at the boundary
compiled to the imperative `Kafka.Streams.Topology` graph that the
runtime executes.

This matters operationally because the topology is **a value that
exists in memory before the broker hears about it**. You can render it,
diff it across versions, validate it without a broker, and snapshot it
into your tests. The [topology evolution](./operating/topology-evolution/)
and [observability](./operating/observability/) pages lean on this.

## Where to go next

| If you want to… | Read |
| --------------- | ---- |
| Understand how a binary rollout interacts with state, changelogs, and consumer groups | [Topology evolution and rolling deploys](./operating/topology-evolution/) |
| Scale beyond what the partition count allows, or rebalance without a stop-the-world pause | [Scaling and rebalancing](./operating/scaling/) |
| Make a sink to a non-Kafka system (Postgres, S3, Iceberg, HTTP) atomic with the upstream commit | [Exactly-once across Kafka and other systems](./operating/exactly-once/) |
| Enrich records from an external API without blocking the stream thread | [Enrichment via external systems](./guides/enrichment/) |
| Wire up metrics, topology JSON, orphan-topic detection, lag | [Observability](./operating/observability/) |
| Know how a stream's "visibility" differs from a SQL `SELECT` | [Visibility versus ACID databases](./operating/visibility/) |
| Know which topology rewrites happen automatically | [Topology optimization](./concepts/topology-optimization/) |
| Know what can be changed at runtime versus what requires a redeploy | [Dynamic topology changes](./concepts/dynamic-topology/) |
| Handle an on-call incident | [Runbooks](./operating/runbooks/) |

## Reading order

A new operator should read these in this order:

1. This page.
2. [Topology evolution](./operating/topology-evolution/) — the single
   highest-stakes thing you will get wrong if you don't read about it
   first.
3. [Scaling](./operating/scaling/).
4. [Visibility versus ACID](./operating/visibility/) — wraps your head
   around why streams behave the way they do.
5. [Observability](./operating/observability/).
6. [Runbooks](./operating/runbooks/) — keep open during incidents.

Everything else is referenced from those five pages.
