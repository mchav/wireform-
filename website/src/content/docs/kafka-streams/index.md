---
title: Kafka Streams
description: A library for building stateful, fault-tolerant streaming pipelines in Haskell — Apache Kafka Streams parity plus the Riffle extensions tier.
sidebar:
  order: 1
  label: Overview
---

`wireform-kafka-streams` is a Haskell library for building
streaming applications on top of Apache Kafka. It mirrors the
Apache Kafka Streams DSL one-to-one and layers an additive
extension tier — **Riffle** — on top, closing the operational
gaps that historically pushed teams toward Flink.

You'll write topologies as ordinary Haskell values, run them
inside your service (no separate cluster), and lean on Kafka for
durability and ordering. The library handles the rest: state
stores, joins, windowing, exactly-once, rebalancing.

## Where to start

| If you... | Go to |
| --------- | ----- |
| Want it running in 5 minutes | [Quickstart](./get-started/quickstart/) |
| Are new to Kafka Streams | [Tutorial part 1: What is Kafka Streams?](./get-started/what-is-kafka-streams/) |
| Have used JVM Kafka Streams before | Skim [Riffle: Flink-class extensions](./riffle/), then jump to [Operations](#operations) |
| Need to ship to production | [Tutorial part 5: Going to production](./get-started/going-to-production/) |
| Are reading an alert | [Runbooks](./operating/runbooks/) |
| Hit a term you don't know | [Glossary](./glossary/) |

## The tutorial

Five parts, about 30 minutes end-to-end. Each part is
self-contained code you can run against an in-process test
driver — no Kafka broker required.

1. **[What is Kafka Streams?](./get-started/what-is-kafka-streams/)** —
   the mental model, vocabulary, and where this library fits next
   to Flink and a plain consumer.
2. **[Your first topology](./get-started/your-first-topology/)** —
   read from one topic, write to another. The minimum viable
   pipeline.
3. **[Stateful processing](./get-started/stateful-processing/)** —
   count words across a stream and look up the counts via
   interactive queries.
4. **[Joins and tables](./get-started/joins-and-tables/)** —
   enrich a stream of page views against a table of user
   profiles.
5. **[Going to production](./get-started/going-to-production/)** —
   the eight things to set up before deploying for real.

## Riffle: the extensions tier

The library has two layers. You can ignore the second until you
need it.

| Layer | What |
| ----- | ---- |
| **Parity** | Operator-for-operator port of Apache Kafka Streams 4.0 |
| **Riffle** | Additive Flink-class extensions: async I/O with backpressure, snapshot-based state recovery, two-phase commit to non-Kafka sinks, cross-source watermarks, key-group rescaling |

Riffle features are strictly additive. A topology using nothing
from Riffle compiles to byte-for-byte the same graph as the
parity-only compiler. Each feature is a new module or a new
smart constructor; opting in for one doesn't change anything
else.

Tour: [Riffle: Flink-class extensions](./riffle/).

## Operations

The operations section is the bulk of the docs. It's reference
material organised by the question you have in front of you:

| You're asking… | Read |
| -------------- | ---- |
| "How do I roll out a new version without breaking state?" | [Topology evolution and rolling deploys](./operating/topology-evolution/) |
| "How do I scale this past my partition count?" | [Scaling and rebalancing](./operating/scaling/) |
| "How do I commit Kafka and Postgres atomically?" | [Exactly-once across Kafka and other systems](./operating/exactly-once/) |
| "What should I be alerting on?" | [Observability](./operating/observability/) |
| "Why doesn't my IQ read return what I just wrote?" | [Visibility versus ACID databases](./operating/visibility/) |
| "It's on fire; what now?" | [Runbooks](./operating/runbooks/) |

## Concepts and guides

| Page | When |
| ---- | ---- |
| [Topology optimization](./concepts/topology-optimization/) | You want to know which rewrites the compiler does automatically and which it doesn't |
| [Dynamic topology changes](./concepts/dynamic-topology/) | You want to know what can change at runtime versus what requires a redeploy |
| [Enrichment via external systems](./guides/enrichment/) | Your topology needs to call out to an HTTP API, a database, or another service |
| [Glossary](./glossary/) | Anything unfamiliar |

## Quick context

Three sentences for orientation:

- A **topology** is a typed Haskell value of type `Topology i o`,
  composed with `Control.Category.(>>>)`. The library compiles it
  to an imperative runtime graph at the boundary.
- The runtime is a **library**, not a cluster. Your service binary
  contains the topology; scaling out means running more processes
  in the same consumer group.
- State stores live **next to** your service (local disk or
  memory), backed by Kafka **changelog topics** for durability and
  by **standby tasks** for fast failover.

That's enough to start the [tutorial](./get-started/what-is-kafka-streams/).
