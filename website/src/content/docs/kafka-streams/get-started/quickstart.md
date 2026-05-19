---
title: Quickstart
description: Run your first wireform-kafka-streams topology in five minutes. No Kafka broker required.
sidebar:
  order: 1
---

You'll have a streams topology running against an in-process test
driver in about five minutes. No broker, no Docker, no networking.

## What you need

- GHC 9.6 or newer.
- `cabal-install` 3.x.

That's it. The library ships an in-process `TopologyTestDriver`
that runs your topology end-to-end without talking to Kafka.

## Step 1: Run the word-count demo

Clone the repo and run the canonical example:

```bash
git clone https://github.com/iand675/wireform-.git
cd wireform-
cabal run wireform-kafka-streams-examples -- word-count
```

You'll see something like:

```text
=== WordCountDemo ===
Word-count updates emitted (16):
  all = 1
  streams = 1
  lead = 1
  to = 1
  kafka = 1
  hello = 1
  kafka = 2
  streams = 2
  join = 1
  kafka = 3
  summit = 1
  kafka = 4
  streams = 3
  kafka = 5
  summit = 2
```

You've just run a stateful streaming application. The topology read
four lines, split them into words, grouped by word, counted, and
emitted a running tally on every update. The output reads like a
changelog — the same word's count updates as new occurrences arrive.

## Step 2: Look at the topology

The whole pipeline is 8 lines of code:

```haskell
import Control.Category ((>>>))
import Kafka.Streams
import qualified Kafka.Streams.Topology.Free as F

wordCountTopology :: F.Topology Void ()
wordCountTopology =
  F.source "streams-plaintext-input" textSerde textSerde
    >>> F.concatMapValues (T.words . T.toLower)
    >>> F.groupBy (\r -> recordValue r) (grouped textSerde textSerde)
    >>> F.count countMat
    >>> F.toStream
    >>> F.sink "streams-wordcount-output" textSerde int64Serde
```

Read it left to right:

1. `source` — read records from the `streams-plaintext-input` topic.
2. `concatMapValues` — split each line into words.
3. `groupBy` — re-key by the word itself.
4. `count` — maintain a running count per word.
5. `toStream` — turn the counted table back into a stream of updates.
6. `sink` — write each update to `streams-wordcount-output`.

The topology is a plain Haskell value of type `F.Topology Void ()`.
You can inspect it, optimise it, snapshot it for golden-file tests,
or compile it to the runtime graph at the edge.

## Step 3: Try the others

The same executable ships fifteen runnable demos. They all use the
in-process driver, so they run instantly:

```bash
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- line-split
cabal run wireform-kafka-streams-examples -- page-views
cabal run wireform-kafka-streams-examples -- temperature
cabal run wireform-kafka-streams-examples -- top-articles
cabal run wireform-kafka-streams-examples -- orders
cabal run wireform-kafka-streams-examples -- fraud
cabal run wireform-kafka-streams-examples -- fk-join
cabal run wireform-kafka-streams-examples -- iq
cabal run wireform-kafka-streams-examples -- processor
cabal run wireform-kafka-streams-examples -- branching
cabal run wireform-kafka-streams-examples -- global
cabal run wireform-kafka-streams-examples -- cogroup
cabal run wireform-kafka-streams-examples -- all
```

Each one mirrors one of the canonical Apache Kafka Streams examples.
Source for each lives under
`wireform-kafka/streams/examples/Kafka/Streams/Examples/`.

## Where to go next

You can keep poking at the examples, or follow the
[tutorial](./what-is-kafka-streams/) for a guided walk through the
mental model. The tutorial is five parts and takes about 30 minutes
end-to-end:

1. [What is Kafka Streams?](./what-is-kafka-streams/) — the mental
   model in plain English.
2. [Your first topology](./your-first-topology/) — write and run a
   pipe.
3. [Stateful processing](./stateful-processing/) — word count, state
   stores, and interactive queries.
4. [Joins and tables](./joins-and-tables/) — enrich a stream with
   reference data.
5. [Going to production](./going-to-production/) — what you need to
   know before deploying.

When you're past the basics, the [Overview](../) page is the
operator-facing map of every doc in this section.
