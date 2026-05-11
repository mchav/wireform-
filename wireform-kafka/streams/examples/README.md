# wireform-kafka-streams examples

A set of Haskell topologies that mirror the canonical Apache Kafka
Streams demos under `org.apache.kafka.streams.examples` and the
patterns from `kafka-streams-examples`.

> For a top-level overview of what the library does and doesn't
> support relative to the JVM, see [`../README.md`](../README.md).

Every example is a self-contained module and is wired into a single
executable so you can run them without configuring a broker — the
in-process `TopologyTestDriver` feeds sample data and prints the
resulting sink records.

## Run

```
cabal run wireform-kafka-streams-examples -- <demo>
cabal run wireform-kafka-streams-examples -- all
```

With no arguments the executable prints the index.

## Demos

| Name           | Module                                          | Mirrors                              | Demonstrates                                          |
| -------------- | ----------------------------------------------- | ------------------------------------ | ----------------------------------------------------- |
| `pipe`         | `Kafka.Streams.Examples.Pipe`                   | `PipeDemo`                           | source → sink                                         |
| `line-split`   | `Kafka.Streams.Examples.LineSplit`              | `LineSplitDemo`                      | `flatMapValues`                                       |
| `word-count`   | `Kafka.Streams.Examples.WordCount`              | `WordCountDemo`                      | `flatMapValues` + `groupBy` + `count` + `toStream`    |
| `page-views`   | `Kafka.Streams.Examples.PageViewRegion`         | `PageViewTypedDemo`                  | KStream-KTable inner join                             |
| `temperature`  | `Kafka.Streams.Examples.Temperature`            | `TemperatureDemo`                    | tumbling window + `reduce` + `suppress`               |
| `top-articles` | `Kafka.Streams.Examples.TopArticles`            | `TopArticlesDemo`                    | hopping window + `count`                              |
| `orders`       | `Kafka.Streams.Examples.OrdersEnrichment`       | microservices/Orders                 | KStream-KTable join with mid-stream profile updates   |
| `fraud`        | `Kafka.Streams.Examples.FraudDetection`         | session-window guide                 | session windows + filter                              |
| `fk-join`      | `Kafka.Streams.Examples.InventoryFKJoin`        | FK-join demo                         | KTable-KTable foreign-key join (token verification)   |
| `iq`           | `Kafka.Streams.Examples.InteractiveQueries`     | IQ demo                              | reading state stores from outside the topology        |
| `processor`    | `Kafka.Streams.Examples.ProcessorAPI`           | low-level Processor API guide        | custom `Processor`, `ProcessorContext`, `Punctuator`  |
| `side-effects` | `Kafka.Streams.Examples.SideEffects`            | Confluent "side effects" docs        | `peek` + `mapValuesM` + `foreach` + Punctuator        |
| `branching`    | `Kafka.Streams.Examples.Branching`              | `split` demo                         | predicate-based stream branching                      |
| `idiomatic`    | `Kafka.Streams.Examples.IdiomaticPipeline`      | n/a (Haskell-native shape)           | `TopologyM` `do`-block / `Pipeline` arrow / `OfStream` Functor — three façades over the same topology |
| `global`       | `Kafka.Streams.Examples.GlobalTable`            | GlobalKTable docs                    | cluster-replicated lookup table join                  |
| `cogroup`      | `Kafka.Streams.Examples.Cogroup`                | cogroup demo                         | cogroup of streams with distinct value types          |

Each module starts with a docblock that shows the equivalent
Java/Scala code from upstream Kafka Streams alongside the Haskell
translation, so you can use the modules as a porting reference.

## Why no broker?

The `TopologyTestDriver` (mirror of Java's class of the same name)
runs the topology in-process: `pipeInput` feeds source records,
`readOutput` drains sink emissions, and state stores are queryable
through `queryEngineStore`. That's enough to exercise every demo
deterministically. To run any of these against a real broker, take
the `buildXTopology` function each module exports, hand it to
`Kafka.Streams.Runtime.startKafkaStreams` with the appropriate
`StreamsConfig`, and produce/consume records through
`Kafka.Client.{Producer, Consumer}`.

## Adding a demo

1. Add a new module under `Kafka.Streams.Examples.<Name>`.
2. Export `runDemo :: IO ()` and a `build<Name>Topology` builder.
3. Add it to `streams/examples/Main.hs` and the `other-modules`
   list in `wireform-kafka.cabal`'s
   `wireform-kafka-streams-examples` stanza.
