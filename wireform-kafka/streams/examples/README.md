# Streams Examples

Runnable Kafka Streams topologies. Each mirrors a canonical Apache Kafka example.

## Run

```bash
cabal run wireform-kafka-streams-examples -- pipe
cabal run wireform-kafka-streams-examples -- word-count
cabal run wireform-kafka-streams-examples -- all
```

No broker required. Runs against in-process test driver.

## Examples

| Name | Mirrors | Shows |
|---|---|---|
| `pipe` | `PipeDemo` | Source to sink |
| `line-split` | `LineSplitDemo` | `concatMapValues` |
| `word-count` | `WordCountDemo` | Split, group, count |
| `page-views` | `PageViewTypedDemo` | Stream-table join |
| `temperature` | `TemperatureDemo` | Tumbling window and suppress |
| `top-articles` | `TopArticlesDemo` | Hopping window + count |
| `orders` | Microservices/Orders | Stream-table enrichment |
| `fraud` | Session window guide | Session windows and filter |
| `fk-join` | FK-join demo | Foreign-key table-table join |
| `iq` | IQ demo | Querying state stores |
| `processor` | Processor API guide | Custom `Processor` |
| `side-effects` | Side effects docs | `peek`, `mapValuesM`, `foreach` |
| `branching` | Split demo | Predicate-based branching |
| `global` | GlobalKTable docs | Cluster-wide lookup table |
| `cogroup` | Cogroup demo | Combining streams |

## Against a real broker

Some simple examples work with live brokers:

```bash
cabal run wireform-kafka-streams-examples -- --broker localhost:9092 pipe
cabal run wireform-kafka-streams-examples -- --broker localhost:9092 word-count
```

## Structure

Each module exports:
- `runDemo :: IO ()` - runs against test driver
- `build<Name>Topology` - returns the pure topology value

Topologies use the Free DSL (`Kafka.Streams.Topology.Free`), composed with `Control.Category.(>>>)`.

## From example to your code

1. Find closest example
2. Read the module (has Java equivalent in comments)
3. Copy the `buildXTopology` pattern
4. For production, swap test driver for `Kafka.Streams.Runtime.startKafkaStreams`

Start with `word-count` for basics, `page-views` for joins, `processor` for escape hatches.

## See also

- [Streams README](../README.md) - Full DSL reference
- [Tutorial](../../TUTORIAL.md) - Walkthrough
