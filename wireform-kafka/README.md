# wireform-kafka

A Haskell Kafka client + Streams DSL with full wire-protocol
coverage and operator-level parity with the Java reference.

`wireform-kafka` ships two layers behind one cabal package:

- **Client** (`Kafka.Client.*`) — producer, consumer, admin,
  pipelined I/O, SASL (PLAIN / SCRAM-SHA-256 / SCRAM-SHA-512 /
  OAUTHBEARER), TLS, transactions with exactly-once semantics.
- **Streams DSL** (`Kafka.Streams.*`) — `KStream` / `KTable` /
  `KGroupedStream` / `KGroupedTable` (with subtractor) /
  `GlobalKTable` / windowed aggregations / sessions / joins /
  foreign-key joins / cogroup / suppress / interactive
  queries / fixed-key processors. The runtime drives a real
  broker through the same connection layer the client uses.

The DSL is documented end-to-end in
[`streams/README.md`](streams/README.md). A runnable
introduction that walks from "hello world" to a transactional
Streams pipeline lives in [`TUTORIAL.md`](TUTORIAL.md).

## Status

Wire-compatible with Apache Kafka 4.0. Tested against the
docker-compose fixture in `test-integration/docker-compose.yml`
across Kafka 3.7 + 4.0 and GHC 9.6 / 9.8 / 9.10 / 9.12.

## Install

Add `wireform-kafka` to your cabal file:

```cabal
build-depends:
  base,
  wireform-kafka,
  wireform-kafka-streams,    -- if you want the Streams DSL
```

The package is part of the
[wireform monorepo](https://github.com/iand675/wireform).
Clone the repo and `cabal build wireform-kafka` to compile
locally.

## Hello world

### Producer

```haskell
import Kafka.Client.Producer

main :: IO ()
main = do
  Right producer <- createProducer ["localhost:9092"]
                      defaultProducerConfig
  Right md <- sendMessage producer "my-topic" Nothing "hello"
  print md
  closeProducer producer
```

### Consumer

```haskell
import Kafka.Client.Consumer
import Control.Monad (forever)

main :: IO ()
main = do
  Right c <- createConsumer ["localhost:9092"]
               "my-group" defaultConsumerConfig
  subscribe c ["my-topic"]
  forever $ do
    Right recs <- poll c 1000
    mapM_ (\r -> print (crValue r)) recs
    commitSync c
```

### Streams

```haskell
import Kafka.Streams

main :: IO ()
main = do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in")
           (consumed textSerde textSerde)
  out <- mapValues (\v -> v <> "!") src
  toTopic (topicName "out") (produced textSerde textSerde) out
  topo <- buildTopology b
  -- For tests, run against the in-process driver:
  -- driver <- newDriver topo "my-app"
  -- pipeInput driver (topicName "in") Nothing "hi" (Timestamp 0) 0
  -- For production, run against a live broker via
  -- 'Kafka.Streams.Runtime.startKafkaStreams'.
  print topo
```

## Configuration

Producer / consumer / connection configuration mirrors
librdkafka's key names. The full mapping lives in
[`CONFIG_PARITY.md`](CONFIG_PARITY.md). The most common knobs:

```haskell
producerConfig = defaultProducerConfig
  { producerCompression = Zstd
  , producerBatchSize   = 32768
  , producerLingerMs    = 10
  , producerIdempotent  = True
  }

consumerConfig = defaultConsumerConfig
  { consumerGroupId           = "my-group"
  , consumerAutoCommit        = False
  , consumerAssignmentStrategy = StickyAssignment
  , consumerAutoOffsetReset   = Earliest
  }

connectionConfig = defaultConnectionConfig
  { connUseTls = True
  , connTlsSettings = Just (defaultTlsSettings "broker.example.com")
  }
```

## Security

- SASL/PLAIN, SCRAM-SHA-256, SCRAM-SHA-512, OAUTHBEARER. Mid-session
  re-authentication is supported via
  `Kafka.Client.Pipeline.attachReauthDriver`.
- TLS 1.2 + 1.3 via `tls-1.x`.

## Observability

- OpenTelemetry spans + metrics via
  `Kafka.Telemetry.OpenTelemetry`. Follows the messaging
  semantic conventions.
- librdkafka-compatible JSON stats via `Kafka.Client.Stats`.
- Producer / consumer interceptors for per-record telemetry.

## Testing

```bash
# In-process mock broker — no Docker, no network.
cabal test wireform-kafka:wireform-kafka-test
cabal test wireform-kafka:wireform-kafka-streams-test

# Against a real broker (Docker required):
docker compose -f test-integration/docker-compose.yml up -d
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test \
  wireform-kafka:wireform-kafka-integration \
  wireform-kafka:wireform-kafka-streams-integration
```

The in-process suites run on every CI commit; the
docker-compose suite runs on every PR (see
[`INTEGRATION_TESTING.md`](INTEGRATION_TESTING.md) for the
full guide).

## Benchmarks

The streams runtime micro-bench measures the per-record CPU
envelope of representative topology shapes:

```bash
cabal bench wireform-kafka:wireform-kafka-streams-bench \
  --benchmark-options="--time-limit 1.5"
```

Current numbers + reproduction recipe + librdkafka comparison:
[`streams/bench/results/README.md`](streams/bench/results/README.md).

## License

BSD-3-Clause.

## References

- [Apache Kafka protocol guide](https://kafka.apache.org/protocol.html)
- [OpenTelemetry messaging semantic conventions](https://opentelemetry.io/docs/specs/semconv/messaging/kafka/)
- [SASL SCRAM (RFC 5802)](https://tools.ietf.org/html/rfc5802)
- [SASL PLAIN (RFC 4616)](https://tools.ietf.org/html/rfc4616)
