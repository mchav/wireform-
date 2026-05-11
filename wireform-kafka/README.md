# wireform-kafka

A pure-Haskell client for Apache Kafka. It lets your Haskell
program publish records to a Kafka cluster and read records back
out, with full support for everything the Kafka protocol does:
consumer groups, transactions, TLS, SASL, idempotent producers,
record compression, and a stream-processing DSL on top.

> **Never used Kafka before?** See [`CONCEPTS.md`](./CONCEPTS.md)
> for a plain-language primer (topics, partitions, offsets,
> consumer groups, transactions). It's a five-minute read.

## What's in the box

The package is split into three layers. You pick the one that
fits your job:

| You want to… | Use this | Notes |
|---|---|---|
| Send records to a topic | `Kafka.Client.Producer` (or `Kafka` umbrella) | A long-lived `Producer` handle. Use `withProducer` for the bracket. |
| Receive records, one handler per record | `Kafka.Client.Group.runConsumer` | Wraps the poll loop, the group join, and offset commits. Recommended starting point. |
| Receive records and drive the poll loop yourself | `Kafka.Client.Consumer` | Lower level than `Group`. Use `withConsumer` for the bracket. |
| Run a stream-processing topology | `Kafka.Streams` | KStream / KTable / joins / windowed aggregations. |
| Manage topics, groups, ACLs, configs | `Kafka.Client.AdminClient` | The cluster's control plane. |
| Drive a raw wire request | `Kafka.Protocol.Generated.*` | One module per Kafka API; for custom tooling. |

The `Kafka` umbrella module re-exports the high-level producer,
consumer, group runner, and transaction APIs in one place — for
most apps you only need `import qualified Kafka`.

## Hello world

Publish a record and read it back. Requires a Kafka broker
reachable at `localhost:9092` (the integration `docker-compose`
in `test-integration/docker-compose.yml` spins one up).

### Produce

```haskell
import qualified Kafka

main :: IO ()
main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
```

`withProducer` opens the connection, runs your body, and on the
way out flushes anything buffered and closes connections — even
if you throw.

### Consume (high-level)

```haskell
import qualified Kafka
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.bootstrapBrokers = ["localhost:9092"]
      , Kafka.groupId          = "my-service"
      , Kafka.topics           = ["events"]
      }
    $ \rec ->
        BS.putStrLn (Kafka.crValue rec)
```

`runConsumer` joins the consumer group, hands you records one at
a time, commits offsets after each one, and leaves the group on a
normal exit or an exception.

For higher throughput, use `runBatchedConsumer` — same idea, but
the handler receives a whole batch per call and one commit covers
the whole batch.

### Consume (custom poll loop)

If you want to control when you poll and commit:

```haskell
import qualified Kafka.Client.Consumer as Consumer

main :: IO ()
main =
  Consumer.withConsumer
    ["localhost:9092"] "my-service"
    Consumer.defaultConsumerConfig
    ["events"]
    $ \c -> do
        Right recs <- Consumer.poll c 1000
        mapM_ print recs
        _ <- Consumer.commitSync c
        pure ()
```

### Streams

```haskell
import qualified Kafka.Streams as S

main :: IO ()
main = do
  builder <- S.newStreamsBuilder
  src <- S.streamFromTopic builder (S.topicName "in")
           (S.consumed S.textSerde S.textSerde)
  _ <- S.mapValues (\v -> v <> "!") src
       >>= S.toTopic (S.topicName "out") (S.produced S.textSerde S.textSerde)
  topo <- S.buildTopology builder
  print topo
```

See [`streams/README.md`](./streams/README.md) for the full DSL
reference and [`TUTORIAL.md`](./TUTORIAL.md) for a guided
walkthrough from "hello world" to a transactional Streams
pipeline.

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

## Configuration

`ProducerConfig`, `ConsumerConfig`, and `GroupConfig` are plain
Haskell records — every knob has a field. Start from
`defaultProducerConfig` / `defaultConsumerConfig` /
`defaultGroupConfig` and override only the fields you care about:

```haskell
producerConfig = Kafka.defaultProducerConfig
  { Kafka.producerCompression = Kafka.Zstd
  , Kafka.producerBatchSize   = 32768
  , Kafka.producerLingerMs    = 10
  , Kafka.producerIdempotent  = True
  }

groupConfig = Kafka.defaultGroupConfig
  { Kafka.bootstrapBrokers   = ["broker-1:9092"]
  , Kafka.groupId            = "my-service"
  , Kafka.topics             = ["events"]
  , Kafka.commitMode         = Kafka.CommitSync
  , Kafka.autoOffsetReset    = Kafka.Earliest
  }

connectionConfig = Conn.defaultConnectionConfig
  { Conn.connUseTls = True
  , Conn.connTlsSettings = Just (Conn.defaultTlsSettings "broker.example.com")
  }
```

The field haddocks list the librdkafka name every knob mirrors;
the full mapping lives in [`CONFIG_PARITY.md`](./CONFIG_PARITY.md).
Setting any `KAFKA_*` environment variable layers an override on
top of the supplied config automatically.

### TLS offload (sidecar / kTLS / NLB)

When a sidecar process (Envoy, linkerd2-proxy, stunnel,
`kafka-proxy`), a Layer-4 TLS-terminating load balancer, or
kernel TLS (`CONFIG_TLS`) is responsible for the cipher work, the
client can skip its own TLS handshake and route every broker
connection through that endpoint:

```haskell
import qualified Data.Map.Strict as Map
import Kafka.Network.Connection
import Kafka.Network.TlsOffload

-- One Unix-domain socket sidecar terminating mTLS upstream.
viaSidecarUds :: ConnectionConfig
viaSidecarUds = defaultConnectionConfig
  { connTlsOffload = Just $
      staticTlsOffload (TlsOffloadUnix "/var/run/kafka-proxy.sock")
  }

-- Per-broker stunnel listeners on different localhost ports.
viaPerBrokerStunnel :: ConnectionConfig
viaPerBrokerStunnel = defaultConnectionConfig
  { connTlsOffload = Just $ perBrokerTlsOffload $ Map.fromList
      [ (OffloadBrokerKey "b-1.kafka.example.com" 9094,
         TlsOffloadTcp "127.0.0.1" 19094)
      , (OffloadBrokerKey "b-2.kafka.example.com" 9094,
         TlsOffloadTcp "127.0.0.1" 19095)
      ]
  }

-- Kernel TLS / Layer-4 LB: routing is unchanged, just disable
-- the in-process handshake.
viaKtls :: ConnectionConfig
viaKtls = defaultConnectionConfig
  { connTlsOffload = Just transparentTlsOffload }
```

The connection pool is still keyed by the logical broker address —
per-broker SASL state and request pipelining work normally when
several brokers fan in to the same sidecar socket. See
"Kafka.Network.TlsOffload" for the full configuration surface.

## Security

- **SASL** — PLAIN, SCRAM-SHA-256, SCRAM-SHA-512, OAUTHBEARER. Mid-session
  re-authentication is supported via
  `Kafka.Client.Pipeline.attachReauthDriver`.
- **TLS 1.2 + 1.3** via `tls-1.x`.

## Observability

- W3C Trace Context propagation across producer / consumer hops
  via `Kafka.Telemetry.OpenTelemetry` (SDK-independent — bring your
  own tracer for span creation).
- librdkafka-compatible JSON stats via `Kafka.Telemetry.StatsJson`.
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
[`INTEGRATION_TESTING.md`](./INTEGRATION_TESTING.md) for the
full guide).

## Where to look next

- [`CONCEPTS.md`](./CONCEPTS.md) — Kafka primer in plain language.
- [`TUTORIAL.md`](./TUTORIAL.md) — guided walkthrough from
  `runConsumer` to a transactional Streams pipeline.
- [`streams/README.md`](./streams/README.md) — full Streams DSL
  reference.
- [`CONFIG_PARITY.md`](./CONFIG_PARITY.md) — librdkafka
  knob-by-knob configuration mapping.
- [`INTEGRATION_TESTING.md`](./INTEGRATION_TESTING.md) —
  docker-compose + integration suite walkthrough.
- [`PERFORMANCE.md`](./PERFORMANCE.md) — performance numbers and
  tuning guide.

## License

BSD-3-Clause.

## References

- [Apache Kafka protocol guide](https://kafka.apache.org/protocol.html)
- [OpenTelemetry messaging semantic conventions](https://opentelemetry.io/docs/specs/semconv/messaging/kafka/)
- [SASL SCRAM (RFC 5802)](https://tools.ietf.org/html/rfc5802)
- [SASL PLAIN (RFC 4616)](https://tools.ietf.org/html/rfc4616)
