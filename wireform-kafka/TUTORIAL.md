# `wireform-kafka` tutorial

A walkthrough that takes a fresh user from zero to a working
producer + consumer + transactional pipeline + Streams topology
against the in-process mock broker. Every step is runnable from
the same Haskell file; no Docker required.

> **Where this fits in the docs.** The README is the catalogue
> of features; [`FEATURE_PARITY.md`](./FEATURE_PARITY.md) is the
> running plan against the JVM client + librdkafka; this file is
> the "first 30 minutes" walkthrough.

## 1. Hello, mock broker

The `Kafka.Client.Mock.Cluster` module is the in-process Kafka
emulation. Spinning up a "cluster" takes one line:

```haskell
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Producer
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  cluster <- newMockCluster 1            -- one broker
  createTopic cluster "events" 3         -- one topic, three partitions

  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  _ <- sendMock producer "events" 0
         (Just (BS.pack "key")) (BS.pack "hello") 0
  putStrLn "produced"

  consumer <- newMockConsumer cluster faults
                              (GroupId "tutorial") ReadUncommitted 100
  subscribeMC consumer ["events"]
  PollResult records _ <- pollMC consumer
  print (length records)
```

The mock cluster is the workhorse for unit tests — `464` tests in
`wireform-kafka-test` plus `304` in `wireform-kafka-streams-test`
all run against it.

## 2. Producer and consumer against a real broker

Replace the mock pair with the actual client. Set
`WIREFORM_KAFKA_BROKER=localhost:9092` so the integration suite
runs against your local cluster:

```haskell
import qualified Kafka.Client.Producer as P
import qualified Kafka.Client.Consumer as C

producerExample :: IO ()
producerExample = do
  Right p <- P.createProducer ["localhost:9092"] P.defaultProducerConfig
  Right meta <- P.sendMessage p "events" (Just "key") "hello"
  putStrLn ("produced at offset " <> show (P.metadataOffset meta))
  P.closeProducer p
```

The `Kafka.Client.Producer.ProducerConfig` record mirrors
[librdkafka's CONFIGURATION.md](./CONFIG_PARITY.md) one field per
knob; defaults follow Kafka 3.x JVM where the two diverge.

## 3. Transactions (KIP-98 / KIP-447)

The transaction lifecycle is split between
`Kafka.Client.Transaction` (state machine + coordinator wire)
and `Kafka.Client.Producer` (binding):

```haskell
import qualified Kafka.Client.Transaction as T
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV

txExample :: IO ()
txExample = do
  let txId = "my-app-txn-1"
  Right p <- P.createProducer ["localhost:9092"]
               P.defaultProducerConfig
                 { P.producerTransactional = Just txId
                 , P.producerIdempotent    = True
                 }

  connMgr <- Conn.createConnectionManager
  vCache  <- AV.createVersionCache
  let bootstrap = Conn.BrokerAddress "localhost" 9092
  txn <- T.createTransaction
           (T.TransactionalId txId) connMgr vCache "tutorial-client"
           bootstrap 60_000
  Right () <- T.initTransactions txn
  P.bindTransaction p txn

  Right () <- T.beginTransaction txn
  _ <- P.sendMessage p "events" Nothing "in-txn"
  Right () <- T.commitTransaction txn
  P.closeProducer p
```

What changed under the hood compared to a plain producer:

  * `P.sendMessage` is rejected with a typed error unless the
    bound transaction is in `T.InTransaction`.
  * The first send to any (topic, partition) issues
    `AddPartitionsToTxn` to the coordinator.
  * The outgoing record batch is stamped with the transactional
    producer-id / epoch / sequence and the `attrIsTransactional`
    bit is set.
  * `closeProducer` aborts an open transaction before shutdown.

## 4. Streams DSL

```haskell
import qualified Kafka.Streams.DSL.StreamsBuilder as SB
import qualified Kafka.Streams.DSL.KStream        as KS
import qualified Kafka.Streams.Serde              as Serde

streamsExample :: IO ()
streamsExample = do
  let topology =
        SB.runStreamsBuilder $ do
          input  <- SB.streamFromTopic "events"
                       Serde.bytesSerde Serde.bytesSerde
          KS.foreachStream
            input
            (\k v -> putStrLn ("got " <> show k <> "=" <> show v))
  print topology
```

The DSL surface is intentionally close to the JVM:
`filter / map / mapValues / flatMap / branch / merge / through /
toTable`, plus `reduce / aggregate / count` on
`KGroupedStream`. Joins (stream-stream, stream-table, table-table,
foreign-key, windowed) all live under
`Kafka.Streams.DSL.*`. See
`Kafka.Streams.Topology.Optimization` for the
KIP-295 toggles.

## 5. State stores and EOS-V3 (KIP-892)

`Kafka.Streams.State.Transactional` wraps any
`KeyValueStore` so that puts and deletes are buffered until the
producer transaction commits:

```haskell
import qualified Kafka.Streams.State.KeyValue.InMemory as Mem
import qualified Kafka.Streams.State.Transactional     as TX
import qualified Kafka.Streams.State.Store             as Store

eos3Example :: IO ()
eos3Example = do
  underlying <- Mem.inMemoryKeyValueStore (Store.storeName "totals")
  txStore    <- TX.newTransactionalStore underlying
  let store = TX.txnStore txStore
  Store.kvsPut store "k" "v"
  -- read-your-writes within the open transaction:
  Just "v" <- Store.kvsGet store "k"
  -- nothing on the underlying store yet:
  Nothing <- Store.kvsGet underlying "k"
  -- commit applies every buffered op atomically:
  TX.txnCommit txStore
  Just "v" <- Store.kvsGet underlying "k"
  pure ()
```

Bind this to the engine's commit boundaries via the producer's
bound `Transaction` (see Section 3) and you have EOS-V3
semantics: an aborted transaction discards the store buffer too.

## 6. Schema Registry serdes

`Kafka.Streams.Serde.SchemaRegistry` exposes the Confluent wire
envelope (`magicByte + schemaId + payload`) plus a pluggable
`SchemaRegistryClient` interface. Use `inMemoryRegistry` in
tests and your own HTTP client in production:

```haskell
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

srExample :: IO ()
srExample = do
  client <- SR.inMemoryRegistry
  Right sid <- SR.srRegister client (SR.SchemaSubject "events-value")
                              (SR.SchemaPayload "{\"type\":\"string\"}")
  let bs = SR.encodeEnvelope sid "hello"
  case SR.decodeEnvelope bs of
    Right (sid', payload) -> do
      print sid'         -- SchemaId 1
      print payload      -- "hello"
    Left err -> error err
```

We deliberately don't pin an HTTP client; the demo above works
without ever touching the network.

## 7. Observability

`Kafka.Telemetry.StatsJson` mirrors the
[librdkafka stats JSON shape](https://github.com/confluentinc/librdkafka/blob/master/STATISTICS.md)
so dashboards / collectors written against the C client port
without rewrites:

```haskell
import qualified Kafka.Telemetry.StatsJson as Stats
import qualified Data.Map.Strict as Map

statsExample :: IO ()
statsExample = do
  let snap = (Stats.defaultSnapshot "wfkafka" "client-1" Stats.StatsProducer)
        { Stats.ssMsgCount = 100
        , Stats.ssTopics = Map.singleton "events"
            (Stats.TopicStats "events" 100 5 1024 0 0)
        }
  print (Stats.renderStats snap)
```

OTel spans / metrics flow through the existing
`Kafka.Telemetry.OpenTelemetry` module (`MetricsRegistry` + per-op
counters); the JSON snapshot above is the librdkafka-shaped
mirror of the same counters.

## 8. Where to look next

  * **Configuration**: [`CONFIG_PARITY.md`](./CONFIG_PARITY.md)
    is the librdkafka mapping; [`FEATURE_PARITY.md`](./FEATURE_PARITY.md)
    is the running KIP-by-KIP plan.
  * **Live broker tests**: `WIREFORM_KAFKA_BROKER=host:port cabal test
    wireform-kafka:wireform-kafka-integration` runs the full
    transactional + produce/consume integration suite.
  * **Mock cluster reference**: `wireform-kafka/src/Kafka/Client/Mock/`
    is the source of truth for fault injection. The README in
    that folder walks through every primitive.
  * **Streams reference**: `wireform-kafka-streams` mirrors the
    Java Streams DSL one combinator per name; the
    `Kafka.Streams.DSL.*` modules each carry a header comment
    pointing at the upstream Java equivalent.
