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

### 4.1 KTable-driven aggregation (`KGroupedTable`, KIP-150)

`KStream.groupByKey + count / reduce` works on append-only
streams. For a /changelog/ — where every input record updates
an existing entry — use `groupTableBy` so the aggregator runs
the /subtractor/ first, removing the prior value's
contribution before adding the new one. Without the subtractor
an update would double-count.

```haskell
import qualified Kafka.Streams.DSL.KGroupedTable as KGT
import qualified Kafka.Streams.DSL.Grouped        as G

countByCustomer
  :: SB.StreamsBuilder
  -> IO ()
countByCustomer b = do
  orders <- SB.tableFromTopic b (topicName "orders")
              (consumed textSerde textSerde)
              (materializedAs (storeName "orders-store"))
  let grouped = G.grouped textSerde textSerde
      kgt     = KGT.groupTableBy
                  (\_orderId customer -> (customer, customer))
                  grouped orders
  _ <- KGT.countKGroupedTable
        (materializedAs (storeName "counts-store"))
        kgt
  pure ()
```

When `o1` migrates from customer `A` to customer `B` the
runtime emits `A=-1, B=+1` — both records — so downstream
sinks stay consistent.

### 4.2 Stateful value-only transforms (`FixedKeyProcessor`, KIP-820)

When you want the full `ProcessorContext` (state stores,
punctuators, header access) but the type system to /prevent/
you from rekeying the input, reach for `FixedKeyProcessor`:

```haskell
import qualified Kafka.Streams.Processor as P

vatProc :: P.FixedKeyProcessor Text Int Int
vatProc = P.FixedKeyProcessor
  { P.fkpName    = P.processorName "vat-transform"
  , P.fkpInit    = \_ -> pure ()
  , P.fkpProcess = \r -> pure (Just (P.recordValue r * 120 `div` 100))
  , P.fkpClose   = pure ()
  }
```

`liftFixedKeyProcessor vatProc` bridges into the engine's
`Processor` type while still preserving the
"no-rekey" guarantee at the call site. The `ProcessorSupplier`
record additionally declares which state stores the supplier
owns so the DSL can wire them into the topology
automatically:

```haskell
P.ProcessorSupplier
  { P.psSupply = P.liftFixedKeyProcessor vatProc
  , P.psStores = [storeName "vat-cache"]
  }
```

### 4.3 Side effects: blocking and non-blocking foreach

`KStream.foreachStream` is the blocking terminal effect — the
worker thread waits for the callback to return. For metrics
emissions / logging where ordering doesn't matter, use
`foreachStreamAsync`:

```haskell
import qualified Kafka.Streams.DSL.KStream as KS

KS.foreachStreamAsync
  (\r -> Metrics.emit ("processed-" <> recordValue r))
  someStream
```

Each callback forks via `Control.Concurrent.Async` so a slow
sink can't back-pressure the worker.

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

Wire this into the engine's commit cycle via
`Kafka.Streams.Runtime.EOS.withTransactionalStores`:

```haskell
import Kafka.Streams.Runtime.EOS

let coord = withTransactionalStores
              (newRealEOSCoordinator txn)   -- producer side
              [TX.txnCommit txStore]        -- store commits
              [TX.txnAbort  txStore]        -- store aborts
setEOSCoordinator ks coord
```

The runtime now runs the producer commit FIRST; on success
the store commits drain in declaration order. An abort runs
the producer abort + the store aborts so the buffered writes
are discarded and the store stays consistent with the
broker-side log. A failure /between/ the wire commit and the
store commit promotes the cycle to `CommitFatal` — recovery
would leave the two layers permanently inconsistent.

## 5.5 Multi-instance: rebalance hooks, standby tasks, cross-instance IQ

A streams app running as N pods needs three things to work
together: lifecycle hooks that fire when partitions move,
standbys that shadow active tasks so failover is fast, and
cross-instance IQ routing so a user query reaches whatever
pod owns the key.

### Rebalance + thread management

```haskell
import Kafka.Streams.Runtime

-- Programmatic rebalance triggers (KIP-441 probing rebalance):
reportWarmupLag    ks tid 0   -- this standby is caught up
maybeIssueProbe <- streamThreadCount ks

-- Dynamic thread management (KIP-663):
n  <- addStreamThread    ks   -- grow the pool
n' <- removeStreamThread ks   -- and shrink it

-- Graceful close with leaveGroup=False (KIP-812):
closeKafkaStreamsWith ks
  defaultCloseOptions { closeLeaveGroup = False }
```

`closeLeaveGroup = False` skips the LeaveGroup RPC so the
broker waits out the session timeout — useful for fast rolling
restarts with static membership where you don't want the
churn.

### Standby tasks (KIP-441)

`Kafka.Streams.Runtime.StandbyTask` ships the data model;
`Kafka.Streams.Runtime.StandbyDriver` ships the changelog
poll loop. Wire them with a user-supplied poll function (the
production path uses a second `Kafka.Client.Consumer`
subscribed to the changelog topics):

```haskell
import qualified Kafka.Streams.Runtime.StandbyTask   as ST
import qualified Kafka.Streams.Runtime.StandbyDriver as SD

setupStandby ks consumer storeLookup = do
  task <- ST.newStandbyTask (TaskId 0 0) "my-store-changelog" 0
            (storeName "my-store")
  ST.addStandbyTask (ksStandbyManager ks) task
  drv <- SD.newStandbyDriver
           (ksStandbyManager ks)
           (kafkaChangelogPoll consumer)   -- user supplies
           storeLookup
           (reportWarmupLag ks)
           250                              -- poll timeout
  SD.startStandbyDriver drv
```

`reportWarmupLag` feeds the KIP-441 probing-rebalance loop —
when a standby's lag reaches `acceptableRecoveryLag` the
runtime fires a `JoinGroup` so the leader can promote it.

### Cross-instance IQ (KIP-535)

Each instance advertises its `application.server` + owned
stores in the JoinGroup subscription-userdata. The streams
runtime installs the hook automatically (via
`setSubscriptionUserDataHook`) so peers can read every
instance's metadata off the group state. User code then routes
queries with `routeQuery`:

```haskell
import Kafka.Streams.Discovery
import Kafka.Streams.Discovery.RemoteIQ

handleQuery
  :: HostInfo                      -- self
  -> [StreamsMetadata]             -- peers (from group state)
  -> RemoteIQ                      -- user transport (HTTP / gRPC)
  -> ByteString                    -- key
  -> IO (Maybe ByteString)
handleQuery self peers transport key = do
  let part = hashedPartition key   -- user-supplied
      kqm  = makeKeyQueryMetadata peers "my-topic" part
  case routeQuery self kqm of
    RouteMissing       -> pure Nothing
    RouteLocal         -> readLocalStore key
    RouteRemote host   -> respondTo (runRemoteIQ transport host
                            (RemoteIQRequest (storeName "my-store") key))
```

Standby-aware routing: when the active is on a remote pod but
a standby is local, `RouteLocal` wins — saves a network hop at
the cost of potentially-stale reads.

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
  * **Streams examples**: `wireform-kafka/streams/examples`
    has runnable demos for every operator family
    (passthrough, line-split, word-count, page-view region,
    temperature window, top-articles, orders enrichment,
    fraud detection, inventory FK-join, interactive queries,
    processor API, branching, global table, cogroup, side
    effects) plus a README mapping each demo back to its JVM
    equivalent.
  * **Streams runtime benchmarks**:
    `wireform-kafka:wireform-kafka-streams-bench` measures
    the runtime hot paths in-process; numbers + reproduction
    recipe live in `streams/bench/results/README.md`.
  * **Exception handlers**: every KIP-280 / 671 / 1033
    handler is wired into the runtime; see the spec in
    `streams/test/Streams/ExceptionHandlerSpec.hs` for the
    public API + contract.
