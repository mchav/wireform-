# `wireform-kafka` tutorial

A guided walkthrough that takes you from "zero" to a working
producer, consumer, transactional pipeline, and Streams
topology. Every example below is a complete Haskell file that
runs against the in-process mock broker — no Docker required.

> **Where this fits.** The [README](./README.md) is the
> catalogue of features; [`CONCEPTS.md`](./CONCEPTS.md) is the
> plain-language Kafka primer;
> [`streams/README.md`](./streams/README.md) is the Streams DSL
> reference; this file is the "first 30 minutes".

## 1. Send a record (`withProducer`)

The smallest possible Kafka writer:

```haskell
import qualified Kafka

main :: IO ()
main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
```

`withProducer` is a `Control.Exception.bracket`: it builds the
`Producer`, runs your body, and on the way out flushes anything
buffered and closes the connection — even if you throw.

`sendMessage` returns `IO (Either String RecordMetadata)`. The
`Right` carries the assigned partition and offset; the `Left`
is a typed error message ready to log. For fire-and-forget,
use `sendMessage_`. For non-blocking with a result you read
later, use `sendMessageAsync` (returns an `MVar` you take when
ready).

## 2. Receive records (`runConsumer`)

The smallest possible Kafka reader. Joins a consumer group,
calls your handler once per record, commits offsets, and
leaves the group on exit:

```haskell
import qualified Kafka
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.bootstrapBrokers = ["localhost:9092"]
      , Kafka.groupId          = "tutorial"
      , Kafka.topics           = ["events"]
      }
    $ \rec ->
        BS.putStrLn rec.value
```

For higher throughput, use `runBatchedConsumer` — same shape,
but the handler receives a `Vector ConsumerRecord` per call
and a single commit covers the whole batch.

### Error handling

If your handler throws, `runConsumer` consults `onError`:

  * `LogAndRaise` (default) — log to `stderr` and re-raise.
  * `SkipRecord` — log and keep going.
  * `StopLoop` — log and exit cleanly.
  * `CustomError pred` — your own predicate.

### Commit modes

`commitMode` on `GroupConfig`:

  * `CommitSync` (default) — commit after each successful
    handler call. Smallest possible duplicate window on a
    crash.
  * `CommitAsync` — fire-and-forget commit.
  * `CommitManual` — you call `commitSync` / `commitAsync`
    yourself.

## 3. Custom poll loop (`withConsumer`)

If `runConsumer`'s "one handler per record" shape doesn't fit
your control flow, drop down to `Kafka.Client.Consumer` and
own the loop:

```haskell
import qualified Kafka.Client.Consumer as Consumer
import Control.Monad (forever)

main :: IO ()
main =
  Consumer.withConsumer
    ["localhost:9092"] "tutorial"
    Consumer.defaultConsumerConfig
    ["events"]
    $ \c -> forever $ do
        r <- Consumer.poll c 1000
        case r of
          Left err   -> putStrLn ("poll failed: " <> err)
          Right recs -> do
            mapM_ print recs
            _ <- Consumer.commitSync c
            pure ()
```

`withConsumer` joins the group and subscribes for you; on the
way out it commits, sends `LeaveGroup`, and closes connections.

## 4. The in-process mock broker

The full client expects a real Kafka cluster. For unit tests
and learning, use the in-process mock broker — it speaks the
same Producer / Consumer API but lives entirely inside your
process:

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

The mock cluster is the workhorse for unit tests in this
package — 464 tests in `wireform-kafka-test` plus 304 in
`wireform-kafka-streams-test` all run against it.

## 5. Transactions

Transactions group multiple sends across multiple partitions
into one atomic write. Combined with
`commitOffsetsInTransaction`, they give end-to-end "exactly
once".

The lifecycle is split between `Kafka.Client.Transaction`
(state machine + coordinator wire) and `Kafka.Client.Producer`
(binding to a producer):

```haskell
import qualified Kafka
import qualified Kafka.Client.Transaction as T
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV

main :: IO ()
main = do
  let txId = "tutorial-txn-1"
  Kafka.withProducer ["localhost:9092"]
    Kafka.defaultProducerConfig
      { Kafka.producerTransactional = Just txId
      , Kafka.producerIdempotent    = True
      }
    $ \p -> do
        connMgr <- Conn.createConnectionManager
        vCache  <- AV.createVersionCache
        txn <- T.createTransaction
                 (T.TransactionalId txId) connMgr vCache "tutorial-client"
                 (Conn.BrokerAddress "localhost" 9092) 60_000
        Right () <- T.initTransactions txn
        Kafka.bindTransaction p txn

        Right () <- T.beginTransaction txn
        _ <- Kafka.sendMessage p "events" Nothing "in-txn"
        Right () <- T.commitTransaction txn
        pure ()
```

What changed under the hood compared to a plain producer:

  * `sendMessage` is rejected with a typed error unless the
    bound transaction is in `T.InTransaction`.
  * The first send to any (topic, partition) issues
    `AddPartitionsToTxn` to the coordinator.
  * Each outgoing record batch is stamped with the
    transactional producer-id / epoch / sequence and the
    `attrIsTransactional` bit is set.
  * `closeProducer` aborts an open transaction before
    shutdown (so `withProducer` does the right thing too).

## 6. A first Streams topology

The Streams DSL builds a topology of stream operators and
runs it against a real broker (or the in-process test
driver). Combinators mirror the Java DSL one for one.

```haskell
import qualified Kafka.Streams                       as S
import qualified Kafka.Streams.StreamsBuilder    as SB
import qualified Kafka.Streams.KStream           as KS
import qualified Kafka.Streams.Serde                 as Serde

main :: IO ()
main = do
  let topology =
        SB.runStreamsBuilder $ do
          input <- SB.streamFromTopic "events"
                     Serde.bytesSerde Serde.bytesSerde
          KS.foreachStream
            input
            (\k v -> putStrLn ("got " <> show k <> "=" <> show v))
  print topology
```

The DSL is documented end-to-end in
[`streams/README.md`](./streams/README.md); see
`wireform-kafka/streams/examples` for runnable demos of every
operator family.

### Side effects: blocking vs async

`KStream.foreachStream` is the blocking terminal effect — the
worker thread waits for the callback to return. For metrics
emissions / logging where ordering doesn't matter, use
`foreachStreamAsync`:

```haskell
KS.foreachStreamAsync
  (\r -> Metrics.emit ("processed-" <> recordValue r))
  someStream
```

Each callback forks via `Control.Concurrent.Async` so a slow
sink can't back-pressure the worker.

## 7. State stores and exactly-once transactional writes

`Kafka.Streams.State.Transactional` wraps any
`KeyValueStore` so that puts and deletes are buffered until
the producer transaction commits:

```haskell
import qualified Kafka.Streams.State.KeyValue.InMemory as Mem
import qualified Kafka.Streams.State.Transactional     as TX
import qualified Kafka.Streams.State.Store             as Store

main :: IO ()
main = do
  underlying <- Mem.inMemoryKeyValueStore (Store.storeName "totals")
  txStore    <- TX.newTransactionalStore underlying
  let store = TX.txnStore txStore
  Store.kvsPut store "k" "v"
  Just "v" <- Store.kvsGet store "k"        -- read-your-writes
  Nothing  <- Store.kvsGet underlying "k"   -- nothing applied yet
  TX.txnCommit txStore                      -- commit drains
  Just "v" <- Store.kvsGet underlying "k"
  pure ()
```

Wire it into the engine's commit cycle via
`Kafka.Streams.Runtime.EOS.withTransactionalStores`. The
runtime runs the producer commit FIRST; on success the store
commits drain in declaration order. An abort runs the
producer abort + the store aborts so the buffered writes are
discarded and the store stays consistent with the broker-side
log.

## 8. Multi-instance Streams

A streams app running as N pods needs three things working
together: lifecycle hooks that fire when partitions move,
standbys that shadow active tasks so failover is fast, and
cross-instance interactive-query routing so a user query
reaches whatever pod owns the key.

```haskell
import Kafka.Streams.Runtime

-- Probing rebalance: tell the runtime a standby is caught up.
reportWarmupLag    ks tid 0
maybeIssueProbe <- streamThreadCount ks

-- Dynamic thread management:
n  <- addStreamThread    ks
n' <- removeStreamThread ks

-- Graceful close with leaveGroup=False (static membership):
closeKafkaStreamsWith ks
  defaultCloseOptions { leaveGroup = False }
```

Standby tasks (`Kafka.Streams.Runtime.StandbyTask` /
`StandbyDriver`) shadow active state; the changelog poll loop
keeps them caught up. Cross-instance IQ
(`Kafka.Streams.Discovery.RemoteIQ`) routes user queries to
whichever instance owns the key.

## 9. Schema Registry serdes

`Kafka.Streams.Serde.SchemaRegistry` exposes the Confluent wire
envelope (`magicByte + schemaId + payload`) plus a pluggable
`SchemaRegistryClient` interface. Use `inMemoryRegistry` in
tests and your own HTTP client in production:

```haskell
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

main :: IO ()
main = do
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

## 10. Observability

`Kafka.Telemetry.StatsJson` mirrors the
[librdkafka stats JSON shape](https://github.com/confluentinc/librdkafka/blob/master/STATISTICS.md)
so dashboards written against the C client port over without
rewrites:

```haskell
import qualified Kafka.Telemetry.StatsJson as Stats
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let snap = (Stats.defaultSnapshot "wfkafka" "client-1" Stats.StatsProducer)
        { Stats.ssMsgCount = 100
        , Stats.ssTopics = Map.singleton "events"
            (Stats.TopicStats "events" 100 5 1024 0 0)
        }
  print (Stats.renderStats snap)
```

OTel spans / metrics flow through
`Kafka.Telemetry.OpenTelemetry`; the JSON snapshot above is
the librdkafka-shaped mirror of the same counters.

## 11. Where to look next

  * **Configuration**: [`CONFIG_PARITY.md`](./CONFIG_PARITY.md)
    is the librdkafka knob-by-knob mapping.
  * **Live broker tests**:
    `WIREFORM_KAFKA_BROKER=host:port cabal test wireform-kafka:wireform-kafka-integration`
    runs the full transactional + produce/consume suite.
  * **Mock cluster reference**: `wireform-kafka/src/Kafka/Client/Mock/`
    is the source of truth for fault injection. The README in
    that folder walks through every primitive.
  * **Streams reference**: `wireform-kafka-streams` mirrors the
    Java Streams DSL one combinator per name; the
    `Kafka.Streams.*` modules each carry a header comment
    pointing at the upstream Java equivalent.
  * **Streams examples**: `wireform-kafka/streams/examples`
    has runnable demos for every operator family (passthrough,
    line-split, word-count, page-view region, temperature
    window, top-articles, orders enrichment, fraud detection,
    inventory FK-join, interactive queries, processor API,
    branching, global table, cogroup, side effects) plus a
    README mapping each demo back to its JVM equivalent.
  * **Streams runtime benchmarks**:
    `wireform-kafka:wireform-kafka-streams-bench` measures the
    runtime hot paths in-process; numbers + reproduction recipe
    live in `streams/bench/results/README.md`.
  * **Exception handlers**: every production / processing /
    uncaught handler is wired into the runtime; see the spec
    in `streams/test/Streams/ExceptionHandlerSpec.hs` for the
    public API + contract.
