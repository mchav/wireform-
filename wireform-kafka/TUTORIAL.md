# Tutorial

Hands-on walkthrough from first producer to stream processing. All examples run against the in-process mock broker. No Docker needed.

## 1. Send a record

```haskell
import qualified Kafka

main :: IO ()
main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
```

`withProducer` is a bracket. Connection opens, your code runs, then it flushes and closes cleanly.

**Variants:**
- `sendMessage_` - fire-and-forget
- `sendMessageAsync` - returns an `MVar` for later retrieval

## 2. Receive records (managed)

```haskell
{-# LANGUAGE OverloadedRecordDot #-}

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
    $ \rec -> BS.putStrLn rec.value
```

`runConsumer` joins the group, calls your handler per record, commits offsets, and leaves cleanly on exit.

**Batch variant:** `runBatchedConsumer` gives you a `Vector` of records. One commit covers the batch.

**Error handling:**
- `LogAndRaise` (default) - log and re-throw
- `SkipRecord` - log and continue
- `StopLoop` - log and exit
- `CustomError` - your predicate

**Commit modes:**
- `CommitSync` - commit after each successful handler (default)
- `CommitAsync` - fire-and-forget
- `CommitManual` - you commit

## 3. Receive records (manual)

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
            Consumer.commitSync c
```

## 4. Test without a broker

```haskell
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Producer
import Kafka.Client.Mock.Consumer
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  cluster <- newMockCluster 1
  createTopic cluster "events" 3

  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  _ <- sendMock producer "events" 0 (Just "key") "hello" 0

  consumer <- newMockConsumer cluster faults (GroupId "tutorial") ReadUncommitted 100
  subscribeMC consumer ["events"]
  PollResult records _ <- pollMC consumer
  print (length records)
```

The mock cluster powers 768+ tests without external dependencies.

## 5. Use transactions

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
        txn <- T.createTransaction (T.TransactionalId txId) connMgr vCache
                 "tutorial-client" (Conn.BrokerAddress "localhost" 9092) 60_000
        Right () <- T.initTransactions txn
        Kafka.bindTransaction p txn

        Right () <- T.beginTransaction txn
        _ <- Kafka.sendMessage p "events" Nothing "in-txn"
        Right () <- T.commitTransaction txn
        pure ()
```

**Gotcha:** Don't forget `bindTransaction`. The producer and transaction are separate.

## 6. Build a Streams topology

```haskell
import qualified Kafka.Streams as S
import qualified Kafka.Streams.StreamsBuilder as SB
import qualified Kafka.Streams.KStream as KS

main :: IO ()
main = do
  let topology =
        SB.runStreamsBuilder $ do
          input <- SB.streamFromTopic "events" S.bytesSerde S.bytesSerde
          KS.foreachStream input (\k v -> putStrLn ("got " <> show k <> "=" <> show v))
  print topology
```

**Async effects:** Use `foreachStreamAsync` for non-blocking side effects.

**Serde resolution:** The DSL automatically resolves serdes via the `HasSerde` typeclass for common types like `Text`, `Int64`, `Double`. You typically don't need to manually pass `Serde` values unless using custom encodings.

## 7. Transactional state stores

```haskell
import qualified Kafka.Streams.State.KeyValue.InMemory as Mem
import qualified Kafka.Streams.State.Transactional as TX
import qualified Kafka.Streams.State.Store as Store

main :: IO ()
main = do
  underlying <- Mem.inMemoryKeyValueStore (Store.storeName "totals")
  txStore    <- TX.newTransactionalStore underlying
  let store = TX.txnStore txStore
  Store.kvsPut store "k" "v"
  Just "v" <- Store.kvsGet store "k"      -- read your writes
  Nothing  <- Store.kvsGet underlying "k" -- not committed yet
  TX.txnCommit txStore
  Just "v" <- Store.kvsGet underlying "k"
  pure ()
```

## Troubleshooting

| Problem | Check |
|---|---|
| "Connection refused" | Verify broker address and port |
| Consumer not receiving | Check topic name matches; verify `autoOffsetReset` |
| Transaction timeouts | Ensure `commitTransaction` or `abortTransaction` called; increase timeout |
| Build issues | Run `cabal update`; check GHC version (9.6, 9.8, 9.10, 9.12) |

## Next steps

- [`CONCEPTS.md`](./CONCEPTS.md) - Kafka fundamentals
- [`streams/README.md`](./streams/README.md) - Full DSL reference
- [`CONFIG_PARITY.md`](./CONFIG_PARITY.md) - Configuration options
- [`examples/`](./examples/) - Runnable demos
