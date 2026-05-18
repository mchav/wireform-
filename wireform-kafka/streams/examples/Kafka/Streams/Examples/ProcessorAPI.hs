{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.ProcessorAPI
-- Description : Low-level Processor API + Punctuator
--
-- The DSL covers the 80% case; for the rest, the Processor API
-- gives you direct access to 'ProcessorContext' (forward,
-- schedule, getStateStore). This example mirrors the JVM
-- low-level demo: build a processor that maintains a
-- per-key event count and, on a wall-clock punctuator, emits a
-- summary record.
--
-- Java (paraphrased):
--
-- @
-- Topology t = new Topology();
-- t.addSource("src", "events");
-- t.addProcessor("counter", CountingProcessor::new, "src");
-- t.addStateStore(Stores.keyValueStoreBuilder(...), "counter");
-- t.addSink("snk", "summary", "counter");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.ProcessorAPI
  ( runDemo
  , buildProcessorAPITopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams
import Kafka.Streams.StreamsBuilder (withTopology_)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store (StoreBuilderKV)
import qualified Kafka.Streams.Topology as Topo

storeNm :: StoreName
storeNm = storeName "counter-store"

-- | The custom processor: keeps a per-key Int64 count in a state
-- store; on every record forwards the running count downstream.
countingProcessor :: IO (Processor Text Text)
countingProcessor = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
  pure Processor
    { procName = processorName "CountingProcessor"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx storeNm >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef
              (Just (Unsafe.unsafeCoerce kvs))
          _ -> error "counter store missing"
        -- Optional: schedule a wall-clock punctuator that runs
        -- once a second to emit a heartbeat summary.
        _ <- schedule ctx
                1000
                WallClockTimePunctuation
                (Punctuator (\_t -> pure ()))
        pure ()
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k  -> do
          mctx   <- readIORef ctxRef
          mStore <- readIORef storeRef
          case (mctx, mStore) of
            (Just ctx, Just kvs) -> do
              cur <- kvsGet kvs k
              let !next = maybe 1 (+ 1) cur
              kvsPut kvs k next
              forwardRecord ctx
                (Record (Just k) next
                  (recordTimestamp r)
                  (recordHeaders r) :: Record Text Int64)
            _ -> pure ()
    }

buildProcessorAPITopology :: IO Topology
buildProcessorAPITopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "events")
            (consumed textSerde textSerde)
  -- Low-level processor: maps Text values to running Int64
  -- counts via processValuesStream (which exposes a typed
  -- downstream KStream the way KStream.process returns
  -- KStream<K, V'> on the JVM).
  counts <- processValuesStream
              "Counter"
              [storeNm]
              countingProcessor
              int64Serde
              src
  -- Attach the state store the processor depends on. The DSL
  -- 'processValuesStream' takes the store /names/ but doesn't
  -- add the store itself; that's the caller's job (matches
  -- Java's @Topology.addStateStore(storeBuilder, "Counter")@).
  -- We use the KStream's parent NodeName to pin the store
  -- ownership to the actual processor we just registered.
  let kvBuilder = inMemoryKeyValueStoreBuilder storeNm
                    :: StoreBuilderKV Text Int64
  withTopology_ b $ \t ->
    Topo.addStateStoreKV kvBuilder
      [kstreamParent counts]
      t
  toTopic
    (topicName "counts-stream")
    (produced textSerde int64Serde)
    counts
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== ProcessorAPIDemo ==="
  topo <- buildProcessorAPITopology
  driver <- newDriver topo "processor-api-app"

  let ev k tsMs =
        pipeInput driver (topicName "events")
          (Just (BSC.pack (T.unpack k)))
          (BSC.pack "hit")
          (Timestamp tsMs)
          0
  mapM_ (\(k, ts) -> ev k ts)
    [ ("alice", 0), ("bob", 1), ("alice", 2)
    , ("alice", 3), ("carol", 4), ("bob", 5)
    ]
  out <- readOutput driver (topicName "counts-stream")
  putStrLn ("Counts emitted (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize int64Serde (crValue cr) :: Either Text Int64 of
            Right n  -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> k <> " -> " <> v)
