{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.InteractiveQueries
-- Description : KIP-67 / KIP-796 — query state stores from outside the topology
--
-- Interactive Queries lets external code peek at the materialised
-- KTable backing any aggregation. This example builds the same
-- word-count topology as 'Kafka.Streams.Examples.WordCount' but,
-- instead of forwarding the counts to a sink topic, exposes them
-- via a 'ReadOnlyKeyValueStore' handle and runs a few queries.
--
-- Java (paraphrased):
--
-- @
-- ReadOnlyKeyValueStore<String, Long> store = streams.store(
--   StoreQueryParameters.fromNameAndType("counts-store",
--                                        QueryableStoreTypes.keyValueStore()));
-- Long alice = store.get("alice");
-- KeyValueIterator<String, Long> it = store.range("a", "m");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.InteractiveQueries
  ( runDemo
  , buildIQTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildIQTopology :: IO Topology
buildIQTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "words")
            (consumed textSerde textSerde)
  grouped_ <- groupByStream
                (\r -> recordValue r)
                (grouped textSerde textSerde)
                src
  _counts <- countStream
              (materializedAs (storeName "counts-store"))
              grouped_
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== InteractiveQueriesDemo ==="
  topo <- buildIQTopology
  driver <- newDriver topo "iq-app"

  -- Pump some words.
  let send w =
        pipeInput driver (topicName "words")
          Nothing
          (BSC.pack (T.unpack w))
          (Timestamp 0)
          0
  mapM_ send
    ["alice", "bob", "alice", "carol", "alice", "bob", "carol", "carol"]

  -- Point query: get a single key.
  rs <- queryEngineStore @Text @Int64
          (driverEngine driver)
          (storeName "counts-store")
  case rs of
    Nothing  -> putStrLn "store missing"
    Just kvs -> do
      mapM_ (\k -> do
              v <- kvs.roKvGet k
              putStrLn ("  get " <> show k <> " = " <> show v))
        ["alice", "bob", "carol", "dave"]

      -- Range scan: iterate every (key, count) in lexical order.
      it <- kvs.roKvAll
      pairs <- kvIteratorToList it
      putStrLn "  range(all):"
      mapM_ (\(k, v) -> putStrLn ("    " <> show k <> " = " <> show v)) pairs

  closeDriver driver
