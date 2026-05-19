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
-- Haskell (free-arrow). The topology is a five-stage chain that
-- terminates in the count's materialised KTable — no sink — and the
-- demo body queries the resulting state store directly.
module Kafka.Streams.Examples.InteractiveQueries
  ( runDemo
  , iqTopology
  , buildIQTopology
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology.Free as F

iqTopology :: F.Topology Void (KTable Text Int64)
iqTopology =
  F.source @Text @Text "words"
    >>> F.groupBy (\r -> recordValue r)
    >>> F.count countMat
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde
        $ Mat.withKeySerde textSerde
        $ Mat.materializedAs (storeName "counts-store")

buildIQTopology :: IO Topo.Topology
buildIQTopology = snd <$> F.compile iqTopology

runDemo :: IO ()
runDemo = do
  putStrLn "=== InteractiveQueriesDemo ==="
  topo <- buildIQTopology
  driver <- newDriver topo "iq-app"

  let send w =
        pipeInput driver (topicName "words")
          Nothing
          (BSC.pack (T.unpack w))
          (Timestamp 0)
          0
  mapM_ send
    ["alice", "bob", "alice", "carol", "alice", "bob", "carol", "carol"]

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

      it <- kvs.roKvAll
      pairs <- kvIteratorToList it
      putStrLn "  range(all):"
      mapM_ (\(k, v) -> putStrLn ("    " <> show k <> " = " <> show v)) pairs

  closeDriver driver
