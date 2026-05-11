{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.Cogroup
-- Description : KIP-150 cogroup — combine multiple grouped streams
--               with distinct value types into one aggregate
--
-- 'cogroup' lets you merge two (or more) grouped streams of
-- different value types into a shared aggregator. The classic
-- demo: one stream of "deposit" amounts and another of
-- "withdrawal" amounts, both keyed by accountId, contribute to a
-- running balance.
--
-- Java:
--
-- @
-- KStream<String, Long> deposits   = builder.stream("deposits");
-- KStream<String, Long> withdrawals = builder.stream("withdrawals");
-- KGroupedStream<String, Long> dgrp = deposits.groupByKey();
-- KGroupedStream<String, Long> wgrp = withdrawals.groupByKey();
-- KTable<String, Long> balance = dgrp
--   .cogroup((k, v, agg) -> agg + v)
--   .cogroup(wgrp, (k, v, agg) -> agg - v)
--   .aggregate(() -> 0L, Materialized.as("balances"));
-- balance.toStream().to("balances-out");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.Cogroup
  ( runDemo
  , buildCogroupTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildCogroupTopology :: IO Topology
buildCogroupTopology = do
  b <- newStreamsBuilder
  deposits <- streamFromTopic b
                 (topicName "deposits")
                 (consumed textSerde int64Serde)
  withdrawals <- streamFromTopic b
                    (topicName "withdrawals")
                    (consumed textSerde int64Serde)
  let dgrp = groupByKey (grouped textSerde int64Serde) deposits
      wgrp = groupByKey (grouped textSerde int64Serde) withdrawals
  -- Shared aggregator state: Int64 running balance.
  let cog = addCogrouped
              (cogroup dgrp (\_k v acc -> acc + v))
              wgrp
              (\_k v acc -> acc - v)
  balances <- aggregateCogrouped
                (pure (0 :: Int64))
                (materializedAs (storeName "balances-store"))
                cog
  -- Stream the table out as KTable.toStream().to(...).
  -- ctlNode is the cogroup's emit anchor; build a KStream pinned
  -- to it so we can sink it to a topic.
  let s = KStream
            { kstreamBuilder    = ctlBuilder balances
            , kstreamParent     = ctlNode balances
            , kstreamKeySerde   = textSerde
            , kstreamValueSerde = int64Serde
            }
  toTopic
    (topicName "balances-out")
    (produced textSerde int64Serde)
    s
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== CogroupDemo ==="
  topo <- buildCogroupTopology
  driver <- newDriver topo "cogroup-app"

  let dep acct n =
        pipeInput driver (topicName "deposits")
          (Just (BSC.pack (T.unpack acct)))
          (serialize int64Serde n)
          (Timestamp 0) 0
      wdr acct n =
        pipeInput driver (topicName "withdrawals")
          (Just (BSC.pack (T.unpack acct)))
          (serialize int64Serde n)
          (Timestamp 0) 0

  dep "alice" 1000
  dep "bob"   500
  wdr "alice" 200
  dep "alice" 50
  wdr "bob"   300
  dep "carol" 750

  out <- readOutput driver (topicName "balances-out")
  putStrLn ("Balance updates (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize int64Serde (crValue cr) :: Either String Int64 of
            Right n  -> show n
            Left err -> "?(" <> err <> ")"
      in putStrLn ("  " <> k <> " = " <> v)
