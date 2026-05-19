{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.WordCount
-- Description : Classic word-count — concatMapValues + groupBy + count
--
-- Mirror of @org.apache.kafka.streams.examples.wordcount.WordCountDemo@,
-- the canonical "hello world" of stateful streams processing.
--
-- Java:
--
-- @
-- KStream<String, String> source = builder.stream("streams-plaintext-input");
-- source.concatMapValues(v -> Arrays.asList(v.toLowerCase().split("\\W+")))
--       .groupBy((key, word) -> word)
--       .count(Materialized.as("counts-store"))
--       .toStream()
--       .to("streams-wordcount-output", Produced.with(stringSerde, longSerde));
-- @
--
-- Haskell, written against the free-arrow 'F.Topology'. The
-- pipeline reads as a chain of 'Control.Category.(>>>)'-composed
-- combinators and only at the very edge — 'F.buildTopologyFrom' —
-- becomes an imperative graph.
module Kafka.Streams.Examples.WordCount
  ( runDemo
  , wordCountTopology
  , buildWordCountTopology
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

wordCountTopology :: F.Topology Void ()
wordCountTopology =
  F.source "streams-plaintext-input" textSerde textSerde
    >>> F.concatMapValues (T.words . T.toLower :: Text -> [Text])
    >>> F.groupBy
          (\r -> recordValue r)
          (grouped textSerde textSerde)
    >>> F.count countMat
    >>> F.toStream
    >>> F.sink "streams-wordcount-output" textSerde int64Serde
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde
        $ Mat.withKeySerde textSerde
        $ Mat.materializedAs (storeName "counts-store")

buildWordCountTopology :: IO Topo.Topology
buildWordCountTopology = F.buildTopologyFrom wordCountTopology

runDemo :: IO ()
runDemo = do
  putStrLn "=== WordCountDemo ==="
  topo <- buildWordCountTopology
  driver <- newDriver topo "word-count-app"

  mapM_ (sendLine driver)
    [ "all streams lead to kafka"
    , "hello kafka streams"
    , "join kafka summit"
    , "kafka streams kafka summit"
    ]

  out <- readOutput driver (topicName "streams-wordcount-output")
  putStrLn ("Word-count updates emitted (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    sendLine d line =
      pipeInput d (topicName "streams-plaintext-input")
        Nothing
        (BSC.pack (T.unpack line))
        (Timestamp 0)
        0

    printRec :: CollectedRecord -> IO ()
    printRec cr =
      let wd = case crKey cr of
            Just k  -> BSC.unpack k
            Nothing -> "<no-key>"
          cnt = case deserialize int64Serde (crValue cr) :: Either Text Int64 of
            Right n  -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> wd <> " = " <> cnt)
