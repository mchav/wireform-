{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.WordCount
-- Description : Classic word-count — flatMapValues + groupBy + count
--
-- Mirror of @org.apache.kafka.streams.examples.wordcount.WordCountDemo@,
-- the canonical "hello world" of stateful streams processing.
--
-- Java:
--
-- @
-- KStream<String, String> source = builder.stream("streams-plaintext-input");
-- source.flatMapValues(v -> Arrays.asList(v.toLowerCase().split("\\W+")))
--       .groupBy((key, word) -> word)
--       .count(Materialized.as("counts-store"))
--       .toStream()
--       .to("streams-wordcount-output", Produced.with(stringSerde, longSerde));
-- @
--
-- Haskell — same shape, same operator names:
module Kafka.Streams.Examples.WordCount
  ( runDemo
  , buildWordCountTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildWordCountTopology :: IO Topology
buildWordCountTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "streams-plaintext-input")
            (consumed textSerde textSerde)
  -- flatMapValues: lowercase + split on whitespace
  words_ <- flatMapValues
              (T.words . T.toLower :: Text -> [Text])
              src
  -- groupBy(word) — i.e. selectKey(value) + groupByKey
  grouped_ <- groupByStream
                (\r -> recordValue r)
                (grouped textSerde textSerde)
                words_
  -- count
  counts <- countStream
              (materializedAs (storeName "counts-store"))
              grouped_
  -- KTable.toStream() — pin a KStream view at the count
  -- processor's emit node. Same as Java's
  -- @counts.toStream().to("streams-wordcount-output", ...)@.
  let countsStream = KStream
        { kstreamBuilder    = ctlBuilder counts
        , kstreamParent     = ctlNode counts
        , kstreamKeySerde   = textSerde
        , kstreamValueSerde = int64Serde
        }
  toTopic
    (topicName "streams-wordcount-output")
    (produced textSerde int64Serde)
    countsStream
  buildTopology b

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
          cnt = case deserialize int64Serde (crValue cr) :: Either String Int64 of
            Right n  -> show n
            Left err -> "?(" <> err <> ")"
      in putStrLn ("  " <> wd <> " = " <> cnt)
