{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.Pipe
-- Description : "Hello world" — copy from one topic to another
--
-- Mirror of @org.apache.kafka.streams.examples.pipe.PipeDemo@.
-- The simplest possible streams app: subscribe to @streams-plaintext-input@,
-- write everything to @streams-pipe-output@.
--
-- Java:
--
-- @
-- StreamsBuilder builder = new StreamsBuilder();
-- builder.stream("streams-plaintext-input").to("streams-pipe-output");
-- KafkaStreams streams = new KafkaStreams(builder.build(), props);
-- streams.start();
-- @
--
-- Haskell — same shape, exact same operator names:
module Kafka.Streams.Examples.Pipe
  ( runDemo
  , buildPipeTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

-- | Build the topology. Plain pipe: source → sink.
buildPipeTopology :: IO Topology
buildPipeTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "streams-plaintext-input")
            (consumed textSerde textSerde)
  toTopic
    (topicName "streams-pipe-output")
    (produced textSerde textSerde)
    src
  buildTopology b

-- | Demo run against the in-process test driver. Prints what
-- the equivalent JVM PipeDemo would have published to
-- @streams-pipe-output@.
runDemo :: IO ()
runDemo = do
  putStrLn "=== PipeDemo ==="
  topo <- buildPipeTopology
  driver <- newDriver topo "pipe-demo-app"

  let send (k, v) =
        pipeInput driver (topicName "streams-plaintext-input")
          (Just (BSC.pack (T.unpack k)))
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0
  mapM_ send
    [ ("k1", "all streams lead to kafka")
    , ("k2", "hello kafka streams")
    , ("k3", "join kafka summit")
    ]

  out <- readOutput driver (topicName "streams-pipe-output")
  putStrLn ("Records published to streams-pipe-output ("
            <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec :: CollectedRecord -> IO ()
    printRec cr =
      putStrLn $ "  " <> show (crKey cr) <> " -> "
                     <> BSC.unpack (crValue cr)
