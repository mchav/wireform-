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
-- Haskell — same shape, written against
-- "Kafka.Streams.Topology.Free" so the topology is a first-class
-- value built with category composition:
module Kafka.Streams.Examples.Pipe
  ( runDemo
  , pipeTopology
  , buildPipeTopology
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Topology.Free as F

-- | The pipe topology as a first-class 'F.Topology' value. No
-- 'IO' — composition is pure data, ready to be inspected or
-- optimised before compilation.
pipeTopology :: F.Topology Void ()
pipeTopology =
  F.source "streams-plaintext-input" textSerde textSerde
    >>> F.sink   "streams-pipe-output"     textSerde textSerde

-- | Build the imperative 'Topology' graph from the
-- 'pipeTopology' AST.
buildPipeTopology :: IO Topology
buildPipeTopology = F.buildTopologyFrom pipeTopology

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
    [ ("k1" :: Text, "all streams lead to kafka" :: Text)
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
