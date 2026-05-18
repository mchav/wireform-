{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.LineSplit
-- Description : Stateless flatMap — split lines into words
--
-- Mirror of @org.apache.kafka.streams.examples.wordcount.LineSplitDemo@.
-- Reads lines from one topic, splits each line on whitespace, writes
-- one record per word to another topic.
--
-- Java:
--
-- @
-- builder.<String,String>stream("streams-plaintext-input")
--        .concatMapValues(value -> Arrays.asList(value.split("\\W+")))
--        .to("streams-linesplit-output");
-- @
--
-- Haskell (written as a "Kafka.Streams.Topology.Free" value):
module Kafka.Streams.Examples.LineSplit
  ( runDemo
  , lineSplitTopology
  , buildLineSplitTopology
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Topology.Free as F

lineSplitTopology :: F.Topology Void ()
lineSplitTopology =
  F.source "streams-plaintext-input" textSerde textSerde
    >>> F.concatMapValues (T.words :: Text -> [Text])
    >>> F.sink "streams-linesplit-output" textSerde textSerde

buildLineSplitTopology :: IO Topology
buildLineSplitTopology = F.buildTopologyFrom lineSplitTopology

runDemo :: IO ()
runDemo = do
  putStrLn "=== LineSplitDemo ==="
  topo <- buildLineSplitTopology
  driver <- newDriver topo "line-split-app"

  mapM_ (sendLine driver)
    [ "all streams lead to kafka"
    , "hello kafka streams"
    , "join kafka summit"
    ]

  out <- readOutput driver (topicName "streams-linesplit-output")
  putStrLn ("Words emitted (" <> show (length out) <> "):")
  mapM_ (\cr -> putStrLn ("  " <> BSC.unpack (crValue cr))) out
  closeDriver driver
  where
    sendLine d line =
      pipeInput d (topicName "streams-plaintext-input")
        Nothing
        (BSC.pack (T.unpack line))
        (Timestamp 0)
        0
