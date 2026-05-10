{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.Branching
-- Description : Split a stream by predicate (KIP-418)
--
-- The KIP-418 'splitStream' operator takes a list of named
-- predicates and produces one output stream per predicate, plus
-- (optionally) a default branch for records no predicate
-- matched.
--
-- Java:
--
-- @
-- Map<String, KStream<K,V>> branches =
--   stream.split(Named.as("ROUTE-"))
--         .branch((k, v) -> v.contains("error"), Branched.as("errors"))
--         .branch((k, v) -> v.contains("warn"),  Branched.as("warns"))
--         .defaultBranch(Branched.as("info"));
-- @
--
-- Haskell:
module Kafka.Streams.Examples.Branching
  ( runDemo
  , buildBranchingTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Kafka.Streams

buildBranchingTopology :: IO Topology
buildBranchingTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "logs")
            (consumed textSerde textSerde)
  -- splitStream returns a Map (BranchName -> KStream); the keys
  -- come from the Branched.as("...") names, mirroring the JVM.
  branches <- splitStream
                [ branchedFrom "errors"
                    (\r -> "error" `T.isInfixOf` recordValue r)
                , branchedFrom "warns"
                    (\r -> "warn"  `T.isInfixOf` recordValue r)
                ]
                (Just "info")   -- default branch name
                src
  -- Wire each branch to its own sink.
  let sink name topic =
        case Map.lookup name branches of
          Just s  -> toTopic (topicName topic) (produced textSerde textSerde) s
          Nothing -> error $ "missing branch " <> T.unpack name
  sink "errors" "logs-errors"
  sink "warns"  "logs-warns"
  sink "info"   "logs-info"
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== BranchingDemo ==="
  topo <- buildBranchingTopology
  driver <- newDriver topo "branching-app"

  let send v =
        pipeInput driver (topicName "logs")
          Nothing
          (BSC.pack (T.unpack v))
          (Timestamp 0) 0
  mapM_ send
    [ "error: connection lost"
    , "warn: slow query"
    , "info: heartbeat"
    , "error: disk full"
    , "all good"
    ]

  let tap topic = do
        rs <- readOutput driver (topicName topic)
        putStrLn ("  " <> T.unpack topic <> " ("
                       <> show (length rs) <> "):")
        mapM_ (\cr -> putStrLn ("    " <> BSC.unpack (crValue cr))) rs
  mapM_ tap ["logs-errors", "logs-warns", "logs-info"]
  closeDriver driver
