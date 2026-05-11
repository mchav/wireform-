{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.IdiomaticPipeline
-- Description : Reusable composable fragments via 'Pipeline'
--
-- The other examples mirror the JVM fluent-builder shape
-- one combinator at a time. This one shows the
-- "Haskell-native" complement: 'Pipeline' values composed with
-- 'Control.Category.(>>>)' and applied to a 'KStream' at the
-- site that uses them.
--
-- The point of 'Pipeline' isn't to replace the core DSL — it's
-- to let you name and reuse transformation fragments as
-- values. A 'Pipeline' is a 'Control.Category.Category', so
-- two fragments compose into one fragment without losing the
-- ability to apply it later.
--
-- Equivalent JVM idiom: extracting a topology-building helper
-- method that takes a @KStream@ and returns a transformed
-- @KStream@.
module Kafka.Streams.Examples.IdiomaticPipeline
  ( runDemo
  , buildPipelineTopology
    -- * Reusable fragments
  , normalise
  , dropEmpties
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import Kafka.Streams
import Kafka.Streams.DSL.Pipeline

-- | Strip whitespace + uppercase every value. A reusable
-- fragment — could be defined in a shared module and used by
-- many topologies.
normalise :: Pipeline (KStream T.Text T.Text) (KStream T.Text T.Text)
normalise = pmapValues T.strip >>> pmapValues T.toUpper

-- | Drop records whose value is empty after normalisation.
dropEmpties :: Pipeline (KStream T.Text T.Text) (KStream T.Text T.Text)
dropEmpties = pfilter (\r -> recordValue r /= "")

-- | Build a topology that wires the two fragments together
-- using 'Control.Category.(>>>)' and applies them to a single
-- source stream.
buildPipelineTopology :: IO Topology
buildPipelineTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "lines-in")
            (consumed textSerde textSerde)
  out <- applyPipeline (normalise >>> dropEmpties) src
  toTopic (topicName "lines-out") (produced textSerde textSerde) out
  buildTopology b

-- | Run the topology against the in-process driver and print
-- the records routed to @lines-out@.
runDemo :: IO ()
runDemo = do
  putStrLn "=== IdiomaticPipelineDemo ==="
  topo <- buildPipelineTopology
  driver <- newDriver topo "idiomatic-pipeline"
  mapM_ (send driver)
    [ ("k1", "  hello world  ")
    , ("k2", "   ")
    , ("k3", "kafka streams")
    ]
  out <- readOutput driver (topicName "lines-out")
  putStrLn ("Records emitted to lines-out (" <> show (length out) <> "):")
  mapM_ (\cr -> putStrLn ("  " <> BSC.unpack (crValue cr))) out
  closeDriver driver
  where
    send d (k, v) =
      pipeInput d (topicName "lines-in")
        (Just (BSC.pack k))
        (BSC.pack v)
        (Timestamp 0) 0
