{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.IdiomaticPipeline
-- Description : Shows the Haskell-native façades (TopologyM,
--               Pipeline, OfStream) end-to-end
--
-- The other examples mirror the JVM fluent-builder shape one
-- combinator at a time. This one demonstrates the three
-- alternative façades that ship in
-- @Kafka.Streams.DSL.{Topology, Pipeline, Mappable}@ and how
-- they read in idiomatic Haskell.
--
-- The topology does the same thing three different ways:
--
--   * Read text from @lines-in@
--   * Trim, uppercase, drop empty values
--   * Write the result to @lines-out@
--
-- Each section builds the same topology with a different
-- façade, so you can compare. Pick whichever style feels
-- best in your own code.
module Kafka.Streams.Examples.IdiomaticPipeline
  ( runDemo
    -- * Topologies (each builds the same thing a different way)
  , buildMonadicTopology
  , buildPipelineTopology
  , buildFunctorTopology
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import Data.Function ((&))
import qualified Data.Text as T

import Kafka.Streams
  hiding (mapValuesM, filterStreamM)
import Kafka.Streams.DSL.Mappable (liftStream, withStream)
import Kafka.Streams.DSL.Monadic
import Kafka.Streams.DSL.Pipeline

----------------------------------------------------------------------
-- TopologyM (do-block) façade
----------------------------------------------------------------------

-- | Build the topology in a single 'TopologyM' @do@ block.
-- No explicit 'StreamsBuilder' — the monad threads it.
buildMonadicTopology :: IO Topology
buildMonadicTopology = fmap fst $ runTopologyM $ do
  src <- streamFrom (topicName "lines-in")
                    (consumed textSerde textSerde)
  src |> mapValuesM T.strip
      >>= mapValuesM T.toUpper
      >>= filterStreamM (\r -> recordValue r /= "")
      >>= sinkTo (topicName "lines-out") (produced textSerde textSerde)

----------------------------------------------------------------------
-- Pipeline (Category-style) façade
----------------------------------------------------------------------

-- | A reusable transformation as a Pipeline value. Could be
-- defined elsewhere and dropped into any topology.
normalise :: Pipeline (KStream T.Text T.Text)
                      (KStream T.Text T.Text)
normalise =
      pmapValues T.strip
  >>> pmapValues T.toUpper
  >>> pfilter    (\r -> recordValue r /= "")

-- | Same topology, but the transformation is a first-class
-- value. The Kleisli-style composition makes the data-flow
-- read like ordinary function composition.
buildPipelineTopology :: IO Topology
buildPipelineTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "lines-in")
            (consumed textSerde textSerde)
  out <- applyPipeline normalise src
  toTopic (topicName "lines-out") (produced textSerde textSerde) out
  buildTopology b

----------------------------------------------------------------------
-- OfStream (Functor wrapper) façade
----------------------------------------------------------------------

-- | Same topology, but the value-only transforms run through
-- 'fmap' on the 'OfStream' wrapper. The filter still needs
-- @filterStream@ — Functor only covers value-mapping —
-- so this style mixes 'fmap' with one explicit step.
buildFunctorTopology :: IO Topology
buildFunctorTopology = do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "lines-in")
            (consumed textSerde textSerde)
  upper <- withStream $
    liftStream src
      & fmap T.strip
      & fmap T.toUpper
  out <- filterStream (\r -> recordValue r /= "") upper
  toTopic (topicName "lines-out") (produced textSerde textSerde) out
  buildTopology b

----------------------------------------------------------------------
-- Demo
----------------------------------------------------------------------

-- | Runs all three topologies against the in-process test
-- driver with the same inputs and prints their outputs. They
-- should all produce the same records.
runDemo :: IO ()
runDemo = do
  putStrLn "=== IdiomaticPipelineDemo ==="
  runOne "TopologyM do-block" =<< buildMonadicTopology
  runOne "Pipeline arrow"     =<< buildPipelineTopology
  runOne "OfStream Functor"   =<< buildFunctorTopology
  where
    runOne label topo = do
      putStrLn ("\n-- " <> label)
      driver <- newDriver topo "idiomatic"
      mapM_ (send driver)
        [ ("k1", "  hello world  ")
        , ("k2", "   ")
        , ("k3", "kafka streams")
        ]
      out <- readOutput driver (topicName "lines-out")
      mapM_ (\cr -> putStrLn ("  " <> BSC.unpack (crValue cr))) out
      closeDriver driver

    send d (k, v) =
      pipeInput d (topicName "lines-in")
        (Just (BSC.pack k))
        (BSC.pack v)
        (Timestamp 0) 0
