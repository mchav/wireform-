{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end tests for the idiomatic-Haskell façades over
-- the streams DSL:
--
--   * 'TopologyM' monad
--   * 'Pipeline' Kleisli arrow over 'KStream'
--   * 'OfStream' Functor wrapper
--
-- Each spec runs a topology end-to-end through the in-process
-- 'TopologyTestDriver' and asserts the same output the
-- equivalent imperative-DSL version would produce.
module Streams.IdiomaticDSLSpec (tests) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import Data.Function ((&))
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams
  hiding (mapValuesM, mapKeyValueM, filterStreamM, flatMapValuesM
         , flatMapKeyValueM, peekStreamM, selectKeyM)
import Kafka.Streams.DSL.Mappable (OfStream (..), liftStream, withStream)
import Kafka.Streams.DSL.Monadic
import Kafka.Streams.DSL.Pipeline

tests :: TestTree
tests = testGroup "Idiomatic DSL façades"
  [ topology_monad_do_block
  , pipeline_arrow_composition
  , functor_wrapper_chains_mapvalues
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Int -> Timestamp
ts = Timestamp . fromIntegral

----------------------------------------------------------------------
-- TopologyM
----------------------------------------------------------------------

-- A topology written entirely in TopologyM `do`-notation —
-- no explicit StreamsBuilder threading, no IO bookkeeping
-- inside the user's code.
topology_monad_do_block :: TestTree
topology_monad_do_block =
  testCase "TopologyM: do-block topology end-to-end" $ do
    (topo, ()) <- runTopologyM $ do
      src <- streamFrom (topicName "in")
                        (consumed textSerde textSerde)
      src |> mapValuesM T.toUpper
          >>= filterStreamM (\r -> recordValue r /= "SKIP")
          >>= sinkTo (topicName "out") (produced textSerde textSerde)

    driver <- newDriver topo "topology-monad"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hi")   (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "skip") (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "ok")   (ts 2) 0
    let out = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList out
    -- "hi" -> "HI" survives, "skip" -> "SKIP" is filtered out,
    -- "ok" -> "OK" survives.
    [v | Right (_, v) <- rs] @?= ["HI", "OK"]
    closeDriver driver

----------------------------------------------------------------------
-- Pipeline arrow
----------------------------------------------------------------------

-- Build a Pipeline value with Category composition, then
-- apply it to a stream. The pipeline is reusable — you could
-- save it in a top-level binding and stamp it over many
-- streams.
pipeline_arrow_composition :: TestTree
pipeline_arrow_composition =
  testCase "Pipeline: Category-composed value applies end-to-end" $ do
    let normalise :: Pipeline (KStream Text Text) (KStream Text Text)
        normalise =
              pmapValues T.toUpper
          >>> pfilter   (\r -> recordValue r /= "")
          >>> pmapValues (T.take 4)

    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    out <- applyPipeline normalise src
    toTopic (topicName "out") (produced textSerde textSerde) out
    topo <- buildTopology b

    driver <- newDriver topo "pipeline-arrow"
    pipeInput driver (topicName "in") (Just "k1") (bytes "hello")  (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "")        (ts 1) 0
    pipeInput driver (topicName "in") (Just "k3") (bytes "haskell") (ts 2) 0
    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    [v | Right (_, v) <- rs] @?= ["HELL", "HASK"]
    closeDriver driver

----------------------------------------------------------------------
-- Functor wrapper
----------------------------------------------------------------------

-- 'OfStream' makes a KStream usable as a Functor over the
-- value type. Chaining 'fmap' adds 'mapValues' nodes in
-- order; 'withStream' materialises the final stream.
functor_wrapper_chains_mapvalues :: TestTree
functor_wrapper_chains_mapvalues =
  testCase "OfStream: 'fmap' chain queues mapValues operations" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    -- Three deferred mapValues:
    --   * pad to 5 chars
    --   * uppercase
    --   * reverse
    out <- withStream $
      liftStream src
        & fmap (T.justifyLeft 5 ' ')
        & fmap T.toUpper
        & fmap T.reverse
    toTopic (topicName "out") (produced textSerde textSerde) out
    topo <- buildTopology b

    driver <- newDriver topo "functor-wrapper"
    pipeInput driver (topicName "in") (Just "k1") (bytes "ab") (ts 0) 0
    pipeInput driver (topicName "in") (Just "k2") (bytes "xyz") (ts 1) 0
    let outT = createOutputTopic driver (topicName "out") textSerde textSerde
    rs <- readKeyValuesToList outT
    -- "ab" -> "ab   " -> "AB   " -> "   BA"
    -- "xyz" -> "xyz  " -> "XYZ  " -> "  ZYX"
    [v | Right (_, v) <- rs] @?= ["   BA", "  ZYX"]
    closeDriver driver
