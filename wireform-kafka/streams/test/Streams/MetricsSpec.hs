{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.MetricsSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative
import Kafka.Streams.Internal.Engine (engineMetrics)

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "Metrics"
  [ counter_basics
  , gauge_basics
  , duration_stats
  , engine_increments_processTotal
  , engine_increments_commitTotal
  , engine_increments_punctuateTotal
  ]

counter_basics :: TestTree
counter_basics = testCase "incCounter / addCounter / readCounter" $ do
  r <- newMetricsRegistry
  readCounter r "x" >>= (@?= 0)
  incCounter r "x"
  incCounter r "x"
  readCounter r "x" >>= (@?= 2)
  addCounter r "x" 8
  readCounter r "x" >>= (@?= 10)

gauge_basics :: TestTree
gauge_basics = testCase "setGauge / readGauge: last write wins" $ do
  r <- newMetricsRegistry
  readGauge r "g" >>= (@?= Nothing)
  setGauge r "g" 1.5
  readGauge r "g" >>= (@?= Just 1.5)
  setGauge r "g" 9.99
  readGauge r "g" >>= (@?= Just 9.99)

duration_stats :: TestTree
duration_stats = testCase "observeDuration: count / sum / min / max" $ do
  r <- newMetricsRegistry
  mapM_ (observeDuration r "d") [10, 20, 30, 40, 50]
  Just s <- readDurationStats r "d"
  dsCount s @?= 5
  dsSum   s @?= 150
  dsMin   s @?= 10
  dsMax   s @?= 50

engine_increments_processTotal :: TestTree
engine_increments_processTotal =
  testCase "engine bumps processTotal on every successful record" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) src
    topo <- buildTopology b
    driver <- newDriver topo "m-app"

    pipeInput driver (topicName "in") Nothing (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "b") (t 1) 0
    pipeInput driver (topicName "in") Nothing (bytes "c") (t 2) 0

    let m = engineMetrics (driverEngine driver)
    readCounter m processTotal >>= (@?= 3)
    closeDriver driver

engine_increments_commitTotal :: TestTree
engine_increments_commitTotal =
  testCase "commitDriver bumps commitTotal" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) src
    topo <- buildTopology b
    driver <- newDriver topo "m-app"

    let m = engineMetrics (driverEngine driver)
    readCounter m commitTotal >>= (@?= 0)
    commitDriver driver
    commitDriver driver
    readCounter m commitTotal >>= (@?= 2)
    closeDriver driver

engine_increments_punctuateTotal :: TestTree
engine_increments_punctuateTotal =
  testCase "punctuator firings increment punctuateTotal" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let bld = kstreamBuilder src
        proc_ = do
          pure Processor
            { procName    = processorName "TICK"
            , procInit    = \ctx -> do
                _ <- schedule ctx 100 WallClockTimePunctuation
                       (Punctuator (\_ -> pure ()))
                pure ()
            , procClose   = pure ()
            , procProcess = \_ -> pure ()
            }
    nm <- freshNodeName bld "TICK"
    withTopology_ bld $ Kafka.Streams.Imperative.addProcessor nm [kstreamParent src] proc_
    topo <- buildTopology bld
    driver <- newDriver topo "m-app"

    let met = engineMetrics (driverEngine driver)
    advanceWallClockTime driver 250   -- crosses 100ms; fires once
    readCounter met punctuateTotal >>= (@?= 1)
    advanceWallClockTime driver 200
    readCounter met punctuateTotal >>= (@?= 2)
    closeDriver driver

