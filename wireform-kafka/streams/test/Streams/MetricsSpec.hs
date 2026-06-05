{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.MetricsSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Streams.Imperative
import Kafka.Streams.Internal.Engine (engineMetrics)

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: Spec
tests = describe "Metrics" $ sequence_
  [ counter_basics
  , gauge_basics
  , duration_stats
  , engine_increments_processTotal
  , engine_increments_commitTotal
  , engine_increments_punctuateTotal
  ]

counter_basics :: Spec
counter_basics = it "incCounter / addCounter / readCounter" $ do
  r <- newMetricsRegistry
  readCounter r "x" >>= (`shouldBe` 0)
  incCounter r "x"
  incCounter r "x"
  readCounter r "x" >>= (`shouldBe` 2)
  addCounter r "x" 8
  readCounter r "x" >>= (`shouldBe` 10)

gauge_basics :: Spec
gauge_basics = it "setGauge / readGauge: last write wins" $ do
  r <- newMetricsRegistry
  readGauge r "g" >>= (`shouldBe` Nothing)
  setGauge r "g" 1.5
  readGauge r "g" >>= (`shouldBe` Just 1.5)
  setGauge r "g" 9.99
  readGauge r "g" >>= (`shouldBe` Just 9.99)

duration_stats :: Spec
duration_stats = it "observeDuration: count / sum / min / max" $ do
  r <- newMetricsRegistry
  mapM_ (observeDuration r "d") [10, 20, 30, 40, 50]
  Just s <- readDurationStats r "d"
  dsCount s `shouldBe` 5
  dsSum   s `shouldBe` 150
  dsMin   s `shouldBe` 10
  dsMax   s `shouldBe` 50

engine_increments_processTotal :: Spec
engine_increments_processTotal =
  it "engine bumps processTotal on every successful record" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) src
    topo <- buildTopology b
    driver <- newDriver topo "m-app"

    pipeInput driver (topicName "in") Nothing (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "b") (t 1) 0
    pipeInput driver (topicName "in") Nothing (bytes "c") (t 2) 0

    let m = engineMetrics (driverEngine driver)
    readCounter m processTotal >>= (`shouldBe` 3)
    closeDriver driver

engine_increments_commitTotal :: Spec
engine_increments_commitTotal =
  it "commitDriver bumps commitTotal" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) src
    topo <- buildTopology b
    driver <- newDriver topo "m-app"

    let m = engineMetrics (driverEngine driver)
    readCounter m commitTotal >>= (`shouldBe` 0)
    commitDriver driver
    commitDriver driver
    readCounter m commitTotal >>= (`shouldBe` 2)
    closeDriver driver

engine_increments_punctuateTotal :: Spec
engine_increments_punctuateTotal =
  it "punctuator firings increment punctuateTotal" $ do
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
    readCounter met punctuateTotal >>= (`shouldBe` 1)
    advanceWallClockTime driver 200
    readCounter met punctuateTotal >>= (`shouldBe` 2)
    closeDriver driver

