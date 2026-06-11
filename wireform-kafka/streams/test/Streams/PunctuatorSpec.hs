{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.PunctuatorSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "Punctuator" $
    sequence_
      [ stream_time_punctuator
      , wall_clock_punctuator
      , punctuator_can_be_cancelled
      , punctuator_no_fire_before_due
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


mkProc
  :: IORef [Timestamp]
  -- ^ accumulator for fire timestamps
  -> Int
  -- ^ interval ms
  -> PunctuationType
  -> IO (Processor Text Text)
mkProc accRef intervalMs ptype = do
  ctxRef <- newIORef Nothing
  pure
    Processor
      { procName = processorName "PUNCT-PROC"
      , procInit = \ctx -> do
          writeIORef ctxRef (Just ctx)
          _ <- schedule ctx intervalMs ptype $ Punctuator $ \ts ->
            modifyIORef' accRef (ts :)
          pure ()
      , procClose = pure ()
      , procProcess = \_ -> pure ()
      }


stream_time_punctuator :: Spec
stream_time_punctuator =
  it "stream-time punctuator fires as time advances" $ do
    fired <- newIORef ([] :: [Timestamp])
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let bld = kstreamBuilder src
    nm <- freshNodeName bld "PUNCT"
    withTopology_ bld $
      Kafka.Streams.Imperative.addProcessor nm [kstreamParent src] (mkProc fired 100 StreamTimePunctuation)
    topo <- buildTopology bld
    driver <- newDriver topo "punct-app"

    -- The first record at ts=50 advances stream time from MIN_VALUE
    -- to 50. Per Java semantics that's an "infinite gap" so the
    -- punctuator fires once.
    pipeInput driver (topicName "in") Nothing (bytes "x") (Timestamp 50) 0
    f1 <- readIORef fired
    length f1 `shouldBe` 1

    -- Stream time now 50, next-fire at 150. Pushing ts=120 should
    -- not fire.
    pipeInput driver (topicName "in") Nothing (bytes "x") (Timestamp 120) 0
    f2 <- readIORef fired
    length f2 `shouldBe` 1

    -- ts=250 crosses 150: one more fire.
    pipeInput driver (topicName "in") Nothing (bytes "x") (Timestamp 250) 0
    f3 <- readIORef fired
    length f3 `shouldBe` 2
    closeDriver driver


wall_clock_punctuator :: Spec
wall_clock_punctuator =
  it "wall-clock punctuator fires after advanceWallClockTime" $ do
    fired <- newIORef ([] :: [Timestamp])
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let bld = kstreamBuilder src
    nm <- freshNodeName bld "PUNCT"
    withTopology_ bld $
      Kafka.Streams.Imperative.addProcessor
        nm
        [kstreamParent src]
        (mkProc fired 100 WallClockTimePunctuation)
    topo <- buildTopology bld
    driver <- newDriver topo "punct-app"

    advanceWallClockTime driver 50
    f1 <- readIORef fired
    length f1 `shouldBe` 0

    advanceWallClockTime driver 60
    f2 <- readIORef fired
    length f2 `shouldBe` 1

    advanceWallClockTime driver 100
    f3 <- readIORef fired
    length f3 `shouldBe` 2
    closeDriver driver


punctuator_can_be_cancelled :: Spec
punctuator_can_be_cancelled =
  it "schedule returns a cancel that suppresses future fires" $ do
    fired <- newIORef ([] :: [Timestamp])
    cancelRef <- newIORef Nothing
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let bld = kstreamBuilder src
        proc_ = do
          ctxRef <- newIORef Nothing
          pure
            Processor
              { procName = processorName "PUNCT-CANC"
              , procInit = \ctx -> do
                  writeIORef ctxRef (Just ctx)
                  tok <- schedule ctx 100 WallClockTimePunctuation $
                    Punctuator $ \ts ->
                      modifyIORef' fired (ts :)
                  writeIORef cancelRef (Just tok)
              , procClose = pure ()
              , procProcess = \_ -> pure ()
              }
    nm <- freshNodeName bld "PUNCT-CANC"
    withTopology_ bld $
      Kafka.Streams.Imperative.addProcessor nm [kstreamParent src] proc_
    topo <- buildTopology bld
    driver <- newDriver topo "punct-app"

    advanceWallClockTime driver 200 -- fires
    f1 <- readIORef fired
    length f1 `shouldBe` 1

    Just tok <- readIORef cancelRef
    cancel tok

    advanceWallClockTime driver 500 -- should not fire
    f2 <- readIORef fired
    length f2 `shouldBe` 1
    closeDriver driver


punctuator_no_fire_before_due :: Spec
punctuator_no_fire_before_due =
  it "punctuators do not fire before the due time" $ do
    fired <- newIORef ([] :: [Timestamp])
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let bld = kstreamBuilder src
    nm <- freshNodeName bld "PUNCT"
    withTopology_ bld $
      Kafka.Streams.Imperative.addProcessor
        nm
        [kstreamParent src]
        (mkProc fired 1000 WallClockTimePunctuation)
    topo <- buildTopology bld
    driver <- newDriver topo "punct-app"

    advanceWallClockTime driver 100
    advanceWallClockTime driver 100
    advanceWallClockTime driver 100
    f1 <- readIORef fired
    length f1 `shouldBe` 0
    closeDriver driver
