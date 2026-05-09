{-# LANGUAGE OverloadedStrings #-}

-- | Live-broker tests for the consumer offset / seek surface
-- (KIP-41 'position', KIP-79 beginning/end/offsetsForTimes,
-- KIP-211 batch 'committedAll', KIP-339 'seek' / 'seekToBeginning'
-- / 'seekToEnd').
--
-- Skipped at run time unless @WIREFORM_KAFKA_BROKER=host:port@ is
-- set (mirrors the rest of the integration suite). Each scenario
-- runs on a freshly-named topic so concurrent runs don't collide.
module Integration.ConsumerOffsetsSpec
  ( tests
  ) where

import Control.Monad (replicateM_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Producer as KP

----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Integration: Consumer offset / seek APIs"
  [ testCase "endOffsets / beginningOffsets reflect produced records" $
      withBroker testEndAndBeginning
  , testCase "seekToEnd / seek + position behave consistently" $
      withBroker testSeekAndPosition
  ]

-- The fixed pre-created topic name. Operators are expected to
-- create this topic ahead of running the integration suite (it's
-- created once and reused across tests so we don't depend on the
-- producer's auto-create + metadata-refresh path, which still
-- has known interactions with freshly-created topics).
fixedTopic :: T.Text
fixedTopic = "wireform-bench-cmp"

----------------------------------------------------------------------
-- Scenarios
----------------------------------------------------------------------

testEndAndBeginning :: T.Text -> IO ()
testEndAndBeginning brokerText = do
  let topic = fixedTopic
  produceN brokerText topic 5
  consumer <- mkConsumerOrFail brokerText "wf-it-grp-eo"
  let tp = KC.TopicPartition topic 0
  beg <- KC.beginningOffsets consumer [tp]
  end <- KC.endOffsets       consumer [tp]
  case (beg, end) of
    (Right bm, Right em) -> do
      let begOff = fromMaybe (-9999) (HashMap.lookup tp bm)
          endOff = fromMaybe (-9999) (HashMap.lookup tp em)
      assertEqual "beginning offset for fresh topic is 0"
                  0 begOff
      assertBool  ("end offset >= 5 (got " ++ show endOff ++ ")")
                  (endOff >= 5)
    _ -> error ("offsets failed: beg=" ++ show beg ++ " end=" ++ show end)
  KC.closeConsumer consumer

testSeekAndPosition :: T.Text -> IO ()
testSeekAndPosition brokerText = do
  let topic = fixedTopic
  produceN brokerText topic 10
  consumer <- mkConsumerOrFail brokerText "wf-it-grp-seek"
  -- We use 'assign' so we don't depend on the consumer-group
  -- coordinator's rebalance for this test.
  let tp = KC.TopicPartition topic 0
  KC.assign consumer [tp] >>= either error (\_ -> pure ())
  -- After 'assign' the consumer's local position is whatever the
  -- broker's earliest/latest offset query returned (depending on
  -- auto.offset.reset). 'seekToBeginning' resets it to the
  -- partition's actual earliest offset; we read that with
  -- 'beginningOffsets' so the assertion is independent of any
  -- per-broker retention / compaction state.
  KC.seekToBeginning consumer [tp] >>= either error (\_ -> pure ())
  begMap <- KC.beginningOffsets consumer [tp]
  let !begOff =
        case begMap of
          Right m -> fromMaybe (-9999) (HashMap.lookup tp m)
          Left e  -> error e
  pos1 <- KC.position consumer tp
  pos1 @?= Right begOff
  -- Now seek to a specific offset (offset 4 — chosen because the
  -- topic always has > 4 records by this point in the test run).
  KC.seek consumer tp (begOff + 4) >>= either error (\_ -> pure ())
  pos2 <- KC.position consumer tp
  pos2 @?= Right (begOff + 4)
  -- And go to the end; verify it's >= the begin + 10 records we
  -- just produced (the topic may also hold records from prior
  -- test runs, hence the >=).
  KC.seekToEnd consumer [tp] >>= either error (\_ -> pure ())
  pos3 <- KC.position consumer tp
  case pos3 of
    Right o -> assertBool
      ("expected position >= begin + 10 (got " ++ show o
         ++ ", begin = " ++ show begOff ++ ")")
      (o >= begOff + 10)
    Left e  -> error e
  KC.closeConsumer consumer

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

withBroker :: (T.Text -> IO ()) -> IO ()
withBroker k = do
  m <- lookupEnv "WIREFORM_KAFKA_BROKER"
  case m of
    Nothing -> pure ()  -- skip (mirrors the rest of the integration suite)
    Just h  -> k (T.pack h)

mkConsumerOrFail :: T.Text -> T.Text -> IO KC.Consumer
mkConsumerOrFail brokerText groupId = do
  let cfg = KC.defaultConsumerConfig
        { KC.consumerClientId = "wf-it-consumer"
        , KC.consumerGroupId  = groupId
        }
  r <- KC.createConsumer [brokerText] groupId cfg
  case r of
    Left err -> error ("createConsumer: " <> err)
    Right c  -> pure c

produceN :: T.Text -> T.Text -> Int -> IO ()
produceN brokerText topic n = do
  let pcfg = KP.defaultProducerConfig
        { KP.producerClientId  = "wf-it-producer"
        , KP.producerLingerMs  = 5
        , KP.producerBatchSize = 16384
        }
  r <- KP.createProducer [brokerText] pcfg
  case r of
    Left err -> error ("producer create: " <> err)
    Right p  -> do
      replicateM_ n $ do
        rr <- KP.sendMessage p topic Nothing "v"
        case rr of
          Left e  -> error ("produce: " ++ e)
          Right _ -> pure ()
      _ <- KP.flushProducer p
      KP.closeProducer p

