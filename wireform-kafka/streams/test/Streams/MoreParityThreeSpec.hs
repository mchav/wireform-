{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the latest parity batch:
-- KStream.print / values, AutoOffsetReset, StreamPartitioner,
-- BufferConfig, ProcessorContext.commit().
module Streams.MoreParityThreeSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "ParityRoundThree"
  [ values_stream_drops_keys
  , print_stream_writes_to_handle
  , consumed_default_is_earliest
  , consumed_with_offset_reset_policy
  , buffer_config_helpers
  , partitioner_default_returns_nothing
  ]

values_stream_drops_keys :: TestTree
values_stream_drops_keys =
  testCase "valuesStream emits records with a () key" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    valued <- valuesStream src
    seen <- newIORef ([] :: [Text])
    let bld = kstreamBuilder valued
        proc_ = pure Processor
          { procName    = processorName "OBS"
          , procInit    = \_ -> pure ()
          , procClose   = pure ()
          , procProcess = \r -> modifyIORef' seen (recordValue r :)
          }
    nm <- freshNodeName bld "OBS"
    withTopology_ bld $ Kafka.Streams.Imperative.addProcessor nm [kstreamParent valued] proc_
    topo <- buildTopology bld
    driver <- newDriver topo "v-app"
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "b") (t 1) 0
    closeDriver driver
    reverse <$> readIORef seen >>= (@?= ["a", "b"])

print_stream_writes_to_handle :: TestTree
print_stream_writes_to_handle =
  testCase "printToHandle invokes the supplied putLine" $ do
    log_ <- newIORef ([] :: [String])
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    printToHandle "[debug]" (\s -> modifyIORef' log_ (s :)) src
    topo <- buildTopology b
    driver <- newDriver topo "p-app"
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v") (t 0) 0
    closeDriver driver
    lines_ <- reverse <$> readIORef log_
    length lines_ @?= 1
    let l = head lines_
    assertBool ("missing prefix in " <> l) ("[debug]" `T.isInfixOf` T.pack l)

consumed_default_is_earliest :: TestTree
consumed_default_is_earliest =
  testCase "consumed defaults to OffsetEarliest" $ do
    let c = consumed textSerde textSerde
    consumedOffsetReset c @?= OffsetEarliest

consumed_with_offset_reset_policy :: TestTree
consumed_with_offset_reset_policy =
  testCase "withOffsetResetPolicy sets the reset policy" $ do
    let c = withOffsetResetPolicy OffsetLatest (consumed textSerde textSerde)
    consumedOffsetReset c @?= OffsetLatest

buffer_config_helpers :: TestTree
buffer_config_helpers =
  testCase "BufferConfig helpers set the right limit" $ do
    unboundedBufferConfig.maxBytes    @?= Nothing
    unboundedBufferConfig.maxRecords  @?= Nothing
    (maxBytesBufferConfig 1024).maxBytes @?= Just 1024
    (maxRecordsBufferConfig 100).maxRecords @?= Just 100

partitioner_default_returns_nothing :: TestTree
partitioner_default_returns_nothing =
  testCase "defaultStreamPartitioner returns Nothing (delegate to producer)" $ do
    r <- runStreamPartitioner defaultStreamPartitioner "tp" (Just "k") "v" 8
    r @?= Nothing
