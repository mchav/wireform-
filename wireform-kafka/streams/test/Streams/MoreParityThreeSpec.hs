{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the latest parity batch:
KStream.print / values, AutoOffsetReset, StreamPartitioner,
BufferConfig, ProcessorContext.commit().
-}
module Streams.MoreParityThreeSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


tests :: Spec
tests =
  describe "ParityRoundThree" $
    sequence_
      [ values_stream_drops_keys
      , print_stream_writes_to_handle
      , consumed_default_is_earliest
      , consumed_with_offset_reset_policy
      , buffer_config_helpers
      , partitioner_default_returns_nothing
      ]


values_stream_drops_keys :: Spec
values_stream_drops_keys =
  it "valuesStream emits records with a () key" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    valued <- valuesStream src
    seen <- newIORef ([] :: [Text])
    let bld = kstreamBuilder valued
        proc_ =
          pure
            Processor
              { procName = processorName "OBS"
              , procInit = \_ -> pure ()
              , procClose = pure ()
              , procProcess = \r -> modifyIORef' seen (recordValue r :)
              }
    nm <- freshNodeName bld "OBS"
    withTopology_ bld $ Kafka.Streams.Imperative.addProcessor nm [kstreamParent valued] proc_
    topo <- buildTopology bld
    driver <- newDriver topo "v-app"
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "b") (t 1) 0
    closeDriver driver
    reverse <$> readIORef seen >>= (`shouldBe` ["a", "b"])


print_stream_writes_to_handle :: Spec
print_stream_writes_to_handle =
  it "printToHandle invokes the supplied putLine" $ do
    log_ <- newIORef ([] :: [String])
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    printToHandle "[debug]" (\s -> modifyIORef' log_ (s :)) src
    topo <- buildTopology b
    driver <- newDriver topo "p-app"
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v") (t 0) 0
    closeDriver driver
    lines_ <- reverse <$> readIORef log_
    length lines_ `shouldBe` 1
    let l = head lines_
    (if ("[debug]" `T.isInfixOf` T.pack l) then pure () else expectationFailure ("missing prefix in " <> l))


consumed_default_is_earliest :: Spec
consumed_default_is_earliest =
  it "consumed defaults to OffsetEarliest" $ do
    let c = consumed textSerde textSerde
    consumedOffsetReset c `shouldBe` OffsetEarliest


consumed_with_offset_reset_policy :: Spec
consumed_with_offset_reset_policy =
  it "withOffsetResetPolicy sets the reset policy" $ do
    let c = withOffsetResetPolicy OffsetLatest (consumed textSerde textSerde)
    consumedOffsetReset c `shouldBe` OffsetLatest


buffer_config_helpers :: Spec
buffer_config_helpers =
  it "BufferConfig helpers set the right limit" $ do
    unboundedBufferConfig.maxBytes `shouldBe` Nothing
    unboundedBufferConfig.maxRecords `shouldBe` Nothing
    (maxBytesBufferConfig 1024).maxBytes `shouldBe` Just 1024
    (maxRecordsBufferConfig 100).maxRecords `shouldBe` Just 100


partitioner_default_returns_nothing :: Spec
partitioner_default_returns_nothing =
  it "defaultStreamPartitioner returns Nothing (delegate to producer)" $ do
    r <- runStreamPartitioner defaultStreamPartitioner "tp" (Just "k") "v" 8
    r `shouldBe` Nothing
