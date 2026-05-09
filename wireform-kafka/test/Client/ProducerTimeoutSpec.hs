{-# LANGUAGE OverloadedStrings #-}

module Client.ProducerTimeoutSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Int
import qualified Data.Sequence as Seq
import qualified Data.Time.Clock.POSIX as Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit hiding (assert)

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Client.Producer as Producer
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

-- | Test that delivery timeout configuration is properly set
prop_deliveryTimeoutConfigured :: Property
prop_deliveryTimeoutConfigured = property $ do
  let config = Producer.defaultProducerConfig
        { Producer.producerDeliveryTimeoutMs = 60000
        }
  annotate $ "Delivery timeout: " ++ show (Producer.producerDeliveryTimeoutMs config)
  assert $ Producer.producerDeliveryTimeoutMs config == 60000

-- | Test that default delivery timeout is 120000ms (2 minutes)
unit_defaultDeliveryTimeout :: TestTree
unit_defaultDeliveryTimeout = testCase "Default delivery timeout is 120000ms" $ do
  let config = Producer.defaultProducerConfig
  Producer.producerDeliveryTimeoutMs config @?= 120000

-- | Test that batch timeout detection works correctly
unit_batchTimeoutDetection :: TestTree
unit_batchTimeoutDetection = testCase "Batch timeout detection" $ do
  -- Create a test batch with a specific creation time
  let currentTime = 10000  -- 10 seconds in ms
      batchCreateTime = 5000  -- Created at 5 seconds
      deliveryTimeout = 3000  -- 3 second timeout
      
  -- Create a mock batch
  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing "value" []
      batch = BA.ProducerBatch
        { BA.batchTopicPartition = tp
        , BA.batchRecords = Seq.singleton record
        , BA.batchSizeBytes = 100
        , BA.batchCreateTime = batchCreateTime
        , BA.batchBaseTimestamp = batchCreateTime
        , BA.batchState = BA.Ready
        , BA.batchCompression = Compression.NoCompression
        , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
        , BA.batchCallbacks = Seq.empty
        , BA.batchAttempts = 0
        , BA.batchProducerId = RB.noProducerId
        , BA.batchProducerEpoch = RB.noProducerEpoch
        , BA.batchBaseSequence = RB.noSequence
        , BA.batchIsTransactional = False
        }
  
  -- Test that the batch is detected as timed out
  let timedOut = Sender.isBatchTimedOut currentTime deliveryTimeout batch
      elapsed = currentTime - batchCreateTime
  
  assertBool ("Batch should be timed out (elapsed: " ++ show elapsed ++ "ms, timeout: " ++ 
              show deliveryTimeout ++ "ms)") timedOut

-- | Test that batch timeout detection doesn't false-positive
unit_batchNotTimedOut :: TestTree
unit_batchNotTimedOut = testCase "Batch not timed out when within timeout" $ do
  -- Create a test batch with a recent creation time
  let currentTime = 10000  -- 10 seconds in ms
      batchCreateTime = 9000  -- Created at 9 seconds (1 second ago)
      deliveryTimeout = 3000  -- 3 second timeout
      
  -- Create a mock batch
  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing "value" []
      batch = BA.ProducerBatch
        { BA.batchTopicPartition = tp
        , BA.batchRecords = Seq.singleton record
        , BA.batchSizeBytes = 100
        , BA.batchCreateTime = batchCreateTime
        , BA.batchBaseTimestamp = batchCreateTime
        , BA.batchState = BA.Ready
        , BA.batchCompression = Compression.NoCompression
        , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
        , BA.batchCallbacks = Seq.empty
        , BA.batchAttempts = 0
        , BA.batchProducerId = RB.noProducerId
        , BA.batchProducerEpoch = RB.noProducerEpoch
        , BA.batchBaseSequence = RB.noSequence
        , BA.batchIsTransactional = False
        }
  
  -- Test that the batch is NOT detected as timed out
  let timedOut = Sender.isBatchTimedOut currentTime deliveryTimeout batch
      elapsed = currentTime - batchCreateTime
  
  assertBool ("Batch should NOT be timed out (elapsed: " ++ show elapsed ++ "ms, timeout: " ++ 
              show deliveryTimeout ++ "ms)") (not timedOut)

-- | Test property: a batch is timed out iff elapsed time > timeout
prop_timeoutDetectionCorrectness :: Property
prop_timeoutDetectionCorrectness = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 10000 1000000)
  batchCreateTime <- forAll $ Gen.int64 (Range.linear 0 (currentTime - 1))
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 100 10000)
  
  let elapsed = currentTime - batchCreateTime
      shouldBeTimedOut = elapsed > fromIntegral deliveryTimeoutMs
      
      tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing "value" []
      batch = BA.ProducerBatch
        { BA.batchTopicPartition = tp
        , BA.batchRecords = Seq.singleton record
        , BA.batchSizeBytes = 100
        , BA.batchCreateTime = batchCreateTime
        , BA.batchBaseTimestamp = batchCreateTime
        , BA.batchState = BA.Ready
        , BA.batchCompression = Compression.NoCompression
        , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
        , BA.batchCallbacks = Seq.empty
        , BA.batchAttempts = 0
        , BA.batchProducerId = RB.noProducerId
        , BA.batchProducerEpoch = RB.noProducerEpoch
        , BA.batchBaseSequence = RB.noSequence
        , BA.batchIsTransactional = False
        }
      
      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch
  
  annotate $ "Current time: " ++ show currentTime
  annotate $ "Batch create time: " ++ show batchCreateTime
  annotate $ "Elapsed: " ++ show elapsed ++ "ms"
  annotate $ "Timeout: " ++ show deliveryTimeoutMs ++ "ms"
  annotate $ "Should be timed out: " ++ show shouldBeTimedOut
  annotate $ "Is timed out: " ++ show isTimedOut
  
  isTimedOut === shouldBeTimedOut

-- | Test that very old batches are always timed out
prop_veryOldBatchesTimeout :: Property
prop_veryOldBatchesTimeout = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 200000 1000000)
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 1000 10000)
  
  -- Create a batch that's much older than the timeout
  let batchCreateTime = currentTime - (fromIntegral deliveryTimeoutMs * 10)
      
      tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing "value" []
      batch = BA.ProducerBatch
        { BA.batchTopicPartition = tp
        , BA.batchRecords = Seq.singleton record
        , BA.batchSizeBytes = 100
        , BA.batchCreateTime = batchCreateTime
        , BA.batchBaseTimestamp = batchCreateTime
        , BA.batchState = BA.Ready
        , BA.batchCompression = Compression.NoCompression
        , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
        , BA.batchCallbacks = Seq.empty
        , BA.batchAttempts = 0
        , BA.batchProducerId = RB.noProducerId
        , BA.batchProducerEpoch = RB.noProducerEpoch
        , BA.batchBaseSequence = RB.noSequence
        , BA.batchIsTransactional = False
        }
      
      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch
  
  annotate $ "Batch is 10x older than timeout"
  assert isTimedOut

-- | Test that fresh batches are never timed out
prop_freshBatchesNeverTimeout :: Property
prop_freshBatchesNeverTimeout = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 100000 1000000)
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 5000 30000)
  
  -- Create a batch that was just created (within 10% of timeout)
  let recentDelta = fromIntegral deliveryTimeoutMs `div` 10
  batchCreateTime <- forAll $ Gen.int64 (Range.linear (currentTime - recentDelta) currentTime)
  
  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing "value" []
      batch = BA.ProducerBatch
        { BA.batchTopicPartition = tp
        , BA.batchRecords = Seq.singleton record
        , BA.batchSizeBytes = 100
        , BA.batchCreateTime = batchCreateTime
        , BA.batchBaseTimestamp = batchCreateTime
        , BA.batchState = BA.Ready
        , BA.batchCompression = Compression.NoCompression
        , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
        , BA.batchCallbacks = Seq.empty
        , BA.batchAttempts = 0
        , BA.batchProducerId = RB.noProducerId
        , BA.batchProducerEpoch = RB.noProducerEpoch
        , BA.batchBaseSequence = RB.noSequence
        , BA.batchIsTransactional = False
        }
      
      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch
      elapsed = currentTime - batchCreateTime
  
  annotate $ "Elapsed: " ++ show elapsed ++ "ms (should be < 10% of timeout)"
  assert $ not isTimedOut

-- | Test different timeout values
prop_differentTimeoutValues :: Property
prop_differentTimeoutValues = property $ do
  timeoutMs <- forAll $ Gen.int32 (Range.linear 100 300000)
  
  let config = Producer.defaultProducerConfig
        { Producer.producerDeliveryTimeoutMs = fromIntegral timeoutMs
        }
  
  Producer.producerDeliveryTimeoutMs config === fromIntegral timeoutMs

tests :: TestTree
tests = testGroup "Producer Timeout (KIP-91)"
  [ testGroup "Properties"
      [ testProperty "Delivery timeout is configured" prop_deliveryTimeoutConfigured
      , testProperty "Timeout detection correctness" prop_timeoutDetectionCorrectness
      , testProperty "Very old batches timeout" prop_veryOldBatchesTimeout
      , testProperty "Fresh batches never timeout" prop_freshBatchesNeverTimeout
      , testProperty "Different timeout values" prop_differentTimeoutValues
      ]
  , testGroup "Unit Tests"
      [ unit_defaultDeliveryTimeout
      , unit_batchTimeoutDetection
      , unit_batchNotTimedOut
      ]
  ]

