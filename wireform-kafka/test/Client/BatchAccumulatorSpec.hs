{-# LANGUAGE OverloadedStrings #-}

module Client.BatchAccumulatorSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM, replicateM_, when)
import qualified Data.ByteString as BS
import Data.Int
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

-- | Generate a simple record for testing
genRecord :: Gen RB.Record
genRecord = do
  timestampDelta <- Gen.int64 (Range.linear 0 1000)
  offsetDelta <- Gen.int32 (Range.linear 0 100)
  key <- Gen.maybe (Gen.bytes (Range.linear 0 100))
  value <- Gen.bytes (Range.linear 0 1000)
  return $ RB.Record timestampDelta offsetDelta key value []

-- | Generate a topic-partition
genTopicPartition :: Gen BA.TopicPartition
genTopicPartition = do
  topic <- Gen.text (Range.linear 1 10) Gen.alphaNum
  partition <- Gen.int32 (Range.linear 0 10)
  return $ BA.TopicPartition topic partition

-- | Test that records can be appended to the accumulator
prop_canAppendRecords :: Property
prop_canAppendRecords = property $ do
  record <- forAll genRecord
  tp <- forAll genTopicPartition
  
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  -- Append a record
  success <- evalIO $ BA.appendRecord accumulator tp record
  assert success
  
  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator

-- | Test that batches become ready when they reach size limit
prop_batchReadyWhenFull :: Property
prop_batchReadyWhenFull = property $ do
  -- Use a small batch size to make it easy to fill
  let batchSize = 100
  accumulator <- evalIO $ BA.createBatchAccumulator batchSize 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  
  -- Create records that will fill the batch
  let largeValue = BS.replicate 50 42  -- 50 bytes
      largeRecord = RB.Record 0 0 Nothing largeValue []
  
  -- Append enough records to fill a batch (with overhead, 3 records should do it)
  evalIO $ replicateM_ 3 $ BA.appendRecord accumulator tp largeRecord
  
  -- Check that we have ready batches
  hasReady <- evalIO $ BA.hasReadyBatches accumulator
  assert hasReady
  
  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator
  annotate $ "Got " ++ show (length batches) ++ " batches"
  assert $ not (null batches)
  
  evalIO $ BA.closeBatchAccumulator accumulator

-- | Test that batches become ready after linger time
prop_batchReadyAfterLingerTime :: Property
prop_batchReadyAfterLingerTime = property $ do
  -- Use a short linger time for testing
  let lingerMs = 50
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 lingerMs Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  record <- forAll genRecord
  
  -- Append a single record
  evalIO $ BA.appendRecord accumulator tp record
  
  -- Initially should not be ready
  hasReadyBefore <- evalIO $ BA.hasReadyBatches accumulator
  assert $ not hasReadyBefore
  
  -- Wait for linger time to expire (plus buffer)
  evalIO $ threadDelay ((lingerMs + 20) * 1000)
  
  -- Now should be ready
  hasReadyAfter <- evalIO $ BA.hasReadyBatches accumulator
  assert hasReadyAfter
  
  -- Can drain the batch
  batches <- evalIO $ BA.drainReadyBatches accumulator
  assert $ not (null batches)
  
  evalIO $ BA.closeBatchAccumulator accumulator

-- | Test that records are accumulated per partition
prop_perPartitionAccumulation :: Property
prop_perPartitionAccumulation = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  -- Create two different partitions
  tp1 <- forAll genTopicPartition
  tp2 <- forAll genTopicPartition
  
  -- Ensure they're different
  when (tp1 == tp2) discard
  
  record <- forAll genRecord
  
  -- Append records to both partitions
  evalIO $ BA.appendRecord accumulator tp1 record
  evalIO $ BA.appendRecord accumulator tp2 record
  
  -- Close accumulator to mark all batches as ready
  evalIO $ BA.closeBatchAccumulator accumulator
  
  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator
  
  -- Should have at least 2 batches (one per partition)
  annotate $ "Got " ++ show (length batches) ++ " batches"
  assert $ length batches >= 2

-- | Test concurrent appends from multiple threads
prop_concurrentAppends :: Property
prop_concurrentAppends = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  
  -- Create multiple threads appending concurrently
  let numThreads = 10
      recordsPerThread = 10
  
  evalIO $ do
    threads <- replicateM numThreads $ async $ do
      replicateM_ recordsPerThread $ do
        let record = RB.Record 0 0 Nothing "test" []
        BA.appendRecord accumulator tp record
    
    -- Wait for all threads to complete
    mapM_ wait threads
    
    -- Close and drain
    BA.closeBatchAccumulator accumulator
    batches <- BA.drainReadyBatches accumulator
    
    -- Count total records across all batches
    let totalRecords = sum $ map (length . BA.batchRecords) batches
    
    -- Should have all records
    return $ totalRecords == numThreads * recordsPerThread
  
  success

-- | Test that closing the accumulator marks all batches as ready
prop_closeMarksAllReady :: Property
prop_closeMarksAllReady = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  record <- forAll genRecord
  
  -- Append a record
  evalIO $ BA.appendRecord accumulator tp record
  
  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator
  
  -- Should have ready batches
  hasReady <- evalIO $ BA.hasReadyBatches accumulator
  assert hasReady
  
  -- Can drain them
  batches <- evalIO $ BA.drainReadyBatches accumulator
  assert $ not (null batches)

-- | Test that appending after close returns False
prop_appendAfterCloseReturnsFalse :: Property
prop_appendAfterCloseReturnsFalse = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  record <- forAll genRecord
  
  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator
  
  -- Try to append after close
  success <- evalIO $ BA.appendRecord accumulator tp record
  assert $ not success

-- | Test batch state transitions
prop_batchStateTransitions :: Property
prop_batchStateTransitions = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 100 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  
  tp <- forAll genTopicPartition
  
  -- Create large records to fill batch quickly
  let largeRecord = RB.Record 0 0 Nothing (BS.replicate 50 42) []
  
  -- Append records to fill batch
  evalIO $ replicateM_ 3 $ BA.appendRecord accumulator tp largeRecord
  
  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator
  
  -- All drained batches should be in Ready state
  assert $ all (\b -> BA.batchState b == BA.Ready) batches
  
  evalIO $ BA.closeBatchAccumulator accumulator

-- | All tests for BatchAccumulator
tests :: TestTree
tests = testGroup "BatchAccumulator"
  [ testProperty "Can append records" prop_canAppendRecords
  , testProperty "Batch ready when full" prop_batchReadyWhenFull
  , testProperty "Batch ready after linger time" prop_batchReadyAfterLingerTime
  , testProperty "Per-partition accumulation" prop_perPartitionAccumulation
  , testProperty "Concurrent appends" prop_concurrentAppends
  , testProperty "Close marks all ready" prop_closeMarksAllReady
  , testProperty "Append after close returns false" prop_appendAfterCloseReturnsFalse
  , testProperty "Batch state transitions" prop_batchStateTransitions
  ]

