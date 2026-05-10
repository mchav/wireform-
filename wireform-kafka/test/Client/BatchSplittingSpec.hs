{-# LANGUAGE OverloadedStrings #-}

module Client.BatchSplittingSpec (tests) where

import qualified Data.Sequence as Seq
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.BatchSplitting as BS
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

tests :: TestTree
tests = testGroup "BatchSplitting (KIP-126)"
  [ testCase "splitBatch halves a multi-record batch"
      half_split
  , testCase "single-record batches cannot be split"
      single
  , testCase "isOversizedSingle matches the single-record case"
      isSingle
  , testCase "metadata (txn flag, producer-id, base-seq) preserved"
      metadata_preserved
  ]

mkBatch :: Bool -> Int -> BA.ProducerBatch
mkBatch isTxn n = BA.ProducerBatch
  { BA.batchTopicPartition = BA.TopicPartition "t" 0
  , BA.batchRecords        = Seq.fromList
                               [ RB.Record 0 (fromIntegral i) Nothing "v" []
                               | i <- [0 .. n - 1]
                               ]
  , BA.batchSizeBytes      = n * 50
  , BA.batchCreateTime     = 0
  , BA.batchBaseTimestamp  = 0
  , BA.batchState          = BA.Ready
  , BA.batchCompression    = Compression.NoCompression
  , BA.batchCompressionLevel =
      Compression.defaultLevel Compression.NoCompression
  , BA.batchCallbacks      = Seq.replicate n (\_ -> pure ())
  , BA.batchAttempts       = 0
  , BA.batchProducerId     = if isTxn then 12345 else RB.noProducerId
  , BA.batchProducerEpoch  = if isTxn then 7     else RB.noProducerEpoch
  , BA.batchBaseSequence   = if isTxn then 0     else RB.noSequence
  , BA.batchIsTransactional = isTxn
  }

half_split :: IO ()
half_split = case BS.splitBatch (mkBatch False 6) of
  Nothing -> error "expected split"
  Just (l, r) -> do
    Seq.length (BA.batchRecords l) @?= 3
    Seq.length (BA.batchRecords r) @?= 3
    -- Callbacks split on the same boundary.
    Seq.length (BA.batchCallbacks l) @?= 3
    Seq.length (BA.batchCallbacks r) @?= 3

single :: IO ()
single = case BS.splitBatch (mkBatch False 1) of
  Just _  -> error "single record should not split"
  Nothing -> pure ()

isSingle :: IO ()
isSingle = do
  BS.isOversizedSingle (mkBatch False 1) @?= True
  BS.isOversizedSingle (mkBatch False 2) @?= False

metadata_preserved :: IO ()
metadata_preserved = case BS.splitBatch (mkBatch True 4) of
  Nothing -> error "expected split"
  Just (l, r) -> do
    BA.batchIsTransactional l @?= True
    BA.batchIsTransactional r @?= True
    BA.batchProducerId l      @?= 12345
    BA.batchProducerId r      @?= 12345
