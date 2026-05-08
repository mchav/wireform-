{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0086.PurgeLocal
Description : librdkafka @tests\/0086-purge_local.c@

librdkafka's @0086-purge_local@ enqueues messages into the producer
without a broker, calls @rd_kafka_purge@, and asserts that the
producer's queue is drained without leaking memory. Our analogue
is the in-memory batch accumulator that 'Kafka.Client.Producer'
uses internally — we exercise the queue's drain behaviour.
-}
module Conformance.T0086.PurgeLocal (tests) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

tests :: TestTree
tests = testGroup "0086-purge_local"
  [ testCase "fresh accumulator has no ready batches" $ do
      acc <- BA.createBatchAccumulator
              16384  -- 16 KB batch size
              5      -- 5 ms linger
              Compression.NoCompression
              0      -- compression level
      ready <- BA.hasReadyBatches acc
      ready @?= False

  , testCase "appendRecord succeeds on an open accumulator" $ do
      acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
      let tp = BA.TopicPartition "purge-test" 0
          payload = BS.replicate 64 0x41
          rec = RB.Record
                  { RB.recordTimestampDelta = 0
                  , RB.recordOffsetDelta    = 0
                  , RB.recordKey            = Nothing
                  , RB.recordValue          = payload
                  , RB.recordHeaders        = []
                  }
      ok <- BA.appendRecord acc tp rec
      ok @?= True

  , testCase "appendRecord fails on a closed accumulator (purge equivalent)" $ do
      acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
      BA.closeBatchAccumulator acc
      let tp = BA.TopicPartition "purge-test" 0
          rec = RB.Record 0 0 Nothing (BS.replicate 32 0x42) []
      ok <- BA.appendRecord acc tp rec
      ok @?= False

  , testCase "drainReadyBatches after close returns the buffered batch" $ do
      acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
      let tp = BA.TopicPartition "purge-test" 0
          rec = RB.Record 0 0 Nothing (BS.replicate 64 0x43) []
      _ <- BA.appendRecord acc tp rec
      BA.closeBatchAccumulator acc
      drained <- BA.drainReadyBatches acc
      length drained @?= 1
  ]
