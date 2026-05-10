{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.Internal.BatchSplitting
Description : KIP-126 — split oversized producer batches in half on @MESSAGE_TOO_LARGE@

KIP-126 lets the producer recover from a @MESSAGE_TOO_LARGE@
broker error /without/ failing the user's records: it splits
the offending batch into two smaller ones and retries them
separately. If a single record still doesn't fit, /that/
record's callback fails with @RECORD_TOO_LARGE@; everything
else proceeds.

This module is the pure helper. The producer-side hook into
'Kafka.Client.Internal.ProducerSender.processProduceResponse'
calls 'splitBatch' when it sees error code 10
(@MESSAGE_TOO_LARGE@) and re-enqueues the resulting halves.
-}
module Kafka.Client.Internal.BatchSplitting
  ( splitBatch
  , canSplitBatch
  , isOversizedSingle
  ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Foldable as F

import qualified Kafka.Client.Internal.BatchAccumulator as BA

-- | Split a batch into two halves with the same metadata
-- (compression, timestamps, producer-id stamp, …) but disjoint
-- record sequences. Records keep their original order so the
-- per-(topic, partition) sequence numbers stay monotone.
--
-- Returns 'Nothing' when the batch has fewer than 2 records —
-- in that case the only recourse is to fail the single record's
-- callback (see 'isOversizedSingle').
splitBatch :: BA.ProducerBatch -> Maybe (BA.ProducerBatch, BA.ProducerBatch)
splitBatch b
  | not (canSplitBatch b) = Nothing
  | otherwise =
      let !records = BA.batchRecords b
          !n       = Seq.length records
          !mid     = n `div` 2
          (left, right) = Seq.splitAt mid records
          !leftBatch  = b { BA.batchRecords = left
                          , BA.batchSizeBytes = approxSize left
                          , BA.batchCallbacks =
                              Seq.take (Seq.length left) (BA.batchCallbacks b)
                          }
          !rightBatch = b { BA.batchRecords = right
                          , BA.batchSizeBytes = approxSize right
                          , BA.batchCallbacks =
                              Seq.drop (Seq.length left) (BA.batchCallbacks b)
                          }
      in Just (leftBatch, rightBatch)
  where
    approxSize sq = sum (map approxRecord (F.toList sq))
    approxRecord _ = 50  -- rough; real size only matters for accumulator
                         -- accounting, not for the split correctness.

canSplitBatch :: BA.ProducerBatch -> Bool
canSplitBatch b = Seq.length (BA.batchRecords b) >= 2

-- | A batch with exactly one record that the broker refused as
-- @MESSAGE_TOO_LARGE@. The producer should fail that record's
-- callback with @RECORD_TOO_LARGE@; splitting can't help.
isOversizedSingle :: BA.ProducerBatch -> Bool
isOversizedSingle b = Seq.length (BA.batchRecords b) == 1
