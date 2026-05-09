{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.DeleteRecords
Description : KIP-107 — AdminClient.deleteRecords helper

KIP-107 added @AdminClient.deleteRecords(Map<TopicPartition,
RecordsToDelete>)@ for trimming a partition's log up to a given
offset. The wire request (@DeleteRecordsRequest@) is already
generated; this module is the high-level helper that mirrors
the JVM client's signature.
-}
module Kafka.Client.DeleteRecords
  ( DeleteRecordsRequest (..)
  , DeleteRecordsResult (..)
  , buildDeleteRecordsRequest
  , partitionLowWatermark
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Int (Int16, Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Kafka.Client.Consumer as KC

-- | One element of @Map<TopicPartition, RecordsToDelete>@.
-- @drBeforeOffset@ deletes every record /strictly/ before the
-- given offset (records at or above survive).
data DeleteRecordsRequest = DeleteRecordsRequest
  { drBeforeOffset :: !Int64
  }
  deriving stock (Eq, Show, Generic)

-- | One element of the result map: the new low-watermark for
-- the partition.
data DeleteRecordsResult = DeleteRecordsResult
  { drrLowWatermark :: !Int64
  , drrErrorCode    :: !Int16
  }
  deriving stock (Eq, Show, Generic)

-- | Build the broker-side payload (a list of (topic, [(partition,
-- offset)])) from the user's input map.
--
-- The previous implementation pretended that the per-topic
-- accumulator was a multimap by repeatedly walking an
-- association list with @lookup@ + @++ [(part, off)]@; that's
-- O(n^2) in the number of (topic, partition) entries. Rewrite as
-- a single 'HashMap.foldlWithKey'' that builds a 'HashMap' /
-- multimap and then a final pass to materialise the result list.
buildDeleteRecordsRequest
  :: HashMap KC.TopicPartition DeleteRecordsRequest
  -> [(String, [(Int, Int64)])]
buildDeleteRecordsRequest =
  -- Use 'HashMap' (not list) for the per-topic grouping. We keep
  -- the per-topic value as a snoc-list reversed at the end so
  -- partition order matches the input iteration order.
  finalise . HashMap.foldlWithKey' step HashMap.empty
  where
    step :: HashMap String [(Int, Int64)]
         -> KC.TopicPartition
         -> DeleteRecordsRequest
         -> HashMap String [(Int, Int64)]
    step !acc tp dr =
      let !topic = T.unpack (KC.tpTopic tp)
          !part  = fromIntegral (KC.tpPartition tp)
          !off   = drBeforeOffset dr
      in HashMap.insertWith (\_new old -> (part, off) : old)
                            topic
                            [(part, off)]
                            acc

    finalise :: HashMap String [(Int, Int64)] -> [(String, [(Int, Int64)])]
    finalise =
      map (\(t, ps) -> (t, reverse ps)) . HashMap.toList

-- | Pure helper: given a partition's current high-watermark and
-- a delete-records request, return the new low-watermark the
-- broker would accept.
partitionLowWatermark :: Int64 -> DeleteRecordsRequest -> Int64
partitionLowWatermark hwm req =
  -- A request beyond HWM is clamped to HWM (the broker won't
  -- delete records that don't exist yet).
  min (drBeforeOffset req) hwm
