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

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int16, Int64)
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
buildDeleteRecordsRequest
  :: Map KC.TopicPartition DeleteRecordsRequest
  -> [(String, [(Int, Int64)])]
buildDeleteRecordsRequest =
  Map.foldlWithKey' step []
  where
    step acc tp dr =
      let !topic = show (KC.tpTopic tp)
          !part  = fromIntegral (KC.tpPartition tp)
          !off   = drBeforeOffset dr
      in case lookup topic acc of
        Just ps -> map (\(t, p) -> if t == topic
                                     then (t, p ++ [(part, off)])
                                     else (t, p)) acc
        Nothing -> acc ++ [(topic, [(part, off)])]

-- | Pure helper: given a partition's current high-watermark and
-- a delete-records request, return the new low-watermark the
-- broker would accept.
partitionLowWatermark :: Int64 -> DeleteRecordsRequest -> Int64
partitionLowWatermark hwm req =
  -- A request beyond HWM is clamped to HWM (the broker won't
  -- delete records that don't exist yet).
  min (drBeforeOffset req) hwm
