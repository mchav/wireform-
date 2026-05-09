{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
Description : Kafka ReadShareGroupStateSummaryResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 87.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateSummaryResponse
  (
    ReadShareGroupStateSummaryResponse(..),
    ReadStateSummaryResult(..),
    PartitionResult(..),
    encodeReadShareGroupStateSummaryResponse,
    decodeReadShareGroupStateSummaryResponse,
    maxReadShareGroupStateSummaryResponseVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E


-- | The results for the partitions.
data PartitionResult = PartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResultPartition :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)
,

  -- | The state epoch of the share-partition.

  -- Versions: 0+
  partitionResultStateEpoch :: !(Int32)
,

  -- | The leader epoch of the share-partition.

  -- Versions: 0+
  partitionResultLeaderEpoch :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  partitionResultStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartition pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    serialize (partitionResultStateEpoch pmsg)
    serialize (partitionResultLeaderEpoch pmsg)
    serialize (partitionResultStartOffset pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartition <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldstateepoch <- deserialize
    fieldleaderepoch <- deserialize
    fieldstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartition = fieldpartition
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      ,
      partitionResultStateEpoch = fieldstateepoch
      ,
      partitionResultLeaderEpoch = fieldleaderepoch
      ,
      partitionResultStartOffset = fieldstartoffset
      }


-- | The read results.
data ReadStateSummaryResult = ReadStateSummaryResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateSummaryResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  readStateSummaryResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateSummaryResult with version-aware field handling.
encodeReadStateSummaryResult :: MonadPut m => E.ApiVersion -> ReadStateSummaryResult -> m ()
encodeReadStateSummaryResult version rmsg =
  do
    serialize (readStateSummaryResultTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (readStateSummaryResultPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateSummaryResult with version-aware field handling.
decodeReadStateSummaryResult :: MonadGet m => E.ApiVersion -> m ReadStateSummaryResult
decodeReadStateSummaryResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateSummaryResult
      {
      readStateSummaryResultTopicId = fieldtopicid
      ,
      readStateSummaryResultPartitions = fieldpartitions
      }



data ReadShareGroupStateSummaryResponse = ReadShareGroupStateSummaryResponse
  {

  -- | The read results.

  -- Versions: 0+
  readShareGroupStateSummaryResponseResults :: !(KafkaArray (ReadStateSummaryResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateSummaryResponse.
maxReadShareGroupStateSummaryResponseVersion :: Int16
maxReadShareGroupStateSummaryResponseVersion = 0

-- | Encode ReadShareGroupStateSummaryResponse with the given API version.
encodeReadShareGroupStateSummaryResponse :: MonadPut m => E.ApiVersion -> ReadShareGroupStateSummaryResponse -> m ()
encodeReadShareGroupStateSummaryResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeReadStateSummaryResult (case P.unKafkaArray (readShareGroupStateSummaryResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateSummaryResponse with the given API version.
decodeReadShareGroupStateSummaryResponse :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateSummaryResponse
decodeReadShareGroupStateSummaryResponse version
  | version == 0 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateSummaryResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateSummaryResponse
        {
        readShareGroupStateSummaryResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version