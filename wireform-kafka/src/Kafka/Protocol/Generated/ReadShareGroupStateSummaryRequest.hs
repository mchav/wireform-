{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateSummaryRequest
Description : Kafka ReadShareGroupStateSummaryRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 87.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateSummaryRequest
  (
    ReadShareGroupStateSummaryRequest(..),
    ReadStateSummaryData(..),
    PartitionData(..),
    encodeReadShareGroupStateSummaryRequest,
    decodeReadShareGroupStateSummaryRequest,
    maxReadShareGroupStateSummaryRequestVersion
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


-- | The data for the partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartition :: !(Int32)
,

  -- | The leader epoch of the share-partition.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartition pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartition <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartition = fieldpartition
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      }


-- | The data for the topics.
data ReadStateSummaryData = ReadStateSummaryData
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateSummaryDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  readStateSummaryDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateSummaryData with version-aware field handling.
encodeReadStateSummaryData :: MonadPut m => E.ApiVersion -> ReadStateSummaryData -> m ()
encodeReadStateSummaryData version rmsg =
  do
    serialize (readStateSummaryDataTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (readStateSummaryDataPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateSummaryData with version-aware field handling.
decodeReadStateSummaryData :: MonadGet m => E.ApiVersion -> m ReadStateSummaryData
decodeReadStateSummaryData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateSummaryData
      {
      readStateSummaryDataTopicId = fieldtopicid
      ,
      readStateSummaryDataPartitions = fieldpartitions
      }



data ReadShareGroupStateSummaryRequest = ReadShareGroupStateSummaryRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  readShareGroupStateSummaryRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  readShareGroupStateSummaryRequestTopics :: !(KafkaArray (ReadStateSummaryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateSummaryRequest.
maxReadShareGroupStateSummaryRequestVersion :: Int16
maxReadShareGroupStateSummaryRequestVersion = 0

-- | Encode ReadShareGroupStateSummaryRequest with the given API version.
encodeReadShareGroupStateSummaryRequest :: MonadPut m => E.ApiVersion -> ReadShareGroupStateSummaryRequest -> m ()
encodeReadShareGroupStateSummaryRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (readShareGroupStateSummaryRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeReadStateSummaryData (case P.unKafkaArray (readShareGroupStateSummaryRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateSummaryRequest with the given API version.
decodeReadShareGroupStateSummaryRequest :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateSummaryRequest
decodeReadShareGroupStateSummaryRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateSummaryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateSummaryRequest
        {
        readShareGroupStateSummaryRequestGroupId = fieldgroupid
        ,
        readShareGroupStateSummaryRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version