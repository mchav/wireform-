{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateRequest
Description : Kafka ReadShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 84.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateRequest
  (
    ReadShareGroupStateRequest(..),
    ReadStateData(..),
    PartitionData(..),
    encodeReadShareGroupStateRequest,
    decodeReadShareGroupStateRequest,
    maxReadShareGroupStateRequestVersion
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
data ReadStateData = ReadStateData
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  readStateDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateData with version-aware field handling.
encodeReadStateData :: MonadPut m => E.ApiVersion -> ReadStateData -> m ()
encodeReadStateData version rmsg =
  do
    serialize (readStateDataTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (readStateDataPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateData with version-aware field handling.
decodeReadStateData :: MonadGet m => E.ApiVersion -> m ReadStateData
decodeReadStateData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateData
      {
      readStateDataTopicId = fieldtopicid
      ,
      readStateDataPartitions = fieldpartitions
      }



data ReadShareGroupStateRequest = ReadShareGroupStateRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  readShareGroupStateRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  readShareGroupStateRequestTopics :: !(KafkaArray (ReadStateData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateRequest.
maxReadShareGroupStateRequestVersion :: Int16
maxReadShareGroupStateRequestVersion = 0

-- | Encode ReadShareGroupStateRequest with the given API version.
encodeReadShareGroupStateRequest :: MonadPut m => E.ApiVersion -> ReadShareGroupStateRequest -> m ()
encodeReadShareGroupStateRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (readShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeReadStateData (case P.unKafkaArray (readShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateRequest with the given API version.
decodeReadShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateRequest
decodeReadShareGroupStateRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateRequest
        {
        readShareGroupStateRequestGroupId = fieldgroupid
        ,
        readShareGroupStateRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version