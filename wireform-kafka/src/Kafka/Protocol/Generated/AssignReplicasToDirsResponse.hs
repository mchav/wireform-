{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AssignReplicasToDirsResponse
Description : Kafka AssignReplicasToDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 73.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AssignReplicasToDirsResponse
  (
    AssignReplicasToDirsResponse(..),
    DirectoryData(..),
    TopicData(..),
    PartitionData(..),
    encodeAssignReplicasToDirsResponse,
    decodeAssignReplicasToDirsResponse,
    maxAssignReplicasToDirsResponseVersion
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
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The list of assigned partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      }


-- | The list of topics and their assigned partitions.
data TopicData = TopicData
  {

  -- | The ID of the assigned topic.

  -- Versions: 0+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The list of assigned partitions.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    serialize (topicDataTopicId tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicId = fieldtopicid
      ,
      topicDataPartitions = fieldpartitions
      }


-- | The list of directories and their assigned partitions.
data DirectoryData = DirectoryData
  {

  -- | The ID of the directory.

  -- Versions: 0+
  directoryDataId :: !(KafkaUuid)
,

  -- | The list of topics and their assigned partitions.

  -- Versions: 0+
  directoryDataTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)


-- | Encode DirectoryData with version-aware field handling.
encodeDirectoryData :: MonadPut m => E.ApiVersion -> DirectoryData -> m ()
encodeDirectoryData version dmsg =
  do
    serialize (directoryDataId dmsg)
    E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (directoryDataTopics dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DirectoryData with version-aware field handling.
decodeDirectoryData :: MonadGet m => E.ApiVersion -> m DirectoryData
decodeDirectoryData version =
  do
    fieldid <- deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DirectoryData
      {
      directoryDataId = fieldid
      ,
      directoryDataTopics = fieldtopics
      }



data AssignReplicasToDirsResponse = AssignReplicasToDirsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  assignReplicasToDirsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  assignReplicasToDirsResponseErrorCode :: !(Int16)
,

  -- | The list of directories and their assigned partitions.

  -- Versions: 0+
  assignReplicasToDirsResponseDirectories :: !(KafkaArray (DirectoryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AssignReplicasToDirsResponse.
maxAssignReplicasToDirsResponseVersion :: Int16
maxAssignReplicasToDirsResponseVersion = 0

-- | Encode AssignReplicasToDirsResponse with the given API version.
encodeAssignReplicasToDirsResponse :: MonadPut m => E.ApiVersion -> AssignReplicasToDirsResponse -> m ()
encodeAssignReplicasToDirsResponse version msg
  | version == 0 =
    do
      serialize (assignReplicasToDirsResponseThrottleTimeMs msg)
      serialize (assignReplicasToDirsResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeDirectoryData (case P.unKafkaArray (assignReplicasToDirsResponseDirectories msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AssignReplicasToDirsResponse with the given API version.
decodeAssignReplicasToDirsResponse :: MonadGet m => E.ApiVersion -> m AssignReplicasToDirsResponse
decodeAssignReplicasToDirsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielddirectories <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDirectoryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AssignReplicasToDirsResponse
        {
        assignReplicasToDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        assignReplicasToDirsResponseErrorCode = fielderrorcode
        ,
        assignReplicasToDirsResponseDirectories = fielddirectories
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAssignReplicasToDirsResponse' / 'decodeAssignReplicasToDirsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AssignReplicasToDirsResponse where
  wireCodec = Just (WC.serialShimCodec encodeAssignReplicasToDirsResponse decodeAssignReplicasToDirsResponse)
  {-# INLINE wireCodec #-}
