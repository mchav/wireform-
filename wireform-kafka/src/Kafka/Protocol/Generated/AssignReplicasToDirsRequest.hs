{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AssignReplicasToDirsRequest
Description : Kafka AssignReplicasToDirsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 73.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AssignReplicasToDirsRequest
  (
    AssignReplicasToDirsRequest(..),
    DirectoryData(..),
    TopicData(..),
    PartitionData(..),
    encodeAssignReplicasToDirsRequest,
    decodeAssignReplicasToDirsRequest,
    maxAssignReplicasToDirsRequestVersion
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


-- | The partitions assigned to the directory.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      }


-- | The topics assigned to the directory.
data TopicData = TopicData
  {

  -- | The ID of the assigned topic.

  -- Versions: 0+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The partitions assigned to the directory.

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


-- | The directories to which replicas should be assigned.
data DirectoryData = DirectoryData
  {

  -- | The ID of the directory.

  -- Versions: 0+
  directoryDataId :: !(KafkaUuid)
,

  -- | The topics assigned to the directory.

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



data AssignReplicasToDirsRequest = AssignReplicasToDirsRequest
  {

  -- | The ID of the requesting broker.

  -- Versions: 0+
  assignReplicasToDirsRequestBrokerId :: !(Int32)
,

  -- | The epoch of the requesting broker.

  -- Versions: 0+
  assignReplicasToDirsRequestBrokerEpoch :: !(Int64)
,

  -- | The directories to which replicas should be assigned.

  -- Versions: 0+
  assignReplicasToDirsRequestDirectories :: !(KafkaArray (DirectoryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AssignReplicasToDirsRequest.
maxAssignReplicasToDirsRequestVersion :: Int16
maxAssignReplicasToDirsRequestVersion = 0

-- | Encode AssignReplicasToDirsRequest with the given API version.
encodeAssignReplicasToDirsRequest :: MonadPut m => E.ApiVersion -> AssignReplicasToDirsRequest -> m ()
encodeAssignReplicasToDirsRequest version msg
  | version == 0 =
    do
      serialize (assignReplicasToDirsRequestBrokerId msg)
      serialize (assignReplicasToDirsRequestBrokerEpoch msg)
      E.encodeVersionedArray version 0 encodeDirectoryData (case P.unKafkaArray (assignReplicasToDirsRequestDirectories msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AssignReplicasToDirsRequest with the given API version.
decodeAssignReplicasToDirsRequest :: MonadGet m => E.ApiVersion -> m AssignReplicasToDirsRequest
decodeAssignReplicasToDirsRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fielddirectories <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDirectoryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AssignReplicasToDirsRequest
        {
        assignReplicasToDirsRequestBrokerId = fieldbrokerid
        ,
        assignReplicasToDirsRequestBrokerEpoch = fieldbrokerepoch
        ,
        assignReplicasToDirsRequestDirectories = fielddirectories
        }
  | otherwise = fail $ "Unsupported version: " ++ show version