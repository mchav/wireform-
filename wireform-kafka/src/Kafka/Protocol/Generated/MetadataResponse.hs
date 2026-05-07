{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.MetadataResponse
Description : Kafka MetadataResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 3.



Valid versions: 0-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.MetadataResponse
  (
    MetadataResponse(..),
    MetadataResponseBroker(..),
    MetadataResponseTopic(..),
    MetadataResponsePartition(..),
    encodeMetadataResponse,
    decodeMetadataResponse,
    maxMetadataResponseVersion
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


-- | A list of brokers present in the cluster.
data MetadataResponseBroker = MetadataResponseBroker
  {

  -- | The broker ID.

  -- Versions: 0+
  metadataResponseBrokerNodeId :: !(Int32)
,

  -- | The broker hostname.

  -- Versions: 0+
  metadataResponseBrokerHost :: !(KafkaString)
,

  -- | The broker port.

  -- Versions: 0+
  metadataResponseBrokerPort :: !(Int32)
,

  -- | The rack of the broker, or null if it has not been assigned to a rack.

  -- Versions: 1+
  metadataResponseBrokerRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponseBroker with version-aware field handling.
encodeMetadataResponseBroker :: MonadPut m => E.ApiVersion -> MetadataResponseBroker -> m ()
encodeMetadataResponseBroker version mmsg =
  do
    serialize (metadataResponseBrokerNodeId mmsg)
    if version >= 9 then serialize (toCompactString (metadataResponseBrokerHost mmsg)) else serialize (metadataResponseBrokerHost mmsg)
    serialize (metadataResponseBrokerPort mmsg)
    when (version >= 1) $
      if version >= 9 then serialize (toCompactString (metadataResponseBrokerRack mmsg)) else serialize (metadataResponseBrokerRack mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponseBroker with version-aware field handling.
decodeMetadataResponseBroker :: MonadGet m => E.ApiVersion -> m MetadataResponseBroker
decodeMetadataResponseBroker version =
  do
    fieldnodeid <- deserialize
    fieldhost <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 1
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponseBroker
      {
      metadataResponseBrokerNodeId = fieldnodeid
      ,
      metadataResponseBrokerHost = fieldhost
      ,
      metadataResponseBrokerPort = fieldport
      ,
      metadataResponseBrokerRack = fieldrack
      }


-- | Each partition in the topic.
data MetadataResponsePartition = MetadataResponsePartition
  {

  -- | The partition error, or 0 if there was no error.

  -- Versions: 0+
  metadataResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  metadataResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The ID of the leader broker.

  -- Versions: 0+
  metadataResponsePartitionLeaderId :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 7+
  metadataResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The set of all nodes that host this partition.

  -- Versions: 0+
  metadataResponsePartitionReplicaNodes :: !(KafkaArray (Int32))
,

  -- | The set of nodes that are in sync with the leader for this partition.

  -- Versions: 0+
  metadataResponsePartitionIsrNodes :: !(KafkaArray (Int32))
,

  -- | The set of offline replicas of this partition.

  -- Versions: 5+
  metadataResponsePartitionOfflineReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponsePartition with version-aware field handling.
encodeMetadataResponsePartition :: MonadPut m => E.ApiVersion -> MetadataResponsePartition -> m ()
encodeMetadataResponsePartition version mmsg =
  do
    serialize (metadataResponsePartitionErrorCode mmsg)
    serialize (metadataResponsePartitionPartitionIndex mmsg)
    serialize (metadataResponsePartitionLeaderId mmsg)
    when (version >= 7) $
      serialize (metadataResponsePartitionLeaderEpoch mmsg)
    E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionReplicaNodes mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionIsrNodes mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 5) $
      E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionOfflineReplicas mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponsePartition with version-aware field handling.
decodeMetadataResponsePartition :: MonadGet m => E.ApiVersion -> m MetadataResponsePartition
decodeMetadataResponsePartition version =
  do
    fielderrorcode <- deserialize
    fieldpartitionindex <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- if version >= 7
      then deserialize
      else pure ((-1))
    fieldreplicanodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
    fieldisrnodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
    fieldofflinereplicas <- if version >= 5
      then P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponsePartition
      {
      metadataResponsePartitionErrorCode = fielderrorcode
      ,
      metadataResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      metadataResponsePartitionLeaderId = fieldleaderid
      ,
      metadataResponsePartitionLeaderEpoch = fieldleaderepoch
      ,
      metadataResponsePartitionReplicaNodes = fieldreplicanodes
      ,
      metadataResponsePartitionIsrNodes = fieldisrnodes
      ,
      metadataResponsePartitionOfflineReplicas = fieldofflinereplicas
      }


-- | Each topic in the response.
data MetadataResponseTopic = MetadataResponseTopic
  {

  -- | The topic error, or 0 if there was no error.

  -- Versions: 0+
  metadataResponseTopicErrorCode :: !(Int16)
,

  -- | The topic name. Null for non-existing topics queried by ID. This is never null when ErrorCode is zer

  -- Versions: 0+
  metadataResponseTopicName :: !(KafkaString)
,

  -- | The topic id. Zero for non-existing topics queried by name. This is never zero when ErrorCode is zer

  -- Versions: 10+
  metadataResponseTopicTopicId :: !(KafkaUuid)
,

  -- | True if the topic is internal.

  -- Versions: 1+
  metadataResponseTopicIsInternal :: !(Bool)
,

  -- | Each partition in the topic.

  -- Versions: 0+
  metadataResponseTopicPartitions :: !(KafkaArray (MetadataResponsePartition))
,

  -- | 32-bit bitfield to represent authorized operations for this topic.

  -- Versions: 8+
  metadataResponseTopicTopicAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponseTopic with version-aware field handling.
encodeMetadataResponseTopic :: MonadPut m => E.ApiVersion -> MetadataResponseTopic -> m ()
encodeMetadataResponseTopic version mmsg =
  do
    serialize (metadataResponseTopicErrorCode mmsg)
    if version >= 9 then serialize (toCompactString (metadataResponseTopicName mmsg)) else serialize (metadataResponseTopicName mmsg)
    when (version >= 10) $
      serialize (metadataResponseTopicTopicId mmsg)
    when (version >= 1) $
      serialize (metadataResponseTopicIsInternal mmsg)
    E.encodeVersionedArray version 9 encodeMetadataResponsePartition (case P.unKafkaArray (metadataResponseTopicPartitions mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      serialize (metadataResponseTopicTopicAuthorizedOperations mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponseTopic with version-aware field handling.
decodeMetadataResponseTopic :: MonadGet m => E.ApiVersion -> m MetadataResponseTopic
decodeMetadataResponseTopic version =
  do
    fielderrorcode <- deserialize
    fieldname <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldisinternal <- if version >= 1
      then deserialize
      else pure (False)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponsePartition
    fieldtopicauthorizedoperations <- if version >= 8
      then deserialize
      else pure ((-2147483648))
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponseTopic
      {
      metadataResponseTopicErrorCode = fielderrorcode
      ,
      metadataResponseTopicName = fieldname
      ,
      metadataResponseTopicTopicId = fieldtopicid
      ,
      metadataResponseTopicIsInternal = fieldisinternal
      ,
      metadataResponseTopicPartitions = fieldpartitions
      ,
      metadataResponseTopicTopicAuthorizedOperations = fieldtopicauthorizedoperations
      }



data MetadataResponse = MetadataResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  metadataResponseThrottleTimeMs :: !(Int32)
,

  -- | A list of brokers present in the cluster.

  -- Versions: 0+
  metadataResponseBrokers :: !(KafkaArray (MetadataResponseBroker))
,

  -- | The cluster ID that responding broker belongs to.

  -- Versions: 2+
  metadataResponseClusterId :: !(KafkaString)
,

  -- | The ID of the controller broker.

  -- Versions: 1+
  metadataResponseControllerId :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  metadataResponseTopics :: !(KafkaArray (MetadataResponseTopic))
,

  -- | 32-bit bitfield to represent authorized operations for this cluster.

  -- Versions: 8-10
  metadataResponseClusterAuthorizedOperations :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 13+
  metadataResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for MetadataResponse.
maxMetadataResponseVersion :: Int16
maxMetadataResponseVersion = 13

-- | Encode MetadataResponse with the given API version.
encodeMetadataResponse :: MonadPut m => E.ApiVersion -> MetadataResponse -> m ()
encodeMetadataResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 8 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterAuthorizedOperations msg)


  | version == 13 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 9 && version <= 10 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 11 && version <= 12 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 7 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode MetadataResponse with the given API version.
decodeMetadataResponse :: MonadGet m => E.ApiVersion -> m MetadataResponse
decodeMetadataResponse version
  | version == 0 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = P.KafkaString Null
        ,
        metadataResponseControllerId = (-1)
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 1 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = P.KafkaString Null
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 2 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 8 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fieldclusterauthorizedoperations <- deserialize
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        ,
        metadataResponseErrorCode = 0
        }

  | version == 13 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = fielderrorcode
        }

  | version >= 9 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fieldclusterauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        ,
        metadataResponseErrorCode = 0
        }

  | version >= 11 && version <= 12 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version >= 3 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }
  | otherwise = fail $ "Unsupported version: " ++ show version