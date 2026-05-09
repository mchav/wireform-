{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StreamsGroupHeartbeatRequest
Description : Kafka StreamsGroupHeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 88.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StreamsGroupHeartbeatRequest
  (
    StreamsGroupHeartbeatRequest(..),
    Topology(..),
    Subtopology(..),
    TopicInfo(..),
    CopartitionGroup(..),
    TaskIds(..),
    Endpoint(..),
    KeyValue(..),
    TaskOffset(..),
    encodeStreamsGroupHeartbeatRequest,
    decodeStreamsGroupHeartbeatRequest,
    maxStreamsGroupHeartbeatRequestVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


data KeyValue = KeyValue
  {

  -- | key of the config

  -- Versions: 0+
  keyValueKey :: !(KafkaString)
,

  -- | value of the config

  -- Versions: 0+
  keyValueValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode KeyValue with version-aware field handling.
encodeKeyValue :: MonadPut m => E.ApiVersion -> KeyValue -> m ()
encodeKeyValue version kmsg =
  do
    if version >= 0 then serialize (toCompactString (keyValueKey kmsg)) else serialize (keyValueKey kmsg)
    if version >= 0 then serialize (toCompactString (keyValueValue kmsg)) else serialize (keyValueValue kmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode KeyValue with version-aware field handling.
decodeKeyValue :: MonadGet m => E.ApiVersion -> m KeyValue
decodeKeyValue version =
  do
    fieldkey <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure KeyValue
      {
      keyValueKey = fieldkey
      ,
      keyValueValue = fieldvalue
      }



data TopicInfo = TopicInfo
  {

  -- | The name of the topic.

  -- Versions: 0+
  topicInfoName :: !(KafkaString)
,

  -- | The number of partitions in the topic. Can be 0 if no specific number of partitions is enforced. Alw

  -- Versions: 0+
  topicInfoPartitions :: !(Int32)
,

  -- | The replication factor of the topic. Can be 0 if the default replication factor should be used.

  -- Versions: 0+
  topicInfoReplicationFactor :: !(Int16)
,

  -- | Topic-level configurations as key-value pairs.

  -- Versions: 0+
  topicInfoTopicConfigs :: !(KafkaArray (KeyValue))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicInfo with version-aware field handling.
encodeTopicInfo :: MonadPut m => E.ApiVersion -> TopicInfo -> m ()
encodeTopicInfo version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicInfoName tmsg)) else serialize (topicInfoName tmsg)
    serialize (topicInfoPartitions tmsg)
    serialize (topicInfoReplicationFactor tmsg)
    E.encodeVersionedArray version 0 encodeKeyValue (case P.unKafkaArray (topicInfoTopicConfigs tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicInfo with version-aware field handling.
decodeTopicInfo :: MonadGet m => E.ApiVersion -> m TopicInfo
decodeTopicInfo version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- deserialize
    fieldreplicationfactor <- deserialize
    fieldtopicconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeKeyValue
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicInfo
      {
      topicInfoName = fieldname
      ,
      topicInfoPartitions = fieldpartitions
      ,
      topicInfoReplicationFactor = fieldreplicationfactor
      ,
      topicInfoTopicConfigs = fieldtopicconfigs
      }



data Endpoint = Endpoint
  {

  -- | host of the endpoint

  -- Versions: 0+
  endpointHost :: !(KafkaString)
,

  -- | port of the endpoint

  -- Versions: 0+
  endpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode Endpoint with version-aware field handling.
encodeEndpoint :: MonadPut m => E.ApiVersion -> Endpoint -> m ()
encodeEndpoint version emsg =
  do
    if version >= 0 then serialize (toCompactString (endpointHost emsg)) else serialize (endpointHost emsg)
    serialize (endpointPort emsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Endpoint with version-aware field handling.
decodeEndpoint :: MonadGet m => E.ApiVersion -> m Endpoint
decodeEndpoint version =
  do
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Endpoint
      {
      endpointHost = fieldhost
      ,
      endpointPort = fieldport
      }



data TaskOffset = TaskOffset
  {

  -- | The subtopology identifier.

  -- Versions: 0+
  taskOffsetSubtopologyId :: !(KafkaString)
,

  -- | The partition.

  -- Versions: 0+
  taskOffsetPartition :: !(Int32)
,

  -- | The offset.

  -- Versions: 0+
  taskOffsetOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode TaskOffset with version-aware field handling.
encodeTaskOffset :: MonadPut m => E.ApiVersion -> TaskOffset -> m ()
encodeTaskOffset version tmsg =
  do
    if version >= 0 then serialize (toCompactString (taskOffsetSubtopologyId tmsg)) else serialize (taskOffsetSubtopologyId tmsg)
    serialize (taskOffsetPartition tmsg)
    serialize (taskOffsetOffset tmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TaskOffset with version-aware field handling.
decodeTaskOffset :: MonadGet m => E.ApiVersion -> m TaskOffset
decodeTaskOffset version =
  do
    fieldsubtopologyid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartition <- deserialize
    fieldoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TaskOffset
      {
      taskOffsetSubtopologyId = fieldsubtopologyid
      ,
      taskOffsetPartition = fieldpartition
      ,
      taskOffsetOffset = fieldoffset
      }



data TaskIds = TaskIds
  {

  -- | The subtopology identifier.

  -- Versions: 0+
  taskIdsSubtopologyId :: !(KafkaString)
,

  -- | The partitions of the input topics processed by this member.

  -- Versions: 0+
  taskIdsPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TaskIds with version-aware field handling.
encodeTaskIds :: MonadPut m => E.ApiVersion -> TaskIds -> m ()
encodeTaskIds version tmsg =
  do
    if version >= 0 then serialize (toCompactString (taskIdsSubtopologyId tmsg)) else serialize (taskIdsSubtopologyId tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (taskIdsPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TaskIds with version-aware field handling.
decodeTaskIds :: MonadGet m => E.ApiVersion -> m TaskIds
decodeTaskIds version =
  do
    fieldsubtopologyid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TaskIds
      {
      taskIdsSubtopologyId = fieldsubtopologyid
      ,
      taskIdsPartitions = fieldpartitions
      }


-- | A subset of source topics that must be copartitioned.
data CopartitionGroup = CopartitionGroup
  {

  -- | The topics the topology reads from. Index into the array on the subtopology level.

  -- Versions: 0+
  copartitionGroupSourceTopics :: !(KafkaArray (Int16))
,

  -- | Regular expressions identifying topics the subtopology reads from. Index into the array on the subto

  -- Versions: 0+
  copartitionGroupSourceTopicRegex :: !(KafkaArray (Int16))
,

  -- | The set of source topics that are internally created repartition topics. Index into the array on the

  -- Versions: 0+
  copartitionGroupRepartitionSourceTopics :: !(KafkaArray (Int16))

  }
  deriving (Eq, Show, Generic)


-- | Encode CopartitionGroup with version-aware field handling.
encodeCopartitionGroup :: MonadPut m => E.ApiVersion -> CopartitionGroup -> m ()
encodeCopartitionGroup version cmsg =
  do
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (copartitionGroupSourceTopics cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int16"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (copartitionGroupSourceTopicRegex cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int16"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (copartitionGroupRepartitionSourceTopics cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int16"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CopartitionGroup with version-aware field handling.
decodeCopartitionGroup :: MonadGet m => E.ApiVersion -> m CopartitionGroup
decodeCopartitionGroup version =
  do
    fieldsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldsourcetopicregex <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldrepartitionsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CopartitionGroup
      {
      copartitionGroupSourceTopics = fieldsourcetopics
      ,
      copartitionGroupSourceTopicRegex = fieldsourcetopicregex
      ,
      copartitionGroupRepartitionSourceTopics = fieldrepartitionsourcetopics
      }


-- | The sub-topologies of the streams application.
data Subtopology = Subtopology
  {

  -- | String to uniquely identify the subtopology. Deterministically generated from the topology

  -- Versions: 0+
  subtopologySubtopologyId :: !(KafkaString)
,

  -- | The topics the topology reads from.

  -- Versions: 0+
  subtopologySourceTopics :: !(KafkaArray (KafkaString))
,

  -- | The regular expressions identifying topics the subtopology reads from.

  -- Versions: 0+
  subtopologySourceTopicRegex :: !(KafkaArray (KafkaString))
,

  -- | The set of state changelog topics associated with this subtopology. Created automatically.

  -- Versions: 0+
  subtopologyStateChangelogTopics :: !(KafkaArray (TopicInfo))
,

  -- | The repartition topics the subtopology writes to.

  -- Versions: 0+
  subtopologyRepartitionSinkTopics :: !(KafkaArray (KafkaString))
,

  -- | The set of source topics that are internally created repartition topics. Created automatically.

  -- Versions: 0+
  subtopologyRepartitionSourceTopics :: !(KafkaArray (TopicInfo))
,

  -- | A subset of source topics that must be copartitioned.

  -- Versions: 0+
  subtopologyCopartitionGroups :: !(KafkaArray (CopartitionGroup))

  }
  deriving (Eq, Show, Generic)


-- | Encode Subtopology with version-aware field handling.
encodeSubtopology :: MonadPut m => E.ApiVersion -> Subtopology -> m ()
encodeSubtopology version smsg =
  do
    if version >= 0 then serialize (toCompactString (subtopologySubtopologyId smsg)) else serialize (subtopologySubtopologyId smsg)
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (subtopologySourceTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (subtopologySourceTopicRegex smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTopicInfo (case P.unKafkaArray (subtopologyStateChangelogTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (subtopologyRepartitionSinkTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTopicInfo (case P.unKafkaArray (subtopologyRepartitionSourceTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeCopartitionGroup (case P.unKafkaArray (subtopologyCopartitionGroups smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Subtopology with version-aware field handling.
decodeSubtopology :: MonadGet m => E.ApiVersion -> m Subtopology
decodeSubtopology version =
  do
    fieldsubtopologyid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldsourcetopicregex <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldstatechangelogtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicInfo
    fieldrepartitionsinktopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldrepartitionsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicInfo
    fieldcopartitiongroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeCopartitionGroup
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Subtopology
      {
      subtopologySubtopologyId = fieldsubtopologyid
      ,
      subtopologySourceTopics = fieldsourcetopics
      ,
      subtopologySourceTopicRegex = fieldsourcetopicregex
      ,
      subtopologyStateChangelogTopics = fieldstatechangelogtopics
      ,
      subtopologyRepartitionSinkTopics = fieldrepartitionsinktopics
      ,
      subtopologyRepartitionSourceTopics = fieldrepartitionsourcetopics
      ,
      subtopologyCopartitionGroups = fieldcopartitiongroups
      }


-- | The topology metadata of the streams application. Used to initialize the topology of the group and to check if the topology corresponds to the topology initialized for the group. Only sent when member
data Topology = Topology
  {

  -- | The epoch of the topology. Used to check if the topology corresponds to the topology initialized on 

  -- Versions: 0+
  topologyEpoch :: !(Int32)
,

  -- | The sub-topologies of the streams application.

  -- Versions: 0+
  topologySubtopologies :: !(KafkaArray (Subtopology))

  }
  deriving (Eq, Show, Generic)


-- | Encode Topology with version-aware field handling.
encodeTopology :: MonadPut m => E.ApiVersion -> Topology -> m ()
encodeTopology version tmsg =
  do
    serialize (topologyEpoch tmsg)
    E.encodeVersionedArray version 0 encodeSubtopology (case P.unKafkaArray (topologySubtopologies tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Topology with version-aware field handling.
decodeTopology :: MonadGet m => E.ApiVersion -> m Topology
decodeTopology version =
  do
    fieldepoch <- deserialize
    fieldsubtopologies <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeSubtopology
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Topology
      {
      topologyEpoch = fieldepoch
      ,
      topologySubtopologies = fieldsubtopologies
      }



data StreamsGroupHeartbeatRequest = StreamsGroupHeartbeatRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  streamsGroupHeartbeatRequestGroupId :: !(KafkaString)
,

  -- | The member ID generated by the streams consumer. The member ID must be kept during the entire lifeti

  -- Versions: 0+
  streamsGroupHeartbeatRequestMemberId :: !(KafkaString)
,

  -- | The current member epoch; 0 to join the group; -1 to leave the group; -2 to indicate that the static

  -- Versions: 0+
  streamsGroupHeartbeatRequestMemberEpoch :: !(Int32)
,

  -- | The current endpoint epoch of this client, represents the latest endpoint epoch this client received

  -- Versions: 0+
  streamsGroupHeartbeatRequestEndpointInformationEpoch :: !(Int32)
,

  -- | null if not provided or if it didn't change since the last heartbeat; the instance ID for static mem

  -- Versions: 0+
  streamsGroupHeartbeatRequestInstanceId :: !(KafkaString)
,

  -- | null if not provided or if it didn't change since the last heartbeat; the rack ID of the member othe

  -- Versions: 0+
  streamsGroupHeartbeatRequestRackId :: !(KafkaString)
,

  -- | -1 if it didn't change since the last heartbeat; the maximum time in milliseconds that the coordinat

  -- Versions: 0+
  streamsGroupHeartbeatRequestRebalanceTimeoutMs :: !(Int32)
,

  -- | The topology metadata of the streams application. Used to initialize the topology of the group and t

  -- Versions: 0+
  streamsGroupHeartbeatRequestTopology :: !(Nullable (Topology))
,

  -- | Currently owned active tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatRequestActiveTasks :: !(KafkaArray (TaskIds))
,

  -- | Currently owned standby tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatRequestStandbyTasks :: !(KafkaArray (TaskIds))
,

  -- | Currently owned warm-up tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatRequestWarmupTasks :: !(KafkaArray (TaskIds))
,

  -- | Identity of the streams instance that may have multiple consumers. Null if unchanged since last hear

  -- Versions: 0+
  streamsGroupHeartbeatRequestProcessId :: !(KafkaString)
,

  -- | User-defined endpoint for Interactive Queries. Null if unchanged since last heartbeat, or if not def

  -- Versions: 0+
  streamsGroupHeartbeatRequestUserEndpoint :: !(Nullable (Endpoint))
,

  -- | Used for rack-aware assignment algorithm. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatRequestClientTags :: !(KafkaArray (KeyValue))
,

  -- | Cumulative changelog offsets for tasks. Only updated when a warm-up task has caught up, and accordin

  -- Versions: 0+
  streamsGroupHeartbeatRequestTaskOffsets :: !(KafkaArray (TaskOffset))
,

  -- | Cumulative changelog end-offsets for tasks. Only updated when a warm-up task has caught up, and acco

  -- Versions: 0+
  streamsGroupHeartbeatRequestTaskEndOffsets :: !(KafkaArray (TaskOffset))
,

  -- | Whether all Streams clients in the group should shut down.

  -- Versions: 0+
  streamsGroupHeartbeatRequestShutdownApplication :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StreamsGroupHeartbeatRequest.
maxStreamsGroupHeartbeatRequestVersion :: Int16
maxStreamsGroupHeartbeatRequestVersion = 0

-- | KafkaMessage instance for StreamsGroupHeartbeatRequest.
instance KafkaMessage StreamsGroupHeartbeatRequest where
  messageApiKey = 88
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode StreamsGroupHeartbeatRequest with the given API version.
encodeStreamsGroupHeartbeatRequest :: MonadPut m => E.ApiVersion -> StreamsGroupHeartbeatRequest -> m ()
encodeStreamsGroupHeartbeatRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (streamsGroupHeartbeatRequestGroupId msg))
      serialize (toCompactString (streamsGroupHeartbeatRequestMemberId msg))
      serialize (streamsGroupHeartbeatRequestMemberEpoch msg)
      serialize (streamsGroupHeartbeatRequestEndpointInformationEpoch msg)
      serialize (toCompactString (streamsGroupHeartbeatRequestInstanceId msg))
      serialize (toCompactString (streamsGroupHeartbeatRequestRackId msg))
      serialize (streamsGroupHeartbeatRequestRebalanceTimeoutMs msg)
      case (streamsGroupHeartbeatRequestTopology msg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeTopology version val } }
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatRequestActiveTasks msg)
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatRequestStandbyTasks msg)
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatRequestWarmupTasks msg)
      serialize (toCompactString (streamsGroupHeartbeatRequestProcessId msg))
      case (streamsGroupHeartbeatRequestUserEndpoint msg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeEndpoint version val } }
      E.encodeVersionedNullableArray version 0 encodeKeyValue (streamsGroupHeartbeatRequestClientTags msg)
      E.encodeVersionedNullableArray version 0 encodeTaskOffset (streamsGroupHeartbeatRequestTaskOffsets msg)
      E.encodeVersionedNullableArray version 0 encodeTaskOffset (streamsGroupHeartbeatRequestTaskEndOffsets msg)
      serialize (streamsGroupHeartbeatRequestShutdownApplication msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode StreamsGroupHeartbeatRequest with the given API version.
decodeStreamsGroupHeartbeatRequest :: MonadGet m => E.ApiVersion -> m StreamsGroupHeartbeatRequest
decodeStreamsGroupHeartbeatRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberepoch <- deserialize
      fieldendpointinformationepoch <- deserialize
      fieldinstanceid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldrackid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldrebalancetimeoutms <- deserialize
      fieldtopology <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeTopology version; _ -> fail "Invalid nullable flag" } }
      fieldactivetasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldstandbytasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldwarmuptasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldprocessid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fielduserendpoint <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeEndpoint version; _ -> fail "Invalid nullable flag" } }
      fieldclienttags <- E.decodeVersionedNullableArray version 0 decodeKeyValue
      fieldtaskoffsets <- E.decodeVersionedNullableArray version 0 decodeTaskOffset
      fieldtaskendoffsets <- E.decodeVersionedNullableArray version 0 decodeTaskOffset
      fieldshutdownapplication <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure StreamsGroupHeartbeatRequest
        {
        streamsGroupHeartbeatRequestGroupId = fieldgroupid
        ,
        streamsGroupHeartbeatRequestMemberId = fieldmemberid
        ,
        streamsGroupHeartbeatRequestMemberEpoch = fieldmemberepoch
        ,
        streamsGroupHeartbeatRequestEndpointInformationEpoch = fieldendpointinformationepoch
        ,
        streamsGroupHeartbeatRequestInstanceId = fieldinstanceid
        ,
        streamsGroupHeartbeatRequestRackId = fieldrackid
        ,
        streamsGroupHeartbeatRequestRebalanceTimeoutMs = fieldrebalancetimeoutms
        ,
        streamsGroupHeartbeatRequestTopology = fieldtopology
        ,
        streamsGroupHeartbeatRequestActiveTasks = fieldactivetasks
        ,
        streamsGroupHeartbeatRequestStandbyTasks = fieldstandbytasks
        ,
        streamsGroupHeartbeatRequestWarmupTasks = fieldwarmuptasks
        ,
        streamsGroupHeartbeatRequestProcessId = fieldprocessid
        ,
        streamsGroupHeartbeatRequestUserEndpoint = fielduserendpoint
        ,
        streamsGroupHeartbeatRequestClientTags = fieldclienttags
        ,
        streamsGroupHeartbeatRequestTaskOffsets = fieldtaskoffsets
        ,
        streamsGroupHeartbeatRequestTaskEndOffsets = fieldtaskendoffsets
        ,
        streamsGroupHeartbeatRequestShutdownApplication = fieldshutdownapplication
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a KeyValue.
wireMaxSizeKeyValue :: Int -> KeyValue -> Int
wireMaxSizeKeyValue _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (keyValueKey msg))
  + WP.compactStringMaxSize (P.toCompactString (keyValueValue msg))
  + 1

-- | Direct-poke encoder for KeyValue.
wirePokeKeyValue :: Int -> Ptr Word8 -> KeyValue -> IO (Ptr Word8)
wirePokeKeyValue version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (keyValueKey msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (keyValueValue msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for KeyValue.
wirePeekKeyValue :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (KeyValue, Ptr Word8)
wirePeekKeyValue version _fp _basePtr p0 endPtr = do
  (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (KeyValue { keyValueKey = f0_key, keyValueValue = f1_value }, pTagsEnd)

-- | Worst-case wire size of a TopicInfo.
wireMaxSizeTopicInfo :: Int -> TopicInfo -> Int
wireMaxSizeTopicInfo _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicInfoName msg))
  + 4
  + 2
  + (5 + (case P.unKafkaArray (topicInfoTopicConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeKeyValue _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicInfo.
wirePokeTopicInfo :: Int -> Ptr Word8 -> TopicInfo -> IO (Ptr Word8)
wirePokeTopicInfo version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicInfoName msg))
  p2 <- W.pokeInt32BE p1 (topicInfoPartitions msg)
  p3 <- W.pokeInt16BE p2 (topicInfoReplicationFactor msg)
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeKeyValue version p x) p3 (topicInfoTopicConfigs msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for TopicInfo.
wirePeekTopicInfo :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicInfo, Ptr Word8)
wirePeekTopicInfo version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- W.peekInt32BE p1 endPtr
  (f2_replicationfactor, p3) <- W.peekInt16BE p2 endPtr
  (f3_topicconfigs, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekKeyValue version _fp _basePtr p e) p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (TopicInfo { topicInfoName = f0_name, topicInfoPartitions = f1_partitions, topicInfoReplicationFactor = f2_replicationfactor, topicInfoTopicConfigs = f3_topicconfigs }, pTagsEnd)

-- | Worst-case wire size of a Endpoint.
wireMaxSizeEndpoint :: Int -> Endpoint -> Int
wireMaxSizeEndpoint _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (endpointHost msg))
  + 2
  + 1

-- | Direct-poke encoder for Endpoint.
wirePokeEndpoint :: Int -> Ptr Word8 -> Endpoint -> IO (Ptr Word8)
wirePokeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (endpointHost msg))
  p2 <- W.pokeWord16BE p1 (endpointPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Endpoint.
wirePeekEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Endpoint, Ptr Word8)
wirePeekEndpoint version _fp _basePtr p0 endPtr = do
  (f0_host, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_port, p2) <- W.peekWord16BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Endpoint { endpointHost = f0_host, endpointPort = f1_port }, pTagsEnd)

-- | Worst-case wire size of a TaskOffset.
wireMaxSizeTaskOffset :: Int -> TaskOffset -> Int
wireMaxSizeTaskOffset _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (taskOffsetSubtopologyId msg))
  + 4
  + 8
  + 1

-- | Direct-poke encoder for TaskOffset.
wirePokeTaskOffset :: Int -> Ptr Word8 -> TaskOffset -> IO (Ptr Word8)
wirePokeTaskOffset version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (taskOffsetSubtopologyId msg))
  p2 <- W.pokeInt32BE p1 (taskOffsetPartition msg)
  p3 <- W.pokeInt64BE p2 (taskOffsetOffset msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for TaskOffset.
wirePeekTaskOffset :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TaskOffset, Ptr Word8)
wirePeekTaskOffset version _fp _basePtr p0 endPtr = do
  (f0_subtopologyid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partition, p2) <- W.peekInt32BE p1 endPtr
  (f2_offset, p3) <- W.peekInt64BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (TaskOffset { taskOffsetSubtopologyId = f0_subtopologyid, taskOffsetPartition = f1_partition, taskOffsetOffset = f2_offset }, pTagsEnd)

-- | Worst-case wire size of a TaskIds.
wireMaxSizeTaskIds :: Int -> TaskIds -> Int
wireMaxSizeTaskIds _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (taskIdsSubtopologyId msg))
  + (5 + (case P.unKafkaArray (taskIdsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TaskIds.
wirePokeTaskIds :: Int -> Ptr Word8 -> TaskIds -> IO (Ptr Word8)
wirePokeTaskIds version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (taskIdsSubtopologyId msg))
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (taskIdsPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TaskIds.
wirePeekTaskIds :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TaskIds, Ptr Word8)
wirePeekTaskIds version _fp _basePtr p0 endPtr = do
  (f0_subtopologyid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TaskIds { taskIdsSubtopologyId = f0_subtopologyid, taskIdsPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a CopartitionGroup.
wireMaxSizeCopartitionGroup :: Int -> CopartitionGroup -> Int
wireMaxSizeCopartitionGroup _version msg =
  0
  + (5 + (case P.unKafkaArray (copartitionGroupSourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> 2 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (copartitionGroupSourceTopicRegex msg) of { P.NotNull v -> sum (fmap (\x -> 2 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (copartitionGroupRepartitionSourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> 2 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CopartitionGroup.
wirePokeCopartitionGroup :: Int -> Ptr Word8 -> CopartitionGroup -> IO (Ptr Word8)
wirePokeCopartitionGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 0 W.pokeInt16BE p0 (copartitionGroupSourceTopics msg)
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt16BE p1 (copartitionGroupSourceTopicRegex msg)
  p3 <- WP.pokeVersionedArray version 0 W.pokeInt16BE p2 (copartitionGroupRepartitionSourceTopics msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for CopartitionGroup.
wirePeekCopartitionGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CopartitionGroup, Ptr Word8)
wirePeekCopartitionGroup version _fp _basePtr p0 endPtr = do
  (f0_sourcetopics, p1) <- WP.peekVersionedArray version 0 W.peekInt16BE p0 endPtr
  (f1_sourcetopicregex, p2) <- WP.peekVersionedArray version 0 W.peekInt16BE p1 endPtr
  (f2_repartitionsourcetopics, p3) <- WP.peekVersionedArray version 0 W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (CopartitionGroup { copartitionGroupSourceTopics = f0_sourcetopics, copartitionGroupSourceTopicRegex = f1_sourcetopicregex, copartitionGroupRepartitionSourceTopics = f2_repartitionsourcetopics }, pTagsEnd)

-- | Worst-case wire size of a Subtopology.
wireMaxSizeSubtopology :: Int -> Subtopology -> Int
wireMaxSizeSubtopology _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (subtopologySubtopologyId msg))
  + (5 + (case P.unKafkaArray (subtopologySourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologySourceTopicRegex msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyStateChangelogTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicInfo _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyRepartitionSinkTopics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyRepartitionSourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicInfo _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyCopartitionGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCopartitionGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Subtopology.
wirePokeSubtopology :: Int -> Ptr Word8 -> Subtopology -> IO (Ptr Word8)
wirePokeSubtopology version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (subtopologySubtopologyId msg))
  p2 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p1 (subtopologySourceTopics msg)
  p3 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p2 (subtopologySourceTopicRegex msg)
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicInfo version p x) p3 (subtopologyStateChangelogTopics msg)
  p5 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p4 (subtopologyRepartitionSinkTopics msg)
  p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicInfo version p x) p5 (subtopologyRepartitionSourceTopics msg)
  p7 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeCopartitionGroup version p x) p6 (subtopologyCopartitionGroups msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for Subtopology.
wirePeekSubtopology :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Subtopology, Ptr Word8)
wirePeekSubtopology version _fp _basePtr p0 endPtr = do
  (f0_subtopologyid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_sourcetopics, p2) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p1 endPtr
  (f2_sourcetopicregex, p3) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p2 endPtr
  (f3_statechangelogtopics, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicInfo version _fp _basePtr p e) p3 endPtr
  (f4_repartitionsinktopics, p5) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p4 endPtr
  (f5_repartitionsourcetopics, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicInfo version _fp _basePtr p e) p5 endPtr
  (f6_copartitiongroups, p7) <- WP.peekVersionedArray version 0 (\p e -> wirePeekCopartitionGroup version _fp _basePtr p e) p6 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (Subtopology { subtopologySubtopologyId = f0_subtopologyid, subtopologySourceTopics = f1_sourcetopics, subtopologySourceTopicRegex = f2_sourcetopicregex, subtopologyStateChangelogTopics = f3_statechangelogtopics, subtopologyRepartitionSinkTopics = f4_repartitionsinktopics, subtopologyRepartitionSourceTopics = f5_repartitionsourcetopics, subtopologyCopartitionGroups = f6_copartitiongroups }, pTagsEnd)

-- | Worst-case wire size of a Topology.
wireMaxSizeTopology :: Int -> Topology -> Int
wireMaxSizeTopology _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (topologySubtopologies msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeSubtopology _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Topology.
wirePokeTopology :: Int -> Ptr Word8 -> Topology -> IO (Ptr Word8)
wirePokeTopology version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (topologyEpoch msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeSubtopology version p x) p1 (topologySubtopologies msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Topology.
wirePeekTopology :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Topology, Ptr Word8)
wirePeekTopology version _fp _basePtr p0 endPtr = do
  (f0_epoch, p1) <- W.peekInt32BE p0 endPtr
  (f1_subtopologies, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekSubtopology version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Topology { topologyEpoch = f0_epoch, topologySubtopologies = f1_subtopologies }, pTagsEnd)

-- | Worst-case wire size of a StreamsGroupHeartbeatRequest.
wireMaxSizeStreamsGroupHeartbeatRequest :: Int -> StreamsGroupHeartbeatRequest -> Int
wireMaxSizeStreamsGroupHeartbeatRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (streamsGroupHeartbeatRequestGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (streamsGroupHeartbeatRequestMemberId msg))
  + 4
  + 4
  + WP.compactStringMaxSize (P.toCompactString (streamsGroupHeartbeatRequestInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (streamsGroupHeartbeatRequestRackId msg))
  + 4
  + (case (streamsGroupHeartbeatRequestTopology msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeTopology _version s })
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestActiveTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestStandbyTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestWarmupTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (streamsGroupHeartbeatRequestProcessId msg))
  + (case (streamsGroupHeartbeatRequestUserEndpoint msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeEndpoint _version s })
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestClientTags msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeKeyValue _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestTaskOffsets msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskOffset _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (streamsGroupHeartbeatRequestTaskEndOffsets msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskOffset _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for StreamsGroupHeartbeatRequest.
wirePokeStreamsGroupHeartbeatRequest :: Int -> Ptr Word8 -> StreamsGroupHeartbeatRequest -> IO (Ptr Word8)
wirePokeStreamsGroupHeartbeatRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (streamsGroupHeartbeatRequestGroupId msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (streamsGroupHeartbeatRequestMemberId msg))
    p3 <- W.pokeInt32BE p2 (streamsGroupHeartbeatRequestMemberEpoch msg)
    p4 <- W.pokeInt32BE p3 (streamsGroupHeartbeatRequestEndpointInformationEpoch msg)
    p5 <- WP.pokeCompactString p4 (P.toCompactString (streamsGroupHeartbeatRequestInstanceId msg))
    p6 <- WP.pokeCompactString p5 (P.toCompactString (streamsGroupHeartbeatRequestRackId msg))
    p7 <- W.pokeInt32BE p6 (streamsGroupHeartbeatRequestRebalanceTimeoutMs msg)
    p8 <- (case (streamsGroupHeartbeatRequestTopology msg) of { P.Null -> W.pokeWord8 p7 0; P.NotNull s -> W.pokeWord8 p7 1 >>= \p' -> wirePokeTopology version p' s })
    p9 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeTaskIds version p x) p8 (streamsGroupHeartbeatRequestActiveTasks msg)
    p10 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeTaskIds version p x) p9 (streamsGroupHeartbeatRequestStandbyTasks msg)
    p11 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeTaskIds version p x) p10 (streamsGroupHeartbeatRequestWarmupTasks msg)
    p12 <- WP.pokeCompactString p11 (P.toCompactString (streamsGroupHeartbeatRequestProcessId msg))
    p13 <- (case (streamsGroupHeartbeatRequestUserEndpoint msg) of { P.Null -> W.pokeWord8 p12 0; P.NotNull s -> W.pokeWord8 p12 1 >>= \p' -> wirePokeEndpoint version p' s })
    p14 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeKeyValue version p x) p13 (streamsGroupHeartbeatRequestClientTags msg)
    p15 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeTaskOffset version p x) p14 (streamsGroupHeartbeatRequestTaskOffsets msg)
    p16 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeTaskOffset version p x) p15 (streamsGroupHeartbeatRequestTaskEndOffsets msg)
    p17 <- W.pokeWord8 p16 (if (streamsGroupHeartbeatRequestShutdownApplication msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p17
  | otherwise = error $ "wirePoke StreamsGroupHeartbeatRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for StreamsGroupHeartbeatRequest.
wirePeekStreamsGroupHeartbeatRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StreamsGroupHeartbeatRequest, Ptr Word8)
wirePeekStreamsGroupHeartbeatRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_memberid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_memberepoch, p3) <- W.peekInt32BE p2 endPtr
    (f3_endpointinformationepoch, p4) <- W.peekInt32BE p3 endPtr
    (f4_instanceid, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_rackid, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    (f6_rebalancetimeoutms, p7) <- W.peekInt32BE p6 endPtr
    (f7_topology, p8) <- (do { (flag, pAfterFlag) <- W.peekWord8 p7 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekTopology version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
    (f8_activetasks, p9) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p8 endPtr
    (f9_standbytasks, p10) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p9 endPtr
    (f10_warmuptasks, p11) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p10 endPtr
    (f11_processid, p12) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p11 endPtr
    (f12_userendpoint, p13) <- (do { (flag, pAfterFlag) <- W.peekWord8 p12 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekEndpoint version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
    (f13_clienttags, p14) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekKeyValue version _fp _basePtr p e) p13 endPtr
    (f14_taskoffsets, p15) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekTaskOffset version _fp _basePtr p e) p14 endPtr
    (f15_taskendoffsets, p16) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekTaskOffset version _fp _basePtr p e) p15 endPtr
    (f16_shutdownapplication, p17) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p16 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p17 endPtr
    pure (StreamsGroupHeartbeatRequest { streamsGroupHeartbeatRequestGroupId = f0_groupid, streamsGroupHeartbeatRequestMemberId = f1_memberid, streamsGroupHeartbeatRequestMemberEpoch = f2_memberepoch, streamsGroupHeartbeatRequestEndpointInformationEpoch = f3_endpointinformationepoch, streamsGroupHeartbeatRequestInstanceId = f4_instanceid, streamsGroupHeartbeatRequestRackId = f5_rackid, streamsGroupHeartbeatRequestRebalanceTimeoutMs = f6_rebalancetimeoutms, streamsGroupHeartbeatRequestTopology = f7_topology, streamsGroupHeartbeatRequestActiveTasks = f8_activetasks, streamsGroupHeartbeatRequestStandbyTasks = f9_standbytasks, streamsGroupHeartbeatRequestWarmupTasks = f10_warmuptasks, streamsGroupHeartbeatRequestProcessId = f11_processid, streamsGroupHeartbeatRequestUserEndpoint = f12_userendpoint, streamsGroupHeartbeatRequestClientTags = f13_clienttags, streamsGroupHeartbeatRequestTaskOffsets = f14_taskoffsets, streamsGroupHeartbeatRequestTaskEndOffsets = f15_taskendoffsets, streamsGroupHeartbeatRequestShutdownApplication = f16_shutdownapplication }, pTagsEnd)
  | otherwise = error $ "wirePeek StreamsGroupHeartbeatRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec StreamsGroupHeartbeatRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeStreamsGroupHeartbeatRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeStreamsGroupHeartbeatRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekStreamsGroupHeartbeatRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}