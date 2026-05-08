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