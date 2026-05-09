{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StreamsGroupDescribeResponse
Description : Kafka StreamsGroupDescribeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 89.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StreamsGroupDescribeResponse
  (
    StreamsGroupDescribeResponse(..),
    DescribedGroup(..),
    Topology(..),
    Subtopology(..),
    TopicInfo(..),
    Member(..),
    Endpoint(..),
    KeyValue(..),
    TaskOffset(..),
    Assignment(..),
    TopicPartitions(..),
    TaskIds(..),
    encodeStreamsGroupDescribeResponse,
    decodeStreamsGroupDescribeResponse,
    maxStreamsGroupDescribeResponseVersion
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



data TopicPartitions = TopicPartitions
  {

  -- | The topic ID.

  -- Versions: 0+
  topicPartitionsTopicId :: !(KafkaUuid)
,

  -- | The topic name.

  -- Versions: 0+
  topicPartitionsTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  topicPartitionsPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartitions with version-aware field handling.
encodeTopicPartitions :: MonadPut m => E.ApiVersion -> TopicPartitions -> m ()
encodeTopicPartitions version tmsg =
  do
    serialize (topicPartitionsTopicId tmsg)
    if version >= 0 then serialize (toCompactString (topicPartitionsTopicName tmsg)) else serialize (topicPartitionsTopicName tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicPartitionsPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicPartitions with version-aware field handling.
decodeTopicPartitions :: MonadGet m => E.ApiVersion -> m TopicPartitions
decodeTopicPartitions version =
  do
    fieldtopicid <- deserialize
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicPartitions
      {
      topicPartitionsTopicId = fieldtopicid
      ,
      topicPartitionsTopicName = fieldtopicname
      ,
      topicPartitionsPartitions = fieldpartitions
      }



data Assignment = Assignment
  {

  -- | Active tasks for this client.

  -- Versions: 0+
  assignmentActiveTasks :: !(KafkaArray (TaskIds))
,

  -- | Standby tasks for this client.

  -- Versions: 0+
  assignmentStandbyTasks :: !(KafkaArray (TaskIds))
,

  -- | Warm-up tasks for this client. 

  -- Versions: 0+
  assignmentWarmupTasks :: !(KafkaArray (TaskIds))

  }
  deriving (Eq, Show, Generic)


-- | Encode Assignment with version-aware field handling.
encodeAssignment :: MonadPut m => E.ApiVersion -> Assignment -> m ()
encodeAssignment version amsg =
  do
    E.encodeVersionedArray version 0 encodeTaskIds (case P.unKafkaArray (assignmentActiveTasks amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTaskIds (case P.unKafkaArray (assignmentStandbyTasks amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTaskIds (case P.unKafkaArray (assignmentWarmupTasks amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Assignment with version-aware field handling.
decodeAssignment :: MonadGet m => E.ApiVersion -> m Assignment
decodeAssignment version =
  do
    fieldactivetasks <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTaskIds
    fieldstandbytasks <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTaskIds
    fieldwarmuptasks <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTaskIds
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Assignment
      {
      assignmentActiveTasks = fieldactivetasks
      ,
      assignmentStandbyTasks = fieldstandbytasks
      ,
      assignmentWarmupTasks = fieldwarmuptasks
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


-- | The subtopologies of the streams application. This contains the configured subtopologies, where the number of partitions are set and any regular expressions are resolved to actual topics. Null if the 
data Subtopology = Subtopology
  {

  -- | String to uniquely identify the subtopology.

  -- Versions: 0+
  subtopologySubtopologyId :: !(KafkaString)
,

  -- | The topics the subtopology reads from.

  -- Versions: 0+
  subtopologySourceTopics :: !(KafkaArray (KafkaString))
,

  -- | The repartition topics the subtopology writes to.

  -- Versions: 0+
  subtopologyRepartitionSinkTopics :: !(KafkaArray (KafkaString))
,

  -- | The set of state changelog topics associated with this subtopology. Created automatically.

  -- Versions: 0+
  subtopologyStateChangelogTopics :: !(KafkaArray (TopicInfo))
,

  -- | The set of source topics that are internally created repartition topics. Created automatically.

  -- Versions: 0+
  subtopologyRepartitionSourceTopics :: !(KafkaArray (TopicInfo))

  }
  deriving (Eq, Show, Generic)


-- | Encode Subtopology with version-aware field handling.
encodeSubtopology :: MonadPut m => E.ApiVersion -> Subtopology -> m ()
encodeSubtopology version smsg =
  do
    if version >= 0 then serialize (toCompactString (subtopologySubtopologyId smsg)) else serialize (subtopologySubtopologyId smsg)
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (subtopologySourceTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (subtopologyRepartitionSinkTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTopicInfo (case P.unKafkaArray (subtopologyStateChangelogTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTopicInfo (case P.unKafkaArray (subtopologyRepartitionSourceTopics smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Subtopology with version-aware field handling.
decodeSubtopology :: MonadGet m => E.ApiVersion -> m Subtopology
decodeSubtopology version =
  do
    fieldsubtopologyid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldrepartitionsinktopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
    fieldstatechangelogtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicInfo
    fieldrepartitionsourcetopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicInfo
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Subtopology
      {
      subtopologySubtopologyId = fieldsubtopologyid
      ,
      subtopologySourceTopics = fieldsourcetopics
      ,
      subtopologyRepartitionSinkTopics = fieldrepartitionsinktopics
      ,
      subtopologyStateChangelogTopics = fieldstatechangelogtopics
      ,
      subtopologyRepartitionSourceTopics = fieldrepartitionsourcetopics
      }


-- | The topology metadata currently initialized for the streams application. Can be null in case of a describe error.
data Topology = Topology
  {

  -- | The epoch of the currently initialized topology for this group.

  -- Versions: 0+
  topologyEpoch :: !(Int32)
,

  -- | The subtopologies of the streams application. This contains the configured subtopologies, where the 

  -- Versions: 0+
  topologySubtopologies :: !(KafkaArray (Subtopology))

  }
  deriving (Eq, Show, Generic)


-- | Encode Topology with version-aware field handling.
encodeTopology :: MonadPut m => E.ApiVersion -> Topology -> m ()
encodeTopology version tmsg =
  do
    serialize (topologyEpoch tmsg)
    E.encodeVersionedNullableArray version 0 encodeSubtopology (topologySubtopologies tmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Topology with version-aware field handling.
decodeTopology :: MonadGet m => E.ApiVersion -> m Topology
decodeTopology version =
  do
    fieldepoch <- deserialize
    fieldsubtopologies <- E.decodeVersionedNullableArray version 0 decodeSubtopology
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Topology
      {
      topologyEpoch = fieldepoch
      ,
      topologySubtopologies = fieldsubtopologies
      }


-- | The members.
data Member = Member
  {

  -- | The member ID.

  -- Versions: 0+
  memberMemberId :: !(KafkaString)
,

  -- | The member epoch.

  -- Versions: 0+
  memberMemberEpoch :: !(Int32)
,

  -- | The member instance ID for static membership.

  -- Versions: 0+
  memberInstanceId :: !(KafkaString)
,

  -- | The rack ID.

  -- Versions: 0+
  memberRackId :: !(KafkaString)
,

  -- | The client ID.

  -- Versions: 0+
  memberClientId :: !(KafkaString)
,

  -- | The client host.

  -- Versions: 0+
  memberClientHost :: !(KafkaString)
,

  -- | The epoch of the topology on the client.

  -- Versions: 0+
  memberTopologyEpoch :: !(Int32)
,

  -- | Identity of the streams instance that may have multiple clients. 

  -- Versions: 0+
  memberProcessId :: !(KafkaString)
,

  -- | User-defined endpoint for Interactive Queries. Null if not defined for this client.

  -- Versions: 0+
  memberUserEndpoint :: !(Nullable (Endpoint))
,

  -- | Used for rack-aware assignment algorithm.

  -- Versions: 0+
  memberClientTags :: !(KafkaArray (KeyValue))
,

  -- | Cumulative changelog offsets for tasks.

  -- Versions: 0+
  memberTaskOffsets :: !(KafkaArray (TaskOffset))
,

  -- | Cumulative changelog end offsets for tasks.

  -- Versions: 0+
  memberTaskEndOffsets :: !(KafkaArray (TaskOffset))
,

  -- | The current assignment.

  -- Versions: 0+
  memberAssignment :: !(Assignment)
,

  -- | The target assignment.

  -- Versions: 0+
  memberTargetAssignment :: !(Assignment)
,

  -- | True for classic members that have not been upgraded yet.

  -- Versions: 0+
  memberIsClassic :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode Member with version-aware field handling.
encodeMember :: MonadPut m => E.ApiVersion -> Member -> m ()
encodeMember version mmsg =
  do
    if version >= 0 then serialize (toCompactString (memberMemberId mmsg)) else serialize (memberMemberId mmsg)
    serialize (memberMemberEpoch mmsg)
    if version >= 0 then serialize (toCompactString (memberInstanceId mmsg)) else serialize (memberInstanceId mmsg)
    if version >= 0 then serialize (toCompactString (memberRackId mmsg)) else serialize (memberRackId mmsg)
    if version >= 0 then serialize (toCompactString (memberClientId mmsg)) else serialize (memberClientId mmsg)
    if version >= 0 then serialize (toCompactString (memberClientHost mmsg)) else serialize (memberClientHost mmsg)
    serialize (memberTopologyEpoch mmsg)
    if version >= 0 then serialize (toCompactString (memberProcessId mmsg)) else serialize (memberProcessId mmsg)
    case (memberUserEndpoint mmsg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeEndpoint version val } }
    E.encodeVersionedArray version 0 encodeKeyValue (case P.unKafkaArray (memberClientTags mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTaskOffset (case P.unKafkaArray (memberTaskOffsets mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTaskOffset (case P.unKafkaArray (memberTaskEndOffsets mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    encodeAssignment version (memberAssignment mmsg)
    encodeAssignment version (memberTargetAssignment mmsg)
    serialize (memberIsClassic mmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Member with version-aware field handling.
decodeMember :: MonadGet m => E.ApiVersion -> m Member
decodeMember version =
  do
    fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldmemberepoch <- deserialize
    fieldinstanceid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldrackid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldclientid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldclienthost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopologyepoch <- deserialize
    fieldprocessid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fielduserendpoint <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeEndpoint version; _ -> fail "Invalid nullable flag" } }
    fieldclienttags <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeKeyValue
    fieldtaskoffsets <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTaskOffset
    fieldtaskendoffsets <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTaskOffset
    fieldassignment <- decodeAssignment version
    fieldtargetassignment <- decodeAssignment version
    fieldisclassic <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Member
      {
      memberMemberId = fieldmemberid
      ,
      memberMemberEpoch = fieldmemberepoch
      ,
      memberInstanceId = fieldinstanceid
      ,
      memberRackId = fieldrackid
      ,
      memberClientId = fieldclientid
      ,
      memberClientHost = fieldclienthost
      ,
      memberTopologyEpoch = fieldtopologyepoch
      ,
      memberProcessId = fieldprocessid
      ,
      memberUserEndpoint = fielduserendpoint
      ,
      memberClientTags = fieldclienttags
      ,
      memberTaskOffsets = fieldtaskoffsets
      ,
      memberTaskEndOffsets = fieldtaskendoffsets
      ,
      memberAssignment = fieldassignment
      ,
      memberTargetAssignment = fieldtargetassignment
      ,
      memberIsClassic = fieldisclassic
      }


-- | Each described group.
data DescribedGroup = DescribedGroup
  {

  -- | The describe error, or 0 if there was no error.

  -- Versions: 0+
  describedGroupErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  describedGroupErrorMessage :: !(KafkaString)
,

  -- | The group ID string.

  -- Versions: 0+
  describedGroupGroupId :: !(KafkaString)
,

  -- | The group state string, or the empty string.

  -- Versions: 0+
  describedGroupGroupState :: !(KafkaString)
,

  -- | The group epoch.

  -- Versions: 0+
  describedGroupGroupEpoch :: !(Int32)
,

  -- | The assignment epoch.

  -- Versions: 0+
  describedGroupAssignmentEpoch :: !(Int32)
,

  -- | The topology metadata currently initialized for the streams application. Can be null in case of a de

  -- Versions: 0+
  describedGroupTopology :: !(Nullable (Topology))
,

  -- | The members.

  -- Versions: 0+
  describedGroupMembers :: !(KafkaArray (Member))
,

  -- | 32-bit bitfield to represent authorized operations for this group.

  -- Versions: 0+
  describedGroupAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedGroup with version-aware field handling.
encodeDescribedGroup :: MonadPut m => E.ApiVersion -> DescribedGroup -> m ()
encodeDescribedGroup version dmsg =
  do
    serialize (describedGroupErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupErrorMessage dmsg)) else serialize (describedGroupErrorMessage dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupGroupId dmsg)) else serialize (describedGroupGroupId dmsg)
    if version >= 0 then serialize (toCompactString (describedGroupGroupState dmsg)) else serialize (describedGroupGroupState dmsg)
    serialize (describedGroupGroupEpoch dmsg)
    serialize (describedGroupAssignmentEpoch dmsg)
    case (describedGroupTopology dmsg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeTopology version val } }
    E.encodeVersionedArray version 0 encodeMember (case P.unKafkaArray (describedGroupMembers dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    serialize (describedGroupAuthorizedOperations dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroup with version-aware field handling.
decodeDescribedGroup :: MonadGet m => E.ApiVersion -> m DescribedGroup
decodeDescribedGroup version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupstate <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupepoch <- deserialize
    fieldassignmentepoch <- deserialize
    fieldtopology <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeTopology version; _ -> fail "Invalid nullable flag" } }
    fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeMember
    fieldauthorizedoperations <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedGroup
      {
      describedGroupErrorCode = fielderrorcode
      ,
      describedGroupErrorMessage = fielderrormessage
      ,
      describedGroupGroupId = fieldgroupid
      ,
      describedGroupGroupState = fieldgroupstate
      ,
      describedGroupGroupEpoch = fieldgroupepoch
      ,
      describedGroupAssignmentEpoch = fieldassignmentepoch
      ,
      describedGroupTopology = fieldtopology
      ,
      describedGroupMembers = fieldmembers
      ,
      describedGroupAuthorizedOperations = fieldauthorizedoperations
      }



data StreamsGroupDescribeResponse = StreamsGroupDescribeResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  streamsGroupDescribeResponseThrottleTimeMs :: !(Int32)
,

  -- | Each described group.

  -- Versions: 0+
  streamsGroupDescribeResponseGroups :: !(KafkaArray (DescribedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StreamsGroupDescribeResponse.
maxStreamsGroupDescribeResponseVersion :: Int16
maxStreamsGroupDescribeResponseVersion = 0

-- | Encode StreamsGroupDescribeResponse with the given API version.
encodeStreamsGroupDescribeResponse :: MonadPut m => E.ApiVersion -> StreamsGroupDescribeResponse -> m ()
encodeStreamsGroupDescribeResponse version msg
  | version == 0 =
    do
      serialize (streamsGroupDescribeResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeDescribedGroup (case P.unKafkaArray (streamsGroupDescribeResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode StreamsGroupDescribeResponse with the given API version.
decodeStreamsGroupDescribeResponse :: MonadGet m => E.ApiVersion -> m StreamsGroupDescribeResponse
decodeStreamsGroupDescribeResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribedGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure StreamsGroupDescribeResponse
        {
        streamsGroupDescribeResponseThrottleTimeMs = fieldthrottletimems
        ,
        streamsGroupDescribeResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeStreamsGroupDescribeResponse' / 'decodeStreamsGroupDescribeResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec StreamsGroupDescribeResponse where
  wireCodec = Just (WC.serialShimCodec encodeStreamsGroupDescribeResponse decodeStreamsGroupDescribeResponse)
  {-# INLINE wireCodec #-}
