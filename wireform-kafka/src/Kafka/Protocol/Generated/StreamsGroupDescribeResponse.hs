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
    TaskIds(..),
    maxStreamsGroupDescribeResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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

-- | KafkaMessage instance for StreamsGroupDescribeResponse.
instance KafkaMessage StreamsGroupDescribeResponse where
  messageApiKey = 89
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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

-- | Worst-case wire size of a Assignment.
wireMaxSizeAssignment :: Int -> Assignment -> Int
wireMaxSizeAssignment _version msg =
  0
  + (5 + (case P.unKafkaArray (assignmentActiveTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (assignmentStandbyTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (assignmentWarmupTasks msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskIds _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Assignment.
wirePokeAssignment :: Int -> Ptr Word8 -> Assignment -> IO (Ptr Word8)
wirePokeAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTaskIds version p x) p0 (assignmentActiveTasks msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTaskIds version p x) p1 (assignmentStandbyTasks msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTaskIds version p x) p2 (assignmentWarmupTasks msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Assignment.
wirePeekAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Assignment, Ptr Word8)
wirePeekAssignment version _fp _basePtr p0 endPtr = do
  (f0_activetasks, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p0 endPtr
  (f1_standbytasks, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p1 endPtr
  (f2_warmuptasks, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTaskIds version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Assignment { assignmentActiveTasks = f0_activetasks, assignmentStandbyTasks = f1_standbytasks, assignmentWarmupTasks = f2_warmuptasks }, pTagsEnd)

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

-- | Worst-case wire size of a Subtopology.
wireMaxSizeSubtopology :: Int -> Subtopology -> Int
wireMaxSizeSubtopology _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (subtopologySubtopologyId msg))
  + (5 + (case P.unKafkaArray (subtopologySourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyRepartitionSinkTopics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyStateChangelogTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicInfo _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (subtopologyRepartitionSourceTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicInfo _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Subtopology.
wirePokeSubtopology :: Int -> Ptr Word8 -> Subtopology -> IO (Ptr Word8)
wirePokeSubtopology version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (subtopologySubtopologyId msg))
  p2 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p1 (subtopologySourceTopics msg)
  p3 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p2 (subtopologyRepartitionSinkTopics msg)
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicInfo version p x) p3 (subtopologyStateChangelogTopics msg)
  p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicInfo version p x) p4 (subtopologyRepartitionSourceTopics msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for Subtopology.
wirePeekSubtopology :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Subtopology, Ptr Word8)
wirePeekSubtopology version _fp _basePtr p0 endPtr = do
  (f0_subtopologyid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_sourcetopics, p2) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p1 endPtr
  (f2_repartitionsinktopics, p3) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p2 endPtr
  (f3_statechangelogtopics, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicInfo version _fp _basePtr p e) p3 endPtr
  (f4_repartitionsourcetopics, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicInfo version _fp _basePtr p e) p4 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (Subtopology { subtopologySubtopologyId = f0_subtopologyid, subtopologySourceTopics = f1_sourcetopics, subtopologyRepartitionSinkTopics = f2_repartitionsinktopics, subtopologyStateChangelogTopics = f3_statechangelogtopics, subtopologyRepartitionSourceTopics = f4_repartitionsourcetopics }, pTagsEnd)

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
  p2 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeSubtopology version p x) p1 (topologySubtopologies msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Topology.
wirePeekTopology :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Topology, Ptr Word8)
wirePeekTopology version _fp _basePtr p0 endPtr = do
  (f0_epoch, p1) <- W.peekInt32BE p0 endPtr
  (f1_subtopologies, p2) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekSubtopology version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Topology { topologyEpoch = f0_epoch, topologySubtopologies = f1_subtopologies }, pTagsEnd)

-- | Worst-case wire size of a Member.
wireMaxSizeMember :: Int -> Member -> Int
wireMaxSizeMember _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (memberMemberId msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (memberInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberRackId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberClientId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberClientHost msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (memberProcessId msg))
  + (case (memberUserEndpoint msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeEndpoint _version s })
  + (5 + (case P.unKafkaArray (memberClientTags msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeKeyValue _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (memberTaskOffsets msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskOffset _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (memberTaskEndOffsets msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTaskOffset _version x ) v); P.Null -> 0 }))
  + wireMaxSizeAssignment _version (memberAssignment msg)
  + wireMaxSizeAssignment _version (memberTargetAssignment msg)
  + 1
  + 1

-- | Direct-poke encoder for Member.
wirePokeMember :: Int -> Ptr Word8 -> Member -> IO (Ptr Word8)
wirePokeMember version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (memberMemberId msg))
  p2 <- W.pokeInt32BE p1 (memberMemberEpoch msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (memberInstanceId msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (memberRackId msg))
  p5 <- WP.pokeCompactString p4 (P.toCompactString (memberClientId msg))
  p6 <- WP.pokeCompactString p5 (P.toCompactString (memberClientHost msg))
  p7 <- W.pokeInt32BE p6 (memberTopologyEpoch msg)
  p8 <- WP.pokeCompactString p7 (P.toCompactString (memberProcessId msg))
  p9 <- (case (memberUserEndpoint msg) of { P.Null -> W.pokeWord8 p8 0; P.NotNull s -> W.pokeWord8 p8 1 >>= \p' -> wirePokeEndpoint version p' s })
  p10 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeKeyValue version p x) p9 (memberClientTags msg)
  p11 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTaskOffset version p x) p10 (memberTaskOffsets msg)
  p12 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTaskOffset version p x) p11 (memberTaskEndOffsets msg)
  p13 <- wirePokeAssignment version p12 (memberAssignment msg)
  p14 <- wirePokeAssignment version p13 (memberTargetAssignment msg)
  p15 <- W.pokeWord8 p14 (if (memberIsClassic msg) then 1 else 0)
  if version >= 0 then WP.pokeEmptyTaggedFields p15 else pure p15

-- | Direct-poke decoder for Member.
wirePeekMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Member, Ptr Word8)
wirePeekMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_memberepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_instanceid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_rackid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_clientid, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
  (f5_clienthost, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
  (f6_topologyepoch, p7) <- W.peekInt32BE p6 endPtr
  (f7_processid, p8) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr
  (f8_userendpoint, p9) <- (do { (flag, pAfterFlag) <- W.peekWord8 p8 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekEndpoint version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
  (f9_clienttags, p10) <- WP.peekVersionedArray version 0 (\p e -> wirePeekKeyValue version _fp _basePtr p e) p9 endPtr
  (f10_taskoffsets, p11) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTaskOffset version _fp _basePtr p e) p10 endPtr
  (f11_taskendoffsets, p12) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTaskOffset version _fp _basePtr p e) p11 endPtr
  (f12_assignment, p13) <- wirePeekAssignment version _fp _basePtr p12 endPtr
  (f13_targetassignment, p14) <- wirePeekAssignment version _fp _basePtr p13 endPtr
  (f14_isclassic, p15) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p14 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p15 endPtr else pure p15
  pure (Member { memberMemberId = f0_memberid, memberMemberEpoch = f1_memberepoch, memberInstanceId = f2_instanceid, memberRackId = f3_rackid, memberClientId = f4_clientid, memberClientHost = f5_clienthost, memberTopologyEpoch = f6_topologyepoch, memberProcessId = f7_processid, memberUserEndpoint = f8_userendpoint, memberClientTags = f9_clienttags, memberTaskOffsets = f10_taskoffsets, memberTaskEndOffsets = f11_taskendoffsets, memberAssignment = f12_assignment, memberTargetAssignment = f13_targetassignment, memberIsClassic = f14_isclassic }, pTagsEnd)

-- | Worst-case wire size of a DescribedGroup.
wireMaxSizeDescribedGroup :: Int -> DescribedGroup -> Int
wireMaxSizeDescribedGroup _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describedGroupErrorMessage msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupState msg))
  + 4
  + 4
  + (case (describedGroupTopology msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeTopology _version s })
  + (5 + (case P.unKafkaArray (describedGroupMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMember _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribedGroup.
wirePokeDescribedGroup :: Int -> Ptr Word8 -> DescribedGroup -> IO (Ptr Word8)
wirePokeDescribedGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describedGroupErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedGroupErrorMessage msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describedGroupGroupId msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describedGroupGroupState msg))
  p5 <- W.pokeInt32BE p4 (describedGroupGroupEpoch msg)
  p6 <- W.pokeInt32BE p5 (describedGroupAssignmentEpoch msg)
  p7 <- (case (describedGroupTopology msg) of { P.Null -> W.pokeWord8 p6 0; P.NotNull s -> W.pokeWord8 p6 1 >>= \p' -> wirePokeTopology version p' s })
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeMember version p x) p7 (describedGroupMembers msg)
  p9 <- W.pokeInt32BE p8 (describedGroupAuthorizedOperations msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p9 else pure p9

-- | Direct-poke decoder for DescribedGroup.
wirePeekDescribedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroup, Ptr Word8)
wirePeekDescribedGroup version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_groupid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_groupstate, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_groupepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_assignmentepoch, p6) <- W.peekInt32BE p5 endPtr
  (f6_topology, p7) <- (do { (flag, pAfterFlag) <- W.peekWord8 p6 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekTopology version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
  (f7_members, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekMember version _fp _basePtr p e) p7 endPtr
  (f8_authorizedoperations, p9) <- W.peekInt32BE p8 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p9 endPtr else pure p9
  pure (DescribedGroup { describedGroupErrorCode = f0_errorcode, describedGroupErrorMessage = f1_errormessage, describedGroupGroupId = f2_groupid, describedGroupGroupState = f3_groupstate, describedGroupGroupEpoch = f4_groupepoch, describedGroupAssignmentEpoch = f5_assignmentepoch, describedGroupTopology = f6_topology, describedGroupMembers = f7_members, describedGroupAuthorizedOperations = f8_authorizedoperations }, pTagsEnd)

-- | Worst-case wire size of a StreamsGroupDescribeResponse.
wireMaxSizeStreamsGroupDescribeResponse :: Int -> StreamsGroupDescribeResponse -> Int
wireMaxSizeStreamsGroupDescribeResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (streamsGroupDescribeResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for StreamsGroupDescribeResponse.
wirePokeStreamsGroupDescribeResponse :: Int -> Ptr Word8 -> StreamsGroupDescribeResponse -> IO (Ptr Word8)
wirePokeStreamsGroupDescribeResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (streamsGroupDescribeResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribedGroup version p x) p1 (streamsGroupDescribeResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke StreamsGroupDescribeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for StreamsGroupDescribeResponse.
wirePeekStreamsGroupDescribeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StreamsGroupDescribeResponse, Ptr Word8)
wirePeekStreamsGroupDescribeResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (StreamsGroupDescribeResponse { streamsGroupDescribeResponseThrottleTimeMs = f0_throttletimems, streamsGroupDescribeResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek StreamsGroupDescribeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec StreamsGroupDescribeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeStreamsGroupDescribeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeStreamsGroupDescribeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekStreamsGroupDescribeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}