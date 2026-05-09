{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.StreamsGroupHeartbeatResponse
Description : Kafka StreamsGroupHeartbeatResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 88.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.StreamsGroupHeartbeatResponse
  (
    StreamsGroupHeartbeatResponse(..),
    Status(..),
    TaskIds(..),
    EndpointToPartitions(..),
    Endpoint(..),
    TopicPartition(..),
    encodeStreamsGroupHeartbeatResponse,
    decodeStreamsGroupHeartbeatResponse,
    maxStreamsGroupHeartbeatResponseVersion
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


data Status = Status
  {

  -- | A code to indicate that a particular status is active for the group membership

  -- Versions: 0+
  statusStatusCode :: !(Int8)
,

  -- | A string representation of the status.

  -- Versions: 0+
  statusStatusDetail :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode Status with version-aware field handling.
encodeStatus :: MonadPut m => E.ApiVersion -> Status -> m ()
encodeStatus version smsg =
  do
    serialize (statusStatusCode smsg)
    if version >= 0 then serialize (toCompactString (statusStatusDetail smsg)) else serialize (statusStatusDetail smsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Status with version-aware field handling.
decodeStatus :: MonadGet m => E.ApiVersion -> m Status
decodeStatus version =
  do
    fieldstatuscode <- deserialize
    fieldstatusdetail <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Status
      {
      statusStatusCode = fieldstatuscode
      ,
      statusStatusDetail = fieldstatusdetail
      }



data TopicPartition = TopicPartition
  {

  -- | topic name

  -- Versions: 0+
  topicPartitionTopic :: !(KafkaString)
,

  -- | partitions

  -- Versions: 0+
  topicPartitionPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartition with version-aware field handling.
encodeTopicPartition :: MonadPut m => E.ApiVersion -> TopicPartition -> m ()
encodeTopicPartition version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicPartitionTopic tmsg)) else serialize (topicPartitionTopic tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicPartitionPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicPartition with version-aware field handling.
decodeTopicPartition :: MonadGet m => E.ApiVersion -> m TopicPartition
decodeTopicPartition version =
  do
    fieldtopic <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicPartition
      {
      topicPartitionTopic = fieldtopic
      ,
      topicPartitionPartitions = fieldpartitions
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


-- | Global assignment information used for IQ. Null if unchanged since last heartbeat.
data EndpointToPartitions = EndpointToPartitions
  {

  -- | User-defined endpoint to connect to the node

  -- Versions: 0+
  endpointToPartitionsUserEndpoint :: !(Endpoint)
,

  -- | All topic partitions materialized by active tasks on the node

  -- Versions: 0+
  endpointToPartitionsActivePartitions :: !(KafkaArray (TopicPartition))
,

  -- | All topic partitions materialized by standby tasks on the node

  -- Versions: 0+
  endpointToPartitionsStandbyPartitions :: !(KafkaArray (TopicPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode EndpointToPartitions with version-aware field handling.
encodeEndpointToPartitions :: MonadPut m => E.ApiVersion -> EndpointToPartitions -> m ()
encodeEndpointToPartitions version emsg =
  do
    encodeEndpoint version (endpointToPartitionsUserEndpoint emsg)
    E.encodeVersionedArray version 0 encodeTopicPartition (case P.unKafkaArray (endpointToPartitionsActivePartitions emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeTopicPartition (case P.unKafkaArray (endpointToPartitionsStandbyPartitions emsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EndpointToPartitions with version-aware field handling.
decodeEndpointToPartitions :: MonadGet m => E.ApiVersion -> m EndpointToPartitions
decodeEndpointToPartitions version =
  do
    fielduserendpoint <- decodeEndpoint version
    fieldactivepartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicPartition
    fieldstandbypartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicPartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EndpointToPartitions
      {
      endpointToPartitionsUserEndpoint = fielduserendpoint
      ,
      endpointToPartitionsActivePartitions = fieldactivepartitions
      ,
      endpointToPartitionsStandbyPartitions = fieldstandbypartitions
      }



data StreamsGroupHeartbeatResponse = StreamsGroupHeartbeatResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  streamsGroupHeartbeatResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error

  -- Versions: 0+
  streamsGroupHeartbeatResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  streamsGroupHeartbeatResponseErrorMessage :: !(KafkaString)
,

  -- | The member id is always generated by the streams consumer.

  -- Versions: 0+
  streamsGroupHeartbeatResponseMemberId :: !(KafkaString)
,

  -- | The member epoch.

  -- Versions: 0+
  streamsGroupHeartbeatResponseMemberEpoch :: !(Int32)
,

  -- | The heartbeat interval in milliseconds.

  -- Versions: 0+
  streamsGroupHeartbeatResponseHeartbeatIntervalMs :: !(Int32)
,

  -- | The maximal lag a warm-up task can have to be considered caught-up.

  -- Versions: 0+
  streamsGroupHeartbeatResponseAcceptableRecoveryLag :: !(Int32)
,

  -- | The interval in which the task changelog offsets on a client are updated on the broker. The offsets 

  -- Versions: 0+
  streamsGroupHeartbeatResponseTaskOffsetIntervalMs :: !(Int32)
,

  -- | Indicate zero or more status for the group.  Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatResponseStatus :: !(KafkaArray (Status))
,

  -- | Assigned active tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatResponseActiveTasks :: !(KafkaArray (TaskIds))
,

  -- | Assigned standby tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatResponseStandbyTasks :: !(KafkaArray (TaskIds))
,

  -- | Assigned warm-up tasks for this client. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatResponseWarmupTasks :: !(KafkaArray (TaskIds))
,

  -- | The endpoint epoch set in the response

  -- Versions: 0+
  streamsGroupHeartbeatResponseEndpointInformationEpoch :: !(Int32)
,

  -- | Global assignment information used for IQ. Null if unchanged since last heartbeat.

  -- Versions: 0+
  streamsGroupHeartbeatResponsePartitionsByUserEndpoint :: !(KafkaArray (EndpointToPartitions))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for StreamsGroupHeartbeatResponse.
maxStreamsGroupHeartbeatResponseVersion :: Int16
maxStreamsGroupHeartbeatResponseVersion = 0

-- | Encode StreamsGroupHeartbeatResponse with the given API version.
encodeStreamsGroupHeartbeatResponse :: MonadPut m => E.ApiVersion -> StreamsGroupHeartbeatResponse -> m ()
encodeStreamsGroupHeartbeatResponse version msg
  | version == 0 =
    do
      serialize (streamsGroupHeartbeatResponseThrottleTimeMs msg)
      serialize (streamsGroupHeartbeatResponseErrorCode msg)
      serialize (toCompactString (streamsGroupHeartbeatResponseErrorMessage msg))
      serialize (toCompactString (streamsGroupHeartbeatResponseMemberId msg))
      serialize (streamsGroupHeartbeatResponseMemberEpoch msg)
      serialize (streamsGroupHeartbeatResponseHeartbeatIntervalMs msg)
      serialize (streamsGroupHeartbeatResponseAcceptableRecoveryLag msg)
      serialize (streamsGroupHeartbeatResponseTaskOffsetIntervalMs msg)
      E.encodeVersionedNullableArray version 0 encodeStatus (streamsGroupHeartbeatResponseStatus msg)
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatResponseActiveTasks msg)
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatResponseStandbyTasks msg)
      E.encodeVersionedNullableArray version 0 encodeTaskIds (streamsGroupHeartbeatResponseWarmupTasks msg)
      serialize (streamsGroupHeartbeatResponseEndpointInformationEpoch msg)
      E.encodeVersionedNullableArray version 0 encodeEndpointToPartitions (streamsGroupHeartbeatResponsePartitionsByUserEndpoint msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode StreamsGroupHeartbeatResponse with the given API version.
decodeStreamsGroupHeartbeatResponse :: MonadGet m => E.ApiVersion -> m StreamsGroupHeartbeatResponse
decodeStreamsGroupHeartbeatResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldmemberepoch <- deserialize
      fieldheartbeatintervalms <- deserialize
      fieldacceptablerecoverylag <- deserialize
      fieldtaskoffsetintervalms <- deserialize
      fieldstatus <- E.decodeVersionedNullableArray version 0 decodeStatus
      fieldactivetasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldstandbytasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldwarmuptasks <- E.decodeVersionedNullableArray version 0 decodeTaskIds
      fieldendpointinformationepoch <- deserialize
      fieldpartitionsbyuserendpoint <- E.decodeVersionedNullableArray version 0 decodeEndpointToPartitions
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure StreamsGroupHeartbeatResponse
        {
        streamsGroupHeartbeatResponseThrottleTimeMs = fieldthrottletimems
        ,
        streamsGroupHeartbeatResponseErrorCode = fielderrorcode
        ,
        streamsGroupHeartbeatResponseErrorMessage = fielderrormessage
        ,
        streamsGroupHeartbeatResponseMemberId = fieldmemberid
        ,
        streamsGroupHeartbeatResponseMemberEpoch = fieldmemberepoch
        ,
        streamsGroupHeartbeatResponseHeartbeatIntervalMs = fieldheartbeatintervalms
        ,
        streamsGroupHeartbeatResponseAcceptableRecoveryLag = fieldacceptablerecoverylag
        ,
        streamsGroupHeartbeatResponseTaskOffsetIntervalMs = fieldtaskoffsetintervalms
        ,
        streamsGroupHeartbeatResponseStatus = fieldstatus
        ,
        streamsGroupHeartbeatResponseActiveTasks = fieldactivetasks
        ,
        streamsGroupHeartbeatResponseStandbyTasks = fieldstandbytasks
        ,
        streamsGroupHeartbeatResponseWarmupTasks = fieldwarmuptasks
        ,
        streamsGroupHeartbeatResponseEndpointInformationEpoch = fieldendpointinformationepoch
        ,
        streamsGroupHeartbeatResponsePartitionsByUserEndpoint = fieldpartitionsbyuserendpoint
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeStreamsGroupHeartbeatResponse' / 'decodeStreamsGroupHeartbeatResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec StreamsGroupHeartbeatResponse where
  wireCodec = Just (WC.serialShimCodec encodeStreamsGroupHeartbeatResponse decodeStreamsGroupHeartbeatResponse)
  {-# INLINE wireCodec #-}
