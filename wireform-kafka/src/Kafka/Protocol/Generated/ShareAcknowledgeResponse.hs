{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareAcknowledgeResponse
Description : Kafka ShareAcknowledgeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 79.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareAcknowledgeResponse
  (
    ShareAcknowledgeResponse(..),
    ShareAcknowledgeTopicResponse(..),
    PartitionData(..),
    LeaderIdAndEpoch(..),
    NodeEndpoint(..),
    encodeShareAcknowledgeResponse,
    decodeShareAcknowledgeResponse,
    maxShareAcknowledgeResponseVersion
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


-- | The current leader of the partition.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    serialize (leaderIdAndEpochLeaderId lmsg)
    serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | The topic partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  partitionDataErrorMessage :: !(KafkaString)
,

  -- | The current leader of the partition.

  -- Versions: 0+
  partitionDataCurrentLeader :: !(LeaderIdAndEpoch)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionDataErrorMessage pmsg)) else serialize (partitionDataErrorMessage pmsg)
    encodeLeaderIdAndEpoch version (partitionDataCurrentLeader pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldcurrentleader <- decodeLeaderIdAndEpoch version
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataErrorMessage = fielderrormessage
      ,
      partitionDataCurrentLeader = fieldcurrentleader
      }


-- | The response topics.
data ShareAcknowledgeTopicResponse = ShareAcknowledgeTopicResponse
  {

  -- | The unique topic ID.

  -- Versions: 0+
  shareAcknowledgeTopicResponseTopicId :: !(KafkaUuid)
,

  -- | The topic partitions.

  -- Versions: 0+
  shareAcknowledgeTopicResponsePartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ShareAcknowledgeTopicResponse with version-aware field handling.
encodeShareAcknowledgeTopicResponse :: MonadPut m => E.ApiVersion -> ShareAcknowledgeTopicResponse -> m ()
encodeShareAcknowledgeTopicResponse version smsg =
  do
    serialize (shareAcknowledgeTopicResponseTopicId smsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (shareAcknowledgeTopicResponsePartitions smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ShareAcknowledgeTopicResponse with version-aware field handling.
decodeShareAcknowledgeTopicResponse :: MonadGet m => E.ApiVersion -> m ShareAcknowledgeTopicResponse
decodeShareAcknowledgeTopicResponse version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ShareAcknowledgeTopicResponse
      {
      shareAcknowledgeTopicResponseTopicId = fieldtopicid
      ,
      shareAcknowledgeTopicResponsePartitions = fieldpartitions
      }


-- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 0+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 0+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 0+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 0+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    serialize (nodeEndpointNodeId nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    serialize (nodeEndpointPort nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure NodeEndpoint
      {
      nodeEndpointNodeId = fieldnodeid
      ,
      nodeEndpointHost = fieldhost
      ,
      nodeEndpointPort = fieldport
      ,
      nodeEndpointRack = fieldrack
      }



data ShareAcknowledgeResponse = ShareAcknowledgeResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  shareAcknowledgeResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  shareAcknowledgeResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  shareAcknowledgeResponseErrorMessage :: !(KafkaString)
,

  -- | The response topics.

  -- Versions: 0+
  shareAcknowledgeResponseResponses :: !(KafkaArray (ShareAcknowledgeTopicResponse))
,

  -- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.

  -- Versions: 0+
  shareAcknowledgeResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareAcknowledgeResponse.
maxShareAcknowledgeResponseVersion :: Int16
maxShareAcknowledgeResponseVersion = 2

-- | Encode ShareAcknowledgeResponse with the given API version.
encodeShareAcknowledgeResponse :: MonadPut m => E.ApiVersion -> ShareAcknowledgeResponse -> m ()
encodeShareAcknowledgeResponse version msg
  | version >= 1 && version <= 2 =
    do
      serialize (shareAcknowledgeResponseThrottleTimeMs msg)
      serialize (shareAcknowledgeResponseErrorCode msg)
      serialize (toCompactString (shareAcknowledgeResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeShareAcknowledgeTopicResponse (case P.unKafkaArray (shareAcknowledgeResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNodeEndpoint (case P.unKafkaArray (shareAcknowledgeResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareAcknowledgeResponse with the given API version.
decodeShareAcknowledgeResponse :: MonadGet m => E.ApiVersion -> m ShareAcknowledgeResponse
decodeShareAcknowledgeResponse version
  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeShareAcknowledgeTopicResponse
      fieldnodeendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeNodeEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareAcknowledgeResponse
        {
        shareAcknowledgeResponseThrottleTimeMs = fieldthrottletimems
        ,
        shareAcknowledgeResponseErrorCode = fielderrorcode
        ,
        shareAcknowledgeResponseErrorMessage = fielderrormessage
        ,
        shareAcknowledgeResponseResponses = fieldresponses
        ,
        shareAcknowledgeResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeShareAcknowledgeResponse' / 'decodeShareAcknowledgeResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ShareAcknowledgeResponse where
  wireCodec = Just (WC.serialShimCodec encodeShareAcknowledgeResponse decodeShareAcknowledgeResponse)
  {-# INLINE wireCodec #-}
