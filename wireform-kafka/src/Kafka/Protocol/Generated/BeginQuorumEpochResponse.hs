{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BeginQuorumEpochResponse
Description : Kafka BeginQuorumEpochResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 53.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BeginQuorumEpochResponse
  (
    BeginQuorumEpochResponse(..),
    TopicData(..),
    PartitionData(..),
    NodeEndpoint(..),
    encodeBeginQuorumEpochResponse,
    decodeBeginQuorumEpochResponse,
    maxBeginQuorumEpochResponseVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The error code for this partition.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      }


-- | The topic data.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partition data.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 1 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 1 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodePartitionData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }


-- | Endpoints for all leaders enumerated in PartitionData.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 1+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 1+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 1+
  nodeEndpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    when (version >= 1) $
      serialize (nodeEndpointNodeId nmsg)
    when (version >= 1) $
      if version >= 1 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    when (version >= 1) $
      serialize (nodeEndpointPort nmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- if version >= 1
      then deserialize
      else pure (0)
    fieldhost <- if version >= 1
      then if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure NodeEndpoint
      {
      nodeEndpointNodeId = fieldnodeid
      ,
      nodeEndpointHost = fieldhost
      ,
      nodeEndpointPort = fieldport
      }



data BeginQuorumEpochResponse = BeginQuorumEpochResponse
  {

  -- | The top level error code.

  -- Versions: 0+
  beginQuorumEpochResponseErrorCode :: !(Int16)
,

  -- | The topic data.

  -- Versions: 0+
  beginQuorumEpochResponseTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for all leaders enumerated in PartitionData.

  -- Versions: 1+
  beginQuorumEpochResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BeginQuorumEpochResponse.
maxBeginQuorumEpochResponseVersion :: Int16
maxBeginQuorumEpochResponseVersion = 1

-- | Encode BeginQuorumEpochResponse with the given API version.
encodeBeginQuorumEpochResponse :: MonadPut m => E.ApiVersion -> BeginQuorumEpochResponse -> m ()
encodeBeginQuorumEpochResponse version msg
  | version == 0 =
    do
      serialize (beginQuorumEpochResponseErrorCode msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (beginQuorumEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (beginQuorumEpochResponseErrorCode msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (beginQuorumEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BeginQuorumEpochResponse with the given API version.
decodeBeginQuorumEpochResponse :: MonadGet m => E.ApiVersion -> m BeginQuorumEpochResponse
decodeBeginQuorumEpochResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      pure BeginQuorumEpochResponse
        {
        beginQuorumEpochResponseErrorCode = fielderrorcode
        ,
        beginQuorumEpochResponseTopics = fieldtopics
        ,
        beginQuorumEpochResponseNodeEndpoints = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BeginQuorumEpochResponse
        {
        beginQuorumEpochResponseErrorCode = fielderrorcode
        ,
        beginQuorumEpochResponseTopics = fieldtopics
        ,
        beginQuorumEpochResponseNodeEndpoints = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec BeginQuorumEpochResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
