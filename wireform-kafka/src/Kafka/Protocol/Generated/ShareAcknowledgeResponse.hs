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
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

  -- | The time in milliseconds for which the acquired records are locked.

  -- Versions: 2+
  shareAcknowledgeResponseAcquisitionLockTimeoutMs :: !(Int32)
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

-- | KafkaMessage instance for ShareAcknowledgeResponse.
instance KafkaMessage ShareAcknowledgeResponse where
  messageApiKey = 79
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Encode ShareAcknowledgeResponse with the given API version.
encodeShareAcknowledgeResponse :: MonadPut m => E.ApiVersion -> ShareAcknowledgeResponse -> m ()
encodeShareAcknowledgeResponse version msg
  | version == 1 =
    do
      serialize (shareAcknowledgeResponseThrottleTimeMs msg)
      serialize (shareAcknowledgeResponseErrorCode msg)
      serialize (toCompactString (shareAcknowledgeResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeShareAcknowledgeTopicResponse (case P.unKafkaArray (shareAcknowledgeResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNodeEndpoint (case P.unKafkaArray (shareAcknowledgeResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      serialize (shareAcknowledgeResponseThrottleTimeMs msg)
      serialize (shareAcknowledgeResponseErrorCode msg)
      serialize (toCompactString (shareAcknowledgeResponseErrorMessage msg))
      serialize (shareAcknowledgeResponseAcquisitionLockTimeoutMs msg)
      E.encodeVersionedArray version 0 encodeShareAcknowledgeTopicResponse (case P.unKafkaArray (shareAcknowledgeResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNodeEndpoint (case P.unKafkaArray (shareAcknowledgeResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareAcknowledgeResponse with the given API version.
decodeShareAcknowledgeResponse :: MonadGet m => E.ApiVersion -> m ShareAcknowledgeResponse
decodeShareAcknowledgeResponse version
  | version == 1 =
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
        shareAcknowledgeResponseAcquisitionLockTimeoutMs = 0
        ,
        shareAcknowledgeResponseResponses = fieldresponses
        ,
        shareAcknowledgeResponseNodeEndpoints = fieldnodeendpoints
        }

  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldacquisitionlocktimeoutms <- deserialize
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
        shareAcknowledgeResponseAcquisitionLockTimeoutMs = fieldacquisitionlocktimeoutms
        ,
        shareAcknowledgeResponseResponses = fieldresponses
        ,
        shareAcknowledgeResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a LeaderIdAndEpoch.
wireMaxSizeLeaderIdAndEpoch :: Int -> LeaderIdAndEpoch -> Int
wireMaxSizeLeaderIdAndEpoch _version msg =
  0
  + 4
  + 4
  + 1

-- | Direct-poke encoder for LeaderIdAndEpoch.
wirePokeLeaderIdAndEpoch :: Int -> Ptr Word8 -> LeaderIdAndEpoch -> IO (Ptr Word8)
wirePokeLeaderIdAndEpoch version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (leaderIdAndEpochLeaderId msg)
  p2 <- W.pokeInt32BE p1 (leaderIdAndEpochLeaderEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for LeaderIdAndEpoch.
wirePeekLeaderIdAndEpoch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderIdAndEpoch, Ptr Word8)
wirePeekLeaderIdAndEpoch version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = f0_leaderid, leaderIdAndEpochLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionDataErrorMessage msg))
  + wireMaxSizeLeaderIdAndEpoch _version (partitionDataCurrentLeader msg)
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionDataErrorMessage msg))
  p4 <- wirePokeLeaderIdAndEpoch version p3 (partitionDataCurrentLeader msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_currentleader, p4) <- wirePeekLeaderIdAndEpoch version _fp _basePtr p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataErrorMessage = f2_errormessage, partitionDataCurrentLeader = f3_currentleader }, pTagsEnd)

-- | Worst-case wire size of a ShareAcknowledgeTopicResponse.
wireMaxSizeShareAcknowledgeTopicResponse :: Int -> ShareAcknowledgeTopicResponse -> Int
wireMaxSizeShareAcknowledgeTopicResponse _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (shareAcknowledgeTopicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareAcknowledgeTopicResponse.
wirePokeShareAcknowledgeTopicResponse :: Int -> Ptr Word8 -> ShareAcknowledgeTopicResponse -> IO (Ptr Word8)
wirePokeShareAcknowledgeTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (shareAcknowledgeTopicResponseTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (shareAcknowledgeTopicResponsePartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ShareAcknowledgeTopicResponse.
wirePeekShareAcknowledgeTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareAcknowledgeTopicResponse, Ptr Word8)
wirePeekShareAcknowledgeTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ShareAcknowledgeTopicResponse { shareAcknowledgeTopicResponseTopicId = f0_topicid, shareAcknowledgeTopicResponsePartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (nodeEndpointHost msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (nodeEndpointRack msg))
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (nodeEndpointNodeId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg))
  p3 <- W.pokeInt32BE p2 (nodeEndpointPort msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (nodeEndpointRack msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port, nodeEndpointRack = f3_rack }, pTagsEnd)

-- | Worst-case wire size of a ShareAcknowledgeResponse.
wireMaxSizeShareAcknowledgeResponse :: Int -> ShareAcknowledgeResponse -> Int
wireMaxSizeShareAcknowledgeResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (shareAcknowledgeResponseErrorMessage msg))
  + 4
  + (5 + (case P.unKafkaArray (shareAcknowledgeResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeShareAcknowledgeTopicResponse _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (shareAcknowledgeResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareAcknowledgeResponse.
wirePokeShareAcknowledgeResponse :: Int -> Ptr Word8 -> ShareAcknowledgeResponse -> IO (Ptr Word8)
wirePokeShareAcknowledgeResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (shareAcknowledgeResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (shareAcknowledgeResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (shareAcknowledgeResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeShareAcknowledgeTopicResponse version p x) p3 (shareAcknowledgeResponseResponses msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeNodeEndpoint version p x) p4 (shareAcknowledgeResponseNodeEndpoints msg)
    WP.pokeEmptyTaggedFields p5
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (shareAcknowledgeResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (shareAcknowledgeResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (shareAcknowledgeResponseErrorMessage msg))
    p4 <- W.pokeInt32BE p3 (shareAcknowledgeResponseAcquisitionLockTimeoutMs msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeShareAcknowledgeTopicResponse version p x) p4 (shareAcknowledgeResponseResponses msg)
    p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeNodeEndpoint version p x) p5 (shareAcknowledgeResponseNodeEndpoints msg)
    WP.pokeEmptyTaggedFields p6
  | otherwise = error $ "wirePoke ShareAcknowledgeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareAcknowledgeResponse.
wirePeekShareAcknowledgeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareAcknowledgeResponse, Ptr Word8)
wirePeekShareAcknowledgeResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekShareAcknowledgeTopicResponse version _fp _basePtr p e) p3 endPtr
    (f4_nodeendpoints, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekNodeEndpoint version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (ShareAcknowledgeResponse { shareAcknowledgeResponseThrottleTimeMs = f0_throttletimems, shareAcknowledgeResponseErrorCode = f1_errorcode, shareAcknowledgeResponseErrorMessage = f2_errormessage, shareAcknowledgeResponseAcquisitionLockTimeoutMs = 0, shareAcknowledgeResponseResponses = f3_responses, shareAcknowledgeResponseNodeEndpoints = f4_nodeendpoints }, pTagsEnd)
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_acquisitionlocktimeoutms, p4) <- W.peekInt32BE p3 endPtr
    (f4_responses, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekShareAcknowledgeTopicResponse version _fp _basePtr p e) p4 endPtr
    (f5_nodeendpoints, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekNodeEndpoint version _fp _basePtr p e) p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (ShareAcknowledgeResponse { shareAcknowledgeResponseThrottleTimeMs = f0_throttletimems, shareAcknowledgeResponseErrorCode = f1_errorcode, shareAcknowledgeResponseErrorMessage = f2_errormessage, shareAcknowledgeResponseAcquisitionLockTimeoutMs = f3_acquisitionlocktimeoutms, shareAcknowledgeResponseResponses = f4_responses, shareAcknowledgeResponseNodeEndpoints = f5_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareAcknowledgeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ShareAcknowledgeResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareAcknowledgeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareAcknowledgeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareAcknowledgeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}