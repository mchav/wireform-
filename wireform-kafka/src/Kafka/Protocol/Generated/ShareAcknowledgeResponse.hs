{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareAcknowledgeResponse
Description : Kafka ShareAcknowledgeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 79.



Valid versions: 0
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
    maxShareAcknowledgeResponseVersion
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
maxShareAcknowledgeResponseVersion = 0

-- | KafkaMessage instance for ShareAcknowledgeResponse.
instance KafkaMessage ShareAcknowledgeResponse where
  messageApiKey = 79
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultLeaderIdAndEpoch :: LeaderIdAndEpoch
defaultLeaderIdAndEpoch = LeaderIdAndEpoch { leaderIdAndEpochLeaderId = 0, leaderIdAndEpochLeaderEpoch = 0 }

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (partitionDataErrorMessage msg)
  + wireMaxSizeLeaderIdAndEpoch _version (partitionDataCurrentLeader msg)
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (partitionDataErrorMessage msg)) else WP.pokeKafkaString p2 (partitionDataErrorMessage msg))
  p4 <- wirePokeLeaderIdAndEpoch version p3 (partitionDataCurrentLeader msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  (f3_currentleader, p4) <- wirePeekLeaderIdAndEpoch version _fp _basePtr p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataErrorMessage = f2_errormessage, partitionDataCurrentLeader = f3_currentleader }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataErrorCode = 0, partitionDataErrorMessage = P.KafkaString Null, partitionDataCurrentLeader = defaultLeaderIdAndEpoch }

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

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultShareAcknowledgeTopicResponse :: ShareAcknowledgeTopicResponse
defaultShareAcknowledgeTopicResponse = ShareAcknowledgeTopicResponse { shareAcknowledgeTopicResponseTopicId = P.nullUuid, shareAcknowledgeTopicResponsePartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.dualStringMaxSize (nodeEndpointHost msg)
  + 4
  + WP.dualStringMaxSize (nodeEndpointRack msg)
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (nodeEndpointNodeId msg)
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg)) else WP.pokeKafkaString p1 (nodeEndpointHost msg))
  p3 <- W.pokeInt32BE p2 (nodeEndpointPort msg)
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (nodeEndpointRack msg)) else WP.pokeKafkaString p3 (nodeEndpointRack msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port, nodeEndpointRack = f3_rack }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultNodeEndpoint :: NodeEndpoint
defaultNodeEndpoint = NodeEndpoint { nodeEndpointNodeId = 0, nodeEndpointHost = P.KafkaString Null, nodeEndpointPort = 0, nodeEndpointRack = P.KafkaString Null }

-- | Worst-case wire size of a ShareAcknowledgeResponse.
wireMaxSizeShareAcknowledgeResponse :: Int -> ShareAcknowledgeResponse -> Int
wireMaxSizeShareAcknowledgeResponse _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (shareAcknowledgeResponseErrorMessage msg)
  + (5 + (case P.unKafkaArray (shareAcknowledgeResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeShareAcknowledgeTopicResponse _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (shareAcknowledgeResponseNodeEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNodeEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ShareAcknowledgeResponse.
wirePokeShareAcknowledgeResponse :: Int -> Ptr Word8 -> ShareAcknowledgeResponse -> IO (Ptr Word8)
wirePokeShareAcknowledgeResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (shareAcknowledgeResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (shareAcknowledgeResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (shareAcknowledgeResponseErrorMessage msg)) else WP.pokeKafkaString p2 (shareAcknowledgeResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeShareAcknowledgeTopicResponse version p x) p3 (shareAcknowledgeResponseResponses msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeNodeEndpoint version p x) p4 (shareAcknowledgeResponseNodeEndpoints msg)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke ShareAcknowledgeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ShareAcknowledgeResponse.
wirePeekShareAcknowledgeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ShareAcknowledgeResponse, Ptr Word8)
wirePeekShareAcknowledgeResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_responses, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekShareAcknowledgeTopicResponse version _fp _basePtr p e) p3 endPtr
    (f4_nodeendpoints, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekNodeEndpoint version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (ShareAcknowledgeResponse { shareAcknowledgeResponseThrottleTimeMs = f0_throttletimems, shareAcknowledgeResponseErrorCode = f1_errorcode, shareAcknowledgeResponseErrorMessage = f2_errormessage, shareAcknowledgeResponseResponses = f3_responses, shareAcknowledgeResponseNodeEndpoints = f4_nodeendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek ShareAcknowledgeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ShareAcknowledgeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeShareAcknowledgeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeShareAcknowledgeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekShareAcknowledgeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}