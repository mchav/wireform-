{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndQuorumEpochResponse
Description : Kafka EndQuorumEpochResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 54.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndQuorumEpochResponse
  (
    EndQuorumEpochResponse(..),
    TopicData(..),
    PartitionData(..),
    NodeEndpoint(..),
    encodeEndQuorumEpochResponse,
    decodeEndQuorumEpochResponse,
    maxEndQuorumEpochResponseVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

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



data EndQuorumEpochResponse = EndQuorumEpochResponse
  {

  -- | The top level error code.

  -- Versions: 0+
  endQuorumEpochResponseErrorCode :: !(Int16)
,

  -- | The topic data.

  -- Versions: 0+
  endQuorumEpochResponseTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for all leaders enumerated in PartitionData.

  -- Versions: 1+
  endQuorumEpochResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndQuorumEpochResponse.
maxEndQuorumEpochResponseVersion :: Int16
maxEndQuorumEpochResponseVersion = 1

-- | KafkaMessage instance for EndQuorumEpochResponse.
instance KafkaMessage EndQuorumEpochResponse where
  messageApiKey = 54
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Encode EndQuorumEpochResponse with the given API version.
encodeEndQuorumEpochResponse :: MonadPut m => E.ApiVersion -> EndQuorumEpochResponse -> m ()
encodeEndQuorumEpochResponse version msg
  | version == 0 =
    do
      serialize (endQuorumEpochResponseErrorCode msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (endQuorumEpochResponseErrorCode msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (endQuorumEpochResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndQuorumEpochResponse with the given API version.
decodeEndQuorumEpochResponse :: MonadGet m => E.ApiVersion -> m EndQuorumEpochResponse
decodeEndQuorumEpochResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      pure EndQuorumEpochResponse
        {
        endQuorumEpochResponseErrorCode = fielderrorcode
        ,
        endQuorumEpochResponseTopics = fieldtopics
        ,
        endQuorumEpochResponseNodeEndpoints = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure EndQuorumEpochResponse
        {
        endQuorumEpochResponseErrorCode = fielderrorcode
        ,
        endQuorumEpochResponseTopics = fieldtopics
        ,
        endQuorumEpochResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 4
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderId msg)
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderEpoch msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataLeaderId = f2_leaderid, partitionDataLeaderEpoch = f3_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicDataTopicName msg))
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a NodeEndpoint.
wireMaxSizeNodeEndpoint :: Int -> NodeEndpoint -> Int
wireMaxSizeNodeEndpoint _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (nodeEndpointHost msg))
  + 2
  + 1

-- | Direct-poke encoder for NodeEndpoint.
wirePokeNodeEndpoint :: Int -> Ptr Word8 -> NodeEndpoint -> IO (Ptr Word8)
wirePokeNodeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (nodeEndpointNodeId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (nodeEndpointHost msg))
  p3 <- W.pokeWord16BE p2 (nodeEndpointPort msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port }, pTagsEnd)

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeEndQuorumEpochResponse' / 'decodeEndQuorumEpochResponse'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec EndQuorumEpochResponse where
  wireCodec = Just (WC.serialShimCodec encodeEndQuorumEpochResponse decodeEndQuorumEpochResponse)
  {-# INLINE wireCodec #-}