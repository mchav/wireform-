{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchSnapshotResponse
Description : Kafka FetchSnapshotResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 59.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchSnapshotResponse
  (
    FetchSnapshotResponse(..),
    TopicSnapshot(..),
    PartitionSnapshot(..),
    SnapshotId(..),
    LeaderIdAndEpoch(..),
    NodeEndpoint(..),
    encodeFetchSnapshotResponse,
    decodeFetchSnapshotResponse,
    maxFetchSnapshotResponseVersion
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


-- | The snapshot endOffset and epoch fetched.
data SnapshotId = SnapshotId
  {

  -- | The snapshot end offset.

  -- Versions: 0+
  snapshotIdEndOffset :: !(Int64)
,

  -- | The snapshot epoch.

  -- Versions: 0+
  snapshotIdEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode SnapshotId with version-aware field handling.
encodeSnapshotId :: MonadPut m => E.ApiVersion -> SnapshotId -> m ()
encodeSnapshotId version smsg =
  do
    serialize (snapshotIdEndOffset smsg)
    serialize (snapshotIdEpoch smsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode SnapshotId with version-aware field handling.
decodeSnapshotId :: MonadGet m => E.ApiVersion -> m SnapshotId
decodeSnapshotId version =
  do
    fieldendoffset <- deserialize
    fieldepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure SnapshotId
      {
      snapshotIdEndOffset = fieldendoffset
      ,
      snapshotIdEpoch = fieldepoch
      }


-- | The leader of the partition at the time of the snapshot.
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


-- | The partitions to fetch.
data PartitionSnapshot = PartitionSnapshot
  {

  -- | The partition index.

  -- Versions: 0+
  partitionSnapshotIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionSnapshotErrorCode :: !(Int16)
,

  -- | The snapshot endOffset and epoch fetched.

  -- Versions: 0+
  partitionSnapshotSnapshotId :: !(SnapshotId)
,

  -- | The leader of the partition at the time of the snapshot.

  -- Versions: 0+
  partitionSnapshotCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | The total size of the snapshot.

  -- Versions: 0+
  partitionSnapshotSize :: !(Int64)
,

  -- | The starting byte position within the snapshot included in the Bytes field.

  -- Versions: 0+
  partitionSnapshotPosition :: !(Int64)
,

  -- | Snapshot data in records format which may not be aligned on an offset boundary.

  -- Versions: 0+
  partitionSnapshotUnalignedRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionSnapshot with version-aware field handling.
encodePartitionSnapshot :: MonadPut m => E.ApiVersion -> PartitionSnapshot -> m ()
encodePartitionSnapshot version pmsg =
  do
    serialize (partitionSnapshotIndex pmsg)
    serialize (partitionSnapshotErrorCode pmsg)
    encodeSnapshotId version (partitionSnapshotSnapshotId pmsg)
    serialize (partitionSnapshotSize pmsg)
    serialize (partitionSnapshotPosition pmsg)
    if version >= 0 then serialize (toCompactBytes (partitionSnapshotUnalignedRecords pmsg)) else serialize (partitionSnapshotUnalignedRecords pmsg)
    when (version >= 0) $ do
      let _entries = (if version >= 0 then [(0, Data.Bytes.Put.runPutS (encodeLeaderIdAndEpoch version (partitionSnapshotCurrentLeader pmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode PartitionSnapshot with version-aware field handling.
decodePartitionSnapshot :: MonadGet m => E.ApiVersion -> m PartitionSnapshot
decodePartitionSnapshot version =
  do
    fieldindex <- deserialize
    fielderrorcode <- deserialize
    fieldsnapshotid <- decodeSnapshotId version
    fieldsize <- deserialize
    fieldposition <- deserialize
    fieldunalignedrecords <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
    _taggedFields <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fieldcurrentleader =
          if version >= 0
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (decodeLeaderIdAndEpoch version) _bs of
                  Right _v -> _v
                  Left  _  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = 0, leaderIdAndEpochLeaderEpoch = 0 })
              Nothing  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = 0, leaderIdAndEpochLeaderEpoch = 0 })
            else (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = 0, leaderIdAndEpochLeaderEpoch = 0 })
    pure PartitionSnapshot
      {
      partitionSnapshotIndex = fieldindex
      ,
      partitionSnapshotErrorCode = fielderrorcode
      ,
      partitionSnapshotSnapshotId = fieldsnapshotid
      ,
      partitionSnapshotCurrentLeader = fieldcurrentleader
      ,
      partitionSnapshotSize = fieldsize
      ,
      partitionSnapshotPosition = fieldposition
      ,
      partitionSnapshotUnalignedRecords = fieldunalignedrecords
      }


-- | The topics to fetch.
data TopicSnapshot = TopicSnapshot
  {

  -- | The name of the topic to fetch.

  -- Versions: 0+
  topicSnapshotName :: !(KafkaString)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  topicSnapshotPartitions :: !(KafkaArray (PartitionSnapshot))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicSnapshot with version-aware field handling.
encodeTopicSnapshot :: MonadPut m => E.ApiVersion -> TopicSnapshot -> m ()
encodeTopicSnapshot version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicSnapshotName tmsg)) else serialize (topicSnapshotName tmsg)
    E.encodeVersionedArray version 0 encodePartitionSnapshot (case P.unKafkaArray (topicSnapshotPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicSnapshot with version-aware field handling.
decodeTopicSnapshot :: MonadGet m => E.ApiVersion -> m TopicSnapshot
decodeTopicSnapshot version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionSnapshot
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicSnapshot
      {
      topicSnapshotName = fieldname
      ,
      topicSnapshotPartitions = fieldpartitions
      }


-- | Endpoints for all current-leaders enumerated in PartitionSnapshot.
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
      if version >= 0 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    when (version >= 1) $
      serialize (nodeEndpointPort nmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- if version >= 1
      then deserialize
      else pure (0)
    fieldhost <- if version >= 1
      then if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure NodeEndpoint
      {
      nodeEndpointNodeId = fieldnodeid
      ,
      nodeEndpointHost = fieldhost
      ,
      nodeEndpointPort = fieldport
      }



data FetchSnapshotResponse = FetchSnapshotResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  fetchSnapshotResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  fetchSnapshotResponseErrorCode :: !(Int16)
,

  -- | The topics to fetch.

  -- Versions: 0+
  fetchSnapshotResponseTopics :: !(KafkaArray (TopicSnapshot))
,

  -- | Endpoints for all current-leaders enumerated in PartitionSnapshot.

  -- Versions: 1+
  fetchSnapshotResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchSnapshotResponse.
maxFetchSnapshotResponseVersion :: Int16
maxFetchSnapshotResponseVersion = 1

-- | KafkaMessage instance for FetchSnapshotResponse.
instance KafkaMessage FetchSnapshotResponse where
  messageApiKey = 59
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode FetchSnapshotResponse with the given API version.
encodeFetchSnapshotResponse :: MonadPut m => E.ApiVersion -> FetchSnapshotResponse -> m ()
encodeFetchSnapshotResponse version msg
  | version == 0 =
    do
      serialize (fetchSnapshotResponseThrottleTimeMs msg)
      serialize (fetchSnapshotResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeTopicSnapshot (case P.unKafkaArray (fetchSnapshotResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (fetchSnapshotResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries

  | version == 1 =
    do
      serialize (fetchSnapshotResponseThrottleTimeMs msg)
      serialize (fetchSnapshotResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeTopicSnapshot (case P.unKafkaArray (fetchSnapshotResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (fetchSnapshotResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FetchSnapshotResponse with the given API version.
decodeFetchSnapshotResponse :: MonadGet m => E.ApiVersion -> m FetchSnapshotResponse
decodeFetchSnapshotResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicSnapshot
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure FetchSnapshotResponse
        {
        fetchSnapshotResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchSnapshotResponseErrorCode = fielderrorcode
        ,
        fetchSnapshotResponseTopics = fieldtopics
        ,
        fetchSnapshotResponseNodeEndpoints = fieldnodeendpoints
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicSnapshot
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure FetchSnapshotResponse
        {
        fetchSnapshotResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchSnapshotResponseErrorCode = fielderrorcode
        ,
        fetchSnapshotResponseTopics = fieldtopics
        ,
        fetchSnapshotResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a SnapshotId.
wireMaxSizeSnapshotId :: Int -> SnapshotId -> Int
wireMaxSizeSnapshotId _version msg =
  0
  + 8
  + 4
  + 1

-- | Direct-poke encoder for SnapshotId.
wirePokeSnapshotId :: Int -> Ptr Word8 -> SnapshotId -> IO (Ptr Word8)
wirePokeSnapshotId version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (snapshotIdEndOffset msg)
  p2 <- W.pokeInt32BE p1 (snapshotIdEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for SnapshotId.
wirePeekSnapshotId :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SnapshotId, Ptr Word8)
wirePeekSnapshotId version _fp _basePtr p0 endPtr = do
  (f0_endoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_epoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (SnapshotId { snapshotIdEndOffset = f0_endoffset, snapshotIdEpoch = f1_epoch }, pTagsEnd)

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
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for NodeEndpoint.
wirePeekNodeEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (NodeEndpoint, Ptr Word8)
wirePeekNodeEndpoint version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (NodeEndpoint { nodeEndpointNodeId = f0_nodeid, nodeEndpointHost = f1_host, nodeEndpointPort = f2_port }, pTagsEnd)

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeFetchSnapshotResponse' / 'decodeFetchSnapshotResponse'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec FetchSnapshotResponse where
  wireCodec = Just (WC.serialShimCodec encodeFetchSnapshotResponse decodeFetchSnapshotResponse)
  {-# INLINE wireCodec #-}