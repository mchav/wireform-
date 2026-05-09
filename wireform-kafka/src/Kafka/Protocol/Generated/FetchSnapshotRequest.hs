{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchSnapshotRequest
Description : Kafka FetchSnapshotRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 59.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchSnapshotRequest
  (
    FetchSnapshotRequest(..),
    TopicSnapshot(..),
    PartitionSnapshot(..),
    SnapshotId(..),
    encodeFetchSnapshotRequest,
    decodeFetchSnapshotRequest,
    maxFetchSnapshotRequestVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The snapshot endOffset and epoch to fetch.
data SnapshotId = SnapshotId
  {

  -- | The end offset of the snapshot.

  -- Versions: 0+
  snapshotIdEndOffset :: !(Int64)
,

  -- | The epoch of the snapshot.

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


-- | The partitions to fetch.
data PartitionSnapshot = PartitionSnapshot
  {

  -- | The partition index.

  -- Versions: 0+
  partitionSnapshotPartition :: !(Int32)
,

  -- | The current leader epoch of the partition, -1 for unknown leader epoch.

  -- Versions: 0+
  partitionSnapshotCurrentLeaderEpoch :: !(Int32)
,

  -- | The snapshot endOffset and epoch to fetch.

  -- Versions: 0+
  partitionSnapshotSnapshotId :: !(SnapshotId)
,

  -- | The byte position within the snapshot to start fetching from.

  -- Versions: 0+
  partitionSnapshotPosition :: !(Int64)
,

  -- | The directory id of the follower fetching.

  -- Versions: 1+
  partitionSnapshotReplicaDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionSnapshot with version-aware field handling.
encodePartitionSnapshot :: MonadPut m => E.ApiVersion -> PartitionSnapshot -> m ()
encodePartitionSnapshot version pmsg =
  do
    serialize (partitionSnapshotPartition pmsg)
    serialize (partitionSnapshotCurrentLeaderEpoch pmsg)
    encodeSnapshotId version (partitionSnapshotSnapshotId pmsg)
    serialize (partitionSnapshotPosition pmsg)
    when (version >= 0) $ do
      let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (serialize (partitionSnapshotReplicaDirectoryId pmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode PartitionSnapshot with version-aware field handling.
decodePartitionSnapshot :: MonadGet m => E.ApiVersion -> m PartitionSnapshot
decodePartitionSnapshot version =
  do
    fieldpartition <- deserialize
    fieldcurrentleaderepoch <- deserialize
    fieldsnapshotid <- decodeSnapshotId version
    fieldposition <- deserialize
    _taggedFields <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fieldreplicadirectoryid =
          if version >= 1
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                  Right _v -> _v
                  Left  _  -> (P.nullUuid)
              Nothing  -> (P.nullUuid)
            else (P.nullUuid)
    pure PartitionSnapshot
      {
      partitionSnapshotPartition = fieldpartition
      ,
      partitionSnapshotCurrentLeaderEpoch = fieldcurrentleaderepoch
      ,
      partitionSnapshotSnapshotId = fieldsnapshotid
      ,
      partitionSnapshotPosition = fieldposition
      ,
      partitionSnapshotReplicaDirectoryId = fieldreplicadirectoryid
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



data FetchSnapshotRequest = FetchSnapshotRequest
  {

  -- | The clusterId if known, this is used to validate metadata fetches prior to broker registration.

  -- Versions: 0+
  fetchSnapshotRequestClusterId :: !(KafkaString)
,

  -- | The broker ID of the follower.

  -- Versions: 0+
  fetchSnapshotRequestReplicaId :: !(Int32)
,

  -- | The maximum bytes to fetch from all of the snapshots.

  -- Versions: 0+
  fetchSnapshotRequestMaxBytes :: !(Int32)
,

  -- | The topics to fetch.

  -- Versions: 0+
  fetchSnapshotRequestTopics :: !(KafkaArray (TopicSnapshot))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchSnapshotRequest.
maxFetchSnapshotRequestVersion :: Int16
maxFetchSnapshotRequestVersion = 1

-- | KafkaMessage instance for FetchSnapshotRequest.
instance KafkaMessage FetchSnapshotRequest where
  messageApiKey = 59
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode FetchSnapshotRequest with the given API version.
encodeFetchSnapshotRequest :: MonadPut m => E.ApiVersion -> FetchSnapshotRequest -> m ()
encodeFetchSnapshotRequest version msg
  | version >= 0 && version <= 1 =
    do
      serialize (fetchSnapshotRequestReplicaId msg)
      serialize (fetchSnapshotRequestMaxBytes msg)
      E.encodeVersionedArray version 0 encodeTopicSnapshot (case P.unKafkaArray (fetchSnapshotRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 0 then [(0, Data.Bytes.Put.runPutS (serialize (toCompactString (fetchSnapshotRequestClusterId msg))))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FetchSnapshotRequest with the given API version.
decodeFetchSnapshotRequest :: MonadGet m => E.ApiVersion -> m FetchSnapshotRequest
decodeFetchSnapshotRequest version
  | version >= 0 && version <= 1 =
    do
      fieldreplicaid <- deserialize
      fieldmaxbytes <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicSnapshot
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldclusterid =
            if version >= 0
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.fromCompactString <$> deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaString Null)
                Nothing  -> (P.KafkaString Null)
              else (P.KafkaString Null)
      pure FetchSnapshotRequest
        {
        fetchSnapshotRequestClusterId = fieldclusterid
        ,
        fetchSnapshotRequestReplicaId = fieldreplicaid
        ,
        fetchSnapshotRequestMaxBytes = fieldmaxbytes
        ,
        fetchSnapshotRequestTopics = fieldtopics
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

-- | Worst-case wire size of a PartitionSnapshot.
wireMaxSizePartitionSnapshot :: Int -> PartitionSnapshot -> Int
wireMaxSizePartitionSnapshot _version msg =
  0
  + 4
  + 4
  + wireMaxSizeSnapshotId _version (partitionSnapshotSnapshotId msg)
  + 8
  + 16
  + 1

-- | Direct-poke encoder for PartitionSnapshot.
wirePokePartitionSnapshot :: Int -> Ptr Word8 -> PartitionSnapshot -> IO (Ptr Word8)
wirePokePartitionSnapshot version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionSnapshotPartition msg)
  p2 <- W.pokeInt32BE p1 (partitionSnapshotCurrentLeaderEpoch msg)
  p3 <- wirePokeSnapshotId version p2 (partitionSnapshotSnapshotId msg)
  p4 <- W.pokeInt64BE p3 (partitionSnapshotPosition msg)
  if version >= 0 then do
    let !_taggedEntries = (if version >= 1 then [(0, W.runWirePut (partitionSnapshotReplicaDirectoryId msg))] else [])
    WP.pokeTaggedFieldEntries p4 _taggedEntries
  else pure p4

-- | Direct-poke decoder for PartitionSnapshot.
wirePeekPartitionSnapshot :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionSnapshot, Ptr Word8)
wirePeekPartitionSnapshot version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_currentleaderepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_snapshotid, p3) <- wirePeekSnapshotId version _fp _basePtr p2 endPtr
  (f3_position, p4) <- W.peekInt64BE p3 endPtr
  (_taggedMap, pTagsEnd) <- if version >= 0 then WP.peekTaggedFieldsMap p4 endPtr else pure (Data.Map.Strict.empty, p4)
  let !_tag_replicadirectoryid = if version >= 1 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String P.KafkaUuid) _bs of { Right _v -> _v ; Left _ -> P.nullUuid}; Nothing -> P.nullUuid} else P.nullUuid
  pure (PartitionSnapshot { partitionSnapshotPartition = f0_partition, partitionSnapshotCurrentLeaderEpoch = f1_currentleaderepoch, partitionSnapshotSnapshotId = f2_snapshotid, partitionSnapshotPosition = f3_position, partitionSnapshotReplicaDirectoryId = _tag_replicadirectoryid }, pTagsEnd)

-- | Worst-case wire size of a TopicSnapshot.
wireMaxSizeTopicSnapshot :: Int -> TopicSnapshot -> Int
wireMaxSizeTopicSnapshot _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicSnapshotName msg))
  + (5 + (case P.unKafkaArray (topicSnapshotPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionSnapshot _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicSnapshot.
wirePokeTopicSnapshot :: Int -> Ptr Word8 -> TopicSnapshot -> IO (Ptr Word8)
wirePokeTopicSnapshot version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicSnapshotName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionSnapshot version p x) p1 (topicSnapshotPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicSnapshot.
wirePeekTopicSnapshot :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicSnapshot, Ptr Word8)
wirePeekTopicSnapshot version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionSnapshot version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicSnapshot { topicSnapshotName = f0_name, topicSnapshotPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a FetchSnapshotRequest.
wireMaxSizeFetchSnapshotRequest :: Int -> FetchSnapshotRequest -> Int
wireMaxSizeFetchSnapshotRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (fetchSnapshotRequestClusterId msg))
  + 4
  + 4
  + (5 + (case P.unKafkaArray (fetchSnapshotRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicSnapshot _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FetchSnapshotRequest.
wirePokeFetchSnapshotRequest :: Int -> Ptr Word8 -> FetchSnapshotRequest -> IO (Ptr Word8)
wirePokeFetchSnapshotRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (fetchSnapshotRequestReplicaId msg)
    p2 <- W.pokeInt32BE p1 (fetchSnapshotRequestMaxBytes msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicSnapshot version p x) p2 (fetchSnapshotRequestTopics msg)
    let !_taggedEntries = (if version >= 0 then [(0, W.runWirePut (P.toCompactString (fetchSnapshotRequestClusterId msg)))] else [])
    WP.pokeTaggedFieldEntries p3 _taggedEntries
  | otherwise = error $ "wirePoke FetchSnapshotRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for FetchSnapshotRequest.
wirePeekFetchSnapshotRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FetchSnapshotRequest, Ptr Word8)
wirePeekFetchSnapshotRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_maxbytes, p2) <- W.peekInt32BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicSnapshot version _fp _basePtr p e) p2 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p3 endPtr
    let !_tag_clusterid = if version >= 0 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (\b -> fmap P.fromCompactString ((W.runWireGet :: Data.ByteString.ByteString -> Either String P.CompactString) b)) _bs of { Right _v -> _v ; Left _ -> P.KafkaString Null}; Nothing -> P.KafkaString Null} else P.KafkaString Null
    pure (FetchSnapshotRequest { fetchSnapshotRequestClusterId = _tag_clusterid, fetchSnapshotRequestReplicaId = f0_replicaid, fetchSnapshotRequestMaxBytes = f1_maxbytes, fetchSnapshotRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek FetchSnapshotRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec FetchSnapshotRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFetchSnapshotRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFetchSnapshotRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFetchSnapshotRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}