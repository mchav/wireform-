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

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeFetchSnapshotRequest' / 'decodeFetchSnapshotRequest'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec FetchSnapshotRequest where
  wireCodec = Just (WC.serialShimCodec encodeFetchSnapshotRequest decodeFetchSnapshotRequest)
  {-# INLINE wireCodec #-}