{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.VoteRequest
Description : Kafka VoteRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 52.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.VoteRequest
  (
    VoteRequest(..),
    TopicData(..),
    PartitionData(..),
    encodeVoteRequest,
    decodeVoteRequest,
    maxVoteRequestVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The epoch of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaEpoch :: !(Int32)
,

  -- | The replica id of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaId :: !(Int32)
,

  -- | The directory id of the voter sending the request

  -- Versions: 1+
  partitionDataReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The directory id of the voter receiving the request

  -- Versions: 1+
  partitionDataVoterDirectoryId :: !(KafkaUuid)
,

  -- | The epoch of the last record written to the metadata log.

  -- Versions: 0+
  partitionDataLastOffsetEpoch :: !(Int32)
,

  -- | The log end offset of the metadata log of the voter sending the request.

  -- Versions: 0+
  partitionDataLastOffset :: !(Int64)
,

  -- | Whether the request is a PreVote request (not persisted) or not.

  -- Versions: 2+
  partitionDataPreVote :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataReplicaEpoch pmsg)
    serialize (partitionDataReplicaId pmsg)
    when (version >= 1) $
      serialize (partitionDataReplicaDirectoryId pmsg)
    when (version >= 1) $
      serialize (partitionDataVoterDirectoryId pmsg)
    serialize (partitionDataLastOffsetEpoch pmsg)
    serialize (partitionDataLastOffset pmsg)
    when (version >= 2) $
      serialize (partitionDataPreVote pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldreplicaepoch <- deserialize
    fieldreplicaid <- deserialize
    fieldreplicadirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    fieldvoterdirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    fieldlastoffsetepoch <- deserialize
    fieldlastoffset <- deserialize
    fieldprevote <- if version >= 2
      then deserialize
      else pure (False)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataReplicaEpoch = fieldreplicaepoch
      ,
      partitionDataReplicaId = fieldreplicaid
      ,
      partitionDataReplicaDirectoryId = fieldreplicadirectoryid
      ,
      partitionDataVoterDirectoryId = fieldvoterdirectoryid
      ,
      partitionDataLastOffsetEpoch = fieldlastoffsetepoch
      ,
      partitionDataLastOffset = fieldlastoffset
      ,
      partitionDataPreVote = fieldprevote
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
    if version >= 0 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }



data VoteRequest = VoteRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  voteRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter receiving the request.

  -- Versions: 1+
  voteRequestVoterId :: !(Int32)
,

  -- | The topic data.

  -- Versions: 0+
  voteRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for VoteRequest.
maxVoteRequestVersion :: Int16
maxVoteRequestVersion = 2

-- | KafkaMessage instance for VoteRequest.
instance KafkaMessage VoteRequest where
  messageApiKey = 52
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Encode VoteRequest with the given API version.
encodeVoteRequest :: MonadPut m => E.ApiVersion -> VoteRequest -> m ()
encodeVoteRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (voteRequestClusterId msg))
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (toCompactString (voteRequestClusterId msg))
      serialize (voteRequestVoterId msg)
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode VoteRequest with the given API version.
decodeVoteRequest :: MonadGet m => E.ApiVersion -> m VoteRequest
decodeVoteRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure VoteRequest
        {
        voteRequestClusterId = fieldclusterid
        ,
        voteRequestVoterId = (-1)
        ,
        voteRequestTopics = fieldtopics
        }

  | version >= 1 && version <= 2 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldvoterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure VoteRequest
        {
        voteRequestClusterId = fieldclusterid
        ,
        voteRequestVoterId = fieldvoterid
        ,
        voteRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 4
  + 16
  + 16
  + 4
  + 8
  + 1
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (partitionDataReplicaEpoch msg)
  p3 <- W.pokeInt32BE p2 (partitionDataReplicaId msg)
  p4 <- WP.pokeKafkaUuid p3 (partitionDataReplicaDirectoryId msg)
  p5 <- WP.pokeKafkaUuid p4 (partitionDataVoterDirectoryId msg)
  p6 <- W.pokeInt32BE p5 (partitionDataLastOffsetEpoch msg)
  p7 <- W.pokeInt64BE p6 (partitionDataLastOffset msg)
  p8 <- W.pokeWord8 p7 (if (partitionDataPreVote msg) then 1 else 0)
  if version >= 0 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicaepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_replicaid, p3) <- W.peekInt32BE p2 endPtr
  (f3_replicadirectoryid, p4) <- WP.peekKafkaUuid p3 endPtr
  (f4_voterdirectoryid, p5) <- WP.peekKafkaUuid p4 endPtr
  (f5_lastoffsetepoch, p6) <- W.peekInt32BE p5 endPtr
  (f6_lastoffset, p7) <- W.peekInt64BE p6 endPtr
  (f7_prevote, p8) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p7 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataReplicaEpoch = f1_replicaepoch, partitionDataReplicaId = f2_replicaid, partitionDataReplicaDirectoryId = f3_replicadirectoryid, partitionDataVoterDirectoryId = f4_voterdirectoryid, partitionDataLastOffsetEpoch = f5_lastoffsetepoch, partitionDataLastOffset = f6_lastoffset, partitionDataPreVote = f7_prevote }, pTagsEnd)

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
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a VoteRequest.
wireMaxSizeVoteRequest :: Int -> VoteRequest -> Int
wireMaxSizeVoteRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (voteRequestClusterId msg))
  + 4
  + (5 + (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for VoteRequest.
wirePokeVoteRequest :: Int -> Ptr Word8 -> VoteRequest -> IO (Ptr Word8)
wirePokeVoteRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (voteRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (voteRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (voteRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (voteRequestVoterId msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (voteRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke VoteRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for VoteRequest.
wirePeekVoteRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (VoteRequest, Ptr Word8)
wirePeekVoteRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (VoteRequest { voteRequestClusterId = f0_clusterid, voteRequestVoterId = 0, voteRequestTopics = f1_topics }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_voterid, p2) <- W.peekInt32BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (VoteRequest { voteRequestClusterId = f0_clusterid, voteRequestVoterId = f1_voterid, voteRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek VoteRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec VoteRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeVoteRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeVoteRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekVoteRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}