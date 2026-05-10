{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetFetchRequest
Description : Kafka OffsetFetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 9.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetFetchRequest
  (
    OffsetFetchRequest(..),
    OffsetFetchRequestTopic(..),
    OffsetFetchRequestGroup(..),
    OffsetFetchRequestTopics(..),
    maxOffsetFetchRequestVersion
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


-- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
data OffsetFetchRequestTopic = OffsetFetchRequestTopic
  {

  -- | The topic name.

  -- Versions: 0-7
  offsetFetchRequestTopicName :: !(KafkaString)
,

  -- | The partition indexes we would like to fetch offsets for.

  -- Versions: 0-7
  offsetFetchRequestTopicPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
data OffsetFetchRequestTopics = OffsetFetchRequestTopics
  {

  -- | The topic name.

  -- Versions: 8-9
  offsetFetchRequestTopicsName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetFetchRequestTopicsTopicId :: !(KafkaUuid)
,

  -- | The partition indexes we would like to fetch offsets for.

  -- Versions: 8+
  offsetFetchRequestTopicsPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | Each group we would like to fetch offsets for.
data OffsetFetchRequestGroup = OffsetFetchRequestGroup
  {

  -- | The group ID.

  -- Versions: 8+
  offsetFetchRequestGroupGroupId :: !(KafkaString)
,

  -- | The member id.

  -- Versions: 9+
  offsetFetchRequestGroupMemberId :: !(KafkaString)
,

  -- | The member epoch if using the new consumer protocol (KIP-848).

  -- Versions: 9+
  offsetFetchRequestGroupMemberEpoch :: !(Int32)
,

  -- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.

  -- Versions: 8+
  offsetFetchRequestGroupTopics :: !(KafkaArray (OffsetFetchRequestTopics))

  }
  deriving (Eq, Show, Generic)


data OffsetFetchRequest = OffsetFetchRequest
  {

  -- | The group to fetch offsets for.

  -- Versions: 0-7
  offsetFetchRequestGroupId :: !(KafkaString)
,

  -- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.

  -- Versions: 0-7
  offsetFetchRequestTopics :: !(KafkaArray (OffsetFetchRequestTopic))
,

  -- | Each group we would like to fetch offsets for.

  -- Versions: 8+
  offsetFetchRequestGroups :: !(KafkaArray (OffsetFetchRequestGroup))
,

  -- | Whether broker should hold on returning unstable offsets but set a retriable error code for the part

  -- Versions: 7+
  offsetFetchRequestRequireStable :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetFetchRequest.
maxOffsetFetchRequestVersion :: Int16
maxOffsetFetchRequestVersion = 10

-- | KafkaMessage instance for OffsetFetchRequest.
instance KafkaMessage OffsetFetchRequest where
  messageApiKey = 9
  messageMinVersion = 1
  messageMaxVersion = 10
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a OffsetFetchRequestTopic.
wireMaxSizeOffsetFetchRequestTopic :: Int -> OffsetFetchRequestTopic -> Int
wireMaxSizeOffsetFetchRequestTopic _version msg =
  0
  + WP.dualStringMaxSize (offsetFetchRequestTopicName msg)
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopicPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestTopic.
wirePokeOffsetFetchRequestTopic :: Int -> Ptr Word8 -> OffsetFetchRequestTopic -> IO (Ptr Word8)
wirePokeOffsetFetchRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestTopicName msg)) else WP.pokeKafkaString p0 (offsetFetchRequestTopicName msg)) else pure p0)
  p2 <- (if version <= 7 then WP.pokeVersionedArray version 6 W.pokeInt32BE p1 (offsetFetchRequestTopicPartitionIndexes msg) else pure p1)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetFetchRequestTopic.
wirePeekOffsetFetchRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestTopic, Ptr Word8)
wirePeekOffsetFetchRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_partitionindexes, p2) <- (if version <= 7 then WP.peekVersionedArray version 6 W.peekInt32BE p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetFetchRequestTopic { offsetFetchRequestTopicName = f0_name, offsetFetchRequestTopicPartitionIndexes = f1_partitionindexes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchRequestTopic :: OffsetFetchRequestTopic
defaultOffsetFetchRequestTopic = OffsetFetchRequestTopic { offsetFetchRequestTopicName = P.KafkaString Null, offsetFetchRequestTopicPartitionIndexes = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetFetchRequestTopics.
wireMaxSizeOffsetFetchRequestTopics :: Int -> OffsetFetchRequestTopics -> Int
wireMaxSizeOffsetFetchRequestTopics _version msg =
  0
  + WP.dualStringMaxSize (offsetFetchRequestTopicsName msg)
  + 16
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopicsPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestTopics.
wirePokeOffsetFetchRequestTopics :: Int -> Ptr Word8 -> OffsetFetchRequestTopics -> IO (Ptr Word8)
wirePokeOffsetFetchRequestTopics version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 && version <= 9 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestTopicsName msg)) else WP.pokeKafkaString p0 (offsetFetchRequestTopicsName msg)) else pure p0)
  p2 <- (if version >= 10 then WP.pokeKafkaUuid p1 (offsetFetchRequestTopicsTopicId msg) else pure p1)
  p3 <- (if version >= 8 then WP.pokeVersionedArray version 6 W.pokeInt32BE p2 (offsetFetchRequestTopicsPartitionIndexes msg) else pure p2)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchRequestTopics.
wirePeekOffsetFetchRequestTopics :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestTopics, Ptr Word8)
wirePeekOffsetFetchRequestTopics version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 8 && version <= 9 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 10 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitionindexes, p3) <- (if version >= 8 then WP.peekVersionedArray version 6 W.peekInt32BE p2 endPtr else pure (P.mkKafkaArray V.empty, p2))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchRequestTopics { offsetFetchRequestTopicsName = f0_name, offsetFetchRequestTopicsTopicId = f1_topicid, offsetFetchRequestTopicsPartitionIndexes = f2_partitionindexes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchRequestTopics :: OffsetFetchRequestTopics
defaultOffsetFetchRequestTopics = OffsetFetchRequestTopics { offsetFetchRequestTopicsName = P.KafkaString Null, offsetFetchRequestTopicsTopicId = P.nullUuid, offsetFetchRequestTopicsPartitionIndexes = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetFetchRequestGroup.
wireMaxSizeOffsetFetchRequestGroup :: Int -> OffsetFetchRequestGroup -> Int
wireMaxSizeOffsetFetchRequestGroup _version msg =
  0
  + WP.dualStringMaxSize (offsetFetchRequestGroupGroupId msg)
  + WP.dualStringMaxSize (offsetFetchRequestGroupMemberId msg)
  + 4
  + (5 + (case P.unKafkaArray (offsetFetchRequestGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestTopics _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestGroup.
wirePokeOffsetFetchRequestGroup :: Int -> Ptr Word8 -> OffsetFetchRequestGroup -> IO (Ptr Word8)
wirePokeOffsetFetchRequestGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupGroupId msg)) else WP.pokeKafkaString p0 (offsetFetchRequestGroupGroupId msg)) else pure p0)
  p2 <- (if version >= 9 then (if version >= 6 then WP.pokeCompactString p1 (P.toCompactString (offsetFetchRequestGroupMemberId msg)) else WP.pokeKafkaString p1 (offsetFetchRequestGroupMemberId msg)) else pure p1)
  p3 <- (if version >= 9 then W.pokeInt32BE p2 (offsetFetchRequestGroupMemberEpoch msg) else pure p2)
  p4 <- (if version >= 8 then WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopics version p x) p3 (offsetFetchRequestGroupTopics msg) else pure p3)
  if version >= 6 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for OffsetFetchRequestGroup.
wirePeekOffsetFetchRequestGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestGroup, Ptr Word8)
wirePeekOffsetFetchRequestGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (if version >= 8 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_memberid, p2) <- (if version >= 9 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_memberepoch, p3) <- (if version >= 9 then W.peekInt32BE p2 endPtr else pure (-1, p2))
  (f3_topics, p4) <- (if version >= 8 then WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopics version _fp _basePtr p e) p3 endPtr else pure (P.KafkaArray P.Null, p3))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (OffsetFetchRequestGroup { offsetFetchRequestGroupGroupId = f0_groupid, offsetFetchRequestGroupMemberId = f1_memberid, offsetFetchRequestGroupMemberEpoch = f2_memberepoch, offsetFetchRequestGroupTopics = f3_topics }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchRequestGroup :: OffsetFetchRequestGroup
defaultOffsetFetchRequestGroup = OffsetFetchRequestGroup { offsetFetchRequestGroupGroupId = P.KafkaString Null, offsetFetchRequestGroupMemberId = P.KafkaString Null, offsetFetchRequestGroupMemberEpoch = -1, offsetFetchRequestGroupTopics = P.KafkaArray P.Null }

-- | Worst-case wire size of a OffsetFetchRequest.
wireMaxSizeOffsetFetchRequest :: Int -> OffsetFetchRequest -> Int
wireMaxSizeOffsetFetchRequest _version msg =
  0
  + WP.dualStringMaxSize (offsetFetchRequestGroupId msg)
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestTopic _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (offsetFetchRequestGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestGroup _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for OffsetFetchRequest.
wirePokeOffsetFetchRequest :: Int -> Ptr Word8 -> OffsetFetchRequest -> IO (Ptr Word8)
wirePokeOffsetFetchRequest version basePtr msg
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetFetchRequestGroupId msg)) else pure p0)
    p2 <- (if version <= 7 then WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version == 7 = do
    p0 <- pure basePtr
    p1 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetFetchRequestGroupId msg)) else pure p0)
    p2 <- (if version <= 7 then WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg) else pure p1)
    p3 <- (if version >= 7 then W.pokeWord8 p2 (if (offsetFetchRequestRequireStable msg) then 1 else 0) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- (if version >= 8 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchRequestGroup version p x) p0 (offsetFetchRequestGroups msg) else pure p0)
    p2 <- (if version >= 7 then W.pokeWord8 p1 (if (offsetFetchRequestRequireStable msg) then 1 else 0) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetFetchRequestGroupId msg)) else pure p0)
    p2 <- (if version <= 7 then WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg) else pure p1)
    pure p2
  | otherwise = error $ "wirePoke OffsetFetchRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetFetchRequest.
wirePeekOffsetFetchRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequest, Ptr Word8)
wirePeekOffsetFetchRequest version _fp _basePtr p0 endPtr
  | version == 6 = do
    (f0_groupid, p1) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_topics, p2) <- (if version <= 7 then WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr else pure (P.KafkaArray P.Null, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = False }, pTagsEnd)
  | version == 7 = do
    (f0_groupid, p1) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_topics, p2) <- (if version <= 7 then WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr else pure (P.KafkaArray P.Null, p1))
    (f2_requirestable, p3) <- (if version >= 7 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = f2_requirestable }, pTagsEnd)
  | version >= 8 && version <= 10 = do
    (f0_groups, p1) <- (if version >= 8 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchRequestGroup version _fp _basePtr p e) p0 endPtr else pure (P.mkKafkaArray V.empty, p0))
    (f1_requirestable, p2) <- (if version >= 7 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = P.KafkaString Null, offsetFetchRequestTopics = P.KafkaArray P.Null, offsetFetchRequestGroups = f0_groups, offsetFetchRequestRequireStable = f1_requirestable }, pTagsEnd)
  | version >= 1 && version <= 5 = do
    (f0_groupid, p1) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
    (f1_topics, p2) <- (if version <= 7 then WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr else pure (P.KafkaArray P.Null, p1))
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = False }, p2)
  | otherwise = error $ "wirePeek OffsetFetchRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetFetchRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetFetchRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetFetchRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetFetchRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}