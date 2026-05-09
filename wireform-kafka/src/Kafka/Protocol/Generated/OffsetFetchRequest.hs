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
    encodeOffsetFetchRequest,
    decodeOffsetFetchRequest,
    maxOffsetFetchRequestVersion
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


-- | Encode OffsetFetchRequestTopic with version-aware field handling.
encodeOffsetFetchRequestTopic :: MonadPut m => E.ApiVersion -> OffsetFetchRequestTopic -> m ()
encodeOffsetFetchRequestTopic version omsg =
  do
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestTopicName omsg)) else serialize (offsetFetchRequestTopicName omsg)
    when (version >= 0 && version <= 7) $
      E.encodeVersionedArray version 6 (\_ x -> serialize x) (case P.unKafkaArray (offsetFetchRequestTopicPartitionIndexes omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestTopic with version-aware field handling.
decodeOffsetFetchRequestTopic :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestTopic
decodeOffsetFetchRequestTopic version =
  do
    fieldname <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldpartitionindexes <- if version >= 0 && version <= 7
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestTopic
      {
      offsetFetchRequestTopicName = fieldname
      ,
      offsetFetchRequestTopicPartitionIndexes = fieldpartitionindexes
      }


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


-- | Encode OffsetFetchRequestTopics with version-aware field handling.
encodeOffsetFetchRequestTopics :: MonadPut m => E.ApiVersion -> OffsetFetchRequestTopics -> m ()
encodeOffsetFetchRequestTopics version omsg =
  do
    when (version >= 8 && version <= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestTopicsName omsg)) else serialize (offsetFetchRequestTopicsName omsg)
    when (version >= 10) $
      serialize (offsetFetchRequestTopicsTopicId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 (\_ x -> serialize x) (case P.unKafkaArray (offsetFetchRequestTopicsPartitionIndexes omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestTopics with version-aware field handling.
decodeOffsetFetchRequestTopics :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestTopics
decodeOffsetFetchRequestTopics version =
  do
    fieldname <- if version >= 8 && version <= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitionindexes <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestTopics
      {
      offsetFetchRequestTopicsName = fieldname
      ,
      offsetFetchRequestTopicsTopicId = fieldtopicid
      ,
      offsetFetchRequestTopicsPartitionIndexes = fieldpartitionindexes
      }


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


-- | Encode OffsetFetchRequestGroup with version-aware field handling.
encodeOffsetFetchRequestGroup :: MonadPut m => E.ApiVersion -> OffsetFetchRequestGroup -> m ()
encodeOffsetFetchRequestGroup version omsg =
  do
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestGroupGroupId omsg)) else serialize (offsetFetchRequestGroupGroupId omsg)
    when (version >= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestGroupMemberId omsg)) else serialize (offsetFetchRequestGroupMemberId omsg)
    when (version >= 9) $
      serialize (offsetFetchRequestGroupMemberEpoch omsg)
    when (version >= 8) $
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopics (offsetFetchRequestGroupTopics omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestGroup with version-aware field handling.
decodeOffsetFetchRequestGroup :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestGroup
decodeOffsetFetchRequestGroup version =
  do
    fieldgroupid <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmemberid <- if version >= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmemberepoch <- if version >= 9
      then deserialize
      else pure ((-1))
    fieldtopics <- if version >= 8
      then E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopics
      else pure (P.KafkaArray P.Null)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestGroup
      {
      offsetFetchRequestGroupGroupId = fieldgroupid
      ,
      offsetFetchRequestGroupMemberId = fieldmemberid
      ,
      offsetFetchRequestGroupMemberEpoch = fieldmemberepoch
      ,
      offsetFetchRequestGroupTopics = fieldtopics
      }



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

-- | Encode OffsetFetchRequest with the given API version.
encodeOffsetFetchRequest :: MonadPut m => E.ApiVersion -> OffsetFetchRequest -> m ()
encodeOffsetFetchRequest version msg
  | version == 6 =
    do
      serialize (toCompactString (offsetFetchRequestGroupId msg))
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 7 =
    do
      serialize (toCompactString (offsetFetchRequestGroupId msg))
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)
      serialize (offsetFetchRequestRequireStable msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 8 && version <= 10 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchRequestGroup (case P.unKafkaArray (offsetFetchRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchRequestRequireStable msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 5 =
    do
      serialize (offsetFetchRequestGroupId msg)
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetFetchRequest with the given API version.
decodeOffsetFetchRequest :: MonadGet m => E.ApiVersion -> m OffsetFetchRequest
decodeOffsetFetchRequest version
  | version == 6 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = False
        }

  | version == 7 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      fieldrequirestable <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = fieldrequirestable
        }

  | version >= 8 && version <= 10 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchRequestGroup
      fieldrequirestable <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = P.KafkaString Null
        ,
        offsetFetchRequestTopics = P.KafkaArray P.Null
        ,
        offsetFetchRequestGroups = fieldgroups
        ,
        offsetFetchRequestRequireStable = fieldrequirestable
        }

  | version >= 1 && version <= 5 =
    do
      fieldgroupid <- deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a OffsetFetchRequestTopic.
wireMaxSizeOffsetFetchRequestTopic :: Int -> OffsetFetchRequestTopic -> Int
wireMaxSizeOffsetFetchRequestTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchRequestTopicName msg))
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopicPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestTopic.
wirePokeOffsetFetchRequestTopic :: Int -> Ptr Word8 -> OffsetFetchRequestTopic -> IO (Ptr Word8)
wirePokeOffsetFetchRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestTopicName msg))
  p2 <- WP.pokeVersionedArray version 6 W.pokeInt32BE p1 (offsetFetchRequestTopicPartitionIndexes msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetFetchRequestTopic.
wirePeekOffsetFetchRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestTopic, Ptr Word8)
wirePeekOffsetFetchRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitionindexes, p2) <- WP.peekVersionedArray version 6 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetFetchRequestTopic { offsetFetchRequestTopicName = f0_name, offsetFetchRequestTopicPartitionIndexes = f1_partitionindexes }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchRequestTopics.
wireMaxSizeOffsetFetchRequestTopics :: Int -> OffsetFetchRequestTopics -> Int
wireMaxSizeOffsetFetchRequestTopics _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchRequestTopicsName msg))
  + 16
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopicsPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestTopics.
wirePokeOffsetFetchRequestTopics :: Int -> Ptr Word8 -> OffsetFetchRequestTopics -> IO (Ptr Word8)
wirePokeOffsetFetchRequestTopics version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestTopicsName msg))
  p2 <- WP.pokeKafkaUuid p1 (offsetFetchRequestTopicsTopicId msg)
  p3 <- WP.pokeVersionedArray version 6 W.pokeInt32BE p2 (offsetFetchRequestTopicsPartitionIndexes msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchRequestTopics.
wirePeekOffsetFetchRequestTopics :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestTopics, Ptr Word8)
wirePeekOffsetFetchRequestTopics version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitionindexes, p3) <- WP.peekVersionedArray version 6 W.peekInt32BE p2 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchRequestTopics { offsetFetchRequestTopicsName = f0_name, offsetFetchRequestTopicsTopicId = f1_topicid, offsetFetchRequestTopicsPartitionIndexes = f2_partitionindexes }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchRequestGroup.
wireMaxSizeOffsetFetchRequestGroup :: Int -> OffsetFetchRequestGroup -> Int
wireMaxSizeOffsetFetchRequestGroup _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchRequestGroupGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchRequestGroupMemberId msg))
  + 4
  + (5 + (case P.unKafkaArray (offsetFetchRequestGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestTopics _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchRequestGroup.
wirePokeOffsetFetchRequestGroup :: Int -> Ptr Word8 -> OffsetFetchRequestGroup -> IO (Ptr Word8)
wirePokeOffsetFetchRequestGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupGroupId msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (offsetFetchRequestGroupMemberId msg))
  p3 <- W.pokeInt32BE p2 (offsetFetchRequestGroupMemberEpoch msg)
  p4 <- WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopics version p x) p3 (offsetFetchRequestGroupTopics msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for OffsetFetchRequestGroup.
wirePeekOffsetFetchRequestGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequestGroup, Ptr Word8)
wirePeekOffsetFetchRequestGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_memberid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_memberepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_topics, p4) <- WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopics version _fp _basePtr p e) p3 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (OffsetFetchRequestGroup { offsetFetchRequestGroupGroupId = f0_groupid, offsetFetchRequestGroupMemberId = f1_memberid, offsetFetchRequestGroupMemberEpoch = f2_memberepoch, offsetFetchRequestGroupTopics = f3_topics }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchRequest.
wireMaxSizeOffsetFetchRequest :: Int -> OffsetFetchRequest -> Int
wireMaxSizeOffsetFetchRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchRequestGroupId msg))
  + (5 + (case P.unKafkaArray (offsetFetchRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestTopic _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (offsetFetchRequestGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchRequestGroup _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for OffsetFetchRequest.
wirePokeOffsetFetchRequest :: Int -> Ptr Word8 -> OffsetFetchRequest -> IO (Ptr Word8)
wirePokeOffsetFetchRequest version basePtr msg
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg))
    p2 <- WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version == 7 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg))
    p2 <- WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg)
    p3 <- W.pokeWord8 p2 (if (offsetFetchRequestRequireStable msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p3
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchRequestGroup version p x) p0 (offsetFetchRequestGroups msg)
    p2 <- W.pokeWord8 p1 (if (offsetFetchRequestRequireStable msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchRequestGroupId msg))
    p2 <- WP.pokeVersionedNullableArray version 6 (\p x -> wirePokeOffsetFetchRequestTopic version p x) p1 (offsetFetchRequestTopics msg)
    pure p2
  | otherwise = error $ "wirePoke OffsetFetchRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetFetchRequest.
wirePeekOffsetFetchRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchRequest, Ptr Word8)
wirePeekOffsetFetchRequest version _fp _basePtr p0 endPtr
  | version == 6 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = False }, pTagsEnd)
  | version == 7 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr
    (f2_requirestable, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = f2_requirestable }, pTagsEnd)
  | version >= 8 && version <= 10 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchRequestGroup version _fp _basePtr p e) p0 endPtr
    (f1_requirestable, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = P.KafkaString Null, offsetFetchRequestTopics = P.KafkaArray P.Null, offsetFetchRequestGroups = f0_groups, offsetFetchRequestRequireStable = f1_requirestable }, pTagsEnd)
  | version >= 1 && version <= 5 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedNullableArray version 6 (\p e -> wirePeekOffsetFetchRequestTopic version _fp _basePtr p e) p1 endPtr
    pure (OffsetFetchRequest { offsetFetchRequestGroupId = f0_groupid, offsetFetchRequestTopics = f1_topics, offsetFetchRequestGroups = P.mkKafkaArray V.empty, offsetFetchRequestRequireStable = False }, p2)
  | otherwise = error $ "wirePeek OffsetFetchRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec OffsetFetchRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetFetchRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetFetchRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetFetchRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}