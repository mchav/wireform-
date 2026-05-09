{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeShareGroupOffsetsRequest
Description : Kafka DescribeShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 90.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeShareGroupOffsetsRequest
  (
    DescribeShareGroupOffsetsRequest(..),
    DescribeShareGroupOffsetsRequestGroup(..),
    DescribeShareGroupOffsetsRequestTopic(..),
    encodeDescribeShareGroupOffsetsRequest,
    decodeDescribeShareGroupOffsetsRequest,
    maxDescribeShareGroupOffsetsRequestVersion
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


-- | The topics to describe offsets for, or null for all topic-partitions.
data DescribeShareGroupOffsetsRequestTopic = DescribeShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describeShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  describeShareGroupOffsetsRequestTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsRequestTopic with version-aware field handling.
encodeDescribeShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequestTopic -> m ()
encodeDescribeShareGroupOffsetsRequestTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsRequestTopicTopicName dmsg)) else serialize (describeShareGroupOffsetsRequestTopicTopicName dmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (describeShareGroupOffsetsRequestTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsRequestTopic with version-aware field handling.
decodeDescribeShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequestTopic
decodeDescribeShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsRequestTopic
      {
      describeShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      ,
      describeShareGroupOffsetsRequestTopicPartitions = fieldpartitions
      }


-- | The groups to describe offsets for.
data DescribeShareGroupOffsetsRequestGroup = DescribeShareGroupOffsetsRequestGroup
  {

  -- | The group identifier.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroupGroupId :: !(KafkaString)
,

  -- | The topics to describe offsets for, or null for all topic-partitions.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroupTopics :: !(KafkaArray (DescribeShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsRequestGroup with version-aware field handling.
encodeDescribeShareGroupOffsetsRequestGroup :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequestGroup -> m ()
encodeDescribeShareGroupOffsetsRequestGroup version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsRequestGroupGroupId dmsg)) else serialize (describeShareGroupOffsetsRequestGroupGroupId dmsg)
    E.encodeVersionedNullableArray version 0 encodeDescribeShareGroupOffsetsRequestTopic (describeShareGroupOffsetsRequestGroupTopics dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsRequestGroup with version-aware field handling.
decodeDescribeShareGroupOffsetsRequestGroup :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequestGroup
decodeDescribeShareGroupOffsetsRequestGroup version =
  do
    fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopics <- E.decodeVersionedNullableArray version 0 decodeDescribeShareGroupOffsetsRequestTopic
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsRequestGroup
      {
      describeShareGroupOffsetsRequestGroupGroupId = fieldgroupid
      ,
      describeShareGroupOffsetsRequestGroupTopics = fieldtopics
      }



data DescribeShareGroupOffsetsRequest = DescribeShareGroupOffsetsRequest
  {

  -- | The groups to describe offsets for.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroups :: !(KafkaArray (DescribeShareGroupOffsetsRequestGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeShareGroupOffsetsRequest.
maxDescribeShareGroupOffsetsRequestVersion :: Int16
maxDescribeShareGroupOffsetsRequestVersion = 1

-- | KafkaMessage instance for DescribeShareGroupOffsetsRequest.
instance KafkaMessage DescribeShareGroupOffsetsRequest where
  messageApiKey = 90
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode DescribeShareGroupOffsetsRequest with the given API version.
encodeDescribeShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequest -> m ()
encodeDescribeShareGroupOffsetsRequest version msg
  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 0 encodeDescribeShareGroupOffsetsRequestGroup (case P.unKafkaArray (describeShareGroupOffsetsRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeShareGroupOffsetsRequest with the given API version.
decodeDescribeShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequest
decodeDescribeShareGroupOffsetsRequest version
  | version >= 0 && version <= 1 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeShareGroupOffsetsRequestGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeShareGroupOffsetsRequest
        {
        describeShareGroupOffsetsRequestGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DescribeShareGroupOffsetsRequestTopic.
wireMaxSizeDescribeShareGroupOffsetsRequestTopic :: Int -> DescribeShareGroupOffsetsRequestTopic -> Int
wireMaxSizeDescribeShareGroupOffsetsRequestTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeShareGroupOffsetsRequestTopicTopicName msg))
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsRequestTopic.
wirePokeDescribeShareGroupOffsetsRequestTopic :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsRequestTopic -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeShareGroupOffsetsRequestTopicTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (describeShareGroupOffsetsRequestTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribeShareGroupOffsetsRequestTopic.
wirePeekDescribeShareGroupOffsetsRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsRequestTopic, Ptr Word8)
wirePeekDescribeShareGroupOffsetsRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribeShareGroupOffsetsRequestTopic { describeShareGroupOffsetsRequestTopicTopicName = f0_topicname, describeShareGroupOffsetsRequestTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DescribeShareGroupOffsetsRequestGroup.
wireMaxSizeDescribeShareGroupOffsetsRequestGroup :: Int -> DescribeShareGroupOffsetsRequestGroup -> Int
wireMaxSizeDescribeShareGroupOffsetsRequestGroup _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeShareGroupOffsetsRequestGroupGroupId msg))
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsRequestGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeShareGroupOffsetsRequestTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsRequestGroup.
wirePokeDescribeShareGroupOffsetsRequestGroup :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsRequestGroup -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsRequestGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeShareGroupOffsetsRequestGroupGroupId msg))
  p2 <- WP.pokeVersionedNullableArray version 0 (\p x -> wirePokeDescribeShareGroupOffsetsRequestTopic version p x) p1 (describeShareGroupOffsetsRequestGroupTopics msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribeShareGroupOffsetsRequestGroup.
wirePeekDescribeShareGroupOffsetsRequestGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsRequestGroup, Ptr Word8)
wirePeekDescribeShareGroupOffsetsRequestGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topics, p2) <- WP.peekVersionedNullableArray version 0 (\p e -> wirePeekDescribeShareGroupOffsetsRequestTopic version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribeShareGroupOffsetsRequestGroup { describeShareGroupOffsetsRequestGroupGroupId = f0_groupid, describeShareGroupOffsetsRequestGroupTopics = f1_topics }, pTagsEnd)

-- | Worst-case wire size of a DescribeShareGroupOffsetsRequest.
wireMaxSizeDescribeShareGroupOffsetsRequest :: Int -> DescribeShareGroupOffsetsRequest -> Int
wireMaxSizeDescribeShareGroupOffsetsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsRequestGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeShareGroupOffsetsRequestGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsRequest.
wirePokeDescribeShareGroupOffsetsRequest :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsRequest -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeShareGroupOffsetsRequestGroup version p x) p0 (describeShareGroupOffsetsRequestGroups msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeShareGroupOffsetsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeShareGroupOffsetsRequest.
wirePeekDescribeShareGroupOffsetsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsRequest, Ptr Word8)
wirePeekDescribeShareGroupOffsetsRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeShareGroupOffsetsRequestGroup version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeShareGroupOffsetsRequest { describeShareGroupOffsetsRequestGroups = f0_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeShareGroupOffsetsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeShareGroupOffsetsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeShareGroupOffsetsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeShareGroupOffsetsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeShareGroupOffsetsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}