{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterShareGroupOffsetsRequest
Description : Kafka AlterShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 91.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterShareGroupOffsetsRequest
  (
    AlterShareGroupOffsetsRequest(..),
    AlterShareGroupOffsetsRequestTopic(..),
    AlterShareGroupOffsetsRequestPartition(..),
    encodeAlterShareGroupOffsetsRequest,
    decodeAlterShareGroupOffsetsRequest,
    maxAlterShareGroupOffsetsRequestVersion
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


-- | Each partition to alter offsets for.
data AlterShareGroupOffsetsRequestPartition = AlterShareGroupOffsetsRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  alterShareGroupOffsetsRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  alterShareGroupOffsetsRequestPartitionStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsRequestPartition with version-aware field handling.
encodeAlterShareGroupOffsetsRequestPartition :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequestPartition -> m ()
encodeAlterShareGroupOffsetsRequestPartition version amsg =
  do
    serialize (alterShareGroupOffsetsRequestPartitionPartitionIndex amsg)
    serialize (alterShareGroupOffsetsRequestPartitionStartOffset amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsRequestPartition with version-aware field handling.
decodeAlterShareGroupOffsetsRequestPartition :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequestPartition
decodeAlterShareGroupOffsetsRequestPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsRequestPartition
      {
      alterShareGroupOffsetsRequestPartitionPartitionIndex = fieldpartitionindex
      ,
      alterShareGroupOffsetsRequestPartitionStartOffset = fieldstartoffset
      }


-- | The topics to alter offsets for.
data AlterShareGroupOffsetsRequestTopic = AlterShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)
,

  -- | Each partition to alter offsets for.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopicPartitions :: !(KafkaArray (AlterShareGroupOffsetsRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsRequestTopic with version-aware field handling.
encodeAlterShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequestTopic -> m ()
encodeAlterShareGroupOffsetsRequestTopic version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsRequestTopicTopicName amsg)) else serialize (alterShareGroupOffsetsRequestTopicTopicName amsg)
    E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsRequestPartition (case P.unKafkaArray (alterShareGroupOffsetsRequestTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsRequestTopic with version-aware field handling.
decodeAlterShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequestTopic
decodeAlterShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsRequestPartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsRequestTopic
      {
      alterShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      ,
      alterShareGroupOffsetsRequestTopicPartitions = fieldpartitions
      }



data AlterShareGroupOffsetsRequest = AlterShareGroupOffsetsRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  alterShareGroupOffsetsRequestGroupId :: !(KafkaString)
,

  -- | The topics to alter offsets for.

  -- Versions: 0+
  alterShareGroupOffsetsRequestTopics :: !(KafkaArray (AlterShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterShareGroupOffsetsRequest.
maxAlterShareGroupOffsetsRequestVersion :: Int16
maxAlterShareGroupOffsetsRequestVersion = 0

-- | KafkaMessage instance for AlterShareGroupOffsetsRequest.
instance KafkaMessage AlterShareGroupOffsetsRequest where
  messageApiKey = 91
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode AlterShareGroupOffsetsRequest with the given API version.
encodeAlterShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsRequest -> m ()
encodeAlterShareGroupOffsetsRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (alterShareGroupOffsetsRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsRequestTopic (case P.unKafkaArray (alterShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterShareGroupOffsetsRequest with the given API version.
decodeAlterShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsRequest
decodeAlterShareGroupOffsetsRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterShareGroupOffsetsRequest
        {
        alterShareGroupOffsetsRequestGroupId = fieldgroupid
        ,
        alterShareGroupOffsetsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a AlterShareGroupOffsetsRequestPartition.
wireMaxSizeAlterShareGroupOffsetsRequestPartition :: Int -> AlterShareGroupOffsetsRequestPartition -> Int
wireMaxSizeAlterShareGroupOffsetsRequestPartition _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsRequestPartition.
wirePokeAlterShareGroupOffsetsRequestPartition :: Int -> Ptr Word8 -> AlterShareGroupOffsetsRequestPartition -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsRequestPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (alterShareGroupOffsetsRequestPartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (alterShareGroupOffsetsRequestPartitionStartOffset msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterShareGroupOffsetsRequestPartition.
wirePeekAlterShareGroupOffsetsRequestPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsRequestPartition, Ptr Word8)
wirePeekAlterShareGroupOffsetsRequestPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_startoffset, p2) <- W.peekInt64BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterShareGroupOffsetsRequestPartition { alterShareGroupOffsetsRequestPartitionPartitionIndex = f0_partitionindex, alterShareGroupOffsetsRequestPartitionStartOffset = f1_startoffset }, pTagsEnd)

-- | Worst-case wire size of a AlterShareGroupOffsetsRequestTopic.
wireMaxSizeAlterShareGroupOffsetsRequestTopic :: Int -> AlterShareGroupOffsetsRequestTopic -> Int
wireMaxSizeAlterShareGroupOffsetsRequestTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterShareGroupOffsetsRequestTopicTopicName msg))
  + (5 + (case P.unKafkaArray (alterShareGroupOffsetsRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterShareGroupOffsetsRequestPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsRequestTopic.
wirePokeAlterShareGroupOffsetsRequestTopic :: Int -> Ptr Word8 -> AlterShareGroupOffsetsRequestTopic -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterShareGroupOffsetsRequestTopicTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsRequestPartition version p x) p1 (alterShareGroupOffsetsRequestTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterShareGroupOffsetsRequestTopic.
wirePeekAlterShareGroupOffsetsRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsRequestTopic, Ptr Word8)
wirePeekAlterShareGroupOffsetsRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsRequestPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterShareGroupOffsetsRequestTopic { alterShareGroupOffsetsRequestTopicTopicName = f0_topicname, alterShareGroupOffsetsRequestTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a AlterShareGroupOffsetsRequest.
wireMaxSizeAlterShareGroupOffsetsRequest :: Int -> AlterShareGroupOffsetsRequest -> Int
wireMaxSizeAlterShareGroupOffsetsRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterShareGroupOffsetsRequestGroupId msg))
  + (5 + (case P.unKafkaArray (alterShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterShareGroupOffsetsRequestTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterShareGroupOffsetsRequest.
wirePokeAlterShareGroupOffsetsRequest :: Int -> Ptr Word8 -> AlterShareGroupOffsetsRequest -> IO (Ptr Word8)
wirePokeAlterShareGroupOffsetsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (alterShareGroupOffsetsRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterShareGroupOffsetsRequestTopic version p x) p1 (alterShareGroupOffsetsRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AlterShareGroupOffsetsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterShareGroupOffsetsRequest.
wirePeekAlterShareGroupOffsetsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterShareGroupOffsetsRequest, Ptr Word8)
wirePeekAlterShareGroupOffsetsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterShareGroupOffsetsRequestTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterShareGroupOffsetsRequest { alterShareGroupOffsetsRequestGroupId = f0_groupid, alterShareGroupOffsetsRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterShareGroupOffsetsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterShareGroupOffsetsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterShareGroupOffsetsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterShareGroupOffsetsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterShareGroupOffsetsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}