{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetDeleteRequest
Description : Kafka OffsetDeleteRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 47.



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetDeleteRequest
  (
    OffsetDeleteRequest(..),
    OffsetDeleteRequestTopic(..),
    OffsetDeleteRequestPartition(..),
    encodeOffsetDeleteRequest,
    decodeOffsetDeleteRequest,
    maxOffsetDeleteRequestVersion
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


-- | Each partition to delete offsets for.
data OffsetDeleteRequestPartition = OffsetDeleteRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetDeleteRequestPartitionPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteRequestPartition with version-aware field handling.
encodeOffsetDeleteRequestPartition :: MonadPut m => E.ApiVersion -> OffsetDeleteRequestPartition -> m ()
encodeOffsetDeleteRequestPartition _version omsg =
  do
    serialize (offsetDeleteRequestPartitionPartitionIndex omsg)


-- | Decode OffsetDeleteRequestPartition with version-aware field handling.
decodeOffsetDeleteRequestPartition :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequestPartition
decodeOffsetDeleteRequestPartition _version =
  do
    fieldpartitionindex <- deserialize
    pure OffsetDeleteRequestPartition
      {
      offsetDeleteRequestPartitionPartitionIndex = fieldpartitionindex
      }


-- | The topics to delete offsets for.
data OffsetDeleteRequestTopic = OffsetDeleteRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetDeleteRequestTopicName :: !(KafkaString)
,

  -- | Each partition to delete offsets for.

  -- Versions: 0+
  offsetDeleteRequestTopicPartitions :: !(KafkaArray (OffsetDeleteRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteRequestTopic with version-aware field handling.
encodeOffsetDeleteRequestTopic :: MonadPut m => E.ApiVersion -> OffsetDeleteRequestTopic -> m ()
encodeOffsetDeleteRequestTopic version omsg =
  do
    serialize (offsetDeleteRequestTopicName omsg)
    E.encodeVersionedArray version 999 encodeOffsetDeleteRequestPartition (case P.unKafkaArray (offsetDeleteRequestTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })


-- | Decode OffsetDeleteRequestTopic with version-aware field handling.
decodeOffsetDeleteRequestTopic :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequestTopic
decodeOffsetDeleteRequestTopic version =
  do
    fieldname <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteRequestPartition
    pure OffsetDeleteRequestTopic
      {
      offsetDeleteRequestTopicName = fieldname
      ,
      offsetDeleteRequestTopicPartitions = fieldpartitions
      }



data OffsetDeleteRequest = OffsetDeleteRequest
  {

  -- | The unique group identifier.

  -- Versions: 0+
  offsetDeleteRequestGroupId :: !(KafkaString)
,

  -- | The topics to delete offsets for.

  -- Versions: 0+
  offsetDeleteRequestTopics :: !(KafkaArray (OffsetDeleteRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetDeleteRequest.
maxOffsetDeleteRequestVersion :: Int16
maxOffsetDeleteRequestVersion = 0

-- | KafkaMessage instance for OffsetDeleteRequest.
instance KafkaMessage OffsetDeleteRequest where
  messageApiKey = 47
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode OffsetDeleteRequest with the given API version.
encodeOffsetDeleteRequest :: MonadPut m => E.ApiVersion -> OffsetDeleteRequest -> m ()
encodeOffsetDeleteRequest version msg
  | version == 0 =
    do
      serialize (offsetDeleteRequestGroupId msg)
      E.encodeVersionedArray version 999 encodeOffsetDeleteRequestTopic (case P.unKafkaArray (offsetDeleteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetDeleteRequest with the given API version.
decodeOffsetDeleteRequest :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequest
decodeOffsetDeleteRequest version
  | version == 0 =
    do
      fieldgroupid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteRequestTopic
      pure OffsetDeleteRequest
        {
        offsetDeleteRequestGroupId = fieldgroupid
        ,
        offsetDeleteRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a OffsetDeleteRequestPartition.
wireMaxSizeOffsetDeleteRequestPartition :: Int -> OffsetDeleteRequestPartition -> Int
wireMaxSizeOffsetDeleteRequestPartition _version msg =
  0
  + 4


-- | Direct-poke encoder for OffsetDeleteRequestPartition.
wirePokeOffsetDeleteRequestPartition :: Int -> Ptr Word8 -> OffsetDeleteRequestPartition -> IO (Ptr Word8)
wirePokeOffsetDeleteRequestPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetDeleteRequestPartitionPartitionIndex msg)
  pure p1

-- | Direct-poke decoder for OffsetDeleteRequestPartition.
wirePeekOffsetDeleteRequestPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteRequestPartition, Ptr Word8)
wirePeekOffsetDeleteRequestPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  pure (OffsetDeleteRequestPartition { offsetDeleteRequestPartitionPartitionIndex = f0_partitionindex }, p1)

-- | Worst-case wire size of a OffsetDeleteRequestTopic.
wireMaxSizeOffsetDeleteRequestTopic :: Int -> OffsetDeleteRequestTopic -> Int
wireMaxSizeOffsetDeleteRequestTopic _version msg =
  0
  + WP.kafkaStringMaxSize (offsetDeleteRequestTopicName msg)
  + (5 + (case P.unKafkaArray (offsetDeleteRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetDeleteRequestPartition _version x ) v); P.Null -> 0 }))


-- | Direct-poke encoder for OffsetDeleteRequestTopic.
wirePokeOffsetDeleteRequestTopic :: Int -> Ptr Word8 -> OffsetDeleteRequestTopic -> IO (Ptr Word8)
wirePokeOffsetDeleteRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaString p0 (offsetDeleteRequestTopicName msg)
  p2 <- WP.pokeKafkaArray (\p x -> wirePokeOffsetDeleteRequestPartition version p x) p1 (offsetDeleteRequestTopicPartitions msg)
  pure p2

-- | Direct-poke decoder for OffsetDeleteRequestTopic.
wirePeekOffsetDeleteRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteRequestTopic, Ptr Word8)
wirePeekOffsetDeleteRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- WP.peekKafkaString p0 endPtr
  (f1_partitions, p2) <- WP.peekKafkaArray (\p e -> wirePeekOffsetDeleteRequestPartition version _fp _basePtr p e) p1 endPtr
  pure (OffsetDeleteRequestTopic { offsetDeleteRequestTopicName = f0_name, offsetDeleteRequestTopicPartitions = f1_partitions }, p2)

-- | Worst-case wire size of a OffsetDeleteRequest.
wireMaxSizeOffsetDeleteRequest :: Int -> OffsetDeleteRequest -> Int
wireMaxSizeOffsetDeleteRequest _version msg =
  0
  + WP.kafkaStringMaxSize (offsetDeleteRequestGroupId msg)
  + (5 + (case P.unKafkaArray (offsetDeleteRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetDeleteRequestTopic _version x ) v); P.Null -> 0 }))


-- | Direct-poke encoder for OffsetDeleteRequest.
wirePokeOffsetDeleteRequest :: Int -> Ptr Word8 -> OffsetDeleteRequest -> IO (Ptr Word8)
wirePokeOffsetDeleteRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaString p0 (offsetDeleteRequestGroupId msg)
    p2 <- WP.pokeKafkaArray (\p x -> wirePokeOffsetDeleteRequestTopic version p x) p1 (offsetDeleteRequestTopics msg)
    pure p2
  | otherwise = error $ "wirePoke OffsetDeleteRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetDeleteRequest.
wirePeekOffsetDeleteRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteRequest, Ptr Word8)
wirePeekOffsetDeleteRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- WP.peekKafkaString p0 endPtr
    (f1_topics, p2) <- WP.peekKafkaArray (\p e -> wirePeekOffsetDeleteRequestTopic version _fp _basePtr p e) p1 endPtr
    pure (OffsetDeleteRequest { offsetDeleteRequestGroupId = f0_groupid, offsetDeleteRequestTopics = f1_topics }, p2)
  | otherwise = error $ "wirePeek OffsetDeleteRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetDeleteRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetDeleteRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetDeleteRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetDeleteRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}