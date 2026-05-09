{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetDeleteResponse
Description : Kafka OffsetDeleteResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 47.



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetDeleteResponse
  (
    OffsetDeleteResponse(..),
    OffsetDeleteResponseTopic(..),
    OffsetDeleteResponsePartition(..),
    encodeOffsetDeleteResponse,
    decodeOffsetDeleteResponse,
    maxOffsetDeleteResponseVersion
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


-- | The responses for each partition in the topic.
data OffsetDeleteResponsePartition = OffsetDeleteResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetDeleteResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  offsetDeleteResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteResponsePartition with version-aware field handling.
encodeOffsetDeleteResponsePartition :: MonadPut m => E.ApiVersion -> OffsetDeleteResponsePartition -> m ()
encodeOffsetDeleteResponsePartition _version omsg =
  do
    serialize (offsetDeleteResponsePartitionPartitionIndex omsg)
    serialize (offsetDeleteResponsePartitionErrorCode omsg)


-- | Decode OffsetDeleteResponsePartition with version-aware field handling.
decodeOffsetDeleteResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponsePartition
decodeOffsetDeleteResponsePartition _version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    pure OffsetDeleteResponsePartition
      {
      offsetDeleteResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetDeleteResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses for each topic.
data OffsetDeleteResponseTopic = OffsetDeleteResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetDeleteResponseTopicName :: !(KafkaString)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  offsetDeleteResponseTopicPartitions :: !(KafkaArray (OffsetDeleteResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteResponseTopic with version-aware field handling.
encodeOffsetDeleteResponseTopic :: MonadPut m => E.ApiVersion -> OffsetDeleteResponseTopic -> m ()
encodeOffsetDeleteResponseTopic version omsg =
  do
    serialize (offsetDeleteResponseTopicName omsg)
    E.encodeVersionedArray version 999 encodeOffsetDeleteResponsePartition (case P.unKafkaArray (offsetDeleteResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })


-- | Decode OffsetDeleteResponseTopic with version-aware field handling.
decodeOffsetDeleteResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponseTopic
decodeOffsetDeleteResponseTopic version =
  do
    fieldname <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteResponsePartition
    pure OffsetDeleteResponseTopic
      {
      offsetDeleteResponseTopicName = fieldname
      ,
      offsetDeleteResponseTopicPartitions = fieldpartitions
      }



data OffsetDeleteResponse = OffsetDeleteResponse
  {

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  offsetDeleteResponseErrorCode :: !(Int16)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  offsetDeleteResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  offsetDeleteResponseTopics :: !(KafkaArray (OffsetDeleteResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetDeleteResponse.
maxOffsetDeleteResponseVersion :: Int16
maxOffsetDeleteResponseVersion = 0

-- | KafkaMessage instance for OffsetDeleteResponse.
instance KafkaMessage OffsetDeleteResponse where
  messageApiKey = 47
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode OffsetDeleteResponse with the given API version.
encodeOffsetDeleteResponse :: MonadPut m => E.ApiVersion -> OffsetDeleteResponse -> m ()
encodeOffsetDeleteResponse version msg
  | version == 0 =
    do
      serialize (offsetDeleteResponseErrorCode msg)
      serialize (offsetDeleteResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 999 encodeOffsetDeleteResponseTopic (case P.unKafkaArray (offsetDeleteResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetDeleteResponse with the given API version.
decodeOffsetDeleteResponse :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponse
decodeOffsetDeleteResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteResponseTopic
      pure OffsetDeleteResponse
        {
        offsetDeleteResponseErrorCode = fielderrorcode
        ,
        offsetDeleteResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetDeleteResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a OffsetDeleteResponsePartition.
wireMaxSizeOffsetDeleteResponsePartition :: Int -> OffsetDeleteResponsePartition -> Int
wireMaxSizeOffsetDeleteResponsePartition _version msg =
  0
  + 4
  + 2


-- | Direct-poke encoder for OffsetDeleteResponsePartition.
wirePokeOffsetDeleteResponsePartition :: Int -> Ptr Word8 -> OffsetDeleteResponsePartition -> IO (Ptr Word8)
wirePokeOffsetDeleteResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetDeleteResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (offsetDeleteResponsePartitionErrorCode msg)
  pure p2

-- | Direct-poke decoder for OffsetDeleteResponsePartition.
wirePeekOffsetDeleteResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteResponsePartition, Ptr Word8)
wirePeekOffsetDeleteResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pure (OffsetDeleteResponsePartition { offsetDeleteResponsePartitionPartitionIndex = f0_partitionindex, offsetDeleteResponsePartitionErrorCode = f1_errorcode }, p2)

-- | Worst-case wire size of a OffsetDeleteResponseTopic.
wireMaxSizeOffsetDeleteResponseTopic :: Int -> OffsetDeleteResponseTopic -> Int
wireMaxSizeOffsetDeleteResponseTopic _version msg =
  0
  + WP.kafkaStringMaxSize (offsetDeleteResponseTopicName msg)
  + (5 + (case P.unKafkaArray (offsetDeleteResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetDeleteResponsePartition _version x ) v); P.Null -> 0 }))


-- | Direct-poke encoder for OffsetDeleteResponseTopic.
wirePokeOffsetDeleteResponseTopic :: Int -> Ptr Word8 -> OffsetDeleteResponseTopic -> IO (Ptr Word8)
wirePokeOffsetDeleteResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaString p0 (offsetDeleteResponseTopicName msg)
  p2 <- WP.pokeKafkaArray (\p x -> wirePokeOffsetDeleteResponsePartition version p x) p1 (offsetDeleteResponseTopicPartitions msg)
  pure p2

-- | Direct-poke decoder for OffsetDeleteResponseTopic.
wirePeekOffsetDeleteResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteResponseTopic, Ptr Word8)
wirePeekOffsetDeleteResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- WP.peekKafkaString p0 endPtr
  (f1_partitions, p2) <- WP.peekKafkaArray (\p e -> wirePeekOffsetDeleteResponsePartition version _fp _basePtr p e) p1 endPtr
  pure (OffsetDeleteResponseTopic { offsetDeleteResponseTopicName = f0_name, offsetDeleteResponseTopicPartitions = f1_partitions }, p2)

-- | Worst-case wire size of a OffsetDeleteResponse.
wireMaxSizeOffsetDeleteResponse :: Int -> OffsetDeleteResponse -> Int
wireMaxSizeOffsetDeleteResponse _version msg =
  0
  + 2
  + 4
  + (5 + (case P.unKafkaArray (offsetDeleteResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetDeleteResponseTopic _version x ) v); P.Null -> 0 }))


-- | Direct-poke encoder for OffsetDeleteResponse.
wirePokeOffsetDeleteResponse :: Int -> Ptr Word8 -> OffsetDeleteResponse -> IO (Ptr Word8)
wirePokeOffsetDeleteResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (offsetDeleteResponseErrorCode msg)
    p2 <- W.pokeInt32BE p1 (offsetDeleteResponseThrottleTimeMs msg)
    p3 <- WP.pokeKafkaArray (\p x -> wirePokeOffsetDeleteResponseTopic version p x) p2 (offsetDeleteResponseTopics msg)
    pure p3
  | otherwise = error $ "wirePoke OffsetDeleteResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetDeleteResponse.
wirePeekOffsetDeleteResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetDeleteResponse, Ptr Word8)
wirePeekOffsetDeleteResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_throttletimems, p2) <- W.peekInt32BE p1 endPtr
    (f2_topics, p3) <- WP.peekKafkaArray (\p e -> wirePeekOffsetDeleteResponseTopic version _fp _basePtr p e) p2 endPtr
    pure (OffsetDeleteResponse { offsetDeleteResponseErrorCode = f0_errorcode, offsetDeleteResponseThrottleTimeMs = f1_throttletimems, offsetDeleteResponseTopics = f2_topics }, p3)
  | otherwise = error $ "wirePeek OffsetDeleteResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetDeleteResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetDeleteResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetDeleteResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetDeleteResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}