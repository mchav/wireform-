{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetCommitResponse
Description : Kafka OffsetCommitResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 8.



Valid versions: 2-9
Flexible versions: 8+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetCommitResponse
  (
    OffsetCommitResponse(..),
    OffsetCommitResponseTopic(..),
    OffsetCommitResponsePartition(..),
    maxOffsetCommitResponseVersion
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


-- | The responses for each partition in the topic.
data OffsetCommitResponsePartition = OffsetCommitResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetCommitResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  offsetCommitResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The responses for each topic.
data OffsetCommitResponseTopic = OffsetCommitResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetCommitResponseTopicName :: !(KafkaString)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  offsetCommitResponseTopicPartitions :: !(KafkaArray (OffsetCommitResponsePartition))

  }
  deriving (Eq, Show, Generic)


data OffsetCommitResponse = OffsetCommitResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  offsetCommitResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  offsetCommitResponseTopics :: !(KafkaArray (OffsetCommitResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetCommitResponse.
maxOffsetCommitResponseVersion :: Int16
maxOffsetCommitResponseVersion = 9

-- | KafkaMessage instance for OffsetCommitResponse.
instance KafkaMessage OffsetCommitResponse where
  messageApiKey = 8
  messageMinVersion = 2
  messageMaxVersion = 9
  messageFlexibleVersion = Just 8

-- | Worst-case wire size of a OffsetCommitResponsePartition.
wireMaxSizeOffsetCommitResponsePartition :: Int -> OffsetCommitResponsePartition -> Int
wireMaxSizeOffsetCommitResponsePartition _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for OffsetCommitResponsePartition.
wirePokeOffsetCommitResponsePartition :: Int -> Ptr Word8 -> OffsetCommitResponsePartition -> IO (Ptr Word8)
wirePokeOffsetCommitResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetCommitResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (offsetCommitResponsePartitionErrorCode msg)
  if version >= 8 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetCommitResponsePartition.
wirePeekOffsetCommitResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitResponsePartition, Ptr Word8)
wirePeekOffsetCommitResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 8 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetCommitResponsePartition { offsetCommitResponsePartitionPartitionIndex = f0_partitionindex, offsetCommitResponsePartitionErrorCode = f1_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetCommitResponsePartition :: OffsetCommitResponsePartition
defaultOffsetCommitResponsePartition = OffsetCommitResponsePartition { offsetCommitResponsePartitionPartitionIndex = 0, offsetCommitResponsePartitionErrorCode = 0 }

-- | Worst-case wire size of a OffsetCommitResponseTopic.
wireMaxSizeOffsetCommitResponseTopic :: Int -> OffsetCommitResponseTopic -> Int
wireMaxSizeOffsetCommitResponseTopic _version msg =
  0
  + WP.dualStringMaxSize (offsetCommitResponseTopicName msg)
  + (5 + (case P.unKafkaArray (offsetCommitResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetCommitResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetCommitResponseTopic.
wirePokeOffsetCommitResponseTopic :: Int -> Ptr Word8 -> OffsetCommitResponseTopic -> IO (Ptr Word8)
wirePokeOffsetCommitResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitResponseTopicName msg)) else WP.pokeKafkaString p0 (offsetCommitResponseTopicName msg))
  p2 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponsePartition version p x) p1 (offsetCommitResponseTopicPartitions msg)
  if version >= 8 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetCommitResponseTopic.
wirePeekOffsetCommitResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitResponseTopic, Ptr Word8)
wirePeekOffsetCommitResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponsePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 8 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetCommitResponseTopic { offsetCommitResponseTopicName = f0_name, offsetCommitResponseTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetCommitResponseTopic :: OffsetCommitResponseTopic
defaultOffsetCommitResponseTopic = OffsetCommitResponseTopic { offsetCommitResponseTopicName = P.KafkaString Null, offsetCommitResponseTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetCommitResponse.
wireMaxSizeOffsetCommitResponse :: Int -> OffsetCommitResponse -> Int
wireMaxSizeOffsetCommitResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetCommitResponseTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetCommitResponse.
wirePokeOffsetCommitResponse :: Int -> Ptr Word8 -> OffsetCommitResponse -> IO (Ptr Word8)
wirePokeOffsetCommitResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponseTopic version p x) p0 (offsetCommitResponseTopics msg)
    pure p1
  | version >= 8 && version <= 9 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then W.pokeInt32BE p0 (offsetCommitResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponseTopic version p x) p1 (offsetCommitResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 3 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then W.pokeInt32BE p0 (offsetCommitResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponseTopic version p x) p1 (offsetCommitResponseTopics msg)
    pure p2
  | otherwise = error $ "wirePoke OffsetCommitResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetCommitResponse.
wirePeekOffsetCommitResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitResponse, Ptr Word8)
wirePeekOffsetCommitResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p0 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = 0, offsetCommitResponseTopics = f0_topics }, p1)
  | version >= 8 && version <= 9 = do
    (f0_throttletimems, p1) <- (if version >= 3 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = f0_throttletimems, offsetCommitResponseTopics = f1_topics }, pTagsEnd)
  | version >= 3 && version <= 7 = do
    (f0_throttletimems, p1) <- (if version >= 3 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = f0_throttletimems, offsetCommitResponseTopics = f1_topics }, p2)
  | otherwise = error $ "wirePeek OffsetCommitResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec OffsetCommitResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetCommitResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetCommitResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetCommitResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}