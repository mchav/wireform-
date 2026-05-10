{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeLogDirsRequest
Description : Kafka DescribeLogDirsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 35.



Valid versions: 1-5
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeLogDirsRequest
  (
    DescribeLogDirsRequest(..),
    DescribableLogDirTopic(..),
    maxDescribeLogDirsRequestVersion
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


-- | Each topic that we want to describe log directories for, or null for all topics.
data DescribableLogDirTopic = DescribableLogDirTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describableLogDirTopicTopic :: !(KafkaString)
,

  -- | The partition indexes.

  -- Versions: 0+
  describableLogDirTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


data DescribeLogDirsRequest = DescribeLogDirsRequest
  {

  -- | Each topic that we want to describe log directories for, or null for all topics.

  -- Versions: 0+
  describeLogDirsRequestTopics :: !(KafkaArray (DescribableLogDirTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeLogDirsRequest.
maxDescribeLogDirsRequestVersion :: Int16
maxDescribeLogDirsRequestVersion = 5

-- | KafkaMessage instance for DescribeLogDirsRequest.
instance KafkaMessage DescribeLogDirsRequest where
  messageApiKey = 35
  messageMinVersion = 1
  messageMaxVersion = 5
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a DescribableLogDirTopic.
wireMaxSizeDescribableLogDirTopic :: Int -> DescribableLogDirTopic -> Int
wireMaxSizeDescribableLogDirTopic _version msg =
  0
  + WP.dualStringMaxSize (describableLogDirTopicTopic msg)
  + (5 + (case P.unKafkaArray (describableLogDirTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribableLogDirTopic.
wirePokeDescribableLogDirTopic :: Int -> Ptr Word8 -> DescribableLogDirTopic -> IO (Ptr Word8)
wirePokeDescribableLogDirTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (describableLogDirTopicTopic msg)) else WP.pokeKafkaString p0 (describableLogDirTopicTopic msg))
  p2 <- WP.pokeVersionedArray version 2 W.pokeInt32BE p1 (describableLogDirTopicPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribableLogDirTopic.
wirePeekDescribableLogDirTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribableLogDirTopic, Ptr Word8)
wirePeekDescribableLogDirTopic version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribableLogDirTopic { describableLogDirTopicTopic = f0_topic, describableLogDirTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribableLogDirTopic :: DescribableLogDirTopic
defaultDescribableLogDirTopic = DescribableLogDirTopic { describableLogDirTopicTopic = P.KafkaString Null, describableLogDirTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DescribeLogDirsRequest.
wireMaxSizeDescribeLogDirsRequest :: Int -> DescribeLogDirsRequest -> Int
wireMaxSizeDescribeLogDirsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeLogDirsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribableLogDirTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeLogDirsRequest.
wirePokeDescribeLogDirsRequest :: Int -> Ptr Word8 -> DescribeLogDirsRequest -> IO (Ptr Word8)
wirePokeDescribeLogDirsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeDescribableLogDirTopic version p x) p0 (describeLogDirsRequestTopics msg)
    pure p1
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeDescribableLogDirTopic version p x) p0 (describeLogDirsRequestTopics msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeLogDirsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeLogDirsRequest.
wirePeekDescribeLogDirsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsRequest, Ptr Word8)
wirePeekDescribeLogDirsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekDescribableLogDirTopic version _fp _basePtr p e) p0 endPtr
    pure (DescribeLogDirsRequest { describeLogDirsRequestTopics = f0_topics }, p1)
  | version >= 2 && version <= 5 = do
    (f0_topics, p1) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekDescribableLogDirTopic version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeLogDirsRequest { describeLogDirsRequestTopics = f0_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeLogDirsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeLogDirsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeLogDirsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeLogDirsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeLogDirsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}