{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListOffsetsResponse
Description : Kafka ListOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 2.



Valid versions: 1-11
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListOffsetsResponse
  (
    ListOffsetsResponse(..),
    ListOffsetsTopicResponse(..),
    ListOffsetsPartitionResponse(..),
    maxListOffsetsResponseVersion
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


-- | Each partition in the response.
data ListOffsetsPartitionResponse = ListOffsetsPartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  listOffsetsPartitionResponsePartitionIndex :: !(Int32)
,

  -- | The partition error code, or 0 if there was no error.

  -- Versions: 0+
  listOffsetsPartitionResponseErrorCode :: !(Int16)
,

  -- | The timestamp associated with the returned offset.

  -- Versions: 1+
  listOffsetsPartitionResponseTimestamp :: !(Int64)
,

  -- | The returned offset.

  -- Versions: 1+
  listOffsetsPartitionResponseOffset :: !(Int64)
,

  -- | The leader epoch associated with the returned offset.

  -- Versions: 4+
  listOffsetsPartitionResponseLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Each topic in the response.
data ListOffsetsTopicResponse = ListOffsetsTopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  listOffsetsTopicResponseName :: !(KafkaString)
,

  -- | Each partition in the response.

  -- Versions: 0+
  listOffsetsTopicResponsePartitions :: !(KafkaArray (ListOffsetsPartitionResponse))

  }
  deriving (Eq, Show, Generic)


data ListOffsetsResponse = ListOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  listOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  listOffsetsResponseTopics :: !(KafkaArray (ListOffsetsTopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListOffsetsResponse.
maxListOffsetsResponseVersion :: Int16
maxListOffsetsResponseVersion = 11

-- | KafkaMessage instance for ListOffsetsResponse.
instance KafkaMessage ListOffsetsResponse where
  messageApiKey = 2
  messageMinVersion = 1
  messageMaxVersion = 11
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a ListOffsetsPartitionResponse.
wireMaxSizeListOffsetsPartitionResponse :: Int -> ListOffsetsPartitionResponse -> Int
wireMaxSizeListOffsetsPartitionResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 8
  + 4
  + 1

-- | Direct-poke encoder for ListOffsetsPartitionResponse.
wirePokeListOffsetsPartitionResponse :: Int -> Ptr Word8 -> ListOffsetsPartitionResponse -> IO (Ptr Word8)
wirePokeListOffsetsPartitionResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (listOffsetsPartitionResponsePartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (listOffsetsPartitionResponseErrorCode msg)
  p3 <- W.pokeInt64BE p2 (listOffsetsPartitionResponseTimestamp msg)
  p4 <- W.pokeInt64BE p3 (listOffsetsPartitionResponseOffset msg)
  p5 <- W.pokeInt32BE p4 (listOffsetsPartitionResponseLeaderEpoch msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for ListOffsetsPartitionResponse.
wirePeekListOffsetsPartitionResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsPartitionResponse, Ptr Word8)
wirePeekListOffsetsPartitionResponse version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_timestamp, p3) <- W.peekInt64BE p2 endPtr
  (f3_offset, p4) <- W.peekInt64BE p3 endPtr
  (f4_leaderepoch, p5) <- W.peekInt32BE p4 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (ListOffsetsPartitionResponse { listOffsetsPartitionResponsePartitionIndex = f0_partitionindex, listOffsetsPartitionResponseErrorCode = f1_errorcode, listOffsetsPartitionResponseTimestamp = f2_timestamp, listOffsetsPartitionResponseOffset = f3_offset, listOffsetsPartitionResponseLeaderEpoch = f4_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a ListOffsetsTopicResponse.
wireMaxSizeListOffsetsTopicResponse :: Int -> ListOffsetsTopicResponse -> Int
wireMaxSizeListOffsetsTopicResponse _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listOffsetsTopicResponseName msg))
  + (5 + (case P.unKafkaArray (listOffsetsTopicResponsePartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsPartitionResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListOffsetsTopicResponse.
wirePokeListOffsetsTopicResponse :: Int -> Ptr Word8 -> ListOffsetsTopicResponse -> IO (Ptr Word8)
wirePokeListOffsetsTopicResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (listOffsetsTopicResponseName msg))
  p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsPartitionResponse version p x) p1 (listOffsetsTopicResponsePartitions msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ListOffsetsTopicResponse.
wirePeekListOffsetsTopicResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsTopicResponse, Ptr Word8)
wirePeekListOffsetsTopicResponse version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsPartitionResponse version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ListOffsetsTopicResponse { listOffsetsTopicResponseName = f0_name, listOffsetsTopicResponsePartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ListOffsetsResponse.
wireMaxSizeListOffsetsResponse :: Int -> ListOffsetsResponse -> Int
wireMaxSizeListOffsetsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (listOffsetsResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsTopicResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListOffsetsResponse.
wirePokeListOffsetsResponse :: Int -> Ptr Word8 -> ListOffsetsResponse -> IO (Ptr Word8)
wirePokeListOffsetsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopicResponse version p x) p0 (listOffsetsResponseTopics msg)
    pure p1
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopicResponse version p x) p1 (listOffsetsResponseTopics msg)
    pure p2
  | version >= 6 && version <= 11 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopicResponse version p x) p1 (listOffsetsResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ListOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ListOffsetsResponse.
wirePeekListOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsResponse, Ptr Word8)
wirePeekListOffsetsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopicResponse version _fp _basePtr p e) p0 endPtr
    pure (ListOffsetsResponse { listOffsetsResponseThrottleTimeMs = 0, listOffsetsResponseTopics = f0_topics }, p1)
  | version >= 2 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopicResponse version _fp _basePtr p e) p1 endPtr
    pure (ListOffsetsResponse { listOffsetsResponseThrottleTimeMs = f0_throttletimems, listOffsetsResponseTopics = f1_topics }, p2)
  | version >= 6 && version <= 11 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopicResponse version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ListOffsetsResponse { listOffsetsResponseThrottleTimeMs = f0_throttletimems, listOffsetsResponseTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ListOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListOffsetsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}