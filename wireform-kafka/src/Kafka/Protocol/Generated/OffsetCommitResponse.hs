{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetCommitResponse
Description : Kafka OffsetCommitResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 8.



Valid versions: 2-10
Flexible versions: 8+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetCommitResponse
  (
    OffsetCommitResponse(..),
    OffsetCommitResponseTopic(..),
    OffsetCommitResponsePartition(..),
    encodeOffsetCommitResponse,
    decodeOffsetCommitResponse,
    maxOffsetCommitResponseVersion
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


-- | Encode OffsetCommitResponsePartition with version-aware field handling.
encodeOffsetCommitResponsePartition :: MonadPut m => E.ApiVersion -> OffsetCommitResponsePartition -> m ()
encodeOffsetCommitResponsePartition version omsg =
  do
    serialize (offsetCommitResponsePartitionPartitionIndex omsg)
    serialize (offsetCommitResponsePartitionErrorCode omsg)
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitResponsePartition with version-aware field handling.
decodeOffsetCommitResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetCommitResponsePartition
decodeOffsetCommitResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitResponsePartition
      {
      offsetCommitResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetCommitResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses for each topic.
data OffsetCommitResponseTopic = OffsetCommitResponseTopic
  {

  -- | The topic name.

  -- Versions: 0-9
  offsetCommitResponseTopicName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetCommitResponseTopicTopicId :: !(KafkaUuid)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  offsetCommitResponseTopicPartitions :: !(KafkaArray (OffsetCommitResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetCommitResponseTopic with version-aware field handling.
encodeOffsetCommitResponseTopic :: MonadPut m => E.ApiVersion -> OffsetCommitResponseTopic -> m ()
encodeOffsetCommitResponseTopic version omsg =
  do
    when (version >= 0 && version <= 9) $
      if version >= 8 then serialize (toCompactString (offsetCommitResponseTopicName omsg)) else serialize (offsetCommitResponseTopicName omsg)
    when (version >= 10) $
      serialize (offsetCommitResponseTopicTopicId omsg)
    E.encodeVersionedArray version 8 encodeOffsetCommitResponsePartition (case P.unKafkaArray (offsetCommitResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitResponseTopic with version-aware field handling.
decodeOffsetCommitResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetCommitResponseTopic
decodeOffsetCommitResponseTopic version =
  do
    fieldname <- if version >= 0 && version <= 9
      then if version >= 8 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponsePartition
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitResponseTopic
      {
      offsetCommitResponseTopicName = fieldname
      ,
      offsetCommitResponseTopicTopicId = fieldtopicid
      ,
      offsetCommitResponseTopicPartitions = fieldpartitions
      }



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
maxOffsetCommitResponseVersion = 10

-- | KafkaMessage instance for OffsetCommitResponse.
instance KafkaMessage OffsetCommitResponse where
  messageApiKey = 8
  messageMinVersion = 2
  messageMaxVersion = 10
  messageFlexibleVersion = Just 8

-- | Encode OffsetCommitResponse with the given API version.
encodeOffsetCommitResponse :: MonadPut m => E.ApiVersion -> OffsetCommitResponse -> m ()
encodeOffsetCommitResponse version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 8 && version <= 10 =
    do
      serialize (offsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 7 =
    do
      serialize (offsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetCommitResponse with the given API version.
decodeOffsetCommitResponse :: MonadGet m => E.ApiVersion -> m OffsetCommitResponse
decodeOffsetCommitResponse version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = 0
        ,
        offsetCommitResponseTopics = fieldtopics
        }

  | version >= 8 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetCommitResponseTopics = fieldtopics
        }

  | version >= 3 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetCommitResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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

-- | Worst-case wire size of a OffsetCommitResponseTopic.
wireMaxSizeOffsetCommitResponseTopic :: Int -> OffsetCommitResponseTopic -> Int
wireMaxSizeOffsetCommitResponseTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetCommitResponseTopicName msg))
  + 16
  + (5 + (case P.unKafkaArray (offsetCommitResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetCommitResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetCommitResponseTopic.
wirePokeOffsetCommitResponseTopic :: Int -> Ptr Word8 -> OffsetCommitResponseTopic -> IO (Ptr Word8)
wirePokeOffsetCommitResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetCommitResponseTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (offsetCommitResponseTopicTopicId msg)
  p3 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponsePartition version p x) p2 (offsetCommitResponseTopicPartitions msg)
  if version >= 8 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetCommitResponseTopic.
wirePeekOffsetCommitResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitResponseTopic, Ptr Word8)
wirePeekOffsetCommitResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponsePartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 8 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetCommitResponseTopic { offsetCommitResponseTopicName = f0_name, offsetCommitResponseTopicTopicId = f1_topicid, offsetCommitResponseTopicPartitions = f2_partitions }, pTagsEnd)

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
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetCommitResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponseTopic version p x) p1 (offsetCommitResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 3 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetCommitResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitResponseTopic version p x) p1 (offsetCommitResponseTopics msg)
    pure p2
  | otherwise = error $ "wirePoke OffsetCommitResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetCommitResponse.
wirePeekOffsetCommitResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitResponse, Ptr Word8)
wirePeekOffsetCommitResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p0 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = 0, offsetCommitResponseTopics = f0_topics }, p1)
  | version >= 8 && version <= 10 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = f0_throttletimems, offsetCommitResponseTopics = f1_topics }, pTagsEnd)
  | version >= 3 && version <= 7 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitResponseTopic version _fp _basePtr p e) p1 endPtr
    pure (OffsetCommitResponse { offsetCommitResponseThrottleTimeMs = f0_throttletimems, offsetCommitResponseTopics = f1_topics }, p2)
  | otherwise = error $ "wirePeek OffsetCommitResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec OffsetCommitResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetCommitResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetCommitResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetCommitResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}