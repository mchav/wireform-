{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetForLeaderEpochResponse
Description : Kafka OffsetForLeaderEpochResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 23.



Valid versions: 2-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetForLeaderEpochResponse
  (
    OffsetForLeaderEpochResponse(..),
    OffsetForLeaderTopicResult(..),
    EpochEndOffset(..),
    encodeOffsetForLeaderEpochResponse,
    decodeOffsetForLeaderEpochResponse,
    maxOffsetForLeaderEpochResponseVersion
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


-- | Each partition in the topic we fetched offsets for.
data EpochEndOffset = EpochEndOffset
  {

  -- | The error code 0, or if there was no error.

  -- Versions: 0+
  epochEndOffsetErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  epochEndOffsetPartition :: !(Int32)
,

  -- | The leader epoch of the partition.

  -- Versions: 1+
  epochEndOffsetLeaderEpoch :: !(Int32)
,

  -- | The end offset of the epoch.

  -- Versions: 0+
  epochEndOffsetEndOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode EpochEndOffset with version-aware field handling.
encodeEpochEndOffset :: MonadPut m => E.ApiVersion -> EpochEndOffset -> m ()
encodeEpochEndOffset version emsg =
  do
    serialize (epochEndOffsetErrorCode emsg)
    serialize (epochEndOffsetPartition emsg)
    when (version >= 1) $
      serialize (epochEndOffsetLeaderEpoch emsg)
    serialize (epochEndOffsetEndOffset emsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EpochEndOffset with version-aware field handling.
decodeEpochEndOffset :: MonadGet m => E.ApiVersion -> m EpochEndOffset
decodeEpochEndOffset version =
  do
    fielderrorcode <- deserialize
    fieldpartition <- deserialize
    fieldleaderepoch <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldendoffset <- deserialize
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EpochEndOffset
      {
      epochEndOffsetErrorCode = fielderrorcode
      ,
      epochEndOffsetPartition = fieldpartition
      ,
      epochEndOffsetLeaderEpoch = fieldleaderepoch
      ,
      epochEndOffsetEndOffset = fieldendoffset
      }


-- | Each topic we fetched offsets for.
data OffsetForLeaderTopicResult = OffsetForLeaderTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  offsetForLeaderTopicResultTopic :: !(KafkaString)
,

  -- | Each partition in the topic we fetched offsets for.

  -- Versions: 0+
  offsetForLeaderTopicResultPartitions :: !(KafkaArray (EpochEndOffset))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderTopicResult with version-aware field handling.
encodeOffsetForLeaderTopicResult :: MonadPut m => E.ApiVersion -> OffsetForLeaderTopicResult -> m ()
encodeOffsetForLeaderTopicResult version omsg =
  do
    if version >= 4 then serialize (toCompactString (offsetForLeaderTopicResultTopic omsg)) else serialize (offsetForLeaderTopicResultTopic omsg)
    E.encodeVersionedArray version 4 encodeEpochEndOffset (case P.unKafkaArray (offsetForLeaderTopicResultPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderTopicResult with version-aware field handling.
decodeOffsetForLeaderTopicResult :: MonadGet m => E.ApiVersion -> m OffsetForLeaderTopicResult
decodeOffsetForLeaderTopicResult version =
  do
    fieldtopic <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeEpochEndOffset
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderTopicResult
      {
      offsetForLeaderTopicResultTopic = fieldtopic
      ,
      offsetForLeaderTopicResultPartitions = fieldpartitions
      }



data OffsetForLeaderEpochResponse = OffsetForLeaderEpochResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  offsetForLeaderEpochResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic we fetched offsets for.

  -- Versions: 0+
  offsetForLeaderEpochResponseTopics :: !(KafkaArray (OffsetForLeaderTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetForLeaderEpochResponse.
maxOffsetForLeaderEpochResponseVersion :: Int16
maxOffsetForLeaderEpochResponseVersion = 4

-- | KafkaMessage instance for OffsetForLeaderEpochResponse.
instance KafkaMessage OffsetForLeaderEpochResponse where
  messageApiKey = 23
  messageMinVersion = 2
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4

-- | Encode OffsetForLeaderEpochResponse with the given API version.
encodeOffsetForLeaderEpochResponse :: MonadPut m => E.ApiVersion -> OffsetForLeaderEpochResponse -> m ()
encodeOffsetForLeaderEpochResponse version msg
  | version == 4 =
    do
      serialize (offsetForLeaderEpochResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopicResult (case P.unKafkaArray (offsetForLeaderEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 2 && version <= 3 =
    do
      serialize (offsetForLeaderEpochResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopicResult (case P.unKafkaArray (offsetForLeaderEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetForLeaderEpochResponse with the given API version.
decodeOffsetForLeaderEpochResponse :: MonadGet m => E.ApiVersion -> m OffsetForLeaderEpochResponse
decodeOffsetForLeaderEpochResponse version
  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetForLeaderEpochResponse
        {
        offsetForLeaderEpochResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetForLeaderEpochResponseTopics = fieldtopics
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopicResult
      pure OffsetForLeaderEpochResponse
        {
        offsetForLeaderEpochResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetForLeaderEpochResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a EpochEndOffset.
wireMaxSizeEpochEndOffset :: Int -> EpochEndOffset -> Int
wireMaxSizeEpochEndOffset _version msg =
  0
  + 2
  + 4
  + 4
  + 8
  + 1

-- | Direct-poke encoder for EpochEndOffset.
wirePokeEpochEndOffset :: Int -> Ptr Word8 -> EpochEndOffset -> IO (Ptr Word8)
wirePokeEpochEndOffset version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (epochEndOffsetErrorCode msg)
  p2 <- W.pokeInt32BE p1 (epochEndOffsetPartition msg)
  p3 <- W.pokeInt32BE p2 (epochEndOffsetLeaderEpoch msg)
  p4 <- W.pokeInt64BE p3 (epochEndOffsetEndOffset msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for EpochEndOffset.
wirePeekEpochEndOffset :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EpochEndOffset, Ptr Word8)
wirePeekEpochEndOffset version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_partition, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_endoffset, p4) <- W.peekInt64BE p3 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (EpochEndOffset { epochEndOffsetErrorCode = f0_errorcode, epochEndOffsetPartition = f1_partition, epochEndOffsetLeaderEpoch = f2_leaderepoch, epochEndOffsetEndOffset = f3_endoffset }, pTagsEnd)

-- | Worst-case wire size of a OffsetForLeaderTopicResult.
wireMaxSizeOffsetForLeaderTopicResult :: Int -> OffsetForLeaderTopicResult -> Int
wireMaxSizeOffsetForLeaderTopicResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetForLeaderTopicResultTopic msg))
  + (5 + (case P.unKafkaArray (offsetForLeaderTopicResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEpochEndOffset _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetForLeaderTopicResult.
wirePokeOffsetForLeaderTopicResult :: Int -> Ptr Word8 -> OffsetForLeaderTopicResult -> IO (Ptr Word8)
wirePokeOffsetForLeaderTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetForLeaderTopicResultTopic msg))
  p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeEpochEndOffset version p x) p1 (offsetForLeaderTopicResultPartitions msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetForLeaderTopicResult.
wirePeekOffsetForLeaderTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetForLeaderTopicResult, Ptr Word8)
wirePeekOffsetForLeaderTopicResult version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekEpochEndOffset version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetForLeaderTopicResult { offsetForLeaderTopicResultTopic = f0_topic, offsetForLeaderTopicResultPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a OffsetForLeaderEpochResponse.
wireMaxSizeOffsetForLeaderEpochResponse :: Int -> OffsetForLeaderEpochResponse -> Int
wireMaxSizeOffsetForLeaderEpochResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (offsetForLeaderEpochResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetForLeaderTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetForLeaderEpochResponse.
wirePokeOffsetForLeaderEpochResponse :: Int -> Ptr Word8 -> OffsetForLeaderEpochResponse -> IO (Ptr Word8)
wirePokeOffsetForLeaderEpochResponse version basePtr msg
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetForLeaderEpochResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderTopicResult version p x) p1 (offsetForLeaderEpochResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetForLeaderEpochResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderTopicResult version p x) p1 (offsetForLeaderEpochResponseTopics msg)
    pure p2
  | otherwise = error $ "wirePoke OffsetForLeaderEpochResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetForLeaderEpochResponse.
wirePeekOffsetForLeaderEpochResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetForLeaderEpochResponse, Ptr Word8)
wirePeekOffsetForLeaderEpochResponse version _fp _basePtr p0 endPtr
  | version == 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetForLeaderEpochResponse { offsetForLeaderEpochResponseThrottleTimeMs = f0_throttletimems, offsetForLeaderEpochResponseTopics = f1_topics }, pTagsEnd)
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderTopicResult version _fp _basePtr p e) p1 endPtr
    pure (OffsetForLeaderEpochResponse { offsetForLeaderEpochResponseThrottleTimeMs = f0_throttletimems, offsetForLeaderEpochResponseTopics = f1_topics }, p2)
  | otherwise = error $ "wirePeek OffsetForLeaderEpochResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetForLeaderEpochResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetForLeaderEpochResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetForLeaderEpochResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetForLeaderEpochResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}