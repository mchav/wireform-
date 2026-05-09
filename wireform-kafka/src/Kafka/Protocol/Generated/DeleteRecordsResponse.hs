{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteRecordsResponse
Description : Kafka DeleteRecordsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 21.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteRecordsResponse
  (
    DeleteRecordsResponse(..),
    DeleteRecordsTopicResult(..),
    DeleteRecordsPartitionResult(..),
    encodeDeleteRecordsResponse,
    decodeDeleteRecordsResponse,
    maxDeleteRecordsResponseVersion
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


-- | Each partition that we wanted to delete records from.
data DeleteRecordsPartitionResult = DeleteRecordsPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  deleteRecordsPartitionResultPartitionIndex :: !(Int32)
,

  -- | The partition low water mark.

  -- Versions: 0+
  deleteRecordsPartitionResultLowWatermark :: !(Int64)
,

  -- | The deletion error code, or 0 if the deletion succeeded.

  -- Versions: 0+
  deleteRecordsPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsPartitionResult with version-aware field handling.
encodeDeleteRecordsPartitionResult :: MonadPut m => E.ApiVersion -> DeleteRecordsPartitionResult -> m ()
encodeDeleteRecordsPartitionResult version dmsg =
  do
    serialize (deleteRecordsPartitionResultPartitionIndex dmsg)
    serialize (deleteRecordsPartitionResultLowWatermark dmsg)
    serialize (deleteRecordsPartitionResultErrorCode dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsPartitionResult with version-aware field handling.
decodeDeleteRecordsPartitionResult :: MonadGet m => E.ApiVersion -> m DeleteRecordsPartitionResult
decodeDeleteRecordsPartitionResult version =
  do
    fieldpartitionindex <- deserialize
    fieldlowwatermark <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsPartitionResult
      {
      deleteRecordsPartitionResultPartitionIndex = fieldpartitionindex
      ,
      deleteRecordsPartitionResultLowWatermark = fieldlowwatermark
      ,
      deleteRecordsPartitionResultErrorCode = fielderrorcode
      }


-- | Each topic that we wanted to delete records from.
data DeleteRecordsTopicResult = DeleteRecordsTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  deleteRecordsTopicResultName :: !(KafkaString)
,

  -- | Each partition that we wanted to delete records from.

  -- Versions: 0+
  deleteRecordsTopicResultPartitions :: !(KafkaArray (DeleteRecordsPartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsTopicResult with version-aware field handling.
encodeDeleteRecordsTopicResult :: MonadPut m => E.ApiVersion -> DeleteRecordsTopicResult -> m ()
encodeDeleteRecordsTopicResult version dmsg =
  do
    if version >= 2 then serialize (toCompactString (deleteRecordsTopicResultName dmsg)) else serialize (deleteRecordsTopicResultName dmsg)
    E.encodeVersionedArray version 2 encodeDeleteRecordsPartitionResult (case P.unKafkaArray (deleteRecordsTopicResultPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsTopicResult with version-aware field handling.
decodeDeleteRecordsTopicResult :: MonadGet m => E.ApiVersion -> m DeleteRecordsTopicResult
decodeDeleteRecordsTopicResult version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsPartitionResult
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsTopicResult
      {
      deleteRecordsTopicResultName = fieldname
      ,
      deleteRecordsTopicResultPartitions = fieldpartitions
      }



data DeleteRecordsResponse = DeleteRecordsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteRecordsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic that we wanted to delete records from.

  -- Versions: 0+
  deleteRecordsResponseTopics :: !(KafkaArray (DeleteRecordsTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteRecordsResponse.
maxDeleteRecordsResponseVersion :: Int16
maxDeleteRecordsResponseVersion = 2

-- | KafkaMessage instance for DeleteRecordsResponse.
instance KafkaMessage DeleteRecordsResponse where
  messageApiKey = 21
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode DeleteRecordsResponse with the given API version.
encodeDeleteRecordsResponse :: MonadPut m => E.ApiVersion -> DeleteRecordsResponse -> m ()
encodeDeleteRecordsResponse version msg
  | version == 2 =
    do
      serialize (deleteRecordsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopicResult (case P.unKafkaArray (deleteRecordsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (deleteRecordsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopicResult (case P.unKafkaArray (deleteRecordsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteRecordsResponse with the given API version.
decodeDeleteRecordsResponse :: MonadGet m => E.ApiVersion -> m DeleteRecordsResponse
decodeDeleteRecordsResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteRecordsResponse
        {
        deleteRecordsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteRecordsResponseTopics = fieldtopics
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopicResult
      pure DeleteRecordsResponse
        {
        deleteRecordsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteRecordsResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DeleteRecordsPartitionResult.
wireMaxSizeDeleteRecordsPartitionResult :: Int -> DeleteRecordsPartitionResult -> Int
wireMaxSizeDeleteRecordsPartitionResult _version msg =
  0
  + 4
  + 8
  + 2
  + 1

-- | Direct-poke encoder for DeleteRecordsPartitionResult.
wirePokeDeleteRecordsPartitionResult :: Int -> Ptr Word8 -> DeleteRecordsPartitionResult -> IO (Ptr Word8)
wirePokeDeleteRecordsPartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (deleteRecordsPartitionResultPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (deleteRecordsPartitionResultLowWatermark msg)
  p3 <- W.pokeInt16BE p2 (deleteRecordsPartitionResultErrorCode msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for DeleteRecordsPartitionResult.
wirePeekDeleteRecordsPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsPartitionResult, Ptr Word8)
wirePeekDeleteRecordsPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_lowwatermark, p2) <- W.peekInt64BE p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (DeleteRecordsPartitionResult { deleteRecordsPartitionResultPartitionIndex = f0_partitionindex, deleteRecordsPartitionResultLowWatermark = f1_lowwatermark, deleteRecordsPartitionResultErrorCode = f2_errorcode }, pTagsEnd)

-- | Worst-case wire size of a DeleteRecordsTopicResult.
wireMaxSizeDeleteRecordsTopicResult :: Int -> DeleteRecordsTopicResult -> Int
wireMaxSizeDeleteRecordsTopicResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteRecordsTopicResultName msg))
  + (5 + (case P.unKafkaArray (deleteRecordsTopicResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteRecordsPartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteRecordsTopicResult.
wirePokeDeleteRecordsTopicResult :: Int -> Ptr Word8 -> DeleteRecordsTopicResult -> IO (Ptr Word8)
wirePokeDeleteRecordsTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteRecordsTopicResultName msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsPartitionResult version p x) p1 (deleteRecordsTopicResultPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteRecordsTopicResult.
wirePeekDeleteRecordsTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsTopicResult, Ptr Word8)
wirePeekDeleteRecordsTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteRecordsTopicResult { deleteRecordsTopicResultName = f0_name, deleteRecordsTopicResultPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DeleteRecordsResponse.
wireMaxSizeDeleteRecordsResponse :: Int -> DeleteRecordsResponse -> Int
wireMaxSizeDeleteRecordsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (deleteRecordsResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteRecordsTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteRecordsResponse.
wirePokeDeleteRecordsResponse :: Int -> Ptr Word8 -> DeleteRecordsResponse -> IO (Ptr Word8)
wirePokeDeleteRecordsResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteRecordsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsTopicResult version p x) p1 (deleteRecordsResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteRecordsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsTopicResult version p x) p1 (deleteRecordsResponseTopics msg)
    pure p2
  | otherwise = error $ "wirePoke DeleteRecordsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteRecordsResponse.
wirePeekDeleteRecordsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsResponse, Ptr Word8)
wirePeekDeleteRecordsResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteRecordsResponse { deleteRecordsResponseThrottleTimeMs = f0_throttletimems, deleteRecordsResponseTopics = f1_topics }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsTopicResult version _fp _basePtr p e) p1 endPtr
    pure (DeleteRecordsResponse { deleteRecordsResponseThrottleTimeMs = f0_throttletimems, deleteRecordsResponseTopics = f1_topics }, p2)
  | otherwise = error $ "wirePeek DeleteRecordsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DeleteRecordsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteRecordsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteRecordsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteRecordsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}