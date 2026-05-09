{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteRecordsRequest
Description : Kafka DeleteRecordsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 21.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteRecordsRequest
  (
    DeleteRecordsRequest(..),
    DeleteRecordsTopic(..),
    DeleteRecordsPartition(..),
    encodeDeleteRecordsRequest,
    decodeDeleteRecordsRequest,
    maxDeleteRecordsRequestVersion
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


-- | Each partition that we want to delete records from.
data DeleteRecordsPartition = DeleteRecordsPartition
  {

  -- | The partition index.

  -- Versions: 0+
  deleteRecordsPartitionPartitionIndex :: !(Int32)
,

  -- | The deletion offset. -1 means that records should be truncated to the high watermark.

  -- Versions: 0+
  deleteRecordsPartitionOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsPartition with version-aware field handling.
encodeDeleteRecordsPartition :: MonadPut m => E.ApiVersion -> DeleteRecordsPartition -> m ()
encodeDeleteRecordsPartition version dmsg =
  do
    serialize (deleteRecordsPartitionPartitionIndex dmsg)
    serialize (deleteRecordsPartitionOffset dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsPartition with version-aware field handling.
decodeDeleteRecordsPartition :: MonadGet m => E.ApiVersion -> m DeleteRecordsPartition
decodeDeleteRecordsPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldoffset <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsPartition
      {
      deleteRecordsPartitionPartitionIndex = fieldpartitionindex
      ,
      deleteRecordsPartitionOffset = fieldoffset
      }


-- | Each topic that we want to delete records from.
data DeleteRecordsTopic = DeleteRecordsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteRecordsTopicName :: !(KafkaString)
,

  -- | Each partition that we want to delete records from.

  -- Versions: 0+
  deleteRecordsTopicPartitions :: !(KafkaArray (DeleteRecordsPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsTopic with version-aware field handling.
encodeDeleteRecordsTopic :: MonadPut m => E.ApiVersion -> DeleteRecordsTopic -> m ()
encodeDeleteRecordsTopic version dmsg =
  do
    if version >= 2 then serialize (toCompactString (deleteRecordsTopicName dmsg)) else serialize (deleteRecordsTopicName dmsg)
    E.encodeVersionedArray version 2 encodeDeleteRecordsPartition (case P.unKafkaArray (deleteRecordsTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsTopic with version-aware field handling.
decodeDeleteRecordsTopic :: MonadGet m => E.ApiVersion -> m DeleteRecordsTopic
decodeDeleteRecordsTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsPartition
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsTopic
      {
      deleteRecordsTopicName = fieldname
      ,
      deleteRecordsTopicPartitions = fieldpartitions
      }



data DeleteRecordsRequest = DeleteRecordsRequest
  {

  -- | Each topic that we want to delete records from.

  -- Versions: 0+
  deleteRecordsRequestTopics :: !(KafkaArray (DeleteRecordsTopic))
,

  -- | How long to wait for the deletion to complete, in milliseconds.

  -- Versions: 0+
  deleteRecordsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteRecordsRequest.
maxDeleteRecordsRequestVersion :: Int16
maxDeleteRecordsRequestVersion = 2

-- | KafkaMessage instance for DeleteRecordsRequest.
instance KafkaMessage DeleteRecordsRequest where
  messageApiKey = 21
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode DeleteRecordsRequest with the given API version.
encodeDeleteRecordsRequest :: MonadPut m => E.ApiVersion -> DeleteRecordsRequest -> m ()
encodeDeleteRecordsRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopic (case P.unKafkaArray (deleteRecordsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteRecordsRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopic (case P.unKafkaArray (deleteRecordsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteRecordsRequestTimeoutMs msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteRecordsRequest with the given API version.
decodeDeleteRecordsRequest :: MonadGet m => E.ApiVersion -> m DeleteRecordsRequest
decodeDeleteRecordsRequest version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopic
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteRecordsRequest
        {
        deleteRecordsRequestTopics = fieldtopics
        ,
        deleteRecordsRequestTimeoutMs = fieldtimeoutms
        }

  | version >= 0 && version <= 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopic
      fieldtimeoutms <- deserialize
      pure DeleteRecordsRequest
        {
        deleteRecordsRequestTopics = fieldtopics
        ,
        deleteRecordsRequestTimeoutMs = fieldtimeoutms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DeleteRecordsPartition.
wireMaxSizeDeleteRecordsPartition :: Int -> DeleteRecordsPartition -> Int
wireMaxSizeDeleteRecordsPartition _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for DeleteRecordsPartition.
wirePokeDeleteRecordsPartition :: Int -> Ptr Word8 -> DeleteRecordsPartition -> IO (Ptr Word8)
wirePokeDeleteRecordsPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (deleteRecordsPartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (deleteRecordsPartitionOffset msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteRecordsPartition.
wirePeekDeleteRecordsPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsPartition, Ptr Word8)
wirePeekDeleteRecordsPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_offset, p2) <- W.peekInt64BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteRecordsPartition { deleteRecordsPartitionPartitionIndex = f0_partitionindex, deleteRecordsPartitionOffset = f1_offset }, pTagsEnd)

-- | Worst-case wire size of a DeleteRecordsTopic.
wireMaxSizeDeleteRecordsTopic :: Int -> DeleteRecordsTopic -> Int
wireMaxSizeDeleteRecordsTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteRecordsTopicName msg))
  + (5 + (case P.unKafkaArray (deleteRecordsTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteRecordsPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteRecordsTopic.
wirePokeDeleteRecordsTopic :: Int -> Ptr Word8 -> DeleteRecordsTopic -> IO (Ptr Word8)
wirePokeDeleteRecordsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteRecordsTopicName msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsPartition version p x) p1 (deleteRecordsTopicPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteRecordsTopic.
wirePeekDeleteRecordsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsTopic, Ptr Word8)
wirePeekDeleteRecordsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteRecordsTopic { deleteRecordsTopicName = f0_name, deleteRecordsTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DeleteRecordsRequest.
wireMaxSizeDeleteRecordsRequest :: Int -> DeleteRecordsRequest -> Int
wireMaxSizeDeleteRecordsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (deleteRecordsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteRecordsTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DeleteRecordsRequest.
wirePokeDeleteRecordsRequest :: Int -> Ptr Word8 -> DeleteRecordsRequest -> IO (Ptr Word8)
wirePokeDeleteRecordsRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsTopic version p x) p0 (deleteRecordsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (deleteRecordsRequestTimeoutMs msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteRecordsTopic version p x) p0 (deleteRecordsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (deleteRecordsRequestTimeoutMs msg)
    pure p2
  | otherwise = error $ "wirePoke DeleteRecordsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteRecordsRequest.
wirePeekDeleteRecordsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteRecordsRequest, Ptr Word8)
wirePeekDeleteRecordsRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteRecordsRequest { deleteRecordsRequestTopics = f0_topics, deleteRecordsRequestTimeoutMs = f1_timeoutms }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteRecordsTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    pure (DeleteRecordsRequest { deleteRecordsRequestTopics = f0_topics, deleteRecordsRequestTimeoutMs = f1_timeoutms }, p2)
  | otherwise = error $ "wirePeek DeleteRecordsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteRecordsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteRecordsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteRecordsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteRecordsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}