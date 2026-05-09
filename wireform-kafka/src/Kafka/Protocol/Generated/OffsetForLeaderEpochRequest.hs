{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetForLeaderEpochRequest
Description : Kafka OffsetForLeaderEpochRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 23.



Valid versions: 2-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetForLeaderEpochRequest
  (
    OffsetForLeaderEpochRequest(..),
    OffsetForLeaderTopic(..),
    OffsetForLeaderPartition(..),
    encodeOffsetForLeaderEpochRequest,
    decodeOffsetForLeaderEpochRequest,
    maxOffsetForLeaderEpochRequestVersion
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


-- | Each partition to get offsets for.
data OffsetForLeaderPartition = OffsetForLeaderPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetForLeaderPartitionPartition :: !(Int32)
,

  -- | An epoch used to fence consumers/replicas with old metadata. If the epoch provided by the client is 

  -- Versions: 2+
  offsetForLeaderPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The epoch to look up an offset for.

  -- Versions: 0+
  offsetForLeaderPartitionLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderPartition with version-aware field handling.
encodeOffsetForLeaderPartition :: MonadPut m => E.ApiVersion -> OffsetForLeaderPartition -> m ()
encodeOffsetForLeaderPartition version omsg =
  do
    serialize (offsetForLeaderPartitionPartition omsg)
    when (version >= 2) $
      serialize (offsetForLeaderPartitionCurrentLeaderEpoch omsg)
    serialize (offsetForLeaderPartitionLeaderEpoch omsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderPartition with version-aware field handling.
decodeOffsetForLeaderPartition :: MonadGet m => E.ApiVersion -> m OffsetForLeaderPartition
decodeOffsetForLeaderPartition version =
  do
    fieldpartition <- deserialize
    fieldcurrentleaderepoch <- if version >= 2
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- deserialize
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderPartition
      {
      offsetForLeaderPartitionPartition = fieldpartition
      ,
      offsetForLeaderPartitionCurrentLeaderEpoch = fieldcurrentleaderepoch
      ,
      offsetForLeaderPartitionLeaderEpoch = fieldleaderepoch
      }


-- | Each topic to get offsets for.
data OffsetForLeaderTopic = OffsetForLeaderTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetForLeaderTopicTopic :: !(KafkaString)
,

  -- | Each partition to get offsets for.

  -- Versions: 0+
  offsetForLeaderTopicPartitions :: !(KafkaArray (OffsetForLeaderPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderTopic with version-aware field handling.
encodeOffsetForLeaderTopic :: MonadPut m => E.ApiVersion -> OffsetForLeaderTopic -> m ()
encodeOffsetForLeaderTopic version omsg =
  do
    if version >= 4 then serialize (toCompactString (offsetForLeaderTopicTopic omsg)) else serialize (offsetForLeaderTopicTopic omsg)
    E.encodeVersionedArray version 4 encodeOffsetForLeaderPartition (case P.unKafkaArray (offsetForLeaderTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderTopic with version-aware field handling.
decodeOffsetForLeaderTopic :: MonadGet m => E.ApiVersion -> m OffsetForLeaderTopic
decodeOffsetForLeaderTopic version =
  do
    fieldtopic <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderPartition
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderTopic
      {
      offsetForLeaderTopicTopic = fieldtopic
      ,
      offsetForLeaderTopicPartitions = fieldpartitions
      }



data OffsetForLeaderEpochRequest = OffsetForLeaderEpochRequest
  {

  -- | The broker ID of the follower, of -1 if this request is from a consumer.

  -- Versions: 3+
  offsetForLeaderEpochRequestReplicaId :: !(Int32)
,

  -- | Each topic to get offsets for.

  -- Versions: 0+
  offsetForLeaderEpochRequestTopics :: !(KafkaArray (OffsetForLeaderTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetForLeaderEpochRequest.
maxOffsetForLeaderEpochRequestVersion :: Int16
maxOffsetForLeaderEpochRequestVersion = 4

-- | KafkaMessage instance for OffsetForLeaderEpochRequest.
instance KafkaMessage OffsetForLeaderEpochRequest where
  messageApiKey = 23
  messageMinVersion = 2
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4

-- | Encode OffsetForLeaderEpochRequest with the given API version.
encodeOffsetForLeaderEpochRequest :: MonadPut m => E.ApiVersion -> OffsetForLeaderEpochRequest -> m ()
encodeOffsetForLeaderEpochRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 3 =
    do
      serialize (offsetForLeaderEpochRequestReplicaId msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 4 =
    do
      serialize (offsetForLeaderEpochRequestReplicaId msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetForLeaderEpochRequest with the given API version.
decodeOffsetForLeaderEpochRequest :: MonadGet m => E.ApiVersion -> m OffsetForLeaderEpochRequest
decodeOffsetForLeaderEpochRequest version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = (-2)
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }

  | version == 3 =
    do
      fieldreplicaid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = fieldreplicaid
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }

  | version == 4 =
    do
      fieldreplicaid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = fieldreplicaid
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a OffsetForLeaderPartition.
wireMaxSizeOffsetForLeaderPartition :: Int -> OffsetForLeaderPartition -> Int
wireMaxSizeOffsetForLeaderPartition _version msg =
  0
  + 4
  + 4
  + 4
  + 1

-- | Direct-poke encoder for OffsetForLeaderPartition.
wirePokeOffsetForLeaderPartition :: Int -> Ptr Word8 -> OffsetForLeaderPartition -> IO (Ptr Word8)
wirePokeOffsetForLeaderPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetForLeaderPartitionPartition msg)
  p2 <- W.pokeInt32BE p1 (offsetForLeaderPartitionCurrentLeaderEpoch msg)
  p3 <- W.pokeInt32BE p2 (offsetForLeaderPartitionLeaderEpoch msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetForLeaderPartition.
wirePeekOffsetForLeaderPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetForLeaderPartition, Ptr Word8)
wirePeekOffsetForLeaderPartition version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_currentleaderepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetForLeaderPartition { offsetForLeaderPartitionPartition = f0_partition, offsetForLeaderPartitionCurrentLeaderEpoch = f1_currentleaderepoch, offsetForLeaderPartitionLeaderEpoch = f2_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a OffsetForLeaderTopic.
wireMaxSizeOffsetForLeaderTopic :: Int -> OffsetForLeaderTopic -> Int
wireMaxSizeOffsetForLeaderTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetForLeaderTopicTopic msg))
  + (5 + (case P.unKafkaArray (offsetForLeaderTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetForLeaderPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetForLeaderTopic.
wirePokeOffsetForLeaderTopic :: Int -> Ptr Word8 -> OffsetForLeaderTopic -> IO (Ptr Word8)
wirePokeOffsetForLeaderTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetForLeaderTopicTopic msg))
  p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderPartition version p x) p1 (offsetForLeaderTopicPartitions msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetForLeaderTopic.
wirePeekOffsetForLeaderTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetForLeaderTopic, Ptr Word8)
wirePeekOffsetForLeaderTopic version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetForLeaderTopic { offsetForLeaderTopicTopic = f0_topic, offsetForLeaderTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a OffsetForLeaderEpochRequest.
wireMaxSizeOffsetForLeaderEpochRequest :: Int -> OffsetForLeaderEpochRequest -> Int
wireMaxSizeOffsetForLeaderEpochRequest _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetForLeaderTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetForLeaderEpochRequest.
wirePokeOffsetForLeaderEpochRequest :: Int -> Ptr Word8 -> OffsetForLeaderEpochRequest -> IO (Ptr Word8)
wirePokeOffsetForLeaderEpochRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderTopic version p x) p0 (offsetForLeaderEpochRequestTopics msg)
    pure p1
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetForLeaderEpochRequestReplicaId msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderTopic version p x) p1 (offsetForLeaderEpochRequestTopics msg)
    pure p2
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetForLeaderEpochRequestReplicaId msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeOffsetForLeaderTopic version p x) p1 (offsetForLeaderEpochRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke OffsetForLeaderEpochRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetForLeaderEpochRequest.
wirePeekOffsetForLeaderEpochRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetForLeaderEpochRequest, Ptr Word8)
wirePeekOffsetForLeaderEpochRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderTopic version _fp _basePtr p e) p0 endPtr
    pure (OffsetForLeaderEpochRequest { offsetForLeaderEpochRequestReplicaId = 0, offsetForLeaderEpochRequestTopics = f0_topics }, p1)
  | version == 3 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderTopic version _fp _basePtr p e) p1 endPtr
    pure (OffsetForLeaderEpochRequest { offsetForLeaderEpochRequestReplicaId = f0_replicaid, offsetForLeaderEpochRequestTopics = f1_topics }, p2)
  | version == 4 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekOffsetForLeaderTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetForLeaderEpochRequest { offsetForLeaderEpochRequestReplicaId = f0_replicaid, offsetForLeaderEpochRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek OffsetForLeaderEpochRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetForLeaderEpochRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetForLeaderEpochRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetForLeaderEpochRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetForLeaderEpochRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}