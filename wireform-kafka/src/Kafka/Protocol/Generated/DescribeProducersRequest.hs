{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeProducersRequest
Description : Kafka DescribeProducersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 61.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeProducersRequest
  (
    DescribeProducersRequest(..),
    TopicRequest(..),
    encodeDescribeProducersRequest,
    decodeDescribeProducersRequest,
    maxDescribeProducersRequestVersion
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


-- | The topics to list producers for.
data TopicRequest = TopicRequest
  {

  -- | The topic name.

  -- Versions: 0+
  topicRequestName :: !(KafkaString)
,

  -- | The indexes of the partitions to list producers for.

  -- Versions: 0+
  topicRequestPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicRequest with version-aware field handling.
encodeTopicRequest :: MonadPut m => E.ApiVersion -> TopicRequest -> m ()
encodeTopicRequest version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicRequestName tmsg)) else serialize (topicRequestName tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicRequestPartitionIndexes tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicRequest with version-aware field handling.
decodeTopicRequest :: MonadGet m => E.ApiVersion -> m TopicRequest
decodeTopicRequest version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindexes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicRequest
      {
      topicRequestName = fieldname
      ,
      topicRequestPartitionIndexes = fieldpartitionindexes
      }



data DescribeProducersRequest = DescribeProducersRequest
  {

  -- | The topics to list producers for.

  -- Versions: 0+
  describeProducersRequestTopics :: !(KafkaArray (TopicRequest))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeProducersRequest.
maxDescribeProducersRequestVersion :: Int16
maxDescribeProducersRequestVersion = 0

-- | KafkaMessage instance for DescribeProducersRequest.
instance KafkaMessage DescribeProducersRequest where
  messageApiKey = 61
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode DescribeProducersRequest with the given API version.
encodeDescribeProducersRequest :: MonadPut m => E.ApiVersion -> DescribeProducersRequest -> m ()
encodeDescribeProducersRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeTopicRequest (case P.unKafkaArray (describeProducersRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeProducersRequest with the given API version.
decodeDescribeProducersRequest :: MonadGet m => E.ApiVersion -> m DescribeProducersRequest
decodeDescribeProducersRequest version
  | version == 0 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicRequest
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeProducersRequest
        {
        describeProducersRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a TopicRequest.
wireMaxSizeTopicRequest :: Int -> TopicRequest -> Int
wireMaxSizeTopicRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicRequestName msg))
  + (5 + (case P.unKafkaArray (topicRequestPartitionIndexes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicRequest.
wirePokeTopicRequest :: Int -> Ptr Word8 -> TopicRequest -> IO (Ptr Word8)
wirePokeTopicRequest version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicRequestName msg))
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (topicRequestPartitionIndexes msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicRequest.
wirePeekTopicRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicRequest, Ptr Word8)
wirePeekTopicRequest version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitionindexes, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicRequest { topicRequestName = f0_name, topicRequestPartitionIndexes = f1_partitionindexes }, pTagsEnd)

-- | Worst-case wire size of a DescribeProducersRequest.
wireMaxSizeDescribeProducersRequest :: Int -> DescribeProducersRequest -> Int
wireMaxSizeDescribeProducersRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeProducersRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicRequest _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeProducersRequest.
wirePokeDescribeProducersRequest :: Int -> Ptr Word8 -> DescribeProducersRequest -> IO (Ptr Word8)
wirePokeDescribeProducersRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicRequest version p x) p0 (describeProducersRequestTopics msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeProducersRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeProducersRequest.
wirePeekDescribeProducersRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeProducersRequest, Ptr Word8)
wirePeekDescribeProducersRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicRequest version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeProducersRequest { describeProducersRequestTopics = f0_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeProducersRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeProducersRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeProducersRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeProducersRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeProducersRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}