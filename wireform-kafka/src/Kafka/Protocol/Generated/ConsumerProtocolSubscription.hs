{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ConsumerProtocolSubscription
Description : Kafka ConsumerProtocolSubscription message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0-3
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ConsumerProtocolSubscription
  (
    ConsumerProtocolSubscription(..),
    TopicPartition(..),
    encodeConsumerProtocolSubscription,
    decodeConsumerProtocolSubscription,
    maxConsumerProtocolSubscriptionVersion
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


-- | The partitions that the member owns.
data TopicPartition = TopicPartition
  {

  -- | The topic name.

  -- Versions: 1+
  topicPartitionTopic :: !(KafkaString)
,

  -- | The partition ids.

  -- Versions: 1+
  topicPartitionPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartition with version-aware field handling.
encodeTopicPartition :: MonadPut m => E.ApiVersion -> TopicPartition -> m ()
encodeTopicPartition version tmsg =
  do
    when (version >= 1) $
      serialize (topicPartitionTopic tmsg)
    when (version >= 1) $
      serialize (topicPartitionPartitions tmsg) -- ArrayType: PrimitiveType "int32"


-- | Decode TopicPartition with version-aware field handling.
decodeTopicPartition :: MonadGet m => E.ApiVersion -> m TopicPartition
decodeTopicPartition version =
  do
    fieldtopic <- if version >= 1
      then deserialize
      else pure (P.KafkaString Null)
    fieldpartitions <- if version >= 1
      then deserialize
      else pure (P.mkKafkaArray V.empty)
    pure TopicPartition
      {
      topicPartitionTopic = fieldtopic
      ,
      topicPartitionPartitions = fieldpartitions
      }



data ConsumerProtocolSubscription = ConsumerProtocolSubscription
  {

  -- | The topics that the member wants to consume.

  -- Versions: 0+
  consumerProtocolSubscriptionTopics :: !(KafkaArray (KafkaString))
,

  -- | User data that will be passed back to the consumer.

  -- Versions: 0+
  consumerProtocolSubscriptionUserData :: !(KafkaBytes)
,

  -- | The partitions that the member owns.

  -- Versions: 1+
  consumerProtocolSubscriptionOwnedPartitions :: !(KafkaArray (TopicPartition))
,

  -- | The generation id of the member.

  -- Versions: 2+
  consumerProtocolSubscriptionGenerationId :: !(Int32)
,

  -- | The rack id of the member.

  -- Versions: 3+
  consumerProtocolSubscriptionRackId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ConsumerProtocolSubscription.
maxConsumerProtocolSubscriptionVersion :: Int16
maxConsumerProtocolSubscriptionVersion = 3



-- | Encode ConsumerProtocolSubscription with the given API version.
encodeConsumerProtocolSubscription :: MonadPut m => E.ApiVersion -> ConsumerProtocolSubscription -> m ()
encodeConsumerProtocolSubscription version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 999 (\v s -> if v >= 999 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (consumerProtocolSubscriptionTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionUserData msg)


  | version == 1 =
    do
      E.encodeVersionedArray version 999 (\v s -> if v >= 999 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (consumerProtocolSubscriptionTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionUserData msg)
      E.encodeVersionedArray version 999 encodeTopicPartition (case P.unKafkaArray (consumerProtocolSubscriptionOwnedPartitions msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 999 (\v s -> if v >= 999 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (consumerProtocolSubscriptionTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionUserData msg)
      E.encodeVersionedArray version 999 encodeTopicPartition (case P.unKafkaArray (consumerProtocolSubscriptionOwnedPartitions msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionGenerationId msg)


  | version == 3 =
    do
      E.encodeVersionedArray version 999 (\v s -> if v >= 999 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (consumerProtocolSubscriptionTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionUserData msg)
      E.encodeVersionedArray version 999 encodeTopicPartition (case P.unKafkaArray (consumerProtocolSubscriptionOwnedPartitions msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolSubscriptionGenerationId msg)
      serialize (consumerProtocolSubscriptionRackId msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ConsumerProtocolSubscription with the given API version.
decodeConsumerProtocolSubscription :: MonadGet m => E.ApiVersion -> m ConsumerProtocolSubscription
decodeConsumerProtocolSubscription version
  | version == 0 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 (\v -> if v >= 999 then P.fromCompactString <$> deserialize else deserialize)
      fielduserdata <- deserialize
      pure ConsumerProtocolSubscription
        {
        consumerProtocolSubscriptionTopics = fieldtopics
        ,
        consumerProtocolSubscriptionUserData = fielduserdata
        ,
        consumerProtocolSubscriptionOwnedPartitions = P.mkKafkaArray V.empty
        ,
        consumerProtocolSubscriptionGenerationId = (-1)
        ,
        consumerProtocolSubscriptionRackId = P.KafkaString Null
        }

  | version == 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 (\v -> if v >= 999 then P.fromCompactString <$> deserialize else deserialize)
      fielduserdata <- deserialize
      fieldownedpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeTopicPartition
      pure ConsumerProtocolSubscription
        {
        consumerProtocolSubscriptionTopics = fieldtopics
        ,
        consumerProtocolSubscriptionUserData = fielduserdata
        ,
        consumerProtocolSubscriptionOwnedPartitions = fieldownedpartitions
        ,
        consumerProtocolSubscriptionGenerationId = (-1)
        ,
        consumerProtocolSubscriptionRackId = P.KafkaString Null
        }

  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 (\v -> if v >= 999 then P.fromCompactString <$> deserialize else deserialize)
      fielduserdata <- deserialize
      fieldownedpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeTopicPartition
      fieldgenerationid <- deserialize
      pure ConsumerProtocolSubscription
        {
        consumerProtocolSubscriptionTopics = fieldtopics
        ,
        consumerProtocolSubscriptionUserData = fielduserdata
        ,
        consumerProtocolSubscriptionOwnedPartitions = fieldownedpartitions
        ,
        consumerProtocolSubscriptionGenerationId = fieldgenerationid
        ,
        consumerProtocolSubscriptionRackId = P.KafkaString Null
        }

  | version == 3 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 (\v -> if v >= 999 then P.fromCompactString <$> deserialize else deserialize)
      fielduserdata <- deserialize
      fieldownedpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeTopicPartition
      fieldgenerationid <- deserialize
      fieldrackid <- deserialize
      pure ConsumerProtocolSubscription
        {
        consumerProtocolSubscriptionTopics = fieldtopics
        ,
        consumerProtocolSubscriptionUserData = fielduserdata
        ,
        consumerProtocolSubscriptionOwnedPartitions = fieldownedpartitions
        ,
        consumerProtocolSubscriptionGenerationId = fieldgenerationid
        ,
        consumerProtocolSubscriptionRackId = fieldrackid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a TopicPartition.
wireMaxSizeTopicPartition :: Int -> TopicPartition -> Int
wireMaxSizeTopicPartition _version msg =
  0
  + WP.kafkaStringMaxSize (topicPartitionTopic msg)
  + (5 + (case P.unKafkaArray (topicPartitionPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))


-- | Direct-poke encoder for TopicPartition.
wirePokeTopicPartition :: Int -> Ptr Word8 -> TopicPartition -> IO (Ptr Word8)
wirePokeTopicPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaString p0 (topicPartitionTopic msg)
  p2 <- WP.pokeKafkaArray W.pokeInt32BE p1 (topicPartitionPartitions msg)
  pure p2

-- | Direct-poke decoder for TopicPartition.
wirePeekTopicPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicPartition, Ptr Word8)
wirePeekTopicPartition version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- WP.peekKafkaString p0 endPtr
  (f1_partitions, p2) <- WP.peekKafkaArray W.peekInt32BE p1 endPtr
  pure (TopicPartition { topicPartitionTopic = f0_topic, topicPartitionPartitions = f1_partitions }, p2)

-- | Worst-case wire size of a ConsumerProtocolSubscription.
wireMaxSizeConsumerProtocolSubscription :: Int -> ConsumerProtocolSubscription -> Int
wireMaxSizeConsumerProtocolSubscription _version msg =
  0
  + (5 + (case P.unKafkaArray (consumerProtocolSubscriptionTopics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + WP.kafkaBytesMaxSize (consumerProtocolSubscriptionUserData msg)
  + (5 + (case P.unKafkaArray (consumerProtocolSubscriptionOwnedPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicPartition _version x ) v); P.Null -> 0 }))
  + 4
  + WP.kafkaStringMaxSize (consumerProtocolSubscriptionRackId msg)


-- | Direct-poke encoder for ConsumerProtocolSubscription.
wirePokeConsumerProtocolSubscription :: Int -> Ptr Word8 -> ConsumerProtocolSubscription -> IO (Ptr Word8)
wirePokeConsumerProtocolSubscription version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaArray WP.pokeKafkaString p0 (consumerProtocolSubscriptionTopics msg)
    p2 <- WP.pokeKafkaBytes p1 (consumerProtocolSubscriptionUserData msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaArray WP.pokeKafkaString p0 (consumerProtocolSubscriptionTopics msg)
    p2 <- WP.pokeKafkaBytes p1 (consumerProtocolSubscriptionUserData msg)
    p3 <- WP.pokeKafkaArray (\p x -> wirePokeTopicPartition version p x) p2 (consumerProtocolSubscriptionOwnedPartitions msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaArray WP.pokeKafkaString p0 (consumerProtocolSubscriptionTopics msg)
    p2 <- WP.pokeKafkaBytes p1 (consumerProtocolSubscriptionUserData msg)
    p3 <- WP.pokeKafkaArray (\p x -> wirePokeTopicPartition version p x) p2 (consumerProtocolSubscriptionOwnedPartitions msg)
    p4 <- W.pokeInt32BE p3 (consumerProtocolSubscriptionGenerationId msg)
    pure p4
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaArray WP.pokeKafkaString p0 (consumerProtocolSubscriptionTopics msg)
    p2 <- WP.pokeKafkaBytes p1 (consumerProtocolSubscriptionUserData msg)
    p3 <- WP.pokeKafkaArray (\p x -> wirePokeTopicPartition version p x) p2 (consumerProtocolSubscriptionOwnedPartitions msg)
    p4 <- W.pokeInt32BE p3 (consumerProtocolSubscriptionGenerationId msg)
    p5 <- WP.pokeKafkaString p4 (consumerProtocolSubscriptionRackId msg)
    pure p5
  | otherwise = error $ "wirePoke ConsumerProtocolSubscription : unsupported version: " ++ show version

-- | Direct-poke decoder for ConsumerProtocolSubscription.
wirePeekConsumerProtocolSubscription :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ConsumerProtocolSubscription, Ptr Word8)
wirePeekConsumerProtocolSubscription version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_topics, p1) <- WP.peekKafkaArray WP.peekKafkaString p0 endPtr
    (f1_userdata, p2) <- WP.peekKafkaBytes p1 endPtr
    pure (ConsumerProtocolSubscription { consumerProtocolSubscriptionTopics = f0_topics, consumerProtocolSubscriptionUserData = f1_userdata, consumerProtocolSubscriptionOwnedPartitions = P.mkKafkaArray V.empty, consumerProtocolSubscriptionGenerationId = 0, consumerProtocolSubscriptionRackId = P.KafkaString Null }, p2)
  | version == 1 = do
    (f0_topics, p1) <- WP.peekKafkaArray WP.peekKafkaString p0 endPtr
    (f1_userdata, p2) <- WP.peekKafkaBytes p1 endPtr
    (f2_ownedpartitions, p3) <- WP.peekKafkaArray (\p e -> wirePeekTopicPartition version _fp _basePtr p e) p2 endPtr
    pure (ConsumerProtocolSubscription { consumerProtocolSubscriptionTopics = f0_topics, consumerProtocolSubscriptionUserData = f1_userdata, consumerProtocolSubscriptionOwnedPartitions = f2_ownedpartitions, consumerProtocolSubscriptionGenerationId = 0, consumerProtocolSubscriptionRackId = P.KafkaString Null }, p3)
  | version == 2 = do
    (f0_topics, p1) <- WP.peekKafkaArray WP.peekKafkaString p0 endPtr
    (f1_userdata, p2) <- WP.peekKafkaBytes p1 endPtr
    (f2_ownedpartitions, p3) <- WP.peekKafkaArray (\p e -> wirePeekTopicPartition version _fp _basePtr p e) p2 endPtr
    (f3_generationid, p4) <- W.peekInt32BE p3 endPtr
    pure (ConsumerProtocolSubscription { consumerProtocolSubscriptionTopics = f0_topics, consumerProtocolSubscriptionUserData = f1_userdata, consumerProtocolSubscriptionOwnedPartitions = f2_ownedpartitions, consumerProtocolSubscriptionGenerationId = f3_generationid, consumerProtocolSubscriptionRackId = P.KafkaString Null }, p4)
  | version == 3 = do
    (f0_topics, p1) <- WP.peekKafkaArray WP.peekKafkaString p0 endPtr
    (f1_userdata, p2) <- WP.peekKafkaBytes p1 endPtr
    (f2_ownedpartitions, p3) <- WP.peekKafkaArray (\p e -> wirePeekTopicPartition version _fp _basePtr p e) p2 endPtr
    (f3_generationid, p4) <- W.peekInt32BE p3 endPtr
    (f4_rackid, p5) <- WP.peekKafkaString p4 endPtr
    pure (ConsumerProtocolSubscription { consumerProtocolSubscriptionTopics = f0_topics, consumerProtocolSubscriptionUserData = f1_userdata, consumerProtocolSubscriptionOwnedPartitions = f2_ownedpartitions, consumerProtocolSubscriptionGenerationId = f3_generationid, consumerProtocolSubscriptionRackId = f4_rackid }, p5)
  | otherwise = error $ "wirePeek ConsumerProtocolSubscription : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ConsumerProtocolSubscription where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeConsumerProtocolSubscription (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeConsumerProtocolSubscription (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekConsumerProtocolSubscription (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}