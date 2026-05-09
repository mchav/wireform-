{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ConsumerProtocolAssignment
Description : Kafka ConsumerProtocolAssignment message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0-3
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ConsumerProtocolAssignment
  (
    ConsumerProtocolAssignment(..),
    TopicPartition(..),
    encodeConsumerProtocolAssignment,
    decodeConsumerProtocolAssignment,
    maxConsumerProtocolAssignmentVersion
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


-- | The list of topics and partitions assigned to this consumer.
data TopicPartition = TopicPartition
  {

  -- | The topic name.

  -- Versions: 0+
  topicPartitionTopic :: !(KafkaString)
,

  -- | The list of partitions assigned to this consumer.

  -- Versions: 0+
  topicPartitionPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartition with version-aware field handling.
encodeTopicPartition :: MonadPut m => E.ApiVersion -> TopicPartition -> m ()
encodeTopicPartition _version tmsg =
  do
    serialize (topicPartitionTopic tmsg)
    serialize (topicPartitionPartitions tmsg) -- ArrayType: PrimitiveType "int32"


-- | Decode TopicPartition with version-aware field handling.
decodeTopicPartition :: MonadGet m => E.ApiVersion -> m TopicPartition
decodeTopicPartition _version =
  do
    fieldtopic <- deserialize
    fieldpartitions <- deserialize
    pure TopicPartition
      {
      topicPartitionTopic = fieldtopic
      ,
      topicPartitionPartitions = fieldpartitions
      }



data ConsumerProtocolAssignment = ConsumerProtocolAssignment
  {

  -- | The list of topics and partitions assigned to this consumer.

  -- Versions: 0+
  consumerProtocolAssignmentAssignedPartitions :: !(KafkaArray (TopicPartition))
,

  -- | User data.

  -- Versions: 0+
  consumerProtocolAssignmentUserData :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ConsumerProtocolAssignment.
maxConsumerProtocolAssignmentVersion :: Int16
maxConsumerProtocolAssignmentVersion = 3



-- | Encode ConsumerProtocolAssignment with the given API version.
encodeConsumerProtocolAssignment :: MonadPut m => E.ApiVersion -> ConsumerProtocolAssignment -> m ()
encodeConsumerProtocolAssignment version msg
  | version >= 0 && version <= 3 =
    do
      E.encodeVersionedArray version 999 encodeTopicPartition (case P.unKafkaArray (consumerProtocolAssignmentAssignedPartitions msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerProtocolAssignmentUserData msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ConsumerProtocolAssignment with the given API version.
decodeConsumerProtocolAssignment :: MonadGet m => E.ApiVersion -> m ConsumerProtocolAssignment
decodeConsumerProtocolAssignment version
  | version >= 0 && version <= 3 =
    do
      fieldassignedpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeTopicPartition
      fielduserdata <- deserialize
      pure ConsumerProtocolAssignment
        {
        consumerProtocolAssignmentAssignedPartitions = fieldassignedpartitions
        ,
        consumerProtocolAssignmentUserData = fielduserdata
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

-- | Worst-case wire size of a ConsumerProtocolAssignment.
wireMaxSizeConsumerProtocolAssignment :: Int -> ConsumerProtocolAssignment -> Int
wireMaxSizeConsumerProtocolAssignment _version msg =
  0
  + (5 + (case P.unKafkaArray (consumerProtocolAssignmentAssignedPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicPartition _version x ) v); P.Null -> 0 }))
  + WP.kafkaBytesMaxSize (consumerProtocolAssignmentUserData msg)


-- | Direct-poke encoder for ConsumerProtocolAssignment.
wirePokeConsumerProtocolAssignment :: Int -> Ptr Word8 -> ConsumerProtocolAssignment -> IO (Ptr Word8)
wirePokeConsumerProtocolAssignment version basePtr msg
  | version >= 0 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaArray (\p x -> wirePokeTopicPartition version p x) p0 (consumerProtocolAssignmentAssignedPartitions msg)
    p2 <- WP.pokeKafkaBytes p1 (consumerProtocolAssignmentUserData msg)
    pure p2
  | otherwise = error $ "wirePoke ConsumerProtocolAssignment : unsupported version: " ++ show version

-- | Direct-poke decoder for ConsumerProtocolAssignment.
wirePeekConsumerProtocolAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ConsumerProtocolAssignment, Ptr Word8)
wirePeekConsumerProtocolAssignment version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 3 = do
    (f0_assignedpartitions, p1) <- WP.peekKafkaArray (\p e -> wirePeekTopicPartition version _fp _basePtr p e) p0 endPtr
    (f1_userdata, p2) <- WP.peekKafkaBytes p1 endPtr
    pure (ConsumerProtocolAssignment { consumerProtocolAssignmentAssignedPartitions = f0_assignedpartitions, consumerProtocolAssignmentUserData = f1_userdata }, p2)
  | otherwise = error $ "wirePeek ConsumerProtocolAssignment : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ConsumerProtocolAssignment where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeConsumerProtocolAssignment (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeConsumerProtocolAssignment (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekConsumerProtocolAssignment (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}