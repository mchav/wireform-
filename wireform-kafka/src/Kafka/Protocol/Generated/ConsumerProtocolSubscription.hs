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
import Data.Bytes.Get (MonadGet)
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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ConsumerProtocolSubscription where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
