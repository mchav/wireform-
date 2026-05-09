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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ConsumerProtocolAssignment where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
