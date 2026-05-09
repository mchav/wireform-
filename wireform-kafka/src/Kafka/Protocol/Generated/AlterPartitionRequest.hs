{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionRequest
Description : Kafka AlterPartitionRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 56.



Valid versions: 2-3
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionRequest
  (
    AlterPartitionRequest(..),
    TopicData(..),
    PartitionData(..),
    BrokerState(..),
    encodeAlterPartitionRequest,
    decodeAlterPartitionRequest,
    maxAlterPartitionRequestVersion
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
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The ISR for this partition.
data BrokerState = BrokerState
  {

  -- | The ID of the broker.

  -- Versions: 3+
  brokerStateBrokerId :: !(Int32)
,

  -- | The epoch of the broker. It will be -1 if the epoch check is not supported.

  -- Versions: 3+
  brokerStateBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode BrokerState with version-aware field handling.
encodeBrokerState :: MonadPut m => E.ApiVersion -> BrokerState -> m ()
encodeBrokerState version bmsg =
  do
    when (version >= 3) $
      serialize (brokerStateBrokerId bmsg)
    when (version >= 3) $
      serialize (brokerStateBrokerEpoch bmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode BrokerState with version-aware field handling.
decodeBrokerState :: MonadGet m => E.ApiVersion -> m BrokerState
decodeBrokerState version =
  do
    fieldbrokerid <- if version >= 3
      then deserialize
      else pure (0)
    fieldbrokerepoch <- if version >= 3
      then deserialize
      else pure ((-1))
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure BrokerState
      {
      brokerStateBrokerId = fieldbrokerid
      ,
      brokerStateBrokerEpoch = fieldbrokerepoch
      }


-- | The partitions to alter ISRs for.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The ISR for this partition. Deprecated since version 3.

  -- Versions: 0-2
  partitionDataNewIsr :: !(KafkaArray (Int32))
,

  -- | The ISR for this partition.

  -- Versions: 3+
  partitionDataNewIsrWithEpochs :: !(KafkaArray (BrokerState))
,

  -- | 1 if the partition is recovering from an unclean leader election; 0 otherwise.

  -- Versions: 1+
  partitionDataLeaderRecoveryState :: !(Int8)
,

  -- | The expected epoch of the partition which is being updated.

  -- Versions: 0+
  partitionDataPartitionEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version >= 0 && version <= 2) $
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (partitionDataNewIsr pmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 3) $
      E.encodeVersionedArray version 0 encodeBrokerState (case P.unKafkaArray (partitionDataNewIsrWithEpochs pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $
      serialize (partitionDataLeaderRecoveryState pmsg)
    serialize (partitionDataPartitionEpoch pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldleaderepoch <- deserialize
    fieldnewisr <- if version >= 0 && version <= 2
      then P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    fieldnewisrwithepochs <- if version >= 3
      then P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeBrokerState
      else pure (P.mkKafkaArray V.empty)
    fieldleaderrecoverystate <- if version >= 1
      then deserialize
      else pure (0)
    fieldpartitionepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataNewIsr = fieldnewisr
      ,
      partitionDataNewIsrWithEpochs = fieldnewisrwithepochs
      ,
      partitionDataLeaderRecoveryState = fieldleaderrecoverystate
      ,
      partitionDataPartitionEpoch = fieldpartitionepoch
      }


-- | The topics to alter ISRs for.
data TopicData = TopicData
  {

  -- | The ID of the topic to alter ISRs for.

  -- Versions: 2+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The partitions to alter ISRs for.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    when (version >= 2) $
      serialize (topicDataTopicId tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicid <- if version >= 2
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicId = fieldtopicid
      ,
      topicDataPartitions = fieldpartitions
      }



data AlterPartitionRequest = AlterPartitionRequest
  {

  -- | The ID of the requesting broker.

  -- Versions: 0+
  alterPartitionRequestBrokerId :: !(Int32)
,

  -- | The epoch of the requesting broker.

  -- Versions: 0+
  alterPartitionRequestBrokerEpoch :: !(Int64)
,

  -- | The topics to alter ISRs for.

  -- Versions: 0+
  alterPartitionRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionRequest.
maxAlterPartitionRequestVersion :: Int16
maxAlterPartitionRequestVersion = 3

-- | Encode AlterPartitionRequest with the given API version.
encodeAlterPartitionRequest :: MonadPut m => E.ApiVersion -> AlterPartitionRequest -> m ()
encodeAlterPartitionRequest version msg
  | version >= 2 && version <= 3 =
    do
      serialize (alterPartitionRequestBrokerId msg)
      serialize (alterPartitionRequestBrokerEpoch msg)
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (alterPartitionRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionRequest with the given API version.
decodeAlterPartitionRequest :: MonadGet m => E.ApiVersion -> m AlterPartitionRequest
decodeAlterPartitionRequest version
  | version >= 2 && version <= 3 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionRequest
        {
        alterPartitionRequestBrokerId = fieldbrokerid
        ,
        alterPartitionRequestBrokerEpoch = fieldbrokerepoch
        ,
        alterPartitionRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec AlterPartitionRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
