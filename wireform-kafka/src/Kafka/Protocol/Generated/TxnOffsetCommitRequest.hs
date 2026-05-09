{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.TxnOffsetCommitRequest
Description : Kafka TxnOffsetCommitRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 28.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.TxnOffsetCommitRequest
  (
    TxnOffsetCommitRequest(..),
    TxnOffsetCommitRequestTopic(..),
    TxnOffsetCommitRequestPartition(..),
    encodeTxnOffsetCommitRequest,
    decodeTxnOffsetCommitRequest,
    maxTxnOffsetCommitRequestVersion
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


-- | The partitions inside the topic that we want to commit offsets for.
data TxnOffsetCommitRequestPartition = TxnOffsetCommitRequestPartition
  {

  -- | The index of the partition within the topic.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The message offset to be committed.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch of the last consumed record.

  -- Versions: 2+
  txnOffsetCommitRequestPartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | Any associated metadata the client wants to keep.

  -- Versions: 0+
  txnOffsetCommitRequestPartitionCommittedMetadata :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode TxnOffsetCommitRequestPartition with version-aware field handling.
encodeTxnOffsetCommitRequestPartition :: MonadPut m => E.ApiVersion -> TxnOffsetCommitRequestPartition -> m ()
encodeTxnOffsetCommitRequestPartition version tmsg =
  do
    serialize (txnOffsetCommitRequestPartitionPartitionIndex tmsg)
    serialize (txnOffsetCommitRequestPartitionCommittedOffset tmsg)
    when (version >= 2) $
      serialize (txnOffsetCommitRequestPartitionCommittedLeaderEpoch tmsg)
    if version >= 3 then serialize (toCompactString (txnOffsetCommitRequestPartitionCommittedMetadata tmsg)) else serialize (txnOffsetCommitRequestPartitionCommittedMetadata tmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TxnOffsetCommitRequestPartition with version-aware field handling.
decodeTxnOffsetCommitRequestPartition :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitRequestPartition
decodeTxnOffsetCommitRequestPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldcommittedoffset <- deserialize
    fieldcommittedleaderepoch <- if version >= 2
      then deserialize
      else pure ((-1))
    fieldcommittedmetadata <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TxnOffsetCommitRequestPartition
      {
      txnOffsetCommitRequestPartitionPartitionIndex = fieldpartitionindex
      ,
      txnOffsetCommitRequestPartitionCommittedOffset = fieldcommittedoffset
      ,
      txnOffsetCommitRequestPartitionCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      txnOffsetCommitRequestPartitionCommittedMetadata = fieldcommittedmetadata
      }


-- | Each topic that we want to commit offsets for.
data TxnOffsetCommitRequestTopic = TxnOffsetCommitRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  txnOffsetCommitRequestTopicName :: !(KafkaString)
,

  -- | The partitions inside the topic that we want to commit offsets for.

  -- Versions: 0+
  txnOffsetCommitRequestTopicPartitions :: !(KafkaArray (TxnOffsetCommitRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode TxnOffsetCommitRequestTopic with version-aware field handling.
encodeTxnOffsetCommitRequestTopic :: MonadPut m => E.ApiVersion -> TxnOffsetCommitRequestTopic -> m ()
encodeTxnOffsetCommitRequestTopic version tmsg =
  do
    if version >= 3 then serialize (toCompactString (txnOffsetCommitRequestTopicName tmsg)) else serialize (txnOffsetCommitRequestTopicName tmsg)
    E.encodeVersionedArray version 3 encodeTxnOffsetCommitRequestPartition (case P.unKafkaArray (txnOffsetCommitRequestTopicPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TxnOffsetCommitRequestTopic with version-aware field handling.
decodeTxnOffsetCommitRequestTopic :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitRequestTopic
decodeTxnOffsetCommitRequestTopic version =
  do
    fieldname <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitRequestPartition
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TxnOffsetCommitRequestTopic
      {
      txnOffsetCommitRequestTopicName = fieldname
      ,
      txnOffsetCommitRequestTopicPartitions = fieldpartitions
      }



data TxnOffsetCommitRequest = TxnOffsetCommitRequest
  {

  -- | The ID of the transaction.

  -- Versions: 0+
  txnOffsetCommitRequestTransactionalId :: !(KafkaString)
,

  -- | The ID of the group.

  -- Versions: 0+
  txnOffsetCommitRequestGroupId :: !(KafkaString)
,

  -- | The current producer ID in use by the transactional ID.

  -- Versions: 0+
  txnOffsetCommitRequestProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer ID.

  -- Versions: 0+
  txnOffsetCommitRequestProducerEpoch :: !(Int16)
,

  -- | The generation of the consumer.

  -- Versions: 3+
  txnOffsetCommitRequestGenerationId :: !(Int32)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 3+
  txnOffsetCommitRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 3+
  txnOffsetCommitRequestGroupInstanceId :: !(KafkaString)
,

  -- | Each topic that we want to commit offsets for.

  -- Versions: 0+
  txnOffsetCommitRequestTopics :: !(KafkaArray (TxnOffsetCommitRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for TxnOffsetCommitRequest.
maxTxnOffsetCommitRequestVersion :: Int16
maxTxnOffsetCommitRequestVersion = 5

-- | Encode TxnOffsetCommitRequest with the given API version.
encodeTxnOffsetCommitRequest :: MonadPut m => E.ApiVersion -> TxnOffsetCommitRequest -> m ()
encodeTxnOffsetCommitRequest version msg
  | version >= 0 && version <= 2 =
    do
      serialize (txnOffsetCommitRequestTransactionalId msg)
      serialize (txnOffsetCommitRequestGroupId msg)
      serialize (txnOffsetCommitRequestProducerId msg)
      serialize (txnOffsetCommitRequestProducerEpoch msg)
      E.encodeVersionedArray version 3 encodeTxnOffsetCommitRequestTopic (case P.unKafkaArray (txnOffsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 3 && version <= 5 =
    do
      serialize (toCompactString (txnOffsetCommitRequestTransactionalId msg))
      serialize (toCompactString (txnOffsetCommitRequestGroupId msg))
      serialize (txnOffsetCommitRequestProducerId msg)
      serialize (txnOffsetCommitRequestProducerEpoch msg)
      serialize (txnOffsetCommitRequestGenerationId msg)
      serialize (toCompactString (txnOffsetCommitRequestMemberId msg))
      serialize (toCompactString (txnOffsetCommitRequestGroupInstanceId msg))
      E.encodeVersionedArray version 3 encodeTxnOffsetCommitRequestTopic (case P.unKafkaArray (txnOffsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode TxnOffsetCommitRequest with the given API version.
decodeTxnOffsetCommitRequest :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitRequest
decodeTxnOffsetCommitRequest version
  | version >= 0 && version <= 2 =
    do
      fieldtransactionalid <- deserialize
      fieldgroupid <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitRequestTopic
      pure TxnOffsetCommitRequest
        {
        txnOffsetCommitRequestTransactionalId = fieldtransactionalid
        ,
        txnOffsetCommitRequestGroupId = fieldgroupid
        ,
        txnOffsetCommitRequestProducerId = fieldproducerid
        ,
        txnOffsetCommitRequestProducerEpoch = fieldproducerepoch
        ,
        txnOffsetCommitRequestGenerationId = (-1)
        ,
        txnOffsetCommitRequestMemberId = P.KafkaString Null
        ,
        txnOffsetCommitRequestGroupInstanceId = P.KafkaString Null
        ,
        txnOffsetCommitRequestTopics = fieldtopics
        }

  | version >= 3 && version <= 5 =
    do
      fieldtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure TxnOffsetCommitRequest
        {
        txnOffsetCommitRequestTransactionalId = fieldtransactionalid
        ,
        txnOffsetCommitRequestGroupId = fieldgroupid
        ,
        txnOffsetCommitRequestProducerId = fieldproducerid
        ,
        txnOffsetCommitRequestProducerEpoch = fieldproducerepoch
        ,
        txnOffsetCommitRequestGenerationId = fieldgenerationid
        ,
        txnOffsetCommitRequestMemberId = fieldmemberid
        ,
        txnOffsetCommitRequestGroupInstanceId = fieldgroupinstanceid
        ,
        txnOffsetCommitRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeTxnOffsetCommitRequest' / 'decodeTxnOffsetCommitRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec TxnOffsetCommitRequest where
  wireCodec = Just (WC.serialShimCodec encodeTxnOffsetCommitRequest decodeTxnOffsetCommitRequest)
  {-# INLINE wireCodec #-}
