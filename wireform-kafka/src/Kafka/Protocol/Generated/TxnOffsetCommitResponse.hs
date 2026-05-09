{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.TxnOffsetCommitResponse
Description : Kafka TxnOffsetCommitResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 28.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.TxnOffsetCommitResponse
  (
    TxnOffsetCommitResponse(..),
    TxnOffsetCommitResponseTopic(..),
    TxnOffsetCommitResponsePartition(..),
    encodeTxnOffsetCommitResponse,
    decodeTxnOffsetCommitResponse,
    maxTxnOffsetCommitResponseVersion
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


-- | The responses for each partition in the topic.
data TxnOffsetCommitResponsePartition = TxnOffsetCommitResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  txnOffsetCommitResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  txnOffsetCommitResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode TxnOffsetCommitResponsePartition with version-aware field handling.
encodeTxnOffsetCommitResponsePartition :: MonadPut m => E.ApiVersion -> TxnOffsetCommitResponsePartition -> m ()
encodeTxnOffsetCommitResponsePartition version tmsg =
  do
    serialize (txnOffsetCommitResponsePartitionPartitionIndex tmsg)
    serialize (txnOffsetCommitResponsePartitionErrorCode tmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TxnOffsetCommitResponsePartition with version-aware field handling.
decodeTxnOffsetCommitResponsePartition :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitResponsePartition
decodeTxnOffsetCommitResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TxnOffsetCommitResponsePartition
      {
      txnOffsetCommitResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      txnOffsetCommitResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses for each topic.
data TxnOffsetCommitResponseTopic = TxnOffsetCommitResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  txnOffsetCommitResponseTopicName :: !(KafkaString)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  txnOffsetCommitResponseTopicPartitions :: !(KafkaArray (TxnOffsetCommitResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode TxnOffsetCommitResponseTopic with version-aware field handling.
encodeTxnOffsetCommitResponseTopic :: MonadPut m => E.ApiVersion -> TxnOffsetCommitResponseTopic -> m ()
encodeTxnOffsetCommitResponseTopic version tmsg =
  do
    if version >= 3 then serialize (toCompactString (txnOffsetCommitResponseTopicName tmsg)) else serialize (txnOffsetCommitResponseTopicName tmsg)
    E.encodeVersionedArray version 3 encodeTxnOffsetCommitResponsePartition (case P.unKafkaArray (txnOffsetCommitResponseTopicPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TxnOffsetCommitResponseTopic with version-aware field handling.
decodeTxnOffsetCommitResponseTopic :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitResponseTopic
decodeTxnOffsetCommitResponseTopic version =
  do
    fieldname <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitResponsePartition
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TxnOffsetCommitResponseTopic
      {
      txnOffsetCommitResponseTopicName = fieldname
      ,
      txnOffsetCommitResponseTopicPartitions = fieldpartitions
      }



data TxnOffsetCommitResponse = TxnOffsetCommitResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  txnOffsetCommitResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  txnOffsetCommitResponseTopics :: !(KafkaArray (TxnOffsetCommitResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for TxnOffsetCommitResponse.
maxTxnOffsetCommitResponseVersion :: Int16
maxTxnOffsetCommitResponseVersion = 5

-- | Encode TxnOffsetCommitResponse with the given API version.
encodeTxnOffsetCommitResponse :: MonadPut m => E.ApiVersion -> TxnOffsetCommitResponse -> m ()
encodeTxnOffsetCommitResponse version msg
  | version >= 0 && version <= 2 =
    do
      serialize (txnOffsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeTxnOffsetCommitResponseTopic (case P.unKafkaArray (txnOffsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 3 && version <= 5 =
    do
      serialize (txnOffsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeTxnOffsetCommitResponseTopic (case P.unKafkaArray (txnOffsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode TxnOffsetCommitResponse with the given API version.
decodeTxnOffsetCommitResponse :: MonadGet m => E.ApiVersion -> m TxnOffsetCommitResponse
decodeTxnOffsetCommitResponse version
  | version >= 0 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitResponseTopic
      pure TxnOffsetCommitResponse
        {
        txnOffsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        txnOffsetCommitResponseTopics = fieldtopics
        }

  | version >= 3 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeTxnOffsetCommitResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure TxnOffsetCommitResponse
        {
        txnOffsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        txnOffsetCommitResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec TxnOffsetCommitResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
