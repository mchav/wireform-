{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTransactionsResponse
Description : Kafka DescribeTransactionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 65.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTransactionsResponse
  (
    DescribeTransactionsResponse(..),
    TransactionState(..),
    TopicData(..),
    encodeDescribeTransactionsResponse,
    decodeDescribeTransactionsResponse,
    maxDescribeTransactionsResponseVersion
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


-- | The set of partitions included in the current transaction (if active). When a transaction is preparing to commit or abort, this will include only partitions which do not have markers.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopic :: !(KafkaString)
,

  -- | The partition ids included in the current transaction.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicDataTopic tmsg)) else serialize (topicDataTopic tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopic <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopic = fieldtopic
      ,
      topicDataPartitions = fieldpartitions
      }


-- | The current state of the transaction.
data TransactionState = TransactionState
  {

  -- | The error code.

  -- Versions: 0+
  transactionStateErrorCode :: !(Int16)
,

  -- | The transactional id.

  -- Versions: 0+
  transactionStateTransactionalId :: !(KafkaString)
,

  -- | The current transaction state of the producer.

  -- Versions: 0+
  transactionStateTransactionState :: !(KafkaString)
,

  -- | The timeout in milliseconds for the transaction.

  -- Versions: 0+
  transactionStateTransactionTimeoutMs :: !(Int32)
,

  -- | The start time of the transaction in milliseconds.

  -- Versions: 0+
  transactionStateTransactionStartTimeMs :: !(Int64)
,

  -- | The current producer id associated with the transaction.

  -- Versions: 0+
  transactionStateProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer id.

  -- Versions: 0+
  transactionStateProducerEpoch :: !(Int16)
,

  -- | The set of partitions included in the current transaction (if active). When a transaction is prepari

  -- Versions: 0+
  transactionStateTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TransactionState with version-aware field handling.
encodeTransactionState :: MonadPut m => E.ApiVersion -> TransactionState -> m ()
encodeTransactionState version tmsg =
  do
    serialize (transactionStateErrorCode tmsg)
    if version >= 0 then serialize (toCompactString (transactionStateTransactionalId tmsg)) else serialize (transactionStateTransactionalId tmsg)
    if version >= 0 then serialize (toCompactString (transactionStateTransactionState tmsg)) else serialize (transactionStateTransactionState tmsg)
    serialize (transactionStateTransactionTimeoutMs tmsg)
    serialize (transactionStateTransactionStartTimeMs tmsg)
    serialize (transactionStateProducerId tmsg)
    serialize (transactionStateProducerEpoch tmsg)
    E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (transactionStateTopics tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TransactionState with version-aware field handling.
decodeTransactionState :: MonadGet m => E.ApiVersion -> m TransactionState
decodeTransactionState version =
  do
    fielderrorcode <- deserialize
    fieldtransactionalid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtransactionstate <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtransactiontimeoutms <- deserialize
    fieldtransactionstarttimems <- deserialize
    fieldproducerid <- deserialize
    fieldproducerepoch <- deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TransactionState
      {
      transactionStateErrorCode = fielderrorcode
      ,
      transactionStateTransactionalId = fieldtransactionalid
      ,
      transactionStateTransactionState = fieldtransactionstate
      ,
      transactionStateTransactionTimeoutMs = fieldtransactiontimeoutms
      ,
      transactionStateTransactionStartTimeMs = fieldtransactionstarttimems
      ,
      transactionStateProducerId = fieldproducerid
      ,
      transactionStateProducerEpoch = fieldproducerepoch
      ,
      transactionStateTopics = fieldtopics
      }



data DescribeTransactionsResponse = DescribeTransactionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeTransactionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The current state of the transaction.

  -- Versions: 0+
  describeTransactionsResponseTransactionStates :: !(KafkaArray (TransactionState))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTransactionsResponse.
maxDescribeTransactionsResponseVersion :: Int16
maxDescribeTransactionsResponseVersion = 0

-- | Encode DescribeTransactionsResponse with the given API version.
encodeDescribeTransactionsResponse :: MonadPut m => E.ApiVersion -> DescribeTransactionsResponse -> m ()
encodeDescribeTransactionsResponse version msg
  | version == 0 =
    do
      serialize (describeTransactionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeTransactionState (case P.unKafkaArray (describeTransactionsResponseTransactionStates msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeTransactionsResponse with the given API version.
decodeDescribeTransactionsResponse :: MonadGet m => E.ApiVersion -> m DescribeTransactionsResponse
decodeDescribeTransactionsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldtransactionstates <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTransactionState
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeTransactionsResponse
        {
        describeTransactionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeTransactionsResponseTransactionStates = fieldtransactionstates
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeTransactionsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
