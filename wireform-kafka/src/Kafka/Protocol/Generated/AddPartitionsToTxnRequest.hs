{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddPartitionsToTxnRequest
Description : Kafka AddPartitionsToTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 24.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddPartitionsToTxnRequest
  (
    AddPartitionsToTxnRequest(..),
    AddPartitionsToTxnTransaction(..),
    AddPartitionsToTxnTopic(..),
    encodeAddPartitionsToTxnRequest,
    decodeAddPartitionsToTxnRequest,
    maxAddPartitionsToTxnRequestVersion
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


data AddPartitionsToTxnTopic = AddPartitionsToTxnTopic
  {

  -- | The name of the topic.

  -- Versions: 0+
  addPartitionsToTxnTopicName :: !(KafkaString)
,

  -- | The partition indexes to add to the transaction.

  -- Versions: 0+
  addPartitionsToTxnTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode AddPartitionsToTxnTopic with version-aware field handling.
encodeAddPartitionsToTxnTopic :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnTopic -> m ()
encodeAddPartitionsToTxnTopic version amsg =
  do
    if version >= 3 then serialize (toCompactString (addPartitionsToTxnTopicName amsg)) else serialize (addPartitionsToTxnTopicName amsg)
    E.encodeVersionedArray version 3 (\_ x -> serialize x) (case P.unKafkaArray (addPartitionsToTxnTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AddPartitionsToTxnTopic with version-aware field handling.
decodeAddPartitionsToTxnTopic :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnTopic
decodeAddPartitionsToTxnTopic version =
  do
    fieldname <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\_ -> deserialize)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AddPartitionsToTxnTopic
      {
      addPartitionsToTxnTopicName = fieldname
      ,
      addPartitionsToTxnTopicPartitions = fieldpartitions
      }


-- | List of transactions to add partitions to.
data AddPartitionsToTxnTransaction = AddPartitionsToTxnTransaction
  {

  -- | The transactional id corresponding to the transaction.

  -- Versions: 4+
  addPartitionsToTxnTransactionTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 4+
  addPartitionsToTxnTransactionProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 4+
  addPartitionsToTxnTransactionProducerEpoch :: !(Int16)
,

  -- | Boolean to signify if we want to check if the partition is in the transaction rather than add it.

  -- Versions: 4+
  addPartitionsToTxnTransactionVerifyOnly :: !(Bool)
,

  -- | The partitions to add to the transaction.

  -- Versions: 4+
  addPartitionsToTxnTransactionTopics :: !(KafkaArray (AddPartitionsToTxnTopic))

  }
  deriving (Eq, Show, Generic)


-- | Encode AddPartitionsToTxnTransaction with version-aware field handling.
encodeAddPartitionsToTxnTransaction :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnTransaction -> m ()
encodeAddPartitionsToTxnTransaction version amsg =
  do
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (addPartitionsToTxnTransactionTransactionalId amsg)) else serialize (addPartitionsToTxnTransactionTransactionalId amsg)
    when (version >= 4) $
      serialize (addPartitionsToTxnTransactionProducerId amsg)
    when (version >= 4) $
      serialize (addPartitionsToTxnTransactionProducerEpoch amsg)
    when (version >= 4) $
      serialize (addPartitionsToTxnTransactionVerifyOnly amsg)
    when (version >= 4) $
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopic (case P.unKafkaArray (addPartitionsToTxnTransactionTopics amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AddPartitionsToTxnTransaction with version-aware field handling.
decodeAddPartitionsToTxnTransaction :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnTransaction
decodeAddPartitionsToTxnTransaction version =
  do
    fieldtransactionalid <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldproducerid <- if version >= 4
      then deserialize
      else pure (0)
    fieldproducerepoch <- if version >= 4
      then deserialize
      else pure (0)
    fieldverifyonly <- if version >= 4
      then deserialize
      else pure (False)
    fieldtopics <- if version >= 4
      then P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopic
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AddPartitionsToTxnTransaction
      {
      addPartitionsToTxnTransactionTransactionalId = fieldtransactionalid
      ,
      addPartitionsToTxnTransactionProducerId = fieldproducerid
      ,
      addPartitionsToTxnTransactionProducerEpoch = fieldproducerepoch
      ,
      addPartitionsToTxnTransactionVerifyOnly = fieldverifyonly
      ,
      addPartitionsToTxnTransactionTopics = fieldtopics
      }



data AddPartitionsToTxnRequest = AddPartitionsToTxnRequest
  {

  -- | List of transactions to add partitions to.

  -- Versions: 4+
  addPartitionsToTxnRequestTransactions :: !(KafkaArray (AddPartitionsToTxnTransaction))
,

  -- | The transactional id corresponding to the transaction.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowProducerEpoch :: !(Int16)
,

  -- | The partitions to add to the transaction.

  -- Versions: 0-3
  addPartitionsToTxnRequestV3AndBelowTopics :: !(KafkaArray (AddPartitionsToTxnTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddPartitionsToTxnRequest.
maxAddPartitionsToTxnRequestVersion :: Int16
maxAddPartitionsToTxnRequestVersion = 5

-- | Encode AddPartitionsToTxnRequest with the given API version.
encodeAddPartitionsToTxnRequest :: MonadPut m => E.ApiVersion -> AddPartitionsToTxnRequest -> m ()
encodeAddPartitionsToTxnRequest version msg
  | version == 3 =
    do
      serialize (toCompactString (addPartitionsToTxnRequestV3AndBelowTransactionalId msg))
      serialize (addPartitionsToTxnRequestV3AndBelowProducerId msg)
      serialize (addPartitionsToTxnRequestV3AndBelowProducerEpoch msg)
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopic (case P.unKafkaArray (addPartitionsToTxnRequestV3AndBelowTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 4 && version <= 5 =
    do
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTransaction (case P.unKafkaArray (addPartitionsToTxnRequestTransactions msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (addPartitionsToTxnRequestV3AndBelowTransactionalId msg)
      serialize (addPartitionsToTxnRequestV3AndBelowProducerId msg)
      serialize (addPartitionsToTxnRequestV3AndBelowProducerEpoch msg)
      E.encodeVersionedArray version 3 encodeAddPartitionsToTxnTopic (case P.unKafkaArray (addPartitionsToTxnRequestV3AndBelowTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddPartitionsToTxnRequest with the given API version.
decodeAddPartitionsToTxnRequest :: MonadGet m => E.ApiVersion -> m AddPartitionsToTxnRequest
decodeAddPartitionsToTxnRequest version
  | version == 3 =
    do
      fieldv3andbelowtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldv3andbelowproducerid <- deserialize
      fieldv3andbelowproducerepoch <- deserialize
      fieldv3andbelowtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddPartitionsToTxnRequest
        {
        addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty
        ,
        addPartitionsToTxnRequestV3AndBelowTransactionalId = fieldv3andbelowtransactionalid
        ,
        addPartitionsToTxnRequestV3AndBelowProducerId = fieldv3andbelowproducerid
        ,
        addPartitionsToTxnRequestV3AndBelowProducerEpoch = fieldv3andbelowproducerepoch
        ,
        addPartitionsToTxnRequestV3AndBelowTopics = fieldv3andbelowtopics
        }

  | version >= 4 && version <= 5 =
    do
      fieldtransactions <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTransaction
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddPartitionsToTxnRequest
        {
        addPartitionsToTxnRequestTransactions = fieldtransactions
        ,
        addPartitionsToTxnRequestV3AndBelowTransactionalId = P.KafkaString Null
        ,
        addPartitionsToTxnRequestV3AndBelowProducerId = 0
        ,
        addPartitionsToTxnRequestV3AndBelowProducerEpoch = 0
        ,
        addPartitionsToTxnRequestV3AndBelowTopics = P.mkKafkaArray V.empty
        }

  | version >= 0 && version <= 2 =
    do
      fieldv3andbelowtransactionalid <- deserialize
      fieldv3andbelowproducerid <- deserialize
      fieldv3andbelowproducerepoch <- deserialize
      fieldv3andbelowtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeAddPartitionsToTxnTopic
      pure AddPartitionsToTxnRequest
        {
        addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty
        ,
        addPartitionsToTxnRequestV3AndBelowTransactionalId = fieldv3andbelowtransactionalid
        ,
        addPartitionsToTxnRequestV3AndBelowProducerId = fieldv3andbelowproducerid
        ,
        addPartitionsToTxnRequestV3AndBelowProducerEpoch = fieldv3andbelowproducerepoch
        ,
        addPartitionsToTxnRequestV3AndBelowTopics = fieldv3andbelowtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAddPartitionsToTxnRequest' / 'decodeAddPartitionsToTxnRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AddPartitionsToTxnRequest where
  wireCodec = Just (WC.serialShimCodec encodeAddPartitionsToTxnRequest decodeAddPartitionsToTxnRequest)
  {-# INLINE wireCodec #-}
