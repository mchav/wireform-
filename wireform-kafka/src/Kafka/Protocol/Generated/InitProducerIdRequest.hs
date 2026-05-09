{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitProducerIdRequest
Description : Kafka InitProducerIdRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 22.



Valid versions: 0-6
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitProducerIdRequest
  (
    InitProducerIdRequest(..),
    encodeInitProducerIdRequest,
    decodeInitProducerIdRequest,
    maxInitProducerIdRequestVersion
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




data InitProducerIdRequest = InitProducerIdRequest
  {

  -- | The transactional id, or null if the producer is not transactional.

  -- Versions: 0+
  initProducerIdRequestTransactionalId :: !(KafkaString)
,

  -- | The time in ms to wait before aborting idle transactions sent by this producer. This is only relevan

  -- Versions: 0+
  initProducerIdRequestTransactionTimeoutMs :: !(Int32)
,

  -- | The producer id. This is used to disambiguate requests if a transactional id is reused following its

  -- Versions: 3+
  initProducerIdRequestProducerId :: !(Int64)
,

  -- | The producer's current epoch. This will be checked against the producer epoch on the broker, and the

  -- Versions: 3+
  initProducerIdRequestProducerEpoch :: !(Int16)
,

  -- | True if the client wants to enable two-phase commit (2PC) protocol for transactions.

  -- Versions: 6+
  initProducerIdRequestEnable2Pc :: !(Bool)
,

  -- | True if the client wants to keep the currently ongoing transaction instead of aborting it.

  -- Versions: 6+
  initProducerIdRequestKeepPreparedTxn :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitProducerIdRequest.
maxInitProducerIdRequestVersion :: Int16
maxInitProducerIdRequestVersion = 6

-- | Encode InitProducerIdRequest with the given API version.
encodeInitProducerIdRequest :: MonadPut m => E.ApiVersion -> InitProducerIdRequest -> m ()
encodeInitProducerIdRequest version msg
  | version == 2 =
    do
      serialize (toCompactString (initProducerIdRequestTransactionalId msg))
      serialize (initProducerIdRequestTransactionTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 6 =
    do
      serialize (toCompactString (initProducerIdRequestTransactionalId msg))
      serialize (initProducerIdRequestTransactionTimeoutMs msg)
      serialize (initProducerIdRequestProducerId msg)
      serialize (initProducerIdRequestProducerEpoch msg)
      serialize (initProducerIdRequestEnable2Pc msg)
      serialize (initProducerIdRequestKeepPreparedTxn msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (initProducerIdRequestTransactionalId msg)
      serialize (initProducerIdRequestTransactionTimeoutMs msg)


  | version >= 3 && version <= 5 =
    do
      serialize (toCompactString (initProducerIdRequestTransactionalId msg))
      serialize (initProducerIdRequestTransactionTimeoutMs msg)
      serialize (initProducerIdRequestProducerId msg)
      serialize (initProducerIdRequestProducerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode InitProducerIdRequest with the given API version.
decodeInitProducerIdRequest :: MonadGet m => E.ApiVersion -> m InitProducerIdRequest
decodeInitProducerIdRequest version
  | version == 2 =
    do
      fieldtransactionalid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldtransactiontimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitProducerIdRequest
        {
        initProducerIdRequestTransactionalId = fieldtransactionalid
        ,
        initProducerIdRequestTransactionTimeoutMs = fieldtransactiontimeoutms
        ,
        initProducerIdRequestProducerId = (-1)
        ,
        initProducerIdRequestProducerEpoch = (-1)
        ,
        initProducerIdRequestEnable2Pc = False
        ,
        initProducerIdRequestKeepPreparedTxn = False
        }

  | version == 6 =
    do
      fieldtransactionalid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldtransactiontimeoutms <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldenable2pc <- deserialize
      fieldkeeppreparedtxn <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitProducerIdRequest
        {
        initProducerIdRequestTransactionalId = fieldtransactionalid
        ,
        initProducerIdRequestTransactionTimeoutMs = fieldtransactiontimeoutms
        ,
        initProducerIdRequestProducerId = fieldproducerid
        ,
        initProducerIdRequestProducerEpoch = fieldproducerepoch
        ,
        initProducerIdRequestEnable2Pc = fieldenable2pc
        ,
        initProducerIdRequestKeepPreparedTxn = fieldkeeppreparedtxn
        }

  | version >= 0 && version <= 1 =
    do
      fieldtransactionalid <- deserialize
      fieldtransactiontimeoutms <- deserialize
      pure InitProducerIdRequest
        {
        initProducerIdRequestTransactionalId = fieldtransactionalid
        ,
        initProducerIdRequestTransactionTimeoutMs = fieldtransactiontimeoutms
        ,
        initProducerIdRequestProducerId = (-1)
        ,
        initProducerIdRequestProducerEpoch = (-1)
        ,
        initProducerIdRequestEnable2Pc = False
        ,
        initProducerIdRequestKeepPreparedTxn = False
        }

  | version >= 3 && version <= 5 =
    do
      fieldtransactionalid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldtransactiontimeoutms <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitProducerIdRequest
        {
        initProducerIdRequestTransactionalId = fieldtransactionalid
        ,
        initProducerIdRequestTransactionTimeoutMs = fieldtransactiontimeoutms
        ,
        initProducerIdRequestProducerId = fieldproducerid
        ,
        initProducerIdRequestProducerEpoch = fieldproducerepoch
        ,
        initProducerIdRequestEnable2Pc = False
        ,
        initProducerIdRequestKeepPreparedTxn = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeInitProducerIdRequest' / 'decodeInitProducerIdRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec InitProducerIdRequest where
  wireCodec = Just (WC.serialShimCodec encodeInitProducerIdRequest decodeInitProducerIdRequest)
  {-# INLINE wireCodec #-}
