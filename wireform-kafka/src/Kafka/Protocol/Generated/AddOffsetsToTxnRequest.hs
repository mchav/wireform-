{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddOffsetsToTxnRequest
Description : Kafka AddOffsetsToTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 25.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddOffsetsToTxnRequest
  (
    AddOffsetsToTxnRequest(..),
    encodeAddOffsetsToTxnRequest,
    decodeAddOffsetsToTxnRequest,
    maxAddOffsetsToTxnRequestVersion
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




data AddOffsetsToTxnRequest = AddOffsetsToTxnRequest
  {

  -- | The transactional id corresponding to the transaction.

  -- Versions: 0+
  addOffsetsToTxnRequestTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 0+
  addOffsetsToTxnRequestProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 0+
  addOffsetsToTxnRequestProducerEpoch :: !(Int16)
,

  -- | The unique group identifier.

  -- Versions: 0+
  addOffsetsToTxnRequestGroupId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddOffsetsToTxnRequest.
maxAddOffsetsToTxnRequestVersion :: Int16
maxAddOffsetsToTxnRequestVersion = 4

-- | Encode AddOffsetsToTxnRequest with the given API version.
encodeAddOffsetsToTxnRequest :: MonadPut m => E.ApiVersion -> AddOffsetsToTxnRequest -> m ()
encodeAddOffsetsToTxnRequest version msg
  | version >= 3 && version <= 4 =
    do
      serialize (toCompactString (addOffsetsToTxnRequestTransactionalId msg))
      serialize (addOffsetsToTxnRequestProducerId msg)
      serialize (addOffsetsToTxnRequestProducerEpoch msg)
      serialize (toCompactString (addOffsetsToTxnRequestGroupId msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (addOffsetsToTxnRequestTransactionalId msg)
      serialize (addOffsetsToTxnRequestProducerId msg)
      serialize (addOffsetsToTxnRequestProducerEpoch msg)
      serialize (addOffsetsToTxnRequestGroupId msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddOffsetsToTxnRequest with the given API version.
decodeAddOffsetsToTxnRequest :: MonadGet m => E.ApiVersion -> m AddOffsetsToTxnRequest
decodeAddOffsetsToTxnRequest version
  | version >= 3 && version <= 4 =
    do
      fieldtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldgroupid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddOffsetsToTxnRequest
        {
        addOffsetsToTxnRequestTransactionalId = fieldtransactionalid
        ,
        addOffsetsToTxnRequestProducerId = fieldproducerid
        ,
        addOffsetsToTxnRequestProducerEpoch = fieldproducerepoch
        ,
        addOffsetsToTxnRequestGroupId = fieldgroupid
        }

  | version >= 0 && version <= 2 =
    do
      fieldtransactionalid <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldgroupid <- deserialize
      pure AddOffsetsToTxnRequest
        {
        addOffsetsToTxnRequestTransactionalId = fieldtransactionalid
        ,
        addOffsetsToTxnRequestProducerId = fieldproducerid
        ,
        addOffsetsToTxnRequestProducerEpoch = fieldproducerepoch
        ,
        addOffsetsToTxnRequestGroupId = fieldgroupid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAddOffsetsToTxnRequest' / 'decodeAddOffsetsToTxnRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AddOffsetsToTxnRequest where
  wireCodec = Just (WC.serialShimCodec encodeAddOffsetsToTxnRequest decodeAddOffsetsToTxnRequest)
  {-# INLINE wireCodec #-}
