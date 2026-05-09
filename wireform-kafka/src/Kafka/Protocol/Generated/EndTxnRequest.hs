{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndTxnRequest
Description : Kafka EndTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 26.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndTxnRequest
  (
    EndTxnRequest(..),
    encodeEndTxnRequest,
    decodeEndTxnRequest,
    maxEndTxnRequestVersion
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




data EndTxnRequest = EndTxnRequest
  {

  -- | The ID of the transaction to end.

  -- Versions: 0+
  endTxnRequestTransactionalId :: !(KafkaString)
,

  -- | The producer ID.

  -- Versions: 0+
  endTxnRequestProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer.

  -- Versions: 0+
  endTxnRequestProducerEpoch :: !(Int16)
,

  -- | True if the transaction was committed, false if it was aborted.

  -- Versions: 0+
  endTxnRequestCommitted :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndTxnRequest.
maxEndTxnRequestVersion :: Int16
maxEndTxnRequestVersion = 5

-- | Encode EndTxnRequest with the given API version.
encodeEndTxnRequest :: MonadPut m => E.ApiVersion -> EndTxnRequest -> m ()
encodeEndTxnRequest version msg
  | version >= 0 && version <= 2 =
    do
      serialize (endTxnRequestTransactionalId msg)
      serialize (endTxnRequestProducerId msg)
      serialize (endTxnRequestProducerEpoch msg)
      serialize (endTxnRequestCommitted msg)


  | version >= 3 && version <= 5 =
    do
      serialize (toCompactString (endTxnRequestTransactionalId msg))
      serialize (endTxnRequestProducerId msg)
      serialize (endTxnRequestProducerEpoch msg)
      serialize (endTxnRequestCommitted msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndTxnRequest with the given API version.
decodeEndTxnRequest :: MonadGet m => E.ApiVersion -> m EndTxnRequest
decodeEndTxnRequest version
  | version >= 0 && version <= 2 =
    do
      fieldtransactionalid <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldcommitted <- deserialize
      pure EndTxnRequest
        {
        endTxnRequestTransactionalId = fieldtransactionalid
        ,
        endTxnRequestProducerId = fieldproducerid
        ,
        endTxnRequestProducerEpoch = fieldproducerepoch
        ,
        endTxnRequestCommitted = fieldcommitted
        }

  | version >= 3 && version <= 5 =
    do
      fieldtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldcommitted <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndTxnRequest
        {
        endTxnRequestTransactionalId = fieldtransactionalid
        ,
        endTxnRequestProducerId = fieldproducerid
        ,
        endTxnRequestProducerEpoch = fieldproducerepoch
        ,
        endTxnRequestCommitted = fieldcommitted
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec EndTxnRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
