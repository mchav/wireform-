{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndTxnResponse
Description : Kafka EndTxnResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 26.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndTxnResponse
  (
    EndTxnResponse(..),
    encodeEndTxnResponse,
    decodeEndTxnResponse,
    maxEndTxnResponseVersion
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




data EndTxnResponse = EndTxnResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  endTxnResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  endTxnResponseErrorCode :: !(Int16)
,

  -- | The producer ID.

  -- Versions: 5+
  endTxnResponseProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer.

  -- Versions: 5+
  endTxnResponseProducerEpoch :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndTxnResponse.
maxEndTxnResponseVersion :: Int16
maxEndTxnResponseVersion = 5

-- | Encode EndTxnResponse with the given API version.
encodeEndTxnResponse :: MonadPut m => E.ApiVersion -> EndTxnResponse -> m ()
encodeEndTxnResponse version msg
  | version == 5 =
    do
      serialize (endTxnResponseThrottleTimeMs msg)
      serialize (endTxnResponseErrorCode msg)
      serialize (endTxnResponseProducerId msg)
      serialize (endTxnResponseProducerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 4 =
    do
      serialize (endTxnResponseThrottleTimeMs msg)
      serialize (endTxnResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (endTxnResponseThrottleTimeMs msg)
      serialize (endTxnResponseErrorCode msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndTxnResponse with the given API version.
decodeEndTxnResponse :: MonadGet m => E.ApiVersion -> m EndTxnResponse
decodeEndTxnResponse version
  | version == 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndTxnResponse
        {
        endTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        endTxnResponseErrorCode = fielderrorcode
        ,
        endTxnResponseProducerId = fieldproducerid
        ,
        endTxnResponseProducerEpoch = fieldproducerepoch
        }

  | version >= 3 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndTxnResponse
        {
        endTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        endTxnResponseErrorCode = fielderrorcode
        ,
        endTxnResponseProducerId = (-1)
        ,
        endTxnResponseProducerEpoch = (-1)
        }

  | version >= 0 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      pure EndTxnResponse
        {
        endTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        endTxnResponseErrorCode = fielderrorcode
        ,
        endTxnResponseProducerId = (-1)
        ,
        endTxnResponseProducerEpoch = (-1)
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeEndTxnResponse' / 'decodeEndTxnResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec EndTxnResponse where
  wireCodec = Just (WC.serialShimCodec encodeEndTxnResponse decodeEndTxnResponse)
  {-# INLINE wireCodec #-}
