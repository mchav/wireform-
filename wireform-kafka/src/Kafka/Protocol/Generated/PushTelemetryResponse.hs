{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.PushTelemetryResponse
Description : Kafka PushTelemetryResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 72.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.PushTelemetryResponse
  (
    PushTelemetryResponse(..),
    encodePushTelemetryResponse,
    decodePushTelemetryResponse,
    maxPushTelemetryResponseVersion
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




data PushTelemetryResponse = PushTelemetryResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  pushTelemetryResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  pushTelemetryResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for PushTelemetryResponse.
maxPushTelemetryResponseVersion :: Int16
maxPushTelemetryResponseVersion = 0

-- | Encode PushTelemetryResponse with the given API version.
encodePushTelemetryResponse :: MonadPut m => E.ApiVersion -> PushTelemetryResponse -> m ()
encodePushTelemetryResponse version msg
  | version == 0 =
    do
      serialize (pushTelemetryResponseThrottleTimeMs msg)
      serialize (pushTelemetryResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode PushTelemetryResponse with the given API version.
decodePushTelemetryResponse :: MonadGet m => E.ApiVersion -> m PushTelemetryResponse
decodePushTelemetryResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure PushTelemetryResponse
        {
        pushTelemetryResponseThrottleTimeMs = fieldthrottletimems
        ,
        pushTelemetryResponseErrorCode = fielderrorcode
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodePushTelemetryResponse' / 'decodePushTelemetryResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec PushTelemetryResponse where
  wireCodec = Just (WC.serialShimCodec encodePushTelemetryResponse decodePushTelemetryResponse)
  {-# INLINE wireCodec #-}
