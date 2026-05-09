{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.GetTelemetrySubscriptionsResponse
Description : Kafka GetTelemetrySubscriptionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 71.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.GetTelemetrySubscriptionsResponse
  (
    GetTelemetrySubscriptionsResponse(..),
    encodeGetTelemetrySubscriptionsResponse,
    decodeGetTelemetrySubscriptionsResponse,
    maxGetTelemetrySubscriptionsResponseVersion
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




data GetTelemetrySubscriptionsResponse = GetTelemetrySubscriptionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  getTelemetrySubscriptionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseErrorCode :: !(Int16)
,

  -- | Assigned client instance id if ClientInstanceId was 0 in the request, else 0.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseClientInstanceId :: !(KafkaUuid)
,

  -- | Unique identifier for the current subscription set for this client instance.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseSubscriptionId :: !(Int32)
,

  -- | Compression types that broker accepts for the PushTelemetryRequest.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseAcceptedCompressionTypes :: !(KafkaArray (Int8))
,

  -- | Configured push interval, which is the lowest configured interval in the current subscription set.

  -- Versions: 0+
  getTelemetrySubscriptionsResponsePushIntervalMs :: !(Int32)
,

  -- | The maximum bytes of binary data the broker accepts in PushTelemetryRequest.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseTelemetryMaxBytes :: !(Int32)
,

  -- | Flag to indicate monotonic/counter metrics are to be emitted as deltas or cumulative values.

  -- Versions: 0+
  getTelemetrySubscriptionsResponseDeltaTemporality :: !(Bool)
,

  -- | Requested metrics prefix string match. Empty array: No metrics subscribed, Array[0] empty string: Al

  -- Versions: 0+
  getTelemetrySubscriptionsResponseRequestedMetrics :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for GetTelemetrySubscriptionsResponse.
maxGetTelemetrySubscriptionsResponseVersion :: Int16
maxGetTelemetrySubscriptionsResponseVersion = 0

-- | Encode GetTelemetrySubscriptionsResponse with the given API version.
encodeGetTelemetrySubscriptionsResponse :: MonadPut m => E.ApiVersion -> GetTelemetrySubscriptionsResponse -> m ()
encodeGetTelemetrySubscriptionsResponse version msg
  | version == 0 =
    do
      serialize (getTelemetrySubscriptionsResponseThrottleTimeMs msg)
      serialize (getTelemetrySubscriptionsResponseErrorCode msg)
      serialize (getTelemetrySubscriptionsResponseClientInstanceId msg)
      serialize (getTelemetrySubscriptionsResponseSubscriptionId msg)
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (getTelemetrySubscriptionsResponseAcceptedCompressionTypes msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int8"
      serialize (getTelemetrySubscriptionsResponsePushIntervalMs msg)
      serialize (getTelemetrySubscriptionsResponseTelemetryMaxBytes msg)
      serialize (getTelemetrySubscriptionsResponseDeltaTemporality msg)
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (getTelemetrySubscriptionsResponseRequestedMetrics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode GetTelemetrySubscriptionsResponse with the given API version.
decodeGetTelemetrySubscriptionsResponse :: MonadGet m => E.ApiVersion -> m GetTelemetrySubscriptionsResponse
decodeGetTelemetrySubscriptionsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldclientinstanceid <- deserialize
      fieldsubscriptionid <- deserialize
      fieldacceptedcompressiontypes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fieldpushintervalms <- deserialize
      fieldtelemetrymaxbytes <- deserialize
      fielddeltatemporality <- deserialize
      fieldrequestedmetrics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure GetTelemetrySubscriptionsResponse
        {
        getTelemetrySubscriptionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        getTelemetrySubscriptionsResponseErrorCode = fielderrorcode
        ,
        getTelemetrySubscriptionsResponseClientInstanceId = fieldclientinstanceid
        ,
        getTelemetrySubscriptionsResponseSubscriptionId = fieldsubscriptionid
        ,
        getTelemetrySubscriptionsResponseAcceptedCompressionTypes = fieldacceptedcompressiontypes
        ,
        getTelemetrySubscriptionsResponsePushIntervalMs = fieldpushintervalms
        ,
        getTelemetrySubscriptionsResponseTelemetryMaxBytes = fieldtelemetrymaxbytes
        ,
        getTelemetrySubscriptionsResponseDeltaTemporality = fielddeltatemporality
        ,
        getTelemetrySubscriptionsResponseRequestedMetrics = fieldrequestedmetrics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeGetTelemetrySubscriptionsResponse' / 'decodeGetTelemetrySubscriptionsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec GetTelemetrySubscriptionsResponse where
  wireCodec = Just (WC.serialShimCodec encodeGetTelemetrySubscriptionsResponse decodeGetTelemetrySubscriptionsResponse)
  {-# INLINE wireCodec #-}
