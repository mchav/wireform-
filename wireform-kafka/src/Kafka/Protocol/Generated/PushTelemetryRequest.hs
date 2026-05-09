{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.PushTelemetryRequest
Description : Kafka PushTelemetryRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 72.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.PushTelemetryRequest
  (
    PushTelemetryRequest(..),
    encodePushTelemetryRequest,
    decodePushTelemetryRequest,
    maxPushTelemetryRequestVersion
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




data PushTelemetryRequest = PushTelemetryRequest
  {

  -- | Unique id for this client instance.

  -- Versions: 0+
  pushTelemetryRequestClientInstanceId :: !(KafkaUuid)
,

  -- | Unique identifier for the current subscription.

  -- Versions: 0+
  pushTelemetryRequestSubscriptionId :: !(Int32)
,

  -- | Client is terminating the connection.

  -- Versions: 0+
  pushTelemetryRequestTerminating :: !(Bool)
,

  -- | Compression codec used to compress the metrics.

  -- Versions: 0+
  pushTelemetryRequestCompressionType :: !(Int8)
,

  -- | Metrics encoded in OpenTelemetry MetricsData v1 protobuf format.

  -- Versions: 0+
  pushTelemetryRequestMetrics :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for PushTelemetryRequest.
maxPushTelemetryRequestVersion :: Int16
maxPushTelemetryRequestVersion = 0

-- | Encode PushTelemetryRequest with the given API version.
encodePushTelemetryRequest :: MonadPut m => E.ApiVersion -> PushTelemetryRequest -> m ()
encodePushTelemetryRequest version msg
  | version == 0 =
    do
      serialize (pushTelemetryRequestClientInstanceId msg)
      serialize (pushTelemetryRequestSubscriptionId msg)
      serialize (pushTelemetryRequestTerminating msg)
      serialize (pushTelemetryRequestCompressionType msg)
      serialize (toCompactBytes (pushTelemetryRequestMetrics msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode PushTelemetryRequest with the given API version.
decodePushTelemetryRequest :: MonadGet m => E.ApiVersion -> m PushTelemetryRequest
decodePushTelemetryRequest version
  | version == 0 =
    do
      fieldclientinstanceid <- deserialize
      fieldsubscriptionid <- deserialize
      fieldterminating <- deserialize
      fieldcompressiontype <- deserialize
      fieldmetrics <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure PushTelemetryRequest
        {
        pushTelemetryRequestClientInstanceId = fieldclientinstanceid
        ,
        pushTelemetryRequestSubscriptionId = fieldsubscriptionid
        ,
        pushTelemetryRequestTerminating = fieldterminating
        ,
        pushTelemetryRequestCompressionType = fieldcompressiontype
        ,
        pushTelemetryRequestMetrics = fieldmetrics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodePushTelemetryRequest' / 'decodePushTelemetryRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec PushTelemetryRequest where
  wireCodec = Just (WC.serialShimCodec encodePushTelemetryRequest decodePushTelemetryRequest)
  {-# INLINE wireCodec #-}
