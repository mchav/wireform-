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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




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

-- | KafkaMessage instance for PushTelemetryRequest.
instance KafkaMessage PushTelemetryRequest where
  messageApiKey = 72
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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


-- | Worst-case wire size of a PushTelemetryRequest.
wireMaxSizePushTelemetryRequest :: Int -> PushTelemetryRequest -> Int
wireMaxSizePushTelemetryRequest _version msg =
  0
  + 16
  + 4
  + 1
  + 1
  + WP.compactBytesMaxSize (P.toCompactBytes (pushTelemetryRequestMetrics msg))
  + 1

-- | Direct-poke encoder for PushTelemetryRequest.
wirePokePushTelemetryRequest :: Int -> Ptr Word8 -> PushTelemetryRequest -> IO (Ptr Word8)
wirePokePushTelemetryRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaUuid p0 (pushTelemetryRequestClientInstanceId msg)
    p2 <- W.pokeInt32BE p1 (pushTelemetryRequestSubscriptionId msg)
    p3 <- W.pokeWord8 p2 (if (pushTelemetryRequestTerminating msg) then 1 else 0)
    p4 <- W.pokeWord8 p3 (fromIntegral (pushTelemetryRequestCompressionType msg))
    p5 <- WP.pokeCompactBytes p4 (P.toCompactBytes (pushTelemetryRequestMetrics msg))
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke PushTelemetryRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for PushTelemetryRequest.
wirePeekPushTelemetryRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PushTelemetryRequest, Ptr Word8)
wirePeekPushTelemetryRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clientinstanceid, p1) <- WP.peekKafkaUuid p0 endPtr
    (f1_subscriptionid, p2) <- W.peekInt32BE p1 endPtr
    (f2_terminating, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    (f3_compressiontype, p4) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr
    (f4_metrics, p5) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (PushTelemetryRequest { pushTelemetryRequestClientInstanceId = f0_clientinstanceid, pushTelemetryRequestSubscriptionId = f1_subscriptionid, pushTelemetryRequestTerminating = f2_terminating, pushTelemetryRequestCompressionType = f3_compressiontype, pushTelemetryRequestMetrics = f4_metrics }, pTagsEnd)
  | otherwise = error $ "wirePeek PushTelemetryRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec PushTelemetryRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizePushTelemetryRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokePushTelemetryRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekPushTelemetryRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}