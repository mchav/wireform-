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
    maxGetTelemetrySubscriptionsResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




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

-- | KafkaMessage instance for GetTelemetrySubscriptionsResponse.
instance KafkaMessage GetTelemetrySubscriptionsResponse where
  messageApiKey = 71
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a GetTelemetrySubscriptionsResponse.
wireMaxSizeGetTelemetrySubscriptionsResponse :: Int -> GetTelemetrySubscriptionsResponse -> Int
wireMaxSizeGetTelemetrySubscriptionsResponse _version msg =
  0
  + 4
  + 2
  + 16
  + 4
  + (5 + (case P.unKafkaArray (getTelemetrySubscriptionsResponseAcceptedCompressionTypes msg) of { P.NotNull v -> sum (fmap (\x -> 1 ) v); P.Null -> 0 }))
  + 4
  + 4
  + 1
  + (5 + (case P.unKafkaArray (getTelemetrySubscriptionsResponseRequestedMetrics msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for GetTelemetrySubscriptionsResponse.
wirePokeGetTelemetrySubscriptionsResponse :: Int -> Ptr Word8 -> GetTelemetrySubscriptionsResponse -> IO (Ptr Word8)
wirePokeGetTelemetrySubscriptionsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (getTelemetrySubscriptionsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (getTelemetrySubscriptionsResponseErrorCode msg)
    p3 <- WP.pokeKafkaUuid p2 (getTelemetrySubscriptionsResponseClientInstanceId msg)
    p4 <- W.pokeInt32BE p3 (getTelemetrySubscriptionsResponseSubscriptionId msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> W.pokeWord8 p (fromIntegral (x :: Int8))) p4 (getTelemetrySubscriptionsResponseAcceptedCompressionTypes msg)
    p6 <- W.pokeInt32BE p5 (getTelemetrySubscriptionsResponsePushIntervalMs msg)
    p7 <- W.pokeInt32BE p6 (getTelemetrySubscriptionsResponseTelemetryMaxBytes msg)
    p8 <- W.pokeWord8 p7 (if (getTelemetrySubscriptionsResponseDeltaTemporality msg) then 1 else 0)
    p9 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p8 (getTelemetrySubscriptionsResponseRequestedMetrics msg)
    WP.pokeEmptyTaggedFields p9
  | otherwise = error $ "wirePoke GetTelemetrySubscriptionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for GetTelemetrySubscriptionsResponse.
wirePeekGetTelemetrySubscriptionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (GetTelemetrySubscriptionsResponse, Ptr Word8)
wirePeekGetTelemetrySubscriptionsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_clientinstanceid, p3) <- WP.peekKafkaUuid p2 endPtr
    (f3_subscriptionid, p4) <- W.peekInt32BE p3 endPtr
    (f4_acceptedcompressiontypes, p5) <- WP.peekVersionedArray version 0 (\p e -> (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p e) p4 endPtr
    (f5_pushintervalms, p6) <- W.peekInt32BE p5 endPtr
    (f6_telemetrymaxbytes, p7) <- W.peekInt32BE p6 endPtr
    (f7_deltatemporality, p8) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p7 endPtr
    (f8_requestedmetrics, p9) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p8 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p9 endPtr
    pure (GetTelemetrySubscriptionsResponse { getTelemetrySubscriptionsResponseThrottleTimeMs = f0_throttletimems, getTelemetrySubscriptionsResponseErrorCode = f1_errorcode, getTelemetrySubscriptionsResponseClientInstanceId = f2_clientinstanceid, getTelemetrySubscriptionsResponseSubscriptionId = f3_subscriptionid, getTelemetrySubscriptionsResponseAcceptedCompressionTypes = f4_acceptedcompressiontypes, getTelemetrySubscriptionsResponsePushIntervalMs = f5_pushintervalms, getTelemetrySubscriptionsResponseTelemetryMaxBytes = f6_telemetrymaxbytes, getTelemetrySubscriptionsResponseDeltaTemporality = f7_deltatemporality, getTelemetrySubscriptionsResponseRequestedMetrics = f8_requestedmetrics }, pTagsEnd)
  | otherwise = error $ "wirePeek GetTelemetrySubscriptionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec GetTelemetrySubscriptionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeGetTelemetrySubscriptionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeGetTelemetrySubscriptionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekGetTelemetrySubscriptionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}