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

-- | KafkaMessage instance for PushTelemetryResponse.
instance KafkaMessage PushTelemetryResponse where
  messageApiKey = 72
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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


-- | Worst-case wire size of a PushTelemetryResponse.
wireMaxSizePushTelemetryResponse :: Int -> PushTelemetryResponse -> Int
wireMaxSizePushTelemetryResponse _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for PushTelemetryResponse.
wirePokePushTelemetryResponse :: Int -> Ptr Word8 -> PushTelemetryResponse -> IO (Ptr Word8)
wirePokePushTelemetryResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (pushTelemetryResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (pushTelemetryResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke PushTelemetryResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for PushTelemetryResponse.
wirePeekPushTelemetryResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PushTelemetryResponse, Ptr Word8)
wirePeekPushTelemetryResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (PushTelemetryResponse { pushTelemetryResponseThrottleTimeMs = f0_throttletimems, pushTelemetryResponseErrorCode = f1_errorcode }, pTagsEnd)
  | otherwise = error $ "wirePeek PushTelemetryResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec PushTelemetryResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizePushTelemetryResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokePushTelemetryResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekPushTelemetryResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}