{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.GetTelemetrySubscriptionsRequest
Description : Kafka GetTelemetrySubscriptionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 71.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.GetTelemetrySubscriptionsRequest
  (
    GetTelemetrySubscriptionsRequest(..),
    encodeGetTelemetrySubscriptionsRequest,
    decodeGetTelemetrySubscriptionsRequest,
    maxGetTelemetrySubscriptionsRequestVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data GetTelemetrySubscriptionsRequest = GetTelemetrySubscriptionsRequest
  {

  -- | Unique id for this client instance, must be set to 0 on the first request.

  -- Versions: 0+
  getTelemetrySubscriptionsRequestClientInstanceId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for GetTelemetrySubscriptionsRequest.
maxGetTelemetrySubscriptionsRequestVersion :: Int16
maxGetTelemetrySubscriptionsRequestVersion = 0

-- | KafkaMessage instance for GetTelemetrySubscriptionsRequest.
instance KafkaMessage GetTelemetrySubscriptionsRequest where
  messageApiKey = 71
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode GetTelemetrySubscriptionsRequest with the given API version.
encodeGetTelemetrySubscriptionsRequest :: MonadPut m => E.ApiVersion -> GetTelemetrySubscriptionsRequest -> m ()
encodeGetTelemetrySubscriptionsRequest version msg
  | version == 0 =
    do
      serialize (getTelemetrySubscriptionsRequestClientInstanceId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode GetTelemetrySubscriptionsRequest with the given API version.
decodeGetTelemetrySubscriptionsRequest :: MonadGet m => E.ApiVersion -> m GetTelemetrySubscriptionsRequest
decodeGetTelemetrySubscriptionsRequest version
  | version == 0 =
    do
      fieldclientinstanceid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure GetTelemetrySubscriptionsRequest
        {
        getTelemetrySubscriptionsRequestClientInstanceId = fieldclientinstanceid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a GetTelemetrySubscriptionsRequest.
wireMaxSizeGetTelemetrySubscriptionsRequest :: Int -> GetTelemetrySubscriptionsRequest -> Int
wireMaxSizeGetTelemetrySubscriptionsRequest _version msg =
  0
  + 16
  + 1

-- | Direct-poke encoder for GetTelemetrySubscriptionsRequest.
wirePokeGetTelemetrySubscriptionsRequest :: Int -> Ptr Word8 -> GetTelemetrySubscriptionsRequest -> IO (Ptr Word8)
wirePokeGetTelemetrySubscriptionsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaUuid p0 (getTelemetrySubscriptionsRequestClientInstanceId msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke GetTelemetrySubscriptionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for GetTelemetrySubscriptionsRequest.
wirePeekGetTelemetrySubscriptionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (GetTelemetrySubscriptionsRequest, Ptr Word8)
wirePeekGetTelemetrySubscriptionsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clientinstanceid, p1) <- WP.peekKafkaUuid p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (GetTelemetrySubscriptionsRequest { getTelemetrySubscriptionsRequestClientInstanceId = f0_clientinstanceid }, pTagsEnd)
  | otherwise = error $ "wirePeek GetTelemetrySubscriptionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec GetTelemetrySubscriptionsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeGetTelemetrySubscriptionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeGetTelemetrySubscriptionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekGetTelemetrySubscriptionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}