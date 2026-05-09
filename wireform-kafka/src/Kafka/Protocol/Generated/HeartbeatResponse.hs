{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.HeartbeatResponse
Description : Kafka HeartbeatResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 12.



Valid versions: 0-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.HeartbeatResponse
  (
    HeartbeatResponse(..),
    encodeHeartbeatResponse,
    decodeHeartbeatResponse,
    maxHeartbeatResponseVersion
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




data HeartbeatResponse = HeartbeatResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  heartbeatResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  heartbeatResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for HeartbeatResponse.
maxHeartbeatResponseVersion :: Int16
maxHeartbeatResponseVersion = 4

-- | Encode HeartbeatResponse with the given API version.
encodeHeartbeatResponse :: MonadPut m => E.ApiVersion -> HeartbeatResponse -> m ()
encodeHeartbeatResponse version msg
  | version == 0 =
    do
      serialize (heartbeatResponseErrorCode msg)


  | version == 4 =
    do
      serialize (heartbeatResponseThrottleTimeMs msg)
      serialize (heartbeatResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      serialize (heartbeatResponseThrottleTimeMs msg)
      serialize (heartbeatResponseErrorCode msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode HeartbeatResponse with the given API version.
decodeHeartbeatResponse :: MonadGet m => E.ApiVersion -> m HeartbeatResponse
decodeHeartbeatResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      pure HeartbeatResponse
        {
        heartbeatResponseThrottleTimeMs = 0
        ,
        heartbeatResponseErrorCode = fielderrorcode
        }

  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure HeartbeatResponse
        {
        heartbeatResponseThrottleTimeMs = fieldthrottletimems
        ,
        heartbeatResponseErrorCode = fielderrorcode
        }

  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      pure HeartbeatResponse
        {
        heartbeatResponseThrottleTimeMs = fieldthrottletimems
        ,
        heartbeatResponseErrorCode = fielderrorcode
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeHeartbeatResponse' / 'decodeHeartbeatResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec HeartbeatResponse where
  wireCodec = Just (WC.serialShimCodec encodeHeartbeatResponse decodeHeartbeatResponse)
  {-# INLINE wireCodec #-}
