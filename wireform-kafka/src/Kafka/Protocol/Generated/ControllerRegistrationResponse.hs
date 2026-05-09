{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControllerRegistrationResponse
Description : Kafka ControllerRegistrationResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 70.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControllerRegistrationResponse
  (
    ControllerRegistrationResponse(..),
    encodeControllerRegistrationResponse,
    decodeControllerRegistrationResponse,
    maxControllerRegistrationResponseVersion
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




data ControllerRegistrationResponse = ControllerRegistrationResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  controllerRegistrationResponseThrottleTimeMs :: !(Int32)
,

  -- | The response error code.

  -- Versions: 0+
  controllerRegistrationResponseErrorCode :: !(Int16)
,

  -- | The response error message, or null if there was no error.

  -- Versions: 0+
  controllerRegistrationResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControllerRegistrationResponse.
maxControllerRegistrationResponseVersion :: Int16
maxControllerRegistrationResponseVersion = 0

-- | Encode ControllerRegistrationResponse with the given API version.
encodeControllerRegistrationResponse :: MonadPut m => E.ApiVersion -> ControllerRegistrationResponse -> m ()
encodeControllerRegistrationResponse version msg
  | version == 0 =
    do
      serialize (controllerRegistrationResponseThrottleTimeMs msg)
      serialize (controllerRegistrationResponseErrorCode msg)
      serialize (toCompactString (controllerRegistrationResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ControllerRegistrationResponse with the given API version.
decodeControllerRegistrationResponse :: MonadGet m => E.ApiVersion -> m ControllerRegistrationResponse
decodeControllerRegistrationResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ControllerRegistrationResponse
        {
        controllerRegistrationResponseThrottleTimeMs = fieldthrottletimems
        ,
        controllerRegistrationResponseErrorCode = fielderrorcode
        ,
        controllerRegistrationResponseErrorMessage = fielderrormessage
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeControllerRegistrationResponse' / 'decodeControllerRegistrationResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ControllerRegistrationResponse where
  wireCodec = Just (WC.serialShimCodec encodeControllerRegistrationResponse decodeControllerRegistrationResponse)
  {-# INLINE wireCodec #-}
