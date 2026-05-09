{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslHandshakeResponse
Description : Kafka SaslHandshakeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 17.



Valid versions: 0-1
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslHandshakeResponse
  (
    SaslHandshakeResponse(..),
    encodeSaslHandshakeResponse,
    decodeSaslHandshakeResponse,
    maxSaslHandshakeResponseVersion
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC




data SaslHandshakeResponse = SaslHandshakeResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  saslHandshakeResponseErrorCode :: !(Int16)
,

  -- | The mechanisms enabled in the server.

  -- Versions: 0+
  saslHandshakeResponseMechanisms :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslHandshakeResponse.
maxSaslHandshakeResponseVersion :: Int16
maxSaslHandshakeResponseVersion = 1

-- | KafkaMessage instance for SaslHandshakeResponse.
instance KafkaMessage SaslHandshakeResponse where
  messageApiKey = 17
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Nothing

-- | Encode SaslHandshakeResponse with the given API version.
encodeSaslHandshakeResponse :: MonadPut m => E.ApiVersion -> SaslHandshakeResponse -> m ()
encodeSaslHandshakeResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (saslHandshakeResponseErrorCode msg)
      E.encodeVersionedArray version 999 (\v s -> if v >= 999 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (saslHandshakeResponseMechanisms msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SaslHandshakeResponse with the given API version.
decodeSaslHandshakeResponse :: MonadGet m => E.ApiVersion -> m SaslHandshakeResponse
decodeSaslHandshakeResponse version
  | version >= 0 && version <= 1 =
    do
      fielderrorcode <- deserialize
      fieldmechanisms <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 (\v -> if v >= 999 then P.fromCompactString <$> deserialize else deserialize)
      pure SaslHandshakeResponse
        {
        saslHandshakeResponseErrorCode = fielderrorcode
        ,
        saslHandshakeResponseMechanisms = fieldmechanisms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeSaslHandshakeResponse' / 'decodeSaslHandshakeResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec SaslHandshakeResponse where
  wireCodec = Just (WC.serialShimCodec encodeSaslHandshakeResponse decodeSaslHandshakeResponse)
  {-# INLINE wireCodec #-}
