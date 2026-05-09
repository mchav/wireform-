{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EnvelopeRequest
Description : Kafka EnvelopeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 58.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EnvelopeRequest
  (
    EnvelopeRequest(..),
    encodeEnvelopeRequest,
    decodeEnvelopeRequest,
    maxEnvelopeRequestVersion
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




data EnvelopeRequest = EnvelopeRequest
  {

  -- | The embedded request header and data.

  -- Versions: 0+
  envelopeRequestRequestData :: !(KafkaBytes)
,

  -- | Value of the initial client principal when the request is redirected by a broker.

  -- Versions: 0+
  envelopeRequestRequestPrincipal :: !(KafkaBytes)
,

  -- | The original client's address in bytes.

  -- Versions: 0+
  envelopeRequestClientHostAddress :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EnvelopeRequest.
maxEnvelopeRequestVersion :: Int16
maxEnvelopeRequestVersion = 0

-- | Encode EnvelopeRequest with the given API version.
encodeEnvelopeRequest :: MonadPut m => E.ApiVersion -> EnvelopeRequest -> m ()
encodeEnvelopeRequest version msg
  | version == 0 =
    do
      serialize (toCompactBytes (envelopeRequestRequestData msg))
      serialize (toCompactBytes (envelopeRequestRequestPrincipal msg))
      serialize (toCompactBytes (envelopeRequestClientHostAddress msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EnvelopeRequest with the given API version.
decodeEnvelopeRequest :: MonadGet m => E.ApiVersion -> m EnvelopeRequest
decodeEnvelopeRequest version
  | version == 0 =
    do
      fieldrequestdata <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
      fieldrequestprincipal <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
      fieldclienthostaddress <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EnvelopeRequest
        {
        envelopeRequestRequestData = fieldrequestdata
        ,
        envelopeRequestRequestPrincipal = fieldrequestprincipal
        ,
        envelopeRequestClientHostAddress = fieldclienthostaddress
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeEnvelopeRequest' / 'decodeEnvelopeRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec EnvelopeRequest where
  wireCodec = Just (WC.serialShimCodec encodeEnvelopeRequest decodeEnvelopeRequest)
  {-# INLINE wireCodec #-}
