{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ExpireDelegationTokenRequest
Description : Kafka ExpireDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 40.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ExpireDelegationTokenRequest
  (
    ExpireDelegationTokenRequest(..),
    encodeExpireDelegationTokenRequest,
    decodeExpireDelegationTokenRequest,
    maxExpireDelegationTokenRequestVersion
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




data ExpireDelegationTokenRequest = ExpireDelegationTokenRequest
  {

  -- | The HMAC of the delegation token to be expired.

  -- Versions: 0+
  expireDelegationTokenRequestHmac :: !(KafkaBytes)
,

  -- | The expiry time period in milliseconds.

  -- Versions: 0+
  expireDelegationTokenRequestExpiryTimePeriodMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ExpireDelegationTokenRequest.
maxExpireDelegationTokenRequestVersion :: Int16
maxExpireDelegationTokenRequestVersion = 2

-- | Encode ExpireDelegationTokenRequest with the given API version.
encodeExpireDelegationTokenRequest :: MonadPut m => E.ApiVersion -> ExpireDelegationTokenRequest -> m ()
encodeExpireDelegationTokenRequest version msg
  | version == 1 =
    do
      serialize (expireDelegationTokenRequestHmac msg)
      serialize (expireDelegationTokenRequestExpiryTimePeriodMs msg)


  | version == 2 =
    do
      serialize (toCompactBytes (expireDelegationTokenRequestHmac msg))
      serialize (expireDelegationTokenRequestExpiryTimePeriodMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ExpireDelegationTokenRequest with the given API version.
decodeExpireDelegationTokenRequest :: MonadGet m => E.ApiVersion -> m ExpireDelegationTokenRequest
decodeExpireDelegationTokenRequest version
  | version == 1 =
    do
      fieldhmac <- deserialize
      fieldexpirytimeperiodms <- deserialize
      pure ExpireDelegationTokenRequest
        {
        expireDelegationTokenRequestHmac = fieldhmac
        ,
        expireDelegationTokenRequestExpiryTimePeriodMs = fieldexpirytimeperiodms
        }

  | version == 2 =
    do
      fieldhmac <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      fieldexpirytimeperiodms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ExpireDelegationTokenRequest
        {
        expireDelegationTokenRequestHmac = fieldhmac
        ,
        expireDelegationTokenRequestExpiryTimePeriodMs = fieldexpirytimeperiodms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeExpireDelegationTokenRequest' / 'decodeExpireDelegationTokenRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ExpireDelegationTokenRequest where
  wireCodec = Just (WC.serialShimCodec encodeExpireDelegationTokenRequest decodeExpireDelegationTokenRequest)
  {-# INLINE wireCodec #-}
