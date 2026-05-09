{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RenewDelegationTokenRequest
Description : Kafka RenewDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 39.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RenewDelegationTokenRequest
  (
    RenewDelegationTokenRequest(..),
    encodeRenewDelegationTokenRequest,
    decodeRenewDelegationTokenRequest,
    maxRenewDelegationTokenRequestVersion
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




data RenewDelegationTokenRequest = RenewDelegationTokenRequest
  {

  -- | The HMAC of the delegation token to be renewed.

  -- Versions: 0+
  renewDelegationTokenRequestHmac :: !(KafkaBytes)
,

  -- | The renewal time period in milliseconds.

  -- Versions: 0+
  renewDelegationTokenRequestRenewPeriodMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RenewDelegationTokenRequest.
maxRenewDelegationTokenRequestVersion :: Int16
maxRenewDelegationTokenRequestVersion = 2

-- | Encode RenewDelegationTokenRequest with the given API version.
encodeRenewDelegationTokenRequest :: MonadPut m => E.ApiVersion -> RenewDelegationTokenRequest -> m ()
encodeRenewDelegationTokenRequest version msg
  | version == 1 =
    do
      serialize (renewDelegationTokenRequestHmac msg)
      serialize (renewDelegationTokenRequestRenewPeriodMs msg)


  | version == 2 =
    do
      serialize (toCompactBytes (renewDelegationTokenRequestHmac msg))
      serialize (renewDelegationTokenRequestRenewPeriodMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RenewDelegationTokenRequest with the given API version.
decodeRenewDelegationTokenRequest :: MonadGet m => E.ApiVersion -> m RenewDelegationTokenRequest
decodeRenewDelegationTokenRequest version
  | version == 1 =
    do
      fieldhmac <- deserialize
      fieldrenewperiodms <- deserialize
      pure RenewDelegationTokenRequest
        {
        renewDelegationTokenRequestHmac = fieldhmac
        ,
        renewDelegationTokenRequestRenewPeriodMs = fieldrenewperiodms
        }

  | version == 2 =
    do
      fieldhmac <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      fieldrenewperiodms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RenewDelegationTokenRequest
        {
        renewDelegationTokenRequestHmac = fieldhmac
        ,
        renewDelegationTokenRequestRenewPeriodMs = fieldrenewperiodms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeRenewDelegationTokenRequest' / 'decodeRenewDelegationTokenRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec RenewDelegationTokenRequest where
  wireCodec = Just (WC.serialShimCodec encodeRenewDelegationTokenRequest decodeRenewDelegationTokenRequest)
  {-# INLINE wireCodec #-}
