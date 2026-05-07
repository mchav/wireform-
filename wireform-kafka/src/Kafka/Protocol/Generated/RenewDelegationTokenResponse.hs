{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RenewDelegationTokenResponse
Description : Kafka RenewDelegationTokenResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 39.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RenewDelegationTokenResponse
  (
    RenewDelegationTokenResponse(..),
    encodeRenewDelegationTokenResponse,
    decodeRenewDelegationTokenResponse,
    maxRenewDelegationTokenResponseVersion
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




data RenewDelegationTokenResponse = RenewDelegationTokenResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  renewDelegationTokenResponseErrorCode :: !(Int16)
,

  -- | The timestamp in milliseconds at which this token expires.

  -- Versions: 0+
  renewDelegationTokenResponseExpiryTimestampMs :: !(Int64)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  renewDelegationTokenResponseThrottleTimeMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RenewDelegationTokenResponse.
maxRenewDelegationTokenResponseVersion :: Int16
maxRenewDelegationTokenResponseVersion = 2

-- | Encode RenewDelegationTokenResponse with the given API version.
encodeRenewDelegationTokenResponse :: MonadPut m => E.ApiVersion -> RenewDelegationTokenResponse -> m ()
encodeRenewDelegationTokenResponse version msg
  | version == 1 =
    do
      serialize (renewDelegationTokenResponseErrorCode msg)
      serialize (renewDelegationTokenResponseExpiryTimestampMs msg)
      serialize (renewDelegationTokenResponseThrottleTimeMs msg)


  | version == 2 =
    do
      serialize (renewDelegationTokenResponseErrorCode msg)
      serialize (renewDelegationTokenResponseExpiryTimestampMs msg)
      serialize (renewDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RenewDelegationTokenResponse with the given API version.
decodeRenewDelegationTokenResponse :: MonadGet m => E.ApiVersion -> m RenewDelegationTokenResponse
decodeRenewDelegationTokenResponse version
  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      pure RenewDelegationTokenResponse
        {
        renewDelegationTokenResponseErrorCode = fielderrorcode
        ,
        renewDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        renewDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version == 2 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RenewDelegationTokenResponse
        {
        renewDelegationTokenResponseErrorCode = fielderrorcode
        ,
        renewDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        renewDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version