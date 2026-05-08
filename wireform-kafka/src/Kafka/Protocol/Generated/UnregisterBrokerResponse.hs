{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UnregisterBrokerResponse
Description : Kafka UnregisterBrokerResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 64.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UnregisterBrokerResponse
  (
    UnregisterBrokerResponse(..),
    encodeUnregisterBrokerResponse,
    decodeUnregisterBrokerResponse,
    maxUnregisterBrokerResponseVersion
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




data UnregisterBrokerResponse = UnregisterBrokerResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  unregisterBrokerResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  unregisterBrokerResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or `null` if there was no top-level error.

  -- Versions: 0+
  unregisterBrokerResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UnregisterBrokerResponse.
maxUnregisterBrokerResponseVersion :: Int16
maxUnregisterBrokerResponseVersion = 0

-- | Encode UnregisterBrokerResponse with the given API version.
encodeUnregisterBrokerResponse :: MonadPut m => E.ApiVersion -> UnregisterBrokerResponse -> m ()
encodeUnregisterBrokerResponse version msg
  | version == 0 =
    do
      serialize (unregisterBrokerResponseThrottleTimeMs msg)
      serialize (unregisterBrokerResponseErrorCode msg)
      serialize (toCompactString (unregisterBrokerResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UnregisterBrokerResponse with the given API version.
decodeUnregisterBrokerResponse :: MonadGet m => E.ApiVersion -> m UnregisterBrokerResponse
decodeUnregisterBrokerResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UnregisterBrokerResponse
        {
        unregisterBrokerResponseThrottleTimeMs = fieldthrottletimems
        ,
        unregisterBrokerResponseErrorCode = fielderrorcode
        ,
        unregisterBrokerResponseErrorMessage = fielderrormessage
        }
  | otherwise = fail $ "Unsupported version: " ++ show version