{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UnregisterBrokerRequest
Description : Kafka UnregisterBrokerRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 64.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UnregisterBrokerRequest
  (
    UnregisterBrokerRequest(..),
    encodeUnregisterBrokerRequest,
    decodeUnregisterBrokerRequest,
    maxUnregisterBrokerRequestVersion
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




data UnregisterBrokerRequest = UnregisterBrokerRequest
  {

  -- | The broker ID to unregister.

  -- Versions: 0+
  unregisterBrokerRequestBrokerId :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UnregisterBrokerRequest.
maxUnregisterBrokerRequestVersion :: Int16
maxUnregisterBrokerRequestVersion = 0

-- | Encode UnregisterBrokerRequest with the given API version.
encodeUnregisterBrokerRequest :: MonadPut m => E.ApiVersion -> UnregisterBrokerRequest -> m ()
encodeUnregisterBrokerRequest version msg
  | version == 0 =
    do
      serialize (unregisterBrokerRequestBrokerId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UnregisterBrokerRequest with the given API version.
decodeUnregisterBrokerRequest :: MonadGet m => E.ApiVersion -> m UnregisterBrokerRequest
decodeUnregisterBrokerRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UnregisterBrokerRequest
        {
        unregisterBrokerRequestBrokerId = fieldbrokerid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version