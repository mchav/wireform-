{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslHandshakeRequest
Description : Kafka SaslHandshakeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 17.



Valid versions: 0-1
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslHandshakeRequest
  (
    SaslHandshakeRequest(..),
    encodeSaslHandshakeRequest,
    decodeSaslHandshakeRequest,
    maxSaslHandshakeRequestVersion
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




data SaslHandshakeRequest = SaslHandshakeRequest
  {

  -- | The SASL mechanism chosen by the client.

  -- Versions: 0+
  saslHandshakeRequestMechanism :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslHandshakeRequest.
maxSaslHandshakeRequestVersion :: Int16
maxSaslHandshakeRequestVersion = 1

-- | KafkaMessage instance for SaslHandshakeRequest.
instance KafkaMessage SaslHandshakeRequest where
  messageApiKey = 17
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Nothing

-- | Encode SaslHandshakeRequest with the given API version.
encodeSaslHandshakeRequest :: MonadPut m => E.ApiVersion -> SaslHandshakeRequest -> m ()
encodeSaslHandshakeRequest version msg
  | version >= 0 && version <= 1 =
    do
      serialize (saslHandshakeRequestMechanism msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SaslHandshakeRequest with the given API version.
decodeSaslHandshakeRequest :: MonadGet m => E.ApiVersion -> m SaslHandshakeRequest
decodeSaslHandshakeRequest version
  | version >= 0 && version <= 1 =
    do
      fieldmechanism <- deserialize
      pure SaslHandshakeRequest
        {
        saslHandshakeRequestMechanism = fieldmechanism
        }
  | otherwise = fail $ "Unsupported version: " ++ show version