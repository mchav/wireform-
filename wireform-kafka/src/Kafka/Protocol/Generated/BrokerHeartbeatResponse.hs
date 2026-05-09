{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerHeartbeatResponse
Description : Kafka BrokerHeartbeatResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 63.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerHeartbeatResponse
  (
    BrokerHeartbeatResponse(..),
    encodeBrokerHeartbeatResponse,
    decodeBrokerHeartbeatResponse,
    maxBrokerHeartbeatResponseVersion
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




data BrokerHeartbeatResponse = BrokerHeartbeatResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  brokerHeartbeatResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  brokerHeartbeatResponseErrorCode :: !(Int16)
,

  -- | True if the broker has approximately caught up with the latest metadata.

  -- Versions: 0+
  brokerHeartbeatResponseIsCaughtUp :: !(Bool)
,

  -- | True if the broker is fenced.

  -- Versions: 0+
  brokerHeartbeatResponseIsFenced :: !(Bool)
,

  -- | True if the broker should proceed with its shutdown.

  -- Versions: 0+
  brokerHeartbeatResponseShouldShutDown :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerHeartbeatResponse.
maxBrokerHeartbeatResponseVersion :: Int16
maxBrokerHeartbeatResponseVersion = 1

-- | Encode BrokerHeartbeatResponse with the given API version.
encodeBrokerHeartbeatResponse :: MonadPut m => E.ApiVersion -> BrokerHeartbeatResponse -> m ()
encodeBrokerHeartbeatResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (brokerHeartbeatResponseThrottleTimeMs msg)
      serialize (brokerHeartbeatResponseErrorCode msg)
      serialize (brokerHeartbeatResponseIsCaughtUp msg)
      serialize (brokerHeartbeatResponseIsFenced msg)
      serialize (brokerHeartbeatResponseShouldShutDown msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BrokerHeartbeatResponse with the given API version.
decodeBrokerHeartbeatResponse :: MonadGet m => E.ApiVersion -> m BrokerHeartbeatResponse
decodeBrokerHeartbeatResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldiscaughtup <- deserialize
      fieldisfenced <- deserialize
      fieldshouldshutdown <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerHeartbeatResponse
        {
        brokerHeartbeatResponseThrottleTimeMs = fieldthrottletimems
        ,
        brokerHeartbeatResponseErrorCode = fielderrorcode
        ,
        brokerHeartbeatResponseIsCaughtUp = fieldiscaughtup
        ,
        brokerHeartbeatResponseIsFenced = fieldisfenced
        ,
        brokerHeartbeatResponseShouldShutDown = fieldshouldshutdown
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeBrokerHeartbeatResponse' / 'decodeBrokerHeartbeatResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec BrokerHeartbeatResponse where
  wireCodec = Just (WC.serialShimCodec encodeBrokerHeartbeatResponse decodeBrokerHeartbeatResponse)
  {-# INLINE wireCodec #-}
