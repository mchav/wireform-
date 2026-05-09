{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerRegistrationResponse
Description : Kafka BrokerRegistrationResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 62.



Valid versions: 0-4
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerRegistrationResponse
  (
    BrokerRegistrationResponse(..),
    encodeBrokerRegistrationResponse,
    decodeBrokerRegistrationResponse,
    maxBrokerRegistrationResponseVersion
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




data BrokerRegistrationResponse = BrokerRegistrationResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  brokerRegistrationResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  brokerRegistrationResponseErrorCode :: !(Int16)
,

  -- | The broker's assigned epoch, or -1 if none was assigned.

  -- Versions: 0+
  brokerRegistrationResponseBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerRegistrationResponse.
maxBrokerRegistrationResponseVersion :: Int16
maxBrokerRegistrationResponseVersion = 4

-- | Encode BrokerRegistrationResponse with the given API version.
encodeBrokerRegistrationResponse :: MonadPut m => E.ApiVersion -> BrokerRegistrationResponse -> m ()
encodeBrokerRegistrationResponse version msg
  | version >= 0 && version <= 4 =
    do
      serialize (brokerRegistrationResponseThrottleTimeMs msg)
      serialize (brokerRegistrationResponseErrorCode msg)
      serialize (brokerRegistrationResponseBrokerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BrokerRegistrationResponse with the given API version.
decodeBrokerRegistrationResponse :: MonadGet m => E.ApiVersion -> m BrokerRegistrationResponse
decodeBrokerRegistrationResponse version
  | version >= 0 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldbrokerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerRegistrationResponse
        {
        brokerRegistrationResponseThrottleTimeMs = fieldthrottletimems
        ,
        brokerRegistrationResponseErrorCode = fielderrorcode
        ,
        brokerRegistrationResponseBrokerEpoch = fieldbrokerepoch
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec BrokerRegistrationResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
