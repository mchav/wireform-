{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.GetTelemetrySubscriptionsRequest
Description : Kafka GetTelemetrySubscriptionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 71.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.GetTelemetrySubscriptionsRequest
  (
    GetTelemetrySubscriptionsRequest(..),
    encodeGetTelemetrySubscriptionsRequest,
    decodeGetTelemetrySubscriptionsRequest,
    maxGetTelemetrySubscriptionsRequestVersion
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




data GetTelemetrySubscriptionsRequest = GetTelemetrySubscriptionsRequest
  {

  -- | Unique id for this client instance, must be set to 0 on the first request.

  -- Versions: 0+
  getTelemetrySubscriptionsRequestClientInstanceId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for GetTelemetrySubscriptionsRequest.
maxGetTelemetrySubscriptionsRequestVersion :: Int16
maxGetTelemetrySubscriptionsRequestVersion = 0

-- | Encode GetTelemetrySubscriptionsRequest with the given API version.
encodeGetTelemetrySubscriptionsRequest :: MonadPut m => E.ApiVersion -> GetTelemetrySubscriptionsRequest -> m ()
encodeGetTelemetrySubscriptionsRequest version msg
  | version == 0 =
    do
      serialize (getTelemetrySubscriptionsRequestClientInstanceId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode GetTelemetrySubscriptionsRequest with the given API version.
decodeGetTelemetrySubscriptionsRequest :: MonadGet m => E.ApiVersion -> m GetTelemetrySubscriptionsRequest
decodeGetTelemetrySubscriptionsRequest version
  | version == 0 =
    do
      fieldclientinstanceid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure GetTelemetrySubscriptionsRequest
        {
        getTelemetrySubscriptionsRequestClientInstanceId = fieldclientinstanceid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec GetTelemetrySubscriptionsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
