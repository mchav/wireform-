{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerHeartbeatRequest
Description : Kafka BrokerHeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 63.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerHeartbeatRequest
  (
    BrokerHeartbeatRequest(..),
    encodeBrokerHeartbeatRequest,
    decodeBrokerHeartbeatRequest,
    maxBrokerHeartbeatRequestVersion
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




data BrokerHeartbeatRequest = BrokerHeartbeatRequest
  {

  -- | The broker ID.

  -- Versions: 0+
  brokerHeartbeatRequestBrokerId :: !(Int32)
,

  -- | The broker epoch.

  -- Versions: 0+
  brokerHeartbeatRequestBrokerEpoch :: !(Int64)
,

  -- | The highest metadata offset which the broker has reached.

  -- Versions: 0+
  brokerHeartbeatRequestCurrentMetadataOffset :: !(Int64)
,

  -- | True if the broker wants to be fenced, false otherwise.

  -- Versions: 0+
  brokerHeartbeatRequestWantFence :: !(Bool)
,

  -- | True if the broker wants to be shut down, false otherwise.

  -- Versions: 0+
  brokerHeartbeatRequestWantShutDown :: !(Bool)
,

  -- | Log directories that failed and went offline.

  -- Versions: 1+
  brokerHeartbeatRequestOfflineLogDirs :: !(KafkaArray (KafkaUuid))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerHeartbeatRequest.
maxBrokerHeartbeatRequestVersion :: Int16
maxBrokerHeartbeatRequestVersion = 1

-- | Encode BrokerHeartbeatRequest with the given API version.
encodeBrokerHeartbeatRequest :: MonadPut m => E.ApiVersion -> BrokerHeartbeatRequest -> m ()
encodeBrokerHeartbeatRequest version msg
  | version == 0 =
    do
      serialize (brokerHeartbeatRequestBrokerId msg)
      serialize (brokerHeartbeatRequestBrokerEpoch msg)
      serialize (brokerHeartbeatRequestCurrentMetadataOffset msg)
      serialize (brokerHeartbeatRequestWantFence msg)
      serialize (brokerHeartbeatRequestWantShutDown msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (brokerHeartbeatRequestBrokerId msg)
      serialize (brokerHeartbeatRequestBrokerEpoch msg)
      serialize (brokerHeartbeatRequestCurrentMetadataOffset msg)
      serialize (brokerHeartbeatRequestWantFence msg)
      serialize (brokerHeartbeatRequestWantShutDown msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BrokerHeartbeatRequest with the given API version.
decodeBrokerHeartbeatRequest :: MonadGet m => E.ApiVersion -> m BrokerHeartbeatRequest
decodeBrokerHeartbeatRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fieldcurrentmetadataoffset <- deserialize
      fieldwantfence <- deserialize
      fieldwantshutdown <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerHeartbeatRequest
        {
        brokerHeartbeatRequestBrokerId = fieldbrokerid
        ,
        brokerHeartbeatRequestBrokerEpoch = fieldbrokerepoch
        ,
        brokerHeartbeatRequestCurrentMetadataOffset = fieldcurrentmetadataoffset
        ,
        brokerHeartbeatRequestWantFence = fieldwantfence
        ,
        brokerHeartbeatRequestWantShutDown = fieldwantshutdown
        ,
        brokerHeartbeatRequestOfflineLogDirs = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fieldcurrentmetadataoffset <- deserialize
      fieldwantfence <- deserialize
      fieldwantshutdown <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerHeartbeatRequest
        {
        brokerHeartbeatRequestBrokerId = fieldbrokerid
        ,
        brokerHeartbeatRequestBrokerEpoch = fieldbrokerepoch
        ,
        brokerHeartbeatRequestCurrentMetadataOffset = fieldcurrentmetadataoffset
        ,
        brokerHeartbeatRequestWantFence = fieldwantfence
        ,
        brokerHeartbeatRequestWantShutDown = fieldwantshutdown
        ,
        brokerHeartbeatRequestOfflineLogDirs = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec BrokerHeartbeatRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
