{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerHeartbeatRequest
Description : Kafka BrokerHeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 63.



Valid versions: 0-2
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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




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
,

  -- | List of log directories that are cordoned. This is null before the broker reaches the RECOVERY state

  -- Versions: 2+
  brokerHeartbeatRequestCordonedLogDirs :: !(KafkaArray (KafkaUuid))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerHeartbeatRequest.
maxBrokerHeartbeatRequestVersion :: Int16
maxBrokerHeartbeatRequestVersion = 2

-- | KafkaMessage instance for BrokerHeartbeatRequest.
instance KafkaMessage BrokerHeartbeatRequest where
  messageApiKey = 63
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

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
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestOfflineLogDirs msg)))] else []) ++ (if version >= 2 then [(1, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestCordonedLogDirs msg)))] else [])
        P.serializeTaggedFieldEntries _entries

  | version == 1 =
    do
      serialize (brokerHeartbeatRequestBrokerId msg)
      serialize (brokerHeartbeatRequestBrokerEpoch msg)
      serialize (brokerHeartbeatRequestCurrentMetadataOffset msg)
      serialize (brokerHeartbeatRequestWantFence msg)
      serialize (brokerHeartbeatRequestWantShutDown msg)
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestOfflineLogDirs msg)))] else []) ++ (if version >= 2 then [(1, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestCordonedLogDirs msg)))] else [])
        P.serializeTaggedFieldEntries _entries

  | version == 2 =
    do
      serialize (brokerHeartbeatRequestBrokerId msg)
      serialize (brokerHeartbeatRequestBrokerEpoch msg)
      serialize (brokerHeartbeatRequestCurrentMetadataOffset msg)
      serialize (brokerHeartbeatRequestWantFence msg)
      serialize (brokerHeartbeatRequestWantShutDown msg)
      do
        let _entries = (if version >= 1 then [(0, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestOfflineLogDirs msg)))] else []) ++ (if version >= 2 then [(1, Data.Bytes.Put.runPutS (serialize (brokerHeartbeatRequestCordonedLogDirs msg)))] else [])
        P.serializeTaggedFieldEntries _entries
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
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldofflinelogdirs =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      let fieldcordonedlogdirs =
            if version >= 2
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaArray P.Null)
                Nothing  -> (P.KafkaArray P.Null)
              else (P.KafkaArray P.Null)
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
        brokerHeartbeatRequestOfflineLogDirs = fieldofflinelogdirs
        ,
        brokerHeartbeatRequestCordonedLogDirs = fieldcordonedlogdirs
        }

  | version == 1 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fieldcurrentmetadataoffset <- deserialize
      fieldwantfence <- deserialize
      fieldwantshutdown <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldofflinelogdirs =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      let fieldcordonedlogdirs =
            if version >= 2
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaArray P.Null)
                Nothing  -> (P.KafkaArray P.Null)
              else (P.KafkaArray P.Null)
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
        brokerHeartbeatRequestOfflineLogDirs = fieldofflinelogdirs
        ,
        brokerHeartbeatRequestCordonedLogDirs = fieldcordonedlogdirs
        }

  | version == 2 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      fieldcurrentmetadataoffset <- deserialize
      fieldwantfence <- deserialize
      fieldwantshutdown <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldofflinelogdirs =
            if version >= 1
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      let fieldcordonedlogdirs =
            if version >= 2
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaArray P.Null)
                Nothing  -> (P.KafkaArray P.Null)
              else (P.KafkaArray P.Null)
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
        brokerHeartbeatRequestOfflineLogDirs = fieldofflinelogdirs
        ,
        brokerHeartbeatRequestCordonedLogDirs = fieldcordonedlogdirs
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeBrokerHeartbeatRequest' / 'decodeBrokerHeartbeatRequest'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec BrokerHeartbeatRequest where
  wireCodec = Just (WC.serialShimCodec encodeBrokerHeartbeatRequest decodeBrokerHeartbeatRequest)
  {-# INLINE wireCodec #-}