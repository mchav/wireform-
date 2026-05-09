{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerRegistrationRequest
Description : Kafka BrokerRegistrationRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 62.



Valid versions: 0-4
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerRegistrationRequest
  (
    BrokerRegistrationRequest(..),
    Listener(..),
    Feature(..),
    maxBrokerRegistrationRequestVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The listeners of this broker.
data Listener = Listener
  {

  -- | The name of the endpoint.

  -- Versions: 0+
  listenerName :: !(KafkaString)
,

  -- | The hostname.

  -- Versions: 0+
  listenerHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 0+
  listenerPort :: !(Word16)
,

  -- | The security protocol.

  -- Versions: 0+
  listenerSecurityProtocol :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The features on this broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.
data Feature = Feature
  {

  -- | The feature name.

  -- Versions: 0+
  featureName :: !(KafkaString)
,

  -- | The minimum supported feature level.

  -- Versions: 0+
  featureMinSupportedVersion :: !(Int16)
,

  -- | The maximum supported feature level.

  -- Versions: 0+
  featureMaxSupportedVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


data BrokerRegistrationRequest = BrokerRegistrationRequest
  {

  -- | The broker ID.

  -- Versions: 0+
  brokerRegistrationRequestBrokerId :: !(Int32)
,

  -- | The cluster id of the broker process.

  -- Versions: 0+
  brokerRegistrationRequestClusterId :: !(KafkaString)
,

  -- | The incarnation id of the broker process.

  -- Versions: 0+
  brokerRegistrationRequestIncarnationId :: !(KafkaUuid)
,

  -- | The listeners of this broker.

  -- Versions: 0+
  brokerRegistrationRequestListeners :: !(KafkaArray (Listener))
,

  -- | The features on this broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.

  -- Versions: 0+
  brokerRegistrationRequestFeatures :: !(KafkaArray (Feature))
,

  -- | The rack which this broker is in.

  -- Versions: 0+
  brokerRegistrationRequestRack :: !(KafkaString)
,

  -- | If the required configurations for ZK migration are present, this value is set to true.

  -- Versions: 1+
  brokerRegistrationRequestIsMigratingZkBroker :: !(Bool)
,

  -- | Log directories configured in this broker which are available.

  -- Versions: 2+
  brokerRegistrationRequestLogDirs :: !(KafkaArray (KafkaUuid))
,

  -- | The epoch before a clean shutdown.

  -- Versions: 3+
  brokerRegistrationRequestPreviousBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerRegistrationRequest.
maxBrokerRegistrationRequestVersion :: Int16
maxBrokerRegistrationRequestVersion = 4

-- | KafkaMessage instance for BrokerRegistrationRequest.
instance KafkaMessage BrokerRegistrationRequest where
  messageApiKey = 62
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a Listener.
wireMaxSizeListener :: Int -> Listener -> Int
wireMaxSizeListener _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listenerName msg))
  + WP.compactStringMaxSize (P.toCompactString (listenerHost msg))
  + 2
  + 2
  + 1

-- | Direct-poke encoder for Listener.
wirePokeListener :: Int -> Ptr Word8 -> Listener -> IO (Ptr Word8)
wirePokeListener version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (listenerName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (listenerHost msg))
  p3 <- W.pokeWord16BE p2 (listenerPort msg)
  p4 <- W.pokeInt16BE p3 (listenerSecurityProtocol msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  (f3_securityprotocol, p4) <- W.peekInt16BE p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port, listenerSecurityProtocol = f3_securityprotocol }, pTagsEnd)

-- | Worst-case wire size of a Feature.
wireMaxSizeFeature :: Int -> Feature -> Int
wireMaxSizeFeature _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (featureName msg))
  + 2
  + 2
  + 1

-- | Direct-poke encoder for Feature.
wirePokeFeature :: Int -> Ptr Word8 -> Feature -> IO (Ptr Word8)
wirePokeFeature version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (featureName msg))
  p2 <- W.pokeInt16BE p1 (featureMinSupportedVersion msg)
  p3 <- W.pokeInt16BE p2 (featureMaxSupportedVersion msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Feature.
wirePeekFeature :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Feature, Ptr Word8)
wirePeekFeature version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_minsupportedversion, p2) <- W.peekInt16BE p1 endPtr
  (f2_maxsupportedversion, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Feature { featureName = f0_name, featureMinSupportedVersion = f1_minsupportedversion, featureMaxSupportedVersion = f2_maxsupportedversion }, pTagsEnd)

-- | Worst-case wire size of a BrokerRegistrationRequest.
wireMaxSizeBrokerRegistrationRequest :: Int -> BrokerRegistrationRequest -> Int
wireMaxSizeBrokerRegistrationRequest _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (brokerRegistrationRequestClusterId msg))
  + 16
  + (5 + (case P.unKafkaArray (brokerRegistrationRequestListeners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListener _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (brokerRegistrationRequestFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFeature _version x ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (brokerRegistrationRequestRack msg))
  + 1
  + (5 + (case P.unKafkaArray (brokerRegistrationRequestLogDirs msg) of { P.NotNull v -> sum (fmap (\x -> 16 ) v); P.Null -> 0 }))
  + 8
  + 1

-- | Direct-poke encoder for BrokerRegistrationRequest.
wirePokeBrokerRegistrationRequest :: Int -> Ptr Word8 -> BrokerRegistrationRequest -> IO (Ptr Word8)
wirePokeBrokerRegistrationRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerRegistrationRequestBrokerId msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (brokerRegistrationRequestClusterId msg))
    p3 <- WP.pokeKafkaUuid p2 (brokerRegistrationRequestIncarnationId msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p3 (brokerRegistrationRequestListeners msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeature version p x) p4 (brokerRegistrationRequestFeatures msg)
    p6 <- WP.pokeCompactString p5 (P.toCompactString (brokerRegistrationRequestRack msg))
    WP.pokeEmptyTaggedFields p6
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerRegistrationRequestBrokerId msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (brokerRegistrationRequestClusterId msg))
    p3 <- WP.pokeKafkaUuid p2 (brokerRegistrationRequestIncarnationId msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p3 (brokerRegistrationRequestListeners msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeature version p x) p4 (brokerRegistrationRequestFeatures msg)
    p6 <- WP.pokeCompactString p5 (P.toCompactString (brokerRegistrationRequestRack msg))
    p7 <- W.pokeWord8 p6 (if (brokerRegistrationRequestIsMigratingZkBroker msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p7
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerRegistrationRequestBrokerId msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (brokerRegistrationRequestClusterId msg))
    p3 <- WP.pokeKafkaUuid p2 (brokerRegistrationRequestIncarnationId msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p3 (brokerRegistrationRequestListeners msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeature version p x) p4 (brokerRegistrationRequestFeatures msg)
    p6 <- WP.pokeCompactString p5 (P.toCompactString (brokerRegistrationRequestRack msg))
    p7 <- W.pokeWord8 p6 (if (brokerRegistrationRequestIsMigratingZkBroker msg) then 1 else 0)
    p8 <- WP.pokeVersionedArray version 0 WP.pokeKafkaUuid p7 (brokerRegistrationRequestLogDirs msg)
    WP.pokeEmptyTaggedFields p8
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerRegistrationRequestBrokerId msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (brokerRegistrationRequestClusterId msg))
    p3 <- WP.pokeKafkaUuid p2 (brokerRegistrationRequestIncarnationId msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p3 (brokerRegistrationRequestListeners msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeature version p x) p4 (brokerRegistrationRequestFeatures msg)
    p6 <- WP.pokeCompactString p5 (P.toCompactString (brokerRegistrationRequestRack msg))
    p7 <- W.pokeWord8 p6 (if (brokerRegistrationRequestIsMigratingZkBroker msg) then 1 else 0)
    p8 <- WP.pokeVersionedArray version 0 WP.pokeKafkaUuid p7 (brokerRegistrationRequestLogDirs msg)
    p9 <- W.pokeInt64BE p8 (brokerRegistrationRequestPreviousBrokerEpoch msg)
    WP.pokeEmptyTaggedFields p9
  | otherwise = error $ "wirePoke BrokerRegistrationRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for BrokerRegistrationRequest.
wirePeekBrokerRegistrationRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BrokerRegistrationRequest, Ptr Word8)
wirePeekBrokerRegistrationRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_clusterid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_incarnationid, p3) <- WP.peekKafkaUuid p2 endPtr
    (f3_listeners, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p3 endPtr
    (f4_features, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeature version _fp _basePtr p e) p4 endPtr
    (f5_rack, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (BrokerRegistrationRequest { brokerRegistrationRequestBrokerId = f0_brokerid, brokerRegistrationRequestClusterId = f1_clusterid, brokerRegistrationRequestIncarnationId = f2_incarnationid, brokerRegistrationRequestListeners = f3_listeners, brokerRegistrationRequestFeatures = f4_features, brokerRegistrationRequestRack = f5_rack, brokerRegistrationRequestIsMigratingZkBroker = False, brokerRegistrationRequestLogDirs = P.mkKafkaArray V.empty, brokerRegistrationRequestPreviousBrokerEpoch = 0 }, pTagsEnd)
  | version == 1 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_clusterid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_incarnationid, p3) <- WP.peekKafkaUuid p2 endPtr
    (f3_listeners, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p3 endPtr
    (f4_features, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeature version _fp _basePtr p e) p4 endPtr
    (f5_rack, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    (f6_ismigratingzkbroker, p7) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (BrokerRegistrationRequest { brokerRegistrationRequestBrokerId = f0_brokerid, brokerRegistrationRequestClusterId = f1_clusterid, brokerRegistrationRequestIncarnationId = f2_incarnationid, brokerRegistrationRequestListeners = f3_listeners, brokerRegistrationRequestFeatures = f4_features, brokerRegistrationRequestRack = f5_rack, brokerRegistrationRequestIsMigratingZkBroker = f6_ismigratingzkbroker, brokerRegistrationRequestLogDirs = P.mkKafkaArray V.empty, brokerRegistrationRequestPreviousBrokerEpoch = 0 }, pTagsEnd)
  | version == 2 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_clusterid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_incarnationid, p3) <- WP.peekKafkaUuid p2 endPtr
    (f3_listeners, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p3 endPtr
    (f4_features, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeature version _fp _basePtr p e) p4 endPtr
    (f5_rack, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    (f6_ismigratingzkbroker, p7) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p6 endPtr
    (f7_logdirs, p8) <- WP.peekVersionedArray version 0 WP.peekKafkaUuid p7 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (BrokerRegistrationRequest { brokerRegistrationRequestBrokerId = f0_brokerid, brokerRegistrationRequestClusterId = f1_clusterid, brokerRegistrationRequestIncarnationId = f2_incarnationid, brokerRegistrationRequestListeners = f3_listeners, brokerRegistrationRequestFeatures = f4_features, brokerRegistrationRequestRack = f5_rack, brokerRegistrationRequestIsMigratingZkBroker = f6_ismigratingzkbroker, brokerRegistrationRequestLogDirs = f7_logdirs, brokerRegistrationRequestPreviousBrokerEpoch = 0 }, pTagsEnd)
  | version >= 3 && version <= 4 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_clusterid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_incarnationid, p3) <- WP.peekKafkaUuid p2 endPtr
    (f3_listeners, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p3 endPtr
    (f4_features, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeature version _fp _basePtr p e) p4 endPtr
    (f5_rack, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    (f6_ismigratingzkbroker, p7) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p6 endPtr
    (f7_logdirs, p8) <- WP.peekVersionedArray version 0 WP.peekKafkaUuid p7 endPtr
    (f8_previousbrokerepoch, p9) <- W.peekInt64BE p8 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p9 endPtr
    pure (BrokerRegistrationRequest { brokerRegistrationRequestBrokerId = f0_brokerid, brokerRegistrationRequestClusterId = f1_clusterid, brokerRegistrationRequestIncarnationId = f2_incarnationid, brokerRegistrationRequestListeners = f3_listeners, brokerRegistrationRequestFeatures = f4_features, brokerRegistrationRequestRack = f5_rack, brokerRegistrationRequestIsMigratingZkBroker = f6_ismigratingzkbroker, brokerRegistrationRequestLogDirs = f7_logdirs, brokerRegistrationRequestPreviousBrokerEpoch = f8_previousbrokerepoch }, pTagsEnd)
  | otherwise = error $ "wirePeek BrokerRegistrationRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec BrokerRegistrationRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeBrokerRegistrationRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeBrokerRegistrationRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekBrokerRegistrationRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}