{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControllerRegistrationRequest
Description : Kafka ControllerRegistrationRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 70.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControllerRegistrationRequest
  (
    ControllerRegistrationRequest(..),
    Listener(..),
    Feature(..),
    maxControllerRegistrationRequestVersion
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


-- | The listeners of this controller.
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

-- | The features on this controller.
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


data ControllerRegistrationRequest = ControllerRegistrationRequest
  {

  -- | The ID of the controller to register.

  -- Versions: 0+
  controllerRegistrationRequestControllerId :: !(Int32)
,

  -- | The controller incarnation ID, which is unique to each process run.

  -- Versions: 0+
  controllerRegistrationRequestIncarnationId :: !(KafkaUuid)
,

  -- | Set if the required configurations for ZK migration are present.

  -- Versions: 0+
  controllerRegistrationRequestZkMigrationReady :: !(Bool)
,

  -- | The listeners of this controller.

  -- Versions: 0+
  controllerRegistrationRequestListeners :: !(KafkaArray (Listener))
,

  -- | The features on this controller.

  -- Versions: 0+
  controllerRegistrationRequestFeatures :: !(KafkaArray (Feature))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControllerRegistrationRequest.
maxControllerRegistrationRequestVersion :: Int16
maxControllerRegistrationRequestVersion = 0

-- | KafkaMessage instance for ControllerRegistrationRequest.
instance KafkaMessage ControllerRegistrationRequest where
  messageApiKey = 70
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a Listener.
wireMaxSizeListener :: Int -> Listener -> Int
wireMaxSizeListener _version msg =
  0
  + WP.dualStringMaxSize (listenerName msg)
  + WP.dualStringMaxSize (listenerHost msg)
  + 2
  + 2
  + 1

-- | Direct-poke encoder for Listener.
wirePokeListener :: Int -> Ptr Word8 -> Listener -> IO (Ptr Word8)
wirePokeListener version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (listenerName msg)) else WP.pokeKafkaString p0 (listenerName msg))
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (listenerHost msg)) else WP.pokeKafkaString p1 (listenerHost msg))
  p3 <- W.pokeWord16BE p2 (listenerPort msg)
  p4 <- W.pokeInt16BE p3 (listenerSecurityProtocol msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_host, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  (f3_securityprotocol, p4) <- W.peekInt16BE p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port, listenerSecurityProtocol = f3_securityprotocol }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListener :: Listener
defaultListener = Listener { listenerName = P.KafkaString Null, listenerHost = P.KafkaString Null, listenerPort = 0, listenerSecurityProtocol = 0 }

-- | Worst-case wire size of a Feature.
wireMaxSizeFeature :: Int -> Feature -> Int
wireMaxSizeFeature _version msg =
  0
  + WP.dualStringMaxSize (featureName msg)
  + 2
  + 2
  + 1

-- | Direct-poke encoder for Feature.
wirePokeFeature :: Int -> Ptr Word8 -> Feature -> IO (Ptr Word8)
wirePokeFeature version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (featureName msg)) else WP.pokeKafkaString p0 (featureName msg))
  p2 <- W.pokeInt16BE p1 (featureMinSupportedVersion msg)
  p3 <- W.pokeInt16BE p2 (featureMaxSupportedVersion msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Feature.
wirePeekFeature :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Feature, Ptr Word8)
wirePeekFeature version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_minsupportedversion, p2) <- W.peekInt16BE p1 endPtr
  (f2_maxsupportedversion, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Feature { featureName = f0_name, featureMinSupportedVersion = f1_minsupportedversion, featureMaxSupportedVersion = f2_maxsupportedversion }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultFeature :: Feature
defaultFeature = Feature { featureName = P.KafkaString Null, featureMinSupportedVersion = 0, featureMaxSupportedVersion = 0 }

-- | Worst-case wire size of a ControllerRegistrationRequest.
wireMaxSizeControllerRegistrationRequest :: Int -> ControllerRegistrationRequest -> Int
wireMaxSizeControllerRegistrationRequest _version msg =
  0
  + 4
  + 16
  + 1
  + (5 + (case P.unKafkaArray (controllerRegistrationRequestListeners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListener _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (controllerRegistrationRequestFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFeature _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ControllerRegistrationRequest.
wirePokeControllerRegistrationRequest :: Int -> Ptr Word8 -> ControllerRegistrationRequest -> IO (Ptr Word8)
wirePokeControllerRegistrationRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (controllerRegistrationRequestControllerId msg)
    p2 <- WP.pokeKafkaUuid p1 (controllerRegistrationRequestIncarnationId msg)
    p3 <- W.pokeWord8 p2 (if (controllerRegistrationRequestZkMigrationReady msg) then 1 else 0)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p3 (controllerRegistrationRequestListeners msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeature version p x) p4 (controllerRegistrationRequestFeatures msg)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke ControllerRegistrationRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ControllerRegistrationRequest.
wirePeekControllerRegistrationRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ControllerRegistrationRequest, Ptr Word8)
wirePeekControllerRegistrationRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_controllerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_incarnationid, p2) <- WP.peekKafkaUuid p1 endPtr
    (f2_zkmigrationready, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    (f3_listeners, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p3 endPtr
    (f4_features, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeature version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (ControllerRegistrationRequest { controllerRegistrationRequestControllerId = f0_controllerid, controllerRegistrationRequestIncarnationId = f1_incarnationid, controllerRegistrationRequestZkMigrationReady = f2_zkmigrationready, controllerRegistrationRequestListeners = f3_listeners, controllerRegistrationRequestFeatures = f4_features }, pTagsEnd)
  | otherwise = error $ "wirePeek ControllerRegistrationRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ControllerRegistrationRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeControllerRegistrationRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeControllerRegistrationRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekControllerRegistrationRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}