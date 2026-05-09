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
    encodeControllerRegistrationRequest,
    decodeControllerRegistrationRequest,
    maxControllerRegistrationRequestVersion
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


-- | Encode Listener with version-aware field handling.
encodeListener :: MonadPut m => E.ApiVersion -> Listener -> m ()
encodeListener version lmsg =
  do
    if version >= 0 then serialize (toCompactString (listenerName lmsg)) else serialize (listenerName lmsg)
    if version >= 0 then serialize (toCompactString (listenerHost lmsg)) else serialize (listenerHost lmsg)
    serialize (listenerPort lmsg)
    serialize (listenerSecurityProtocol lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Listener with version-aware field handling.
decodeListener :: MonadGet m => E.ApiVersion -> m Listener
decodeListener version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldsecurityprotocol <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Listener
      {
      listenerName = fieldname
      ,
      listenerHost = fieldhost
      ,
      listenerPort = fieldport
      ,
      listenerSecurityProtocol = fieldsecurityprotocol
      }


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


-- | Encode Feature with version-aware field handling.
encodeFeature :: MonadPut m => E.ApiVersion -> Feature -> m ()
encodeFeature version fmsg =
  do
    if version >= 0 then serialize (toCompactString (featureName fmsg)) else serialize (featureName fmsg)
    serialize (featureMinSupportedVersion fmsg)
    serialize (featureMaxSupportedVersion fmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Feature with version-aware field handling.
decodeFeature :: MonadGet m => E.ApiVersion -> m Feature
decodeFeature version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldminsupportedversion <- deserialize
    fieldmaxsupportedversion <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Feature
      {
      featureName = fieldname
      ,
      featureMinSupportedVersion = fieldminsupportedversion
      ,
      featureMaxSupportedVersion = fieldmaxsupportedversion
      }



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

-- | Encode ControllerRegistrationRequest with the given API version.
encodeControllerRegistrationRequest :: MonadPut m => E.ApiVersion -> ControllerRegistrationRequest -> m ()
encodeControllerRegistrationRequest version msg
  | version == 0 =
    do
      serialize (controllerRegistrationRequestControllerId msg)
      serialize (controllerRegistrationRequestIncarnationId msg)
      serialize (controllerRegistrationRequestZkMigrationReady msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (controllerRegistrationRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeFeature (case P.unKafkaArray (controllerRegistrationRequestFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ControllerRegistrationRequest with the given API version.
decodeControllerRegistrationRequest :: MonadGet m => E.ApiVersion -> m ControllerRegistrationRequest
decodeControllerRegistrationRequest version
  | version == 0 =
    do
      fieldcontrollerid <- deserialize
      fieldincarnationid <- deserialize
      fieldzkmigrationready <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldfeatures <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeature
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ControllerRegistrationRequest
        {
        controllerRegistrationRequestControllerId = fieldcontrollerid
        ,
        controllerRegistrationRequestIncarnationId = fieldincarnationid
        ,
        controllerRegistrationRequestZkMigrationReady = fieldzkmigrationready
        ,
        controllerRegistrationRequestListeners = fieldlisteners
        ,
        controllerRegistrationRequestFeatures = fieldfeatures
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeControllerRegistrationRequest' / 'decodeControllerRegistrationRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ControllerRegistrationRequest where
  wireCodec = Just (WC.serialShimCodec encodeControllerRegistrationRequest decodeControllerRegistrationRequest)
  {-# INLINE wireCodec #-}
