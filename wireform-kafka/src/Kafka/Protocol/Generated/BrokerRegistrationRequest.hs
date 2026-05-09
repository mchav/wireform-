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
    encodeBrokerRegistrationRequest,
    decodeBrokerRegistrationRequest,
    maxBrokerRegistrationRequestVersion
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

-- | Encode BrokerRegistrationRequest with the given API version.
encodeBrokerRegistrationRequest :: MonadPut m => E.ApiVersion -> BrokerRegistrationRequest -> m ()
encodeBrokerRegistrationRequest version msg
  | version == 0 =
    do
      serialize (brokerRegistrationRequestBrokerId msg)
      serialize (toCompactString (brokerRegistrationRequestClusterId msg))
      serialize (brokerRegistrationRequestIncarnationId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (brokerRegistrationRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeFeature (case P.unKafkaArray (brokerRegistrationRequestFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (brokerRegistrationRequestRack msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (brokerRegistrationRequestBrokerId msg)
      serialize (toCompactString (brokerRegistrationRequestClusterId msg))
      serialize (brokerRegistrationRequestIncarnationId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (brokerRegistrationRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeFeature (case P.unKafkaArray (brokerRegistrationRequestFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (brokerRegistrationRequestRack msg))
      serialize (brokerRegistrationRequestIsMigratingZkBroker msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      serialize (brokerRegistrationRequestBrokerId msg)
      serialize (toCompactString (brokerRegistrationRequestClusterId msg))
      serialize (brokerRegistrationRequestIncarnationId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (brokerRegistrationRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeFeature (case P.unKafkaArray (brokerRegistrationRequestFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (brokerRegistrationRequestRack msg))
      serialize (brokerRegistrationRequestIsMigratingZkBroker msg)
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (brokerRegistrationRequestLogDirs msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "uuid"
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 4 =
    do
      serialize (brokerRegistrationRequestBrokerId msg)
      serialize (toCompactString (brokerRegistrationRequestClusterId msg))
      serialize (brokerRegistrationRequestIncarnationId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (brokerRegistrationRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeFeature (case P.unKafkaArray (brokerRegistrationRequestFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (brokerRegistrationRequestRack msg))
      serialize (brokerRegistrationRequestIsMigratingZkBroker msg)
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (brokerRegistrationRequestLogDirs msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "uuid"
      serialize (brokerRegistrationRequestPreviousBrokerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BrokerRegistrationRequest with the given API version.
decodeBrokerRegistrationRequest :: MonadGet m => E.ApiVersion -> m BrokerRegistrationRequest
decodeBrokerRegistrationRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldincarnationid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldfeatures <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeature
      fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerRegistrationRequest
        {
        brokerRegistrationRequestBrokerId = fieldbrokerid
        ,
        brokerRegistrationRequestClusterId = fieldclusterid
        ,
        brokerRegistrationRequestIncarnationId = fieldincarnationid
        ,
        brokerRegistrationRequestListeners = fieldlisteners
        ,
        brokerRegistrationRequestFeatures = fieldfeatures
        ,
        brokerRegistrationRequestRack = fieldrack
        ,
        brokerRegistrationRequestIsMigratingZkBroker = False
        ,
        brokerRegistrationRequestLogDirs = P.mkKafkaArray V.empty
        ,
        brokerRegistrationRequestPreviousBrokerEpoch = (-1)
        }

  | version == 1 =
    do
      fieldbrokerid <- deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldincarnationid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldfeatures <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeature
      fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldismigratingzkbroker <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerRegistrationRequest
        {
        brokerRegistrationRequestBrokerId = fieldbrokerid
        ,
        brokerRegistrationRequestClusterId = fieldclusterid
        ,
        brokerRegistrationRequestIncarnationId = fieldincarnationid
        ,
        brokerRegistrationRequestListeners = fieldlisteners
        ,
        brokerRegistrationRequestFeatures = fieldfeatures
        ,
        brokerRegistrationRequestRack = fieldrack
        ,
        brokerRegistrationRequestIsMigratingZkBroker = fieldismigratingzkbroker
        ,
        brokerRegistrationRequestLogDirs = P.mkKafkaArray V.empty
        ,
        brokerRegistrationRequestPreviousBrokerEpoch = (-1)
        }

  | version == 2 =
    do
      fieldbrokerid <- deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldincarnationid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldfeatures <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeature
      fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldismigratingzkbroker <- deserialize
      fieldlogdirs <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerRegistrationRequest
        {
        brokerRegistrationRequestBrokerId = fieldbrokerid
        ,
        brokerRegistrationRequestClusterId = fieldclusterid
        ,
        brokerRegistrationRequestIncarnationId = fieldincarnationid
        ,
        brokerRegistrationRequestListeners = fieldlisteners
        ,
        brokerRegistrationRequestFeatures = fieldfeatures
        ,
        brokerRegistrationRequestRack = fieldrack
        ,
        brokerRegistrationRequestIsMigratingZkBroker = fieldismigratingzkbroker
        ,
        brokerRegistrationRequestLogDirs = fieldlogdirs
        ,
        brokerRegistrationRequestPreviousBrokerEpoch = (-1)
        }

  | version >= 3 && version <= 4 =
    do
      fieldbrokerid <- deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldincarnationid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldfeatures <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeature
      fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldismigratingzkbroker <- deserialize
      fieldlogdirs <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fieldpreviousbrokerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BrokerRegistrationRequest
        {
        brokerRegistrationRequestBrokerId = fieldbrokerid
        ,
        brokerRegistrationRequestClusterId = fieldclusterid
        ,
        brokerRegistrationRequestIncarnationId = fieldincarnationid
        ,
        brokerRegistrationRequestListeners = fieldlisteners
        ,
        brokerRegistrationRequestFeatures = fieldfeatures
        ,
        brokerRegistrationRequestRack = fieldrack
        ,
        brokerRegistrationRequestIsMigratingZkBroker = fieldismigratingzkbroker
        ,
        brokerRegistrationRequestLogDirs = fieldlogdirs
        ,
        brokerRegistrationRequestPreviousBrokerEpoch = fieldpreviousbrokerepoch
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec BrokerRegistrationRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
