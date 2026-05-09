{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateRaftVoterRequest
Description : Kafka UpdateRaftVoterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 82.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateRaftVoterRequest
  (
    UpdateRaftVoterRequest(..),
    Listener(..),
    KRaftVersionFeature(..),
    encodeUpdateRaftVoterRequest,
    decodeUpdateRaftVoterRequest,
    maxUpdateRaftVoterRequestVersion
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


-- | The endpoint that can be used to communicate with the leader.
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

  }
  deriving (Eq, Show, Generic)


-- | Encode Listener with version-aware field handling.
encodeListener :: MonadPut m => E.ApiVersion -> Listener -> m ()
encodeListener version lmsg =
  do
    if version >= 0 then serialize (toCompactString (listenerName lmsg)) else serialize (listenerName lmsg)
    if version >= 0 then serialize (toCompactString (listenerHost lmsg)) else serialize (listenerHost lmsg)
    serialize (listenerPort lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Listener with version-aware field handling.
decodeListener :: MonadGet m => E.ApiVersion -> m Listener
decodeListener version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Listener
      {
      listenerName = fieldname
      ,
      listenerHost = fieldhost
      ,
      listenerPort = fieldport
      }


-- | The range of versions of the protocol that the replica supports.
data KRaftVersionFeature = KRaftVersionFeature
  {

  -- | The minimum supported KRaft protocol version.

  -- Versions: 0+
  kRaftVersionFeatureMinSupportedVersion :: !(Int16)
,

  -- | The maximum supported KRaft protocol version.

  -- Versions: 0+
  kRaftVersionFeatureMaxSupportedVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode KRaftVersionFeature with version-aware field handling.
encodeKRaftVersionFeature :: MonadPut m => E.ApiVersion -> KRaftVersionFeature -> m ()
encodeKRaftVersionFeature version kmsg =
  do
    serialize (kRaftVersionFeatureMinSupportedVersion kmsg)
    serialize (kRaftVersionFeatureMaxSupportedVersion kmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode KRaftVersionFeature with version-aware field handling.
decodeKRaftVersionFeature :: MonadGet m => E.ApiVersion -> m KRaftVersionFeature
decodeKRaftVersionFeature version =
  do
    fieldminsupportedversion <- deserialize
    fieldmaxsupportedversion <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure KRaftVersionFeature
      {
      kRaftVersionFeatureMinSupportedVersion = fieldminsupportedversion
      ,
      kRaftVersionFeatureMaxSupportedVersion = fieldmaxsupportedversion
      }



data UpdateRaftVoterRequest = UpdateRaftVoterRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  updateRaftVoterRequestClusterId :: !(KafkaString)
,

  -- | The current leader epoch of the partition, -1 for unknown leader epoch.

  -- Versions: 0+
  updateRaftVoterRequestCurrentLeaderEpoch :: !(Int32)
,

  -- | The replica id of the voter getting updated in the topic partition.

  -- Versions: 0+
  updateRaftVoterRequestVoterId :: !(Int32)
,

  -- | The directory id of the voter getting updated in the topic partition.

  -- Versions: 0+
  updateRaftVoterRequestVoterDirectoryId :: !(KafkaUuid)
,

  -- | The endpoint that can be used to communicate with the leader.

  -- Versions: 0+
  updateRaftVoterRequestListeners :: !(KafkaArray (Listener))
,

  -- | The range of versions of the protocol that the replica supports.

  -- Versions: 0+
  updateRaftVoterRequestKRaftVersionFeature :: !(KRaftVersionFeature)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateRaftVoterRequest.
maxUpdateRaftVoterRequestVersion :: Int16
maxUpdateRaftVoterRequestVersion = 0

-- | Encode UpdateRaftVoterRequest with the given API version.
encodeUpdateRaftVoterRequest :: MonadPut m => E.ApiVersion -> UpdateRaftVoterRequest -> m ()
encodeUpdateRaftVoterRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (updateRaftVoterRequestClusterId msg))
      serialize (updateRaftVoterRequestCurrentLeaderEpoch msg)
      serialize (updateRaftVoterRequestVoterId msg)
      serialize (updateRaftVoterRequestVoterDirectoryId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (updateRaftVoterRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      encodeKRaftVersionFeature version (updateRaftVoterRequestKRaftVersionFeature msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UpdateRaftVoterRequest with the given API version.
decodeUpdateRaftVoterRequest :: MonadGet m => E.ApiVersion -> m UpdateRaftVoterRequest
decodeUpdateRaftVoterRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldcurrentleaderepoch <- deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldkraftversionfeature <- decodeKRaftVersionFeature version
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateRaftVoterRequest
        {
        updateRaftVoterRequestClusterId = fieldclusterid
        ,
        updateRaftVoterRequestCurrentLeaderEpoch = fieldcurrentleaderepoch
        ,
        updateRaftVoterRequestVoterId = fieldvoterid
        ,
        updateRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        ,
        updateRaftVoterRequestListeners = fieldlisteners
        ,
        updateRaftVoterRequestKRaftVersionFeature = fieldkraftversionfeature
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec UpdateRaftVoterRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
