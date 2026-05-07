{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.VotersRecord
Description : Kafka VotersRecord message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.VotersRecord
  (
    VotersRecord(..),
    Voter(..),
    Endpoint(..),
    KRaftVersionFeature(..),
    encodeVotersRecord,
    decodeVotersRecord,
    maxVotersRecordVersion
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


-- | The endpoint that can be used to communicate with the voter.
data Endpoint = Endpoint
  {

  -- | The name of the endpoint.

  -- Versions: 0+
  endpointName :: !(KafkaString)
,

  -- | The hostname.

  -- Versions: 0+
  endpointHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 0+
  endpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode Endpoint with version-aware field handling.
encodeEndpoint :: MonadPut m => E.ApiVersion -> Endpoint -> m ()
encodeEndpoint version emsg =
  do
    if version >= 0 then serialize (toCompactString (endpointName emsg)) else serialize (endpointName emsg)
    if version >= 0 then serialize (toCompactString (endpointHost emsg)) else serialize (endpointHost emsg)
    serialize (endpointPort emsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Endpoint with version-aware field handling.
decodeEndpoint :: MonadGet m => E.ApiVersion -> m Endpoint
decodeEndpoint version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Endpoint
      {
      endpointName = fieldname
      ,
      endpointHost = fieldhost
      ,
      endpointPort = fieldport
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


-- | The set of voters in the quorum for this epoch.
data Voter = Voter
  {

  -- | The replica id of the voter in the topic partition.

  -- Versions: 0+
  voterVoterId :: !(Int32)
,

  -- | The directory id of the voter in the topic partition.

  -- Versions: 0+
  voterVoterDirectoryId :: !(KafkaUuid)
,

  -- | The endpoint that can be used to communicate with the voter.

  -- Versions: 0+
  voterEndpoints :: !(KafkaArray (Endpoint))
,

  -- | The range of versions of the protocol that the replica supports.

  -- Versions: 0+
  voterKRaftVersionFeature :: !(KRaftVersionFeature)

  }
  deriving (Eq, Show, Generic)


-- | Encode Voter with version-aware field handling.
encodeVoter :: MonadPut m => E.ApiVersion -> Voter -> m ()
encodeVoter version vmsg =
  do
    serialize (voterVoterId vmsg)
    serialize (voterVoterDirectoryId vmsg)
    E.encodeVersionedArray version 0 encodeEndpoint (case P.unKafkaArray (voterEndpoints vmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    encodeKRaftVersionFeature version (voterKRaftVersionFeature vmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Voter with version-aware field handling.
decodeVoter :: MonadGet m => E.ApiVersion -> m Voter
decodeVoter version =
  do
    fieldvoterid <- deserialize
    fieldvoterdirectoryid <- deserialize
    fieldendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeEndpoint
    fieldkraftversionfeature <- decodeKRaftVersionFeature version
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Voter
      {
      voterVoterId = fieldvoterid
      ,
      voterVoterDirectoryId = fieldvoterdirectoryid
      ,
      voterEndpoints = fieldendpoints
      ,
      voterKRaftVersionFeature = fieldkraftversionfeature
      }



data VotersRecord = VotersRecord
  {

  -- | The version of the voters record.

  -- Versions: 0+
  votersRecordVersion :: !(Int16)
,

  -- | The set of voters in the quorum for this epoch.

  -- Versions: 0+
  votersRecordVoters :: !(KafkaArray (Voter))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for VotersRecord.
maxVotersRecordVersion :: Int16
maxVotersRecordVersion = 0

-- | Encode VotersRecord with the given API version.
encodeVotersRecord :: MonadPut m => E.ApiVersion -> VotersRecord -> m ()
encodeVotersRecord version msg
  | version == 0 =
    do
      serialize (votersRecordVersion msg)
      E.encodeVersionedArray version 0 encodeVoter (case P.unKafkaArray (votersRecordVoters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode VotersRecord with the given API version.
decodeVotersRecord :: MonadGet m => E.ApiVersion -> m VotersRecord
decodeVotersRecord version
  | version == 0 =
    do
      fieldversion <- deserialize
      fieldvoters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeVoter
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure VotersRecord
        {
        votersRecordVersion = fieldversion
        ,
        votersRecordVoters = fieldvoters
        }
  | otherwise = fail $ "Unsupported version: " ++ show version