{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddRaftVoterRequest
Description : Kafka AddRaftVoterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 80.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddRaftVoterRequest
  (
    AddRaftVoterRequest(..),
    Listener(..),
    encodeAddRaftVoterRequest,
    decodeAddRaftVoterRequest,
    maxAddRaftVoterRequestVersion
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


-- | The endpoints that can be used to communicate with the voter.
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



data AddRaftVoterRequest = AddRaftVoterRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  addRaftVoterRequestClusterId :: !(KafkaString)
,

  -- | The maximum time to wait for the request to complete before returning.

  -- Versions: 0+
  addRaftVoterRequestTimeoutMs :: !(Int32)
,

  -- | The replica id of the voter getting added to the topic partition.

  -- Versions: 0+
  addRaftVoterRequestVoterId :: !(Int32)
,

  -- | The directory id of the voter getting added to the topic partition.

  -- Versions: 0+
  addRaftVoterRequestVoterDirectoryId :: !(KafkaUuid)
,

  -- | The endpoints that can be used to communicate with the voter.

  -- Versions: 0+
  addRaftVoterRequestListeners :: !(KafkaArray (Listener))
,

  -- | When true, return a response after the new voter set is committed. Otherwise, return after the leade

  -- Versions: 1+
  addRaftVoterRequestAckWhenCommitted :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddRaftVoterRequest.
maxAddRaftVoterRequestVersion :: Int16
maxAddRaftVoterRequestVersion = 1

-- | Encode AddRaftVoterRequest with the given API version.
encodeAddRaftVoterRequest :: MonadPut m => E.ApiVersion -> AddRaftVoterRequest -> m ()
encodeAddRaftVoterRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (addRaftVoterRequestClusterId msg))
      serialize (addRaftVoterRequestTimeoutMs msg)
      serialize (addRaftVoterRequestVoterId msg)
      serialize (addRaftVoterRequestVoterDirectoryId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (addRaftVoterRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (toCompactString (addRaftVoterRequestClusterId msg))
      serialize (addRaftVoterRequestTimeoutMs msg)
      serialize (addRaftVoterRequestVoterId msg)
      serialize (addRaftVoterRequestVoterDirectoryId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (addRaftVoterRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (addRaftVoterRequestAckWhenCommitted msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddRaftVoterRequest with the given API version.
decodeAddRaftVoterRequest :: MonadGet m => E.ApiVersion -> m AddRaftVoterRequest
decodeAddRaftVoterRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtimeoutms <- deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddRaftVoterRequest
        {
        addRaftVoterRequestClusterId = fieldclusterid
        ,
        addRaftVoterRequestTimeoutMs = fieldtimeoutms
        ,
        addRaftVoterRequestVoterId = fieldvoterid
        ,
        addRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        ,
        addRaftVoterRequestListeners = fieldlisteners
        ,
        addRaftVoterRequestAckWhenCommitted = True
        }

  | version == 1 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtimeoutms <- deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldackwhencommitted <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddRaftVoterRequest
        {
        addRaftVoterRequestClusterId = fieldclusterid
        ,
        addRaftVoterRequestTimeoutMs = fieldtimeoutms
        ,
        addRaftVoterRequestVoterId = fieldvoterid
        ,
        addRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        ,
        addRaftVoterRequestListeners = fieldlisteners
        ,
        addRaftVoterRequestAckWhenCommitted = fieldackwhencommitted
        }
  | otherwise = fail $ "Unsupported version: " ++ show version