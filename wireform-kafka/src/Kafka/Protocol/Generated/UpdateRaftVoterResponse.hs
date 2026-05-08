{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateRaftVoterResponse
Description : Kafka UpdateRaftVoterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 82.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateRaftVoterResponse
  (
    UpdateRaftVoterResponse(..),
    CurrentLeader(..),
    encodeUpdateRaftVoterResponse,
    decodeUpdateRaftVoterResponse,
    maxUpdateRaftVoterResponseVersion
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


-- | Details of the current Raft cluster leader.
data CurrentLeader = CurrentLeader
  {

  -- | The replica id of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  currentLeaderLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  currentLeaderLeaderEpoch :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 0+
  currentLeaderHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 0+
  currentLeaderPort :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode CurrentLeader with version-aware field handling.
encodeCurrentLeader :: MonadPut m => E.ApiVersion -> CurrentLeader -> m ()
encodeCurrentLeader version cmsg =
  do
    serialize (currentLeaderLeaderId cmsg)
    serialize (currentLeaderLeaderEpoch cmsg)
    if version >= 0 then serialize (toCompactString (currentLeaderHost cmsg)) else serialize (currentLeaderHost cmsg)
    serialize (currentLeaderPort cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CurrentLeader with version-aware field handling.
decodeCurrentLeader :: MonadGet m => E.ApiVersion -> m CurrentLeader
decodeCurrentLeader version =
  do
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CurrentLeader
      {
      currentLeaderLeaderId = fieldleaderid
      ,
      currentLeaderLeaderEpoch = fieldleaderepoch
      ,
      currentLeaderHost = fieldhost
      ,
      currentLeaderPort = fieldport
      }



data UpdateRaftVoterResponse = UpdateRaftVoterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  updateRaftVoterResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  updateRaftVoterResponseErrorCode :: !(Int16)
,

  -- | Details of the current Raft cluster leader.

  -- Versions: 0+
  updateRaftVoterResponseCurrentLeader :: !(CurrentLeader)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateRaftVoterResponse.
maxUpdateRaftVoterResponseVersion :: Int16
maxUpdateRaftVoterResponseVersion = 0

-- | Encode UpdateRaftVoterResponse with the given API version.
encodeUpdateRaftVoterResponse :: MonadPut m => E.ApiVersion -> UpdateRaftVoterResponse -> m ()
encodeUpdateRaftVoterResponse version msg
  | version == 0 =
    do
      serialize (updateRaftVoterResponseThrottleTimeMs msg)
      serialize (updateRaftVoterResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UpdateRaftVoterResponse with the given API version.
decodeUpdateRaftVoterResponse :: MonadGet m => E.ApiVersion -> m UpdateRaftVoterResponse
decodeUpdateRaftVoterResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateRaftVoterResponse
        {
        updateRaftVoterResponseThrottleTimeMs = fieldthrottletimems
        ,
        updateRaftVoterResponseErrorCode = fielderrorcode
        ,
        updateRaftVoterResponseCurrentLeader = CurrentLeader { currentLeaderLeaderId = (-1), currentLeaderLeaderEpoch = (-1), currentLeaderHost = P.KafkaString Null, currentLeaderPort = 0 }
        }
  | otherwise = fail $ "Unsupported version: " ++ show version