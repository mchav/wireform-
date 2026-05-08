{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaderChangeMessage
Description : Kafka LeaderChangeMessage message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaderChangeMessage
  (
    LeaderChangeMessage(..),
    Voter(..),
    encodeLeaderChangeMessage,
    decodeLeaderChangeMessage,
    maxLeaderChangeMessageVersion
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


data Voter = Voter
  {

  -- | The ID of the voter.

  -- Versions: 0+
  voterVoterId :: !(Int32)
,

  -- | The directory id of the voter.

  -- Versions: 1+
  voterVoterDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


-- | Encode Voter with version-aware field handling.
encodeVoter :: MonadPut m => E.ApiVersion -> Voter -> m ()
encodeVoter version vmsg =
  do
    serialize (voterVoterId vmsg)
    when (version >= 1) $
      serialize (voterVoterDirectoryId vmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Voter with version-aware field handling.
decodeVoter :: MonadGet m => E.ApiVersion -> m Voter
decodeVoter version =
  do
    fieldvoterid <- deserialize
    fieldvoterdirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Voter
      {
      voterVoterId = fieldvoterid
      ,
      voterVoterDirectoryId = fieldvoterdirectoryid
      }




data LeaderChangeMessage = LeaderChangeMessage
  {

  -- | The version of the leader change message.

  -- Versions: 0+
  leaderChangeMessageVersion :: !(Int16)
,

  -- | The ID of the newly elected leader.

  -- Versions: 0+
  leaderChangeMessageLeaderId :: !(Int32)
,

  -- | The set of voters in the quorum for this epoch.

  -- Versions: 0+
  leaderChangeMessageVoters :: !(KafkaArray (Voter))
,

  -- | The voters who voted for the leader at the time of election.

  -- Versions: 0+
  leaderChangeMessageGrantingVoters :: !(KafkaArray (Voter))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderChangeMessage.
maxLeaderChangeMessageVersion :: Int16
maxLeaderChangeMessageVersion = 1

-- | Encode LeaderChangeMessage with the given API version.
encodeLeaderChangeMessage :: MonadPut m => E.ApiVersion -> LeaderChangeMessage -> m ()
encodeLeaderChangeMessage version msg
  | version >= 0 && version <= 1 =
    do
      serialize (leaderChangeMessageVersion msg)
      serialize (leaderChangeMessageLeaderId msg)
      E.encodeVersionedArray version 0 encodeVoter (case P.unKafkaArray (leaderChangeMessageVoters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeVoter (case P.unKafkaArray (leaderChangeMessageGrantingVoters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode LeaderChangeMessage with the given API version.
decodeLeaderChangeMessage :: MonadGet m => E.ApiVersion -> m LeaderChangeMessage
decodeLeaderChangeMessage version
  | version >= 0 && version <= 1 =
    do
      fieldversion <- deserialize
      fieldleaderid <- deserialize
      fieldvoters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeVoter
      fieldgrantingvoters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeVoter
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure LeaderChangeMessage
        {
        leaderChangeMessageVersion = fieldversion
        ,
        leaderChangeMessageLeaderId = fieldleaderid
        ,
        leaderChangeMessageVoters = fieldvoters
        ,
        leaderChangeMessageGrantingVoters = fieldgrantingvoters
        }
  | otherwise = fail $ "Unsupported version: " ++ show version