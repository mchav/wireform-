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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | Worst-case wire size of a Voter.
wireMaxSizeVoter :: Int -> Voter -> Int
wireMaxSizeVoter _version msg =
  0
  + 4
  + 16
  + 1

-- | Direct-poke encoder for Voter.
wirePokeVoter :: Int -> Ptr Word8 -> Voter -> IO (Ptr Word8)
wirePokeVoter version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (voterVoterId msg)
  p2 <- WP.pokeKafkaUuid p1 (voterVoterDirectoryId msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Voter.
wirePeekVoter :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Voter, Ptr Word8)
wirePeekVoter version _fp _basePtr p0 endPtr = do
  (f0_voterid, p1) <- W.peekInt32BE p0 endPtr
  (f1_voterdirectoryid, p2) <- WP.peekKafkaUuid p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Voter { voterVoterId = f0_voterid, voterVoterDirectoryId = f1_voterdirectoryid }, pTagsEnd)

-- | Worst-case wire size of a LeaderChangeMessage.
wireMaxSizeLeaderChangeMessage :: Int -> LeaderChangeMessage -> Int
wireMaxSizeLeaderChangeMessage _version msg =
  0
  + 2
  + 4
  + (5 + (case P.unKafkaArray (leaderChangeMessageVoters msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeVoter _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (leaderChangeMessageGrantingVoters msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeVoter _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for LeaderChangeMessage.
wirePokeLeaderChangeMessage :: Int -> Ptr Word8 -> LeaderChangeMessage -> IO (Ptr Word8)
wirePokeLeaderChangeMessage version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (leaderChangeMessageVersion msg)
    p2 <- W.pokeInt32BE p1 (leaderChangeMessageLeaderId msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeVoter version p x) p2 (leaderChangeMessageVoters msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeVoter version p x) p3 (leaderChangeMessageGrantingVoters msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke LeaderChangeMessage : unsupported version: " ++ show version

-- | Direct-poke decoder for LeaderChangeMessage.
wirePeekLeaderChangeMessage :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderChangeMessage, Ptr Word8)
wirePeekLeaderChangeMessage version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_version, p1) <- W.peekInt16BE p0 endPtr
    (f1_leaderid, p2) <- W.peekInt32BE p1 endPtr
    (f2_voters, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekVoter version _fp _basePtr p e) p2 endPtr
    (f3_grantingvoters, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekVoter version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (LeaderChangeMessage { leaderChangeMessageVersion = f0_version, leaderChangeMessageLeaderId = f1_leaderid, leaderChangeMessageVoters = f2_voters, leaderChangeMessageGrantingVoters = f3_grantingvoters }, pTagsEnd)
  | otherwise = error $ "wirePeek LeaderChangeMessage : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec LeaderChangeMessage where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaderChangeMessage (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaderChangeMessage (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaderChangeMessage (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}