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
    maxVotersRecordVersion
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



-- | Worst-case wire size of a Endpoint.
wireMaxSizeEndpoint :: Int -> Endpoint -> Int
wireMaxSizeEndpoint _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (endpointName msg))
  + WP.compactStringMaxSize (P.toCompactString (endpointHost msg))
  + 2
  + 1

-- | Direct-poke encoder for Endpoint.
wirePokeEndpoint :: Int -> Ptr Word8 -> Endpoint -> IO (Ptr Word8)
wirePokeEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (endpointName msg)) else WP.pokeKafkaString p0 (endpointName msg))
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (endpointHost msg)) else WP.pokeKafkaString p1 (endpointHost msg))
  p3 <- W.pokeWord16BE p2 (endpointPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Endpoint.
wirePeekEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Endpoint, Ptr Word8)
wirePeekEndpoint version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_host, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Endpoint { endpointName = f0_name, endpointHost = f1_host, endpointPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint { endpointName = P.KafkaString Null, endpointHost = P.KafkaString Null, endpointPort = 0 }

-- | Worst-case wire size of a KRaftVersionFeature.
wireMaxSizeKRaftVersionFeature :: Int -> KRaftVersionFeature -> Int
wireMaxSizeKRaftVersionFeature _version msg =
  0
  + 2
  + 2
  + 1

-- | Direct-poke encoder for KRaftVersionFeature.
wirePokeKRaftVersionFeature :: Int -> Ptr Word8 -> KRaftVersionFeature -> IO (Ptr Word8)
wirePokeKRaftVersionFeature version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (kRaftVersionFeatureMinSupportedVersion msg)
  p2 <- W.pokeInt16BE p1 (kRaftVersionFeatureMaxSupportedVersion msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for KRaftVersionFeature.
wirePeekKRaftVersionFeature :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (KRaftVersionFeature, Ptr Word8)
wirePeekKRaftVersionFeature version _fp _basePtr p0 endPtr = do
  (f0_minsupportedversion, p1) <- W.peekInt16BE p0 endPtr
  (f1_maxsupportedversion, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (KRaftVersionFeature { kRaftVersionFeatureMinSupportedVersion = f0_minsupportedversion, kRaftVersionFeatureMaxSupportedVersion = f1_maxsupportedversion }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultKRaftVersionFeature :: KRaftVersionFeature
defaultKRaftVersionFeature = KRaftVersionFeature { kRaftVersionFeatureMinSupportedVersion = 0, kRaftVersionFeatureMaxSupportedVersion = 0 }

-- | Worst-case wire size of a Voter.
wireMaxSizeVoter :: Int -> Voter -> Int
wireMaxSizeVoter _version msg =
  0
  + 4
  + 16
  + (5 + (case P.unKafkaArray (voterEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeEndpoint _version x ) v); P.Null -> 0 }))
  + wireMaxSizeKRaftVersionFeature _version (voterKRaftVersionFeature msg)
  + 1

-- | Direct-poke encoder for Voter.
wirePokeVoter :: Int -> Ptr Word8 -> Voter -> IO (Ptr Word8)
wirePokeVoter version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (voterVoterId msg)
  p2 <- WP.pokeKafkaUuid p1 (voterVoterDirectoryId msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeEndpoint version p x) p2 (voterEndpoints msg)
  p4 <- wirePokeKRaftVersionFeature version p3 (voterKRaftVersionFeature msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for Voter.
wirePeekVoter :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Voter, Ptr Word8)
wirePeekVoter version _fp _basePtr p0 endPtr = do
  (f0_voterid, p1) <- W.peekInt32BE p0 endPtr
  (f1_voterdirectoryid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_endpoints, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekEndpoint version _fp _basePtr p e) p2 endPtr
  (f3_kraftversionfeature, p4) <- wirePeekKRaftVersionFeature version _fp _basePtr p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (Voter { voterVoterId = f0_voterid, voterVoterDirectoryId = f1_voterdirectoryid, voterEndpoints = f2_endpoints, voterKRaftVersionFeature = f3_kraftversionfeature }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultVoter :: Voter
defaultVoter = Voter { voterVoterId = 0, voterVoterDirectoryId = P.nullUuid, voterEndpoints = P.mkKafkaArray V.empty, voterKRaftVersionFeature = defaultKRaftVersionFeature }

-- | Worst-case wire size of a VotersRecord.
wireMaxSizeVotersRecord :: Int -> VotersRecord -> Int
wireMaxSizeVotersRecord _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (votersRecordVoters msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeVoter _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for VotersRecord.
wirePokeVotersRecord :: Int -> Ptr Word8 -> VotersRecord -> IO (Ptr Word8)
wirePokeVotersRecord version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (votersRecordVersion msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeVoter version p x) p1 (votersRecordVoters msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke VotersRecord : unsupported version: " ++ show version

-- | Direct-poke decoder for VotersRecord.
wirePeekVotersRecord :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (VotersRecord, Ptr Word8)
wirePeekVotersRecord version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_version, p1) <- W.peekInt16BE p0 endPtr
    (f1_voters, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekVoter version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (VotersRecord { votersRecordVersion = f0_version, votersRecordVoters = f1_voters }, pTagsEnd)
  | otherwise = error $ "wirePeek VotersRecord : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec VotersRecord where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeVotersRecord (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeVotersRecord (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekVotersRecord (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}