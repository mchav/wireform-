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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (endpointName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (endpointHost msg))
  p3 <- W.pokeWord16BE p2 (endpointPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Endpoint.
wirePeekEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Endpoint, Ptr Word8)
wirePeekEndpoint version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Endpoint { endpointName = f0_name, endpointHost = f1_host, endpointPort = f2_port }, pTagsEnd)

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
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec VotersRecord where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeVotersRecord (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeVotersRecord (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekVotersRecord (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}