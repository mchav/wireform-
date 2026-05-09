{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ElectLeadersResponse
Description : Kafka ElectLeadersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 43.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ElectLeadersResponse
  (
    ElectLeadersResponse(..),
    ReplicaElectionResult(..),
    PartitionResult(..),
    maxElectLeadersResponseVersion
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


-- | The results for each partition.
data PartitionResult = PartitionResult
  {

  -- | The partition id.

  -- Versions: 0+
  partitionResultPartitionId :: !(Int32)
,

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The election results, or an empty array if the requester did not have permission and the request asks for all partitions.
data ReplicaElectionResult = ReplicaElectionResult
  {

  -- | The topic name.

  -- Versions: 0+
  replicaElectionResultTopic :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  replicaElectionResultPartitionResult :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


data ElectLeadersResponse = ElectLeadersResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  electLeadersResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 1+
  electLeadersResponseErrorCode :: !(Int16)
,

  -- | The election results, or an empty array if the requester did not have permission and the request ask

  -- Versions: 0+
  electLeadersResponseReplicaElectionResults :: !(KafkaArray (ReplicaElectionResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ElectLeadersResponse.
maxElectLeadersResponseVersion :: Int16
maxElectLeadersResponseVersion = 2

-- | KafkaMessage instance for ElectLeadersResponse.
instance KafkaMessage ElectLeadersResponse where
  messageApiKey = 43
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a PartitionResult.
wireMaxSizePartitionResult :: Int -> PartitionResult -> Int
wireMaxSizePartitionResult _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for PartitionResult.
wirePokePartitionResult :: Int -> Ptr Word8 -> PartitionResult -> IO (Ptr Word8)
wirePokePartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResultPartitionId msg)
  p2 <- W.pokeInt16BE p1 (partitionResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionResultErrorMessage msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for PartitionResult.
wirePeekPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResult, Ptr Word8)
wirePeekPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partitionid, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (PartitionResult { partitionResultPartitionId = f0_partitionid, partitionResultErrorCode = f1_errorcode, partitionResultErrorMessage = f2_errormessage }, pTagsEnd)

-- | Worst-case wire size of a ReplicaElectionResult.
wireMaxSizeReplicaElectionResult :: Int -> ReplicaElectionResult -> Int
wireMaxSizeReplicaElectionResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (replicaElectionResultTopic msg))
  + (5 + (case P.unKafkaArray (replicaElectionResultPartitionResult msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReplicaElectionResult.
wirePokeReplicaElectionResult :: Int -> Ptr Word8 -> ReplicaElectionResult -> IO (Ptr Word8)
wirePokeReplicaElectionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (replicaElectionResultTopic msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokePartitionResult version p x) p1 (replicaElectionResultPartitionResult msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReplicaElectionResult.
wirePeekReplicaElectionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaElectionResult, Ptr Word8)
wirePeekReplicaElectionResult version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitionresult, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReplicaElectionResult { replicaElectionResultTopic = f0_topic, replicaElectionResultPartitionResult = f1_partitionresult }, pTagsEnd)

-- | Worst-case wire size of a ElectLeadersResponse.
wireMaxSizeElectLeadersResponse :: Int -> ElectLeadersResponse -> Int
wireMaxSizeElectLeadersResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (electLeadersResponseReplicaElectionResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReplicaElectionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ElectLeadersResponse.
wirePokeElectLeadersResponse :: Int -> Ptr Word8 -> ElectLeadersResponse -> IO (Ptr Word8)
wirePokeElectLeadersResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (electLeadersResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeReplicaElectionResult version p x) p1 (electLeadersResponseReplicaElectionResults msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (electLeadersResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (electLeadersResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeReplicaElectionResult version p x) p2 (electLeadersResponseReplicaElectionResults msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (electLeadersResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (electLeadersResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeReplicaElectionResult version p x) p2 (electLeadersResponseReplicaElectionResults msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ElectLeadersResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ElectLeadersResponse.
wirePeekElectLeadersResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ElectLeadersResponse, Ptr Word8)
wirePeekElectLeadersResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_replicaelectionresults, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekReplicaElectionResult version _fp _basePtr p e) p1 endPtr
    pure (ElectLeadersResponse { electLeadersResponseThrottleTimeMs = f0_throttletimems, electLeadersResponseErrorCode = 0, electLeadersResponseReplicaElectionResults = f1_replicaelectionresults }, p2)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_replicaelectionresults, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekReplicaElectionResult version _fp _basePtr p e) p2 endPtr
    pure (ElectLeadersResponse { electLeadersResponseThrottleTimeMs = f0_throttletimems, electLeadersResponseErrorCode = f1_errorcode, electLeadersResponseReplicaElectionResults = f2_replicaelectionresults }, p3)
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_replicaelectionresults, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekReplicaElectionResult version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ElectLeadersResponse { electLeadersResponseThrottleTimeMs = f0_throttletimems, electLeadersResponseErrorCode = f1_errorcode, electLeadersResponseReplicaElectionResults = f2_replicaelectionresults }, pTagsEnd)
  | otherwise = error $ "wirePeek ElectLeadersResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ElectLeadersResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeElectLeadersResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeElectLeadersResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekElectLeadersResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}