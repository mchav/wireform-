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
    maxUpdateRaftVoterRequestVersion
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

-- | KafkaMessage instance for UpdateRaftVoterRequest.
instance KafkaMessage UpdateRaftVoterRequest where
  messageApiKey = 82
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a Listener.
wireMaxSizeListener :: Int -> Listener -> Int
wireMaxSizeListener _version msg =
  0
  + WP.dualStringMaxSize (listenerName msg)
  + WP.dualStringMaxSize (listenerHost msg)
  + 2
  + 1

-- | Direct-poke encoder for Listener.
wirePokeListener :: Int -> Ptr Word8 -> Listener -> IO (Ptr Word8)
wirePokeListener version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (listenerName msg)) else WP.pokeKafkaString p0 (listenerName msg))
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (listenerHost msg)) else WP.pokeKafkaString p1 (listenerHost msg))
  p3 <- W.pokeWord16BE p2 (listenerPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_host, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListener :: Listener
defaultListener = Listener { listenerName = P.KafkaString Null, listenerHost = P.KafkaString Null, listenerPort = 0 }

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

-- | Worst-case wire size of a UpdateRaftVoterRequest.
wireMaxSizeUpdateRaftVoterRequest :: Int -> UpdateRaftVoterRequest -> Int
wireMaxSizeUpdateRaftVoterRequest _version msg =
  0
  + WP.dualStringMaxSize (updateRaftVoterRequestClusterId msg)
  + 4
  + 4
  + 16
  + (5 + (case P.unKafkaArray (updateRaftVoterRequestListeners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListener _version x ) v); P.Null -> 0 }))
  + wireMaxSizeKRaftVersionFeature _version (updateRaftVoterRequestKRaftVersionFeature msg)
  + 1

-- | Direct-poke encoder for UpdateRaftVoterRequest.
wirePokeUpdateRaftVoterRequest :: Int -> Ptr Word8 -> UpdateRaftVoterRequest -> IO (Ptr Word8)
wirePokeUpdateRaftVoterRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (updateRaftVoterRequestClusterId msg)) else WP.pokeKafkaString p0 (updateRaftVoterRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (updateRaftVoterRequestCurrentLeaderEpoch msg)
    p3 <- W.pokeInt32BE p2 (updateRaftVoterRequestVoterId msg)
    p4 <- WP.pokeKafkaUuid p3 (updateRaftVoterRequestVoterDirectoryId msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p4 (updateRaftVoterRequestListeners msg)
    p6 <- wirePokeKRaftVersionFeature version p5 (updateRaftVoterRequestKRaftVersionFeature msg)
    WP.pokeEmptyTaggedFields p6
  | otherwise = error $ "wirePoke UpdateRaftVoterRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for UpdateRaftVoterRequest.
wirePeekUpdateRaftVoterRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateRaftVoterRequest, Ptr Word8)
wirePeekUpdateRaftVoterRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_currentleaderepoch, p2) <- W.peekInt32BE p1 endPtr
    (f2_voterid, p3) <- W.peekInt32BE p2 endPtr
    (f3_voterdirectoryid, p4) <- WP.peekKafkaUuid p3 endPtr
    (f4_listeners, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p4 endPtr
    (f5_kraftversionfeature, p6) <- wirePeekKRaftVersionFeature version _fp _basePtr p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (UpdateRaftVoterRequest { updateRaftVoterRequestClusterId = f0_clusterid, updateRaftVoterRequestCurrentLeaderEpoch = f1_currentleaderepoch, updateRaftVoterRequestVoterId = f2_voterid, updateRaftVoterRequestVoterDirectoryId = f3_voterdirectoryid, updateRaftVoterRequestListeners = f4_listeners, updateRaftVoterRequestKRaftVersionFeature = f5_kraftversionfeature }, pTagsEnd)
  | otherwise = error $ "wirePeek UpdateRaftVoterRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec UpdateRaftVoterRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateRaftVoterRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateRaftVoterRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateRaftVoterRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}