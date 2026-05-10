{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.JoinGroupRequest
Description : Kafka JoinGroupRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 11.



Valid versions: 0-9
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.JoinGroupRequest
  (
    JoinGroupRequest(..),
    JoinGroupRequestProtocol(..),
    maxJoinGroupRequestVersion
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


-- | The list of protocols that the member supports.
data JoinGroupRequestProtocol = JoinGroupRequestProtocol
  {

  -- | The protocol name.

  -- Versions: 0+
  joinGroupRequestProtocolName :: !(KafkaString)
,

  -- | The protocol metadata.

  -- Versions: 0+
  joinGroupRequestProtocolMetadata :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


data JoinGroupRequest = JoinGroupRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  joinGroupRequestGroupId :: !(KafkaString)
,

  -- | The coordinator considers the consumer dead if it receives no heartbeat after this timeout in millis

  -- Versions: 0+
  joinGroupRequestSessionTimeoutMs :: !(Int32)
,

  -- | The maximum time in milliseconds that the coordinator will wait for each member to rejoin when rebal

  -- Versions: 1+
  joinGroupRequestRebalanceTimeoutMs :: !(Int32)
,

  -- | The member id assigned by the group coordinator.

  -- Versions: 0+
  joinGroupRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 5+
  joinGroupRequestGroupInstanceId :: !(KafkaString)
,

  -- | The unique name the for class of protocols implemented by the group we want to join.

  -- Versions: 0+
  joinGroupRequestProtocolType :: !(KafkaString)
,

  -- | The list of protocols that the member supports.

  -- Versions: 0+
  joinGroupRequestProtocols :: !(KafkaArray (JoinGroupRequestProtocol))
,

  -- | The reason why the member (re-)joins the group.

  -- Versions: 8+
  joinGroupRequestReason :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for JoinGroupRequest.
maxJoinGroupRequestVersion :: Int16
maxJoinGroupRequestVersion = 9

-- | KafkaMessage instance for JoinGroupRequest.
instance KafkaMessage JoinGroupRequest where
  messageApiKey = 11
  messageMinVersion = 0
  messageMaxVersion = 9
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a JoinGroupRequestProtocol.
wireMaxSizeJoinGroupRequestProtocol :: Int -> JoinGroupRequestProtocol -> Int
wireMaxSizeJoinGroupRequestProtocol _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestProtocolName msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (joinGroupRequestProtocolMetadata msg))
  + 1

-- | Direct-poke encoder for JoinGroupRequestProtocol.
wirePokeJoinGroupRequestProtocol :: Int -> Ptr Word8 -> JoinGroupRequestProtocol -> IO (Ptr Word8)
wirePokeJoinGroupRequestProtocol version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestProtocolName msg)) else WP.pokeKafkaString p0 (joinGroupRequestProtocolName msg))
  p2 <- (if version >= 6 then WP.pokeCompactBytes p1 (P.toCompactBytes (joinGroupRequestProtocolMetadata msg)) else WP.pokeKafkaBytes p1 (joinGroupRequestProtocolMetadata msg))
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for JoinGroupRequestProtocol.
wirePeekJoinGroupRequestProtocol :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (JoinGroupRequestProtocol, Ptr Word8)
wirePeekJoinGroupRequestProtocol version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_metadata, p2) <- (if version >= 6 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr else WP.peekKafkaBytes p1 endPtr)
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (JoinGroupRequestProtocol { joinGroupRequestProtocolName = f0_name, joinGroupRequestProtocolMetadata = f1_metadata }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultJoinGroupRequestProtocol :: JoinGroupRequestProtocol
defaultJoinGroupRequestProtocol = JoinGroupRequestProtocol { joinGroupRequestProtocolName = P.KafkaString Null, joinGroupRequestProtocolMetadata = P.KafkaBytes Null }

-- | Worst-case wire size of a JoinGroupRequest.
wireMaxSizeJoinGroupRequest :: Int -> JoinGroupRequest -> Int
wireMaxSizeJoinGroupRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestGroupId msg))
  + 4
  + 4
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestGroupInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestProtocolType msg))
  + (5 + (case P.unKafkaArray (joinGroupRequestProtocols msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeJoinGroupRequestProtocol _version x ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupRequestReason msg))
  + 1

-- | Direct-poke encoder for JoinGroupRequest.
wirePokeJoinGroupRequest :: Int -> Ptr Word8 -> JoinGroupRequest -> IO (Ptr Word8)
wirePokeJoinGroupRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (joinGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (joinGroupRequestSessionTimeoutMs msg)
    p3 <- (if version >= 6 then WP.pokeCompactString p2 (P.toCompactString (joinGroupRequestMemberId msg)) else WP.pokeKafkaString p2 (joinGroupRequestMemberId msg))
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupRequestProtocolType msg)) else WP.pokeKafkaString p3 (joinGroupRequestProtocolType msg))
    p5 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupRequestProtocol version p x) p4 (joinGroupRequestProtocols msg)
    pure p5
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (joinGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (joinGroupRequestSessionTimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeInt32BE p2 (joinGroupRequestRebalanceTimeoutMs msg) else pure p2)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupRequestMemberId msg)) else WP.pokeKafkaString p3 (joinGroupRequestMemberId msg))
    p5 <- (if version >= 5 then (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p4 (joinGroupRequestGroupInstanceId msg)) else pure p4)
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupRequestProtocolType msg)) else WP.pokeKafkaString p5 (joinGroupRequestProtocolType msg))
    p7 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupRequestProtocol version p x) p6 (joinGroupRequestProtocols msg)
    pure p7
  | version >= 6 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (joinGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (joinGroupRequestSessionTimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeInt32BE p2 (joinGroupRequestRebalanceTimeoutMs msg) else pure p2)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupRequestMemberId msg)) else WP.pokeKafkaString p3 (joinGroupRequestMemberId msg))
    p5 <- (if version >= 5 then (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p4 (joinGroupRequestGroupInstanceId msg)) else pure p4)
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupRequestProtocolType msg)) else WP.pokeKafkaString p5 (joinGroupRequestProtocolType msg))
    p7 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupRequestProtocol version p x) p6 (joinGroupRequestProtocols msg)
    WP.pokeEmptyTaggedFields p7
  | version >= 8 && version <= 9 = do
    p0 <- pure basePtr
    p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (joinGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (joinGroupRequestSessionTimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeInt32BE p2 (joinGroupRequestRebalanceTimeoutMs msg) else pure p2)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupRequestMemberId msg)) else WP.pokeKafkaString p3 (joinGroupRequestMemberId msg))
    p5 <- (if version >= 5 then (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p4 (joinGroupRequestGroupInstanceId msg)) else pure p4)
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupRequestProtocolType msg)) else WP.pokeKafkaString p5 (joinGroupRequestProtocolType msg))
    p7 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupRequestProtocol version p x) p6 (joinGroupRequestProtocols msg)
    p8 <- (if version >= 8 then (if version >= 6 then WP.pokeCompactString p7 (P.toCompactString (joinGroupRequestReason msg)) else WP.pokeKafkaString p7 (joinGroupRequestReason msg)) else pure p7)
    WP.pokeEmptyTaggedFields p8
  | version >= 1 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (joinGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (joinGroupRequestSessionTimeoutMs msg)
    p3 <- (if version >= 1 then W.pokeInt32BE p2 (joinGroupRequestRebalanceTimeoutMs msg) else pure p2)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupRequestMemberId msg)) else WP.pokeKafkaString p3 (joinGroupRequestMemberId msg))
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupRequestProtocolType msg)) else WP.pokeKafkaString p4 (joinGroupRequestProtocolType msg))
    p6 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupRequestProtocol version p x) p5 (joinGroupRequestProtocols msg)
    pure p6
  | otherwise = error $ "wirePoke JoinGroupRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for JoinGroupRequest.
wirePeekJoinGroupRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (JoinGroupRequest, Ptr Word8)
wirePeekJoinGroupRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_sessiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_protocoltype, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_protocols, p5) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupRequestProtocol version _fp _basePtr p e) p4 endPtr
    pure (JoinGroupRequest { joinGroupRequestGroupId = f0_groupid, joinGroupRequestSessionTimeoutMs = f1_sessiontimeoutms, joinGroupRequestRebalanceTimeoutMs = 0, joinGroupRequestMemberId = f2_memberid, joinGroupRequestGroupInstanceId = P.KafkaString Null, joinGroupRequestProtocolType = f3_protocoltype, joinGroupRequestProtocols = f4_protocols, joinGroupRequestReason = P.KafkaString Null }, p5)
  | version == 5 = do
    (f0_groupid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_sessiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_rebalancetimeoutms, p3) <- (if version >= 1 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_memberid, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_groupinstanceid, p5) <- (if version >= 5 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_protocoltype, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_protocols, p7) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupRequestProtocol version _fp _basePtr p e) p6 endPtr
    pure (JoinGroupRequest { joinGroupRequestGroupId = f0_groupid, joinGroupRequestSessionTimeoutMs = f1_sessiontimeoutms, joinGroupRequestRebalanceTimeoutMs = f2_rebalancetimeoutms, joinGroupRequestMemberId = f3_memberid, joinGroupRequestGroupInstanceId = f4_groupinstanceid, joinGroupRequestProtocolType = f5_protocoltype, joinGroupRequestProtocols = f6_protocols, joinGroupRequestReason = P.KafkaString Null }, p7)
  | version >= 6 && version <= 7 = do
    (f0_groupid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_sessiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_rebalancetimeoutms, p3) <- (if version >= 1 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_memberid, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_groupinstanceid, p5) <- (if version >= 5 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_protocoltype, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_protocols, p7) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupRequestProtocol version _fp _basePtr p e) p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (JoinGroupRequest { joinGroupRequestGroupId = f0_groupid, joinGroupRequestSessionTimeoutMs = f1_sessiontimeoutms, joinGroupRequestRebalanceTimeoutMs = f2_rebalancetimeoutms, joinGroupRequestMemberId = f3_memberid, joinGroupRequestGroupInstanceId = f4_groupinstanceid, joinGroupRequestProtocolType = f5_protocoltype, joinGroupRequestProtocols = f6_protocols, joinGroupRequestReason = P.KafkaString Null }, pTagsEnd)
  | version >= 8 && version <= 9 = do
    (f0_groupid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_sessiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_rebalancetimeoutms, p3) <- (if version >= 1 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_memberid, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_groupinstanceid, p5) <- (if version >= 5 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_protocoltype, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_protocols, p7) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupRequestProtocol version _fp _basePtr p e) p6 endPtr
    (f7_reason, p8) <- (if version >= 8 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr else WP.peekKafkaString p7 endPtr) else pure (P.KafkaString Null, p7))
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (JoinGroupRequest { joinGroupRequestGroupId = f0_groupid, joinGroupRequestSessionTimeoutMs = f1_sessiontimeoutms, joinGroupRequestRebalanceTimeoutMs = f2_rebalancetimeoutms, joinGroupRequestMemberId = f3_memberid, joinGroupRequestGroupInstanceId = f4_groupinstanceid, joinGroupRequestProtocolType = f5_protocoltype, joinGroupRequestProtocols = f6_protocols, joinGroupRequestReason = f7_reason }, pTagsEnd)
  | version >= 1 && version <= 4 = do
    (f0_groupid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_sessiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_rebalancetimeoutms, p3) <- (if version >= 1 then W.peekInt32BE p2 endPtr else pure (0, p2))
    (f3_memberid, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_protocoltype, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_protocols, p6) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupRequestProtocol version _fp _basePtr p e) p5 endPtr
    pure (JoinGroupRequest { joinGroupRequestGroupId = f0_groupid, joinGroupRequestSessionTimeoutMs = f1_sessiontimeoutms, joinGroupRequestRebalanceTimeoutMs = f2_rebalancetimeoutms, joinGroupRequestMemberId = f3_memberid, joinGroupRequestGroupInstanceId = P.KafkaString Null, joinGroupRequestProtocolType = f4_protocoltype, joinGroupRequestProtocols = f5_protocols, joinGroupRequestReason = P.KafkaString Null }, p6)
  | otherwise = error $ "wirePeek JoinGroupRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec JoinGroupRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeJoinGroupRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeJoinGroupRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekJoinGroupRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}