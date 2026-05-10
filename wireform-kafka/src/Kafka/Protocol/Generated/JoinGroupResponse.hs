{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.JoinGroupResponse
Description : Kafka JoinGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 11.



Valid versions: 0-9
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.JoinGroupResponse
  (
    JoinGroupResponse(..),
    JoinGroupResponseMember(..),
    maxJoinGroupResponseVersion
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


-- | The group members.
data JoinGroupResponseMember = JoinGroupResponseMember
  {

  -- | The group member ID.

  -- Versions: 0+
  joinGroupResponseMemberMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 5+
  joinGroupResponseMemberGroupInstanceId :: !(KafkaString)
,

  -- | The group member metadata.

  -- Versions: 0+
  joinGroupResponseMemberMetadata :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


data JoinGroupResponse = JoinGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  joinGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  joinGroupResponseErrorCode :: !(Int16)
,

  -- | The generation ID of the group.

  -- Versions: 0+
  joinGroupResponseGenerationId :: !(Int32)
,

  -- | The group protocol name.

  -- Versions: 7+
  joinGroupResponseProtocolType :: !(KafkaString)
,

  -- | The group protocol selected by the coordinator.

  -- Versions: 0+
  joinGroupResponseProtocolName :: !(KafkaString)
,

  -- | The leader of the group.

  -- Versions: 0+
  joinGroupResponseLeader :: !(KafkaString)
,

  -- | True if the leader must skip running the assignment.

  -- Versions: 9+
  joinGroupResponseSkipAssignment :: !(Bool)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 0+
  joinGroupResponseMemberId :: !(KafkaString)
,

  -- | The group members.

  -- Versions: 0+
  joinGroupResponseMembers :: !(KafkaArray (JoinGroupResponseMember))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for JoinGroupResponse.
maxJoinGroupResponseVersion :: Int16
maxJoinGroupResponseVersion = 9

-- | KafkaMessage instance for JoinGroupResponse.
instance KafkaMessage JoinGroupResponse where
  messageApiKey = 11
  messageMinVersion = 0
  messageMaxVersion = 9
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a JoinGroupResponseMember.
wireMaxSizeJoinGroupResponseMember :: Int -> JoinGroupResponseMember -> Int
wireMaxSizeJoinGroupResponseMember _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseMemberMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseMemberGroupInstanceId msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (joinGroupResponseMemberMetadata msg))
  + 1

-- | Direct-poke encoder for JoinGroupResponseMember.
wirePokeJoinGroupResponseMember :: Int -> Ptr Word8 -> JoinGroupResponseMember -> IO (Ptr Word8)
wirePokeJoinGroupResponseMember version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (joinGroupResponseMemberMemberId msg)) else WP.pokeKafkaString p0 (joinGroupResponseMemberMemberId msg))
  p2 <- (if version >= 5 then (if version >= 6 then WP.pokeCompactString p1 (P.toCompactString (joinGroupResponseMemberGroupInstanceId msg)) else WP.pokeKafkaString p1 (joinGroupResponseMemberGroupInstanceId msg)) else pure p1)
  p3 <- (if version >= 6 then WP.pokeCompactBytes p2 (P.toCompactBytes (joinGroupResponseMemberMetadata msg)) else WP.pokeKafkaBytes p2 (joinGroupResponseMemberMetadata msg))
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for JoinGroupResponseMember.
wirePeekJoinGroupResponseMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (JoinGroupResponseMember, Ptr Word8)
wirePeekJoinGroupResponseMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_groupinstanceid, p2) <- (if version >= 5 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_metadata, p3) <- (if version >= 6 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (JoinGroupResponseMember { joinGroupResponseMemberMemberId = f0_memberid, joinGroupResponseMemberGroupInstanceId = f1_groupinstanceid, joinGroupResponseMemberMetadata = f2_metadata }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultJoinGroupResponseMember :: JoinGroupResponseMember
defaultJoinGroupResponseMember = JoinGroupResponseMember { joinGroupResponseMemberMemberId = P.KafkaString Null, joinGroupResponseMemberGroupInstanceId = P.KafkaString Null, joinGroupResponseMemberMetadata = P.KafkaBytes Null }

-- | Worst-case wire size of a JoinGroupResponse.
wireMaxSizeJoinGroupResponse :: Int -> JoinGroupResponse -> Int
wireMaxSizeJoinGroupResponse _version msg =
  0
  + 4
  + 2
  + 4
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseProtocolType msg))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseProtocolName msg))
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseLeader msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (joinGroupResponseMemberId msg))
  + (5 + (case P.unKafkaArray (joinGroupResponseMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeJoinGroupResponseMember _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for JoinGroupResponse.
wirePokeJoinGroupResponse :: Int -> Ptr Word8 -> JoinGroupResponse -> IO (Ptr Word8)
wirePokeJoinGroupResponse version basePtr msg
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (joinGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (joinGroupResponseErrorCode msg)
    p3 <- W.pokeInt32BE p2 (joinGroupResponseGenerationId msg)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupResponseProtocolName msg)) else WP.pokeKafkaString p3 (joinGroupResponseProtocolName msg))
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupResponseLeader msg)) else WP.pokeKafkaString p4 (joinGroupResponseLeader msg))
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupResponseMemberId msg)) else WP.pokeKafkaString p5 (joinGroupResponseMemberId msg))
    p7 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupResponseMember version p x) p6 (joinGroupResponseMembers msg)
    WP.pokeEmptyTaggedFields p7
  | version == 9 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (joinGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (joinGroupResponseErrorCode msg)
    p3 <- W.pokeInt32BE p2 (joinGroupResponseGenerationId msg)
    p4 <- (if version >= 7 then (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupResponseProtocolType msg)) else WP.pokeKafkaString p3 (joinGroupResponseProtocolType msg)) else pure p3)
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupResponseProtocolName msg)) else WP.pokeKafkaString p4 (joinGroupResponseProtocolName msg))
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupResponseLeader msg)) else WP.pokeKafkaString p5 (joinGroupResponseLeader msg))
    p7 <- (if version >= 9 then W.pokeWord8 p6 (if (joinGroupResponseSkipAssignment msg) then 1 else 0) else pure p6)
    p8 <- (if version >= 6 then WP.pokeCompactString p7 (P.toCompactString (joinGroupResponseMemberId msg)) else WP.pokeKafkaString p7 (joinGroupResponseMemberId msg))
    p9 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupResponseMember version p x) p8 (joinGroupResponseMembers msg)
    WP.pokeEmptyTaggedFields p9
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (joinGroupResponseErrorCode msg)
    p2 <- W.pokeInt32BE p1 (joinGroupResponseGenerationId msg)
    p3 <- (if version >= 6 then WP.pokeCompactString p2 (P.toCompactString (joinGroupResponseProtocolName msg)) else WP.pokeKafkaString p2 (joinGroupResponseProtocolName msg))
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupResponseLeader msg)) else WP.pokeKafkaString p3 (joinGroupResponseLeader msg))
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupResponseMemberId msg)) else WP.pokeKafkaString p4 (joinGroupResponseMemberId msg))
    p6 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupResponseMember version p x) p5 (joinGroupResponseMembers msg)
    pure p6
  | version >= 7 && version <= 8 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (joinGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (joinGroupResponseErrorCode msg)
    p3 <- W.pokeInt32BE p2 (joinGroupResponseGenerationId msg)
    p4 <- (if version >= 7 then (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupResponseProtocolType msg)) else WP.pokeKafkaString p3 (joinGroupResponseProtocolType msg)) else pure p3)
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupResponseProtocolName msg)) else WP.pokeKafkaString p4 (joinGroupResponseProtocolName msg))
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupResponseLeader msg)) else WP.pokeKafkaString p5 (joinGroupResponseLeader msg))
    p7 <- (if version >= 6 then WP.pokeCompactString p6 (P.toCompactString (joinGroupResponseMemberId msg)) else WP.pokeKafkaString p6 (joinGroupResponseMemberId msg))
    p8 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupResponseMember version p x) p7 (joinGroupResponseMembers msg)
    WP.pokeEmptyTaggedFields p8
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then W.pokeInt32BE p0 (joinGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (joinGroupResponseErrorCode msg)
    p3 <- W.pokeInt32BE p2 (joinGroupResponseGenerationId msg)
    p4 <- (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (joinGroupResponseProtocolName msg)) else WP.pokeKafkaString p3 (joinGroupResponseProtocolName msg))
    p5 <- (if version >= 6 then WP.pokeCompactString p4 (P.toCompactString (joinGroupResponseLeader msg)) else WP.pokeKafkaString p4 (joinGroupResponseLeader msg))
    p6 <- (if version >= 6 then WP.pokeCompactString p5 (P.toCompactString (joinGroupResponseMemberId msg)) else WP.pokeKafkaString p5 (joinGroupResponseMemberId msg))
    p7 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeJoinGroupResponseMember version p x) p6 (joinGroupResponseMembers msg)
    pure p7
  | otherwise = error $ "wirePoke JoinGroupResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for JoinGroupResponse.
wirePeekJoinGroupResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (JoinGroupResponse, Ptr Word8)
wirePeekJoinGroupResponse version _fp _basePtr p0 endPtr
  | version == 6 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_generationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_protocolname, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_leader, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_memberid, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_members, p7) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupResponseMember version _fp _basePtr p e) p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (JoinGroupResponse { joinGroupResponseThrottleTimeMs = f0_throttletimems, joinGroupResponseErrorCode = f1_errorcode, joinGroupResponseGenerationId = f2_generationid, joinGroupResponseProtocolType = P.KafkaString Null, joinGroupResponseProtocolName = f3_protocolname, joinGroupResponseLeader = f4_leader, joinGroupResponseSkipAssignment = False, joinGroupResponseMemberId = f5_memberid, joinGroupResponseMembers = f6_members }, pTagsEnd)
  | version == 9 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_generationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_protocoltype, p4) <- (if version >= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_protocolname, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_leader, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_skipassignment, p7) <- (if version >= 9 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p6 endPtr else pure (False, p6))
    (f7_memberid, p8) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr else WP.peekKafkaString p7 endPtr)
    (f8_members, p9) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupResponseMember version _fp _basePtr p e) p8 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p9 endPtr
    pure (JoinGroupResponse { joinGroupResponseThrottleTimeMs = f0_throttletimems, joinGroupResponseErrorCode = f1_errorcode, joinGroupResponseGenerationId = f2_generationid, joinGroupResponseProtocolType = f3_protocoltype, joinGroupResponseProtocolName = f4_protocolname, joinGroupResponseLeader = f5_leader, joinGroupResponseSkipAssignment = f6_skipassignment, joinGroupResponseMemberId = f7_memberid, joinGroupResponseMembers = f8_members }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_protocolname, p3) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_leader, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_memberid, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_members, p6) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupResponseMember version _fp _basePtr p e) p5 endPtr
    pure (JoinGroupResponse { joinGroupResponseThrottleTimeMs = 0, joinGroupResponseErrorCode = f0_errorcode, joinGroupResponseGenerationId = f1_generationid, joinGroupResponseProtocolType = P.KafkaString Null, joinGroupResponseProtocolName = f2_protocolname, joinGroupResponseLeader = f3_leader, joinGroupResponseSkipAssignment = False, joinGroupResponseMemberId = f4_memberid, joinGroupResponseMembers = f5_members }, p6)
  | version >= 7 && version <= 8 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_generationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_protocoltype, p4) <- (if version >= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_protocolname, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_leader, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_memberid, p7) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr else WP.peekKafkaString p6 endPtr)
    (f7_members, p8) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupResponseMember version _fp _basePtr p e) p7 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (JoinGroupResponse { joinGroupResponseThrottleTimeMs = f0_throttletimems, joinGroupResponseErrorCode = f1_errorcode, joinGroupResponseGenerationId = f2_generationid, joinGroupResponseProtocolType = f3_protocoltype, joinGroupResponseProtocolName = f4_protocolname, joinGroupResponseLeader = f5_leader, joinGroupResponseSkipAssignment = False, joinGroupResponseMemberId = f6_memberid, joinGroupResponseMembers = f7_members }, pTagsEnd)
  | version >= 2 && version <= 5 = do
    (f0_throttletimems, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_generationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_protocolname, p4) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_leader, p5) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_memberid, p6) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
    (f6_members, p7) <- WP.peekVersionedArray version 6 (\p e -> wirePeekJoinGroupResponseMember version _fp _basePtr p e) p6 endPtr
    pure (JoinGroupResponse { joinGroupResponseThrottleTimeMs = f0_throttletimems, joinGroupResponseErrorCode = f1_errorcode, joinGroupResponseGenerationId = f2_generationid, joinGroupResponseProtocolType = P.KafkaString Null, joinGroupResponseProtocolName = f3_protocolname, joinGroupResponseLeader = f4_leader, joinGroupResponseSkipAssignment = False, joinGroupResponseMemberId = f5_memberid, joinGroupResponseMembers = f6_members }, p7)
  | otherwise = error $ "wirePeek JoinGroupResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec JoinGroupResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeJoinGroupResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeJoinGroupResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekJoinGroupResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}