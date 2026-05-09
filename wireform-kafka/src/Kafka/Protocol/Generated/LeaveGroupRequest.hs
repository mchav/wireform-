{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaveGroupRequest
Description : Kafka LeaveGroupRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 13.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaveGroupRequest
  (
    LeaveGroupRequest(..),
    MemberIdentity(..),
    maxLeaveGroupRequestVersion
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


-- | List of leaving member identities.
data MemberIdentity = MemberIdentity
  {

  -- | The member ID to remove from the group.

  -- Versions: 3+
  memberIdentityMemberId :: !(KafkaString)
,

  -- | The group instance ID to remove from the group.

  -- Versions: 3+
  memberIdentityGroupInstanceId :: !(KafkaString)
,

  -- | The reason why the member left the group.

  -- Versions: 5+
  memberIdentityReason :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data LeaveGroupRequest = LeaveGroupRequest
  {

  -- | The ID of the group to leave.

  -- Versions: 0+
  leaveGroupRequestGroupId :: !(KafkaString)
,

  -- | The member ID to remove from the group.

  -- Versions: 0-2
  leaveGroupRequestMemberId :: !(KafkaString)
,

  -- | List of leaving member identities.

  -- Versions: 3+
  leaveGroupRequestMembers :: !(KafkaArray (MemberIdentity))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaveGroupRequest.
maxLeaveGroupRequestVersion :: Int16
maxLeaveGroupRequestVersion = 5

-- | KafkaMessage instance for LeaveGroupRequest.
instance KafkaMessage LeaveGroupRequest where
  messageApiKey = 13
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 4

-- | Worst-case wire size of a MemberIdentity.
wireMaxSizeMemberIdentity :: Int -> MemberIdentity -> Int
wireMaxSizeMemberIdentity _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (memberIdentityMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberIdentityGroupInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberIdentityReason msg))
  + 1

-- | Direct-poke encoder for MemberIdentity.
wirePokeMemberIdentity :: Int -> Ptr Word8 -> MemberIdentity -> IO (Ptr Word8)
wirePokeMemberIdentity version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (memberIdentityMemberId msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (memberIdentityGroupInstanceId msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (memberIdentityReason msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for MemberIdentity.
wirePeekMemberIdentity :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MemberIdentity, Ptr Word8)
wirePeekMemberIdentity version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_groupinstanceid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_reason, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (MemberIdentity { memberIdentityMemberId = f0_memberid, memberIdentityGroupInstanceId = f1_groupinstanceid, memberIdentityReason = f2_reason }, pTagsEnd)

-- | Worst-case wire size of a LeaveGroupRequest.
wireMaxSizeLeaveGroupRequest :: Int -> LeaveGroupRequest -> Int
wireMaxSizeLeaveGroupRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (leaveGroupRequestGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (leaveGroupRequestMemberId msg))
  + (5 + (case P.unKafkaArray (leaveGroupRequestMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMemberIdentity _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for LeaveGroupRequest.
wirePokeLeaveGroupRequest :: Int -> Ptr Word8 -> LeaveGroupRequest -> IO (Ptr Word8)
wirePokeLeaveGroupRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberIdentity version p x) p1 (leaveGroupRequestMembers msg)
    pure p2
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberIdentity version p x) p1 (leaveGroupRequestMembers msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (leaveGroupRequestMemberId msg))
    pure p2
  | otherwise = error $ "wirePoke LeaveGroupRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for LeaveGroupRequest.
wirePeekLeaveGroupRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaveGroupRequest, Ptr Word8)
wirePeekLeaveGroupRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_members, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekMemberIdentity version _fp _basePtr p e) p1 endPtr
    pure (LeaveGroupRequest { leaveGroupRequestGroupId = f0_groupid, leaveGroupRequestMemberId = P.KafkaString Null, leaveGroupRequestMembers = f1_members }, p2)
  | version >= 4 && version <= 5 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_members, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekMemberIdentity version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (LeaveGroupRequest { leaveGroupRequestGroupId = f0_groupid, leaveGroupRequestMemberId = P.KafkaString Null, leaveGroupRequestMembers = f1_members }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_memberid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    pure (LeaveGroupRequest { leaveGroupRequestGroupId = f0_groupid, leaveGroupRequestMemberId = f1_memberid, leaveGroupRequestMembers = P.mkKafkaArray V.empty }, p2)
  | otherwise = error $ "wirePeek LeaveGroupRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec LeaveGroupRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaveGroupRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaveGroupRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaveGroupRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}