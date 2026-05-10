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
  + WP.dualStringMaxSize (memberIdentityMemberId msg)
  + WP.dualStringMaxSize (memberIdentityGroupInstanceId msg)
  + WP.dualStringMaxSize (memberIdentityReason msg)
  + 1

-- | Direct-poke encoder for MemberIdentity.
wirePokeMemberIdentity :: Int -> Ptr Word8 -> MemberIdentity -> IO (Ptr Word8)
wirePokeMemberIdentity version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (memberIdentityMemberId msg)) else WP.pokeKafkaString p0 (memberIdentityMemberId msg)) else pure p0)
  p2 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p1 (P.toCompactString (memberIdentityGroupInstanceId msg)) else WP.pokeKafkaString p1 (memberIdentityGroupInstanceId msg)) else pure p1)
  p3 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (memberIdentityReason msg)) else WP.pokeKafkaString p2 (memberIdentityReason msg)) else pure p2)
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for MemberIdentity.
wirePeekMemberIdentity :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MemberIdentity, Ptr Word8)
wirePeekMemberIdentity version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_groupinstanceid, p2) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_reason, p3) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (MemberIdentity { memberIdentityMemberId = f0_memberid, memberIdentityGroupInstanceId = f1_groupinstanceid, memberIdentityReason = f2_reason }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultMemberIdentity :: MemberIdentity
defaultMemberIdentity = MemberIdentity { memberIdentityMemberId = P.KafkaString Null, memberIdentityGroupInstanceId = P.KafkaString Null, memberIdentityReason = P.KafkaString Null }

-- | Worst-case wire size of a LeaveGroupRequest.
wireMaxSizeLeaveGroupRequest :: Int -> LeaveGroupRequest -> Int
wireMaxSizeLeaveGroupRequest _version msg =
  0
  + WP.dualStringMaxSize (leaveGroupRequestGroupId msg)
  + WP.dualStringMaxSize (leaveGroupRequestMemberId msg)
  + (5 + (case P.unKafkaArray (leaveGroupRequestMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMemberIdentity _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for LeaveGroupRequest.
wirePokeLeaveGroupRequest :: Int -> Ptr Word8 -> LeaveGroupRequest -> IO (Ptr Word8)
wirePokeLeaveGroupRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (leaveGroupRequestGroupId msg))
    p2 <- (if version >= 3 then WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberIdentity version p x) p1 (leaveGroupRequestMembers msg) else pure p1)
    pure p2
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (leaveGroupRequestGroupId msg))
    p2 <- (if version >= 3 then WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberIdentity version p x) p1 (leaveGroupRequestMembers msg) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (leaveGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (leaveGroupRequestGroupId msg))
    p2 <- (if version <= 2 then (if version >= 4 then WP.pokeCompactString p1 (P.toCompactString (leaveGroupRequestMemberId msg)) else WP.pokeKafkaString p1 (leaveGroupRequestMemberId msg)) else pure p1)
    pure p2
  | otherwise = error $ "wirePoke LeaveGroupRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for LeaveGroupRequest.
wirePeekLeaveGroupRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaveGroupRequest, Ptr Word8)
wirePeekLeaveGroupRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_members, p2) <- (if version >= 3 then WP.peekVersionedArray version 4 (\p e -> wirePeekMemberIdentity version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pure (LeaveGroupRequest { leaveGroupRequestGroupId = f0_groupid, leaveGroupRequestMemberId = P.KafkaString Null, leaveGroupRequestMembers = f1_members }, p2)
  | version >= 4 && version <= 5 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_members, p2) <- (if version >= 3 then WP.peekVersionedArray version 4 (\p e -> wirePeekMemberIdentity version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (LeaveGroupRequest { leaveGroupRequestGroupId = f0_groupid, leaveGroupRequestMemberId = P.KafkaString Null, leaveGroupRequestMembers = f1_members }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_memberid, p2) <- (if version <= 2 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
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