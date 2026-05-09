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
    encodeLeaveGroupRequest,
    decodeLeaveGroupRequest,
    maxLeaveGroupRequestVersion
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


-- | Encode MemberIdentity with version-aware field handling.
encodeMemberIdentity :: MonadPut m => E.ApiVersion -> MemberIdentity -> m ()
encodeMemberIdentity version mmsg =
  do
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberIdentityMemberId mmsg)) else serialize (memberIdentityMemberId mmsg)
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberIdentityGroupInstanceId mmsg)) else serialize (memberIdentityGroupInstanceId mmsg)
    when (version >= 5) $
      if version >= 4 then serialize (toCompactString (memberIdentityReason mmsg)) else serialize (memberIdentityReason mmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MemberIdentity with version-aware field handling.
decodeMemberIdentity :: MonadGet m => E.ApiVersion -> m MemberIdentity
decodeMemberIdentity version =
  do
    fieldmemberid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgroupinstanceid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldreason <- if version >= 5
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MemberIdentity
      {
      memberIdentityMemberId = fieldmemberid
      ,
      memberIdentityGroupInstanceId = fieldgroupinstanceid
      ,
      memberIdentityReason = fieldreason
      }



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

-- | Encode LeaveGroupRequest with the given API version.
encodeLeaveGroupRequest :: MonadPut m => E.ApiVersion -> LeaveGroupRequest -> m ()
encodeLeaveGroupRequest version msg
  | version == 3 =
    do
      serialize (leaveGroupRequestGroupId msg)
      E.encodeVersionedArray version 4 encodeMemberIdentity (case P.unKafkaArray (leaveGroupRequestMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 4 && version <= 5 =
    do
      serialize (toCompactString (leaveGroupRequestGroupId msg))
      E.encodeVersionedArray version 4 encodeMemberIdentity (case P.unKafkaArray (leaveGroupRequestMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (leaveGroupRequestGroupId msg)
      serialize (leaveGroupRequestMemberId msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode LeaveGroupRequest with the given API version.
decodeLeaveGroupRequest :: MonadGet m => E.ApiVersion -> m LeaveGroupRequest
decodeLeaveGroupRequest version
  | version == 3 =
    do
      fieldgroupid <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberIdentity
      pure LeaveGroupRequest
        {
        leaveGroupRequestGroupId = fieldgroupid
        ,
        leaveGroupRequestMemberId = P.KafkaString Null
        ,
        leaveGroupRequestMembers = fieldmembers
        }

  | version >= 4 && version <= 5 =
    do
      fieldgroupid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberIdentity
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure LeaveGroupRequest
        {
        leaveGroupRequestGroupId = fieldgroupid
        ,
        leaveGroupRequestMemberId = P.KafkaString Null
        ,
        leaveGroupRequestMembers = fieldmembers
        }

  | version >= 0 && version <= 2 =
    do
      fieldgroupid <- deserialize
      fieldmemberid <- deserialize
      pure LeaveGroupRequest
        {
        leaveGroupRequestGroupId = fieldgroupid
        ,
        leaveGroupRequestMemberId = fieldmemberid
        ,
        leaveGroupRequestMembers = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec LeaveGroupRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaveGroupRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaveGroupRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaveGroupRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}