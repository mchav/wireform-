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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC


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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec LeaveGroupRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
