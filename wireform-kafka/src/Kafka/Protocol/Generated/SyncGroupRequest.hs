{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SyncGroupRequest
Description : Kafka SyncGroupRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 14.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SyncGroupRequest
  (
    SyncGroupRequest(..),
    SyncGroupRequestAssignment(..),
    encodeSyncGroupRequest,
    decodeSyncGroupRequest,
    maxSyncGroupRequestVersion
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


-- | Each assignment.
data SyncGroupRequestAssignment = SyncGroupRequestAssignment
  {

  -- | The ID of the member to assign.

  -- Versions: 0+
  syncGroupRequestAssignmentMemberId :: !(KafkaString)
,

  -- | The member assignment.

  -- Versions: 0+
  syncGroupRequestAssignmentAssignment :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode SyncGroupRequestAssignment with version-aware field handling.
encodeSyncGroupRequestAssignment :: MonadPut m => E.ApiVersion -> SyncGroupRequestAssignment -> m ()
encodeSyncGroupRequestAssignment version smsg =
  do
    if version >= 4 then serialize (toCompactString (syncGroupRequestAssignmentMemberId smsg)) else serialize (syncGroupRequestAssignmentMemberId smsg)
    if version >= 4 then serialize (toCompactBytes (syncGroupRequestAssignmentAssignment smsg)) else serialize (syncGroupRequestAssignmentAssignment smsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode SyncGroupRequestAssignment with version-aware field handling.
decodeSyncGroupRequestAssignment :: MonadGet m => E.ApiVersion -> m SyncGroupRequestAssignment
decodeSyncGroupRequestAssignment version =
  do
    fieldmemberid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldassignment <- if version >= 4 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure SyncGroupRequestAssignment
      {
      syncGroupRequestAssignmentMemberId = fieldmemberid
      ,
      syncGroupRequestAssignmentAssignment = fieldassignment
      }



data SyncGroupRequest = SyncGroupRequest
  {

  -- | The unique group identifier.

  -- Versions: 0+
  syncGroupRequestGroupId :: !(KafkaString)
,

  -- | The generation of the group.

  -- Versions: 0+
  syncGroupRequestGenerationId :: !(Int32)
,

  -- | The member ID assigned by the group.

  -- Versions: 0+
  syncGroupRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 3+
  syncGroupRequestGroupInstanceId :: !(KafkaString)
,

  -- | The group protocol type.

  -- Versions: 5+
  syncGroupRequestProtocolType :: !(KafkaString)
,

  -- | The group protocol name.

  -- Versions: 5+
  syncGroupRequestProtocolName :: !(KafkaString)
,

  -- | Each assignment.

  -- Versions: 0+
  syncGroupRequestAssignments :: !(KafkaArray (SyncGroupRequestAssignment))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SyncGroupRequest.
maxSyncGroupRequestVersion :: Int16
maxSyncGroupRequestVersion = 5

-- | Encode SyncGroupRequest with the given API version.
encodeSyncGroupRequest :: MonadPut m => E.ApiVersion -> SyncGroupRequest -> m ()
encodeSyncGroupRequest version msg
  | version == 3 =
    do
      serialize (syncGroupRequestGroupId msg)
      serialize (syncGroupRequestGenerationId msg)
      serialize (syncGroupRequestMemberId msg)
      serialize (syncGroupRequestGroupInstanceId msg)
      E.encodeVersionedArray version 4 encodeSyncGroupRequestAssignment (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 4 =
    do
      serialize (toCompactString (syncGroupRequestGroupId msg))
      serialize (syncGroupRequestGenerationId msg)
      serialize (toCompactString (syncGroupRequestMemberId msg))
      serialize (toCompactString (syncGroupRequestGroupInstanceId msg))
      E.encodeVersionedArray version 4 encodeSyncGroupRequestAssignment (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 5 =
    do
      serialize (toCompactString (syncGroupRequestGroupId msg))
      serialize (syncGroupRequestGenerationId msg)
      serialize (toCompactString (syncGroupRequestMemberId msg))
      serialize (toCompactString (syncGroupRequestGroupInstanceId msg))
      serialize (toCompactString (syncGroupRequestProtocolType msg))
      serialize (toCompactString (syncGroupRequestProtocolName msg))
      E.encodeVersionedArray version 4 encodeSyncGroupRequestAssignment (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (syncGroupRequestGroupId msg)
      serialize (syncGroupRequestGenerationId msg)
      serialize (syncGroupRequestMemberId msg)
      E.encodeVersionedArray version 4 encodeSyncGroupRequestAssignment (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SyncGroupRequest with the given API version.
decodeSyncGroupRequest :: MonadGet m => E.ApiVersion -> m SyncGroupRequest
decodeSyncGroupRequest version
  | version == 3 =
    do
      fieldgroupid <- deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- deserialize
      fieldgroupinstanceid <- deserialize
      fieldassignments <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeSyncGroupRequestAssignment
      pure SyncGroupRequest
        {
        syncGroupRequestGroupId = fieldgroupid
        ,
        syncGroupRequestGenerationId = fieldgenerationid
        ,
        syncGroupRequestMemberId = fieldmemberid
        ,
        syncGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        syncGroupRequestProtocolType = P.KafkaString Null
        ,
        syncGroupRequestProtocolName = P.KafkaString Null
        ,
        syncGroupRequestAssignments = fieldassignments
        }

  | version == 4 =
    do
      fieldgroupid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldassignments <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeSyncGroupRequestAssignment
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupRequest
        {
        syncGroupRequestGroupId = fieldgroupid
        ,
        syncGroupRequestGenerationId = fieldgenerationid
        ,
        syncGroupRequestMemberId = fieldmemberid
        ,
        syncGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        syncGroupRequestProtocolType = P.KafkaString Null
        ,
        syncGroupRequestProtocolName = P.KafkaString Null
        ,
        syncGroupRequestAssignments = fieldassignments
        }

  | version == 5 =
    do
      fieldgroupid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocoltype <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocolname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldassignments <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeSyncGroupRequestAssignment
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupRequest
        {
        syncGroupRequestGroupId = fieldgroupid
        ,
        syncGroupRequestGenerationId = fieldgenerationid
        ,
        syncGroupRequestMemberId = fieldmemberid
        ,
        syncGroupRequestGroupInstanceId = fieldgroupinstanceid
        ,
        syncGroupRequestProtocolType = fieldprotocoltype
        ,
        syncGroupRequestProtocolName = fieldprotocolname
        ,
        syncGroupRequestAssignments = fieldassignments
        }

  | version >= 0 && version <= 2 =
    do
      fieldgroupid <- deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- deserialize
      fieldassignments <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeSyncGroupRequestAssignment
      pure SyncGroupRequest
        {
        syncGroupRequestGroupId = fieldgroupid
        ,
        syncGroupRequestGenerationId = fieldgenerationid
        ,
        syncGroupRequestMemberId = fieldmemberid
        ,
        syncGroupRequestGroupInstanceId = P.KafkaString Null
        ,
        syncGroupRequestProtocolType = P.KafkaString Null
        ,
        syncGroupRequestProtocolName = P.KafkaString Null
        ,
        syncGroupRequestAssignments = fieldassignments
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeSyncGroupRequest' / 'decodeSyncGroupRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec SyncGroupRequest where
  wireCodec = Just (WC.serialShimCodec encodeSyncGroupRequest decodeSyncGroupRequest)
  {-# INLINE wireCodec #-}
