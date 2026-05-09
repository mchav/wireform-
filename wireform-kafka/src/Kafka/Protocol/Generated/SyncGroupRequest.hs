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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | KafkaMessage instance for SyncGroupRequest.
instance KafkaMessage SyncGroupRequest where
  messageApiKey = 14
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 4

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

-- | Worst-case wire size of a SyncGroupRequestAssignment.
wireMaxSizeSyncGroupRequestAssignment :: Int -> SyncGroupRequestAssignment -> Int
wireMaxSizeSyncGroupRequestAssignment _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestAssignmentMemberId msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (syncGroupRequestAssignmentAssignment msg))
  + 1

-- | Direct-poke encoder for SyncGroupRequestAssignment.
wirePokeSyncGroupRequestAssignment :: Int -> Ptr Word8 -> SyncGroupRequestAssignment -> IO (Ptr Word8)
wirePokeSyncGroupRequestAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestAssignmentMemberId msg))
  p2 <- WP.pokeCompactBytes p1 (P.toCompactBytes (syncGroupRequestAssignmentAssignment msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for SyncGroupRequestAssignment.
wirePeekSyncGroupRequestAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupRequestAssignment, Ptr Word8)
wirePeekSyncGroupRequestAssignment version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_assignment, p2) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (SyncGroupRequestAssignment { syncGroupRequestAssignmentMemberId = f0_memberid, syncGroupRequestAssignmentAssignment = f1_assignment }, pTagsEnd)

-- | Worst-case wire size of a SyncGroupRequest.
wireMaxSizeSyncGroupRequest :: Int -> SyncGroupRequest -> Int
wireMaxSizeSyncGroupRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestGroupId msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestGroupInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestProtocolType msg))
  + WP.compactStringMaxSize (P.toCompactString (syncGroupRequestProtocolName msg))
  + (5 + (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeSyncGroupRequestAssignment _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for SyncGroupRequest.
wirePokeSyncGroupRequest :: Int -> Ptr Word8 -> SyncGroupRequest -> IO (Ptr Word8)
wirePokeSyncGroupRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg))
    p5 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p4 (syncGroupRequestAssignments msg)
    pure p5
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg))
    p5 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p4 (syncGroupRequestAssignments msg)
    WP.pokeEmptyTaggedFields p5
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg))
    p5 <- WP.pokeCompactString p4 (P.toCompactString (syncGroupRequestProtocolType msg))
    p6 <- WP.pokeCompactString p5 (P.toCompactString (syncGroupRequestProtocolName msg))
    p7 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p6 (syncGroupRequestAssignments msg)
    WP.pokeEmptyTaggedFields p7
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg))
    p4 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p3 (syncGroupRequestAssignments msg)
    pure p4
  | otherwise = error $ "wirePoke SyncGroupRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for SyncGroupRequest.
wirePeekSyncGroupRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupRequest, Ptr Word8)
wirePeekSyncGroupRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_groupinstanceid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_assignments, p5) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p4 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f4_assignments }, p5)
  | version == 4 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_groupinstanceid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_assignments, p5) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f4_assignments }, pTagsEnd)
  | version == 5 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_groupinstanceid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_protocoltype, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_protocolname, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
    (f6_assignments, p7) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = f4_protocoltype, syncGroupRequestProtocolName = f5_protocolname, syncGroupRequestAssignments = f6_assignments }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_assignments, p4) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p3 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = P.KafkaString Null, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f3_assignments }, p4)
  | otherwise = error $ "wirePeek SyncGroupRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec SyncGroupRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSyncGroupRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSyncGroupRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSyncGroupRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}