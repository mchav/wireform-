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
    maxSyncGroupRequestVersion
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

-- | Worst-case wire size of a SyncGroupRequestAssignment.
wireMaxSizeSyncGroupRequestAssignment :: Int -> SyncGroupRequestAssignment -> Int
wireMaxSizeSyncGroupRequestAssignment _version msg =
  0
  + WP.dualStringMaxSize (syncGroupRequestAssignmentMemberId msg)
  + WP.dualBytesMaxSize (syncGroupRequestAssignmentAssignment msg)
  + 1

-- | Direct-poke encoder for SyncGroupRequestAssignment.
wirePokeSyncGroupRequestAssignment :: Int -> Ptr Word8 -> SyncGroupRequestAssignment -> IO (Ptr Word8)
wirePokeSyncGroupRequestAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestAssignmentMemberId msg)) else WP.pokeKafkaString p0 (syncGroupRequestAssignmentMemberId msg))
  p2 <- (if version >= 4 then WP.pokeCompactBytes p1 (P.toCompactBytes (syncGroupRequestAssignmentAssignment msg)) else WP.pokeKafkaBytes p1 (syncGroupRequestAssignmentAssignment msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for SyncGroupRequestAssignment.
wirePeekSyncGroupRequestAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupRequestAssignment, Ptr Word8)
wirePeekSyncGroupRequestAssignment version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_assignment, p2) <- (if version >= 4 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr else WP.peekKafkaBytes p1 endPtr)
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (SyncGroupRequestAssignment { syncGroupRequestAssignmentMemberId = f0_memberid, syncGroupRequestAssignmentAssignment = f1_assignment }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultSyncGroupRequestAssignment :: SyncGroupRequestAssignment
defaultSyncGroupRequestAssignment = SyncGroupRequestAssignment { syncGroupRequestAssignmentMemberId = P.KafkaString Null, syncGroupRequestAssignmentAssignment = P.KafkaBytes Null }

-- | Worst-case wire size of a SyncGroupRequest.
wireMaxSizeSyncGroupRequest :: Int -> SyncGroupRequest -> Int
wireMaxSizeSyncGroupRequest _version msg =
  0
  + WP.dualStringMaxSize (syncGroupRequestGroupId msg)
  + 4
  + WP.dualStringMaxSize (syncGroupRequestMemberId msg)
  + WP.dualStringMaxSize (syncGroupRequestGroupInstanceId msg)
  + WP.dualStringMaxSize (syncGroupRequestProtocolType msg)
  + WP.dualStringMaxSize (syncGroupRequestProtocolName msg)
  + (5 + (case P.unKafkaArray (syncGroupRequestAssignments msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeSyncGroupRequestAssignment _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for SyncGroupRequest.
wirePokeSyncGroupRequest :: Int -> Ptr Word8 -> SyncGroupRequest -> IO (Ptr Word8)
wirePokeSyncGroupRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg)) else WP.pokeKafkaString p2 (syncGroupRequestMemberId msg))
    p4 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (syncGroupRequestGroupInstanceId msg)) else pure p3)
    p5 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p4 (syncGroupRequestAssignments msg)
    pure p5
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg)) else WP.pokeKafkaString p2 (syncGroupRequestMemberId msg))
    p4 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (syncGroupRequestGroupInstanceId msg)) else pure p3)
    p5 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p4 (syncGroupRequestAssignments msg)
    WP.pokeEmptyTaggedFields p5
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg)) else WP.pokeKafkaString p2 (syncGroupRequestMemberId msg))
    p4 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (syncGroupRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (syncGroupRequestGroupInstanceId msg)) else pure p3)
    p5 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p4 (P.toCompactString (syncGroupRequestProtocolType msg)) else WP.pokeKafkaString p4 (syncGroupRequestProtocolType msg)) else pure p4)
    p6 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p5 (P.toCompactString (syncGroupRequestProtocolName msg)) else WP.pokeKafkaString p5 (syncGroupRequestProtocolName msg)) else pure p5)
    p7 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p6 (syncGroupRequestAssignments msg)
    WP.pokeEmptyTaggedFields p7
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (syncGroupRequestGroupId msg)) else WP.pokeKafkaString p0 (syncGroupRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (syncGroupRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (syncGroupRequestMemberId msg)) else WP.pokeKafkaString p2 (syncGroupRequestMemberId msg))
    p4 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeSyncGroupRequestAssignment version p x) p3 (syncGroupRequestAssignments msg)
    pure p4
  | otherwise = error $ "wirePoke SyncGroupRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for SyncGroupRequest.
wirePeekSyncGroupRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupRequest, Ptr Word8)
wirePeekSyncGroupRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_groupinstanceid, p4) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_assignments, p5) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p4 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f4_assignments }, p5)
  | version == 4 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_groupinstanceid, p4) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_assignments, p5) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f4_assignments }, pTagsEnd)
  | version == 5 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_groupinstanceid, p4) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_protocoltype, p5) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_protocolname, p6) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr) else pure (P.KafkaString Null, p5))
    (f6_assignments, p7) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = f3_groupinstanceid, syncGroupRequestProtocolType = f4_protocoltype, syncGroupRequestProtocolName = f5_protocolname, syncGroupRequestAssignments = f6_assignments }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_assignments, p4) <- WP.peekVersionedArray version 4 (\p e -> wirePeekSyncGroupRequestAssignment version _fp _basePtr p e) p3 endPtr
    pure (SyncGroupRequest { syncGroupRequestGroupId = f0_groupid, syncGroupRequestGenerationId = f1_generationid, syncGroupRequestMemberId = f2_memberid, syncGroupRequestGroupInstanceId = P.KafkaString Null, syncGroupRequestProtocolType = P.KafkaString Null, syncGroupRequestProtocolName = P.KafkaString Null, syncGroupRequestAssignments = f3_assignments }, p4)
  | otherwise = error $ "wirePeek SyncGroupRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec SyncGroupRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSyncGroupRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSyncGroupRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSyncGroupRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}