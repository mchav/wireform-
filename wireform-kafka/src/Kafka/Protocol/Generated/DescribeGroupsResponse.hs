{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeGroupsResponse
Description : Kafka DescribeGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 15.



Valid versions: 0-6
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeGroupsResponse
  (
    DescribeGroupsResponse(..),
    DescribedGroup(..),
    DescribedGroupMember(..),
    maxDescribeGroupsResponseVersion
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
data DescribedGroupMember = DescribedGroupMember
  {

  -- | The member id.

  -- Versions: 0+
  describedGroupMemberMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 4+
  describedGroupMemberGroupInstanceId :: !(KafkaString)
,

  -- | The client ID used in the member's latest join group request.

  -- Versions: 0+
  describedGroupMemberClientId :: !(KafkaString)
,

  -- | The client host.

  -- Versions: 0+
  describedGroupMemberClientHost :: !(KafkaString)
,

  -- | The metadata corresponding to the current group protocol in use.

  -- Versions: 0+
  describedGroupMemberMemberMetadata :: !(KafkaBytes)
,

  -- | The current assignment provided by the group leader.

  -- Versions: 0+
  describedGroupMemberMemberAssignment :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Each described group.
data DescribedGroup = DescribedGroup
  {

  -- | The describe error, or 0 if there was no error.

  -- Versions: 0+
  describedGroupErrorCode :: !(Int16)
,

  -- | The describe error message, or null if there was no error.

  -- Versions: 6+
  describedGroupErrorMessage :: !(KafkaString)
,

  -- | The group ID string.

  -- Versions: 0+
  describedGroupGroupId :: !(KafkaString)
,

  -- | The group state string, or the empty string.

  -- Versions: 0+
  describedGroupGroupState :: !(KafkaString)
,

  -- | The group protocol type, or the empty string.

  -- Versions: 0+
  describedGroupProtocolType :: !(KafkaString)
,

  -- | The group protocol data, or the empty string.

  -- Versions: 0+
  describedGroupProtocolData :: !(KafkaString)
,

  -- | The group members.

  -- Versions: 0+
  describedGroupMembers :: !(KafkaArray (DescribedGroupMember))
,

  -- | 32-bit bitfield to represent authorized operations for this group.

  -- Versions: 3+
  describedGroupAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


data DescribeGroupsResponse = DescribeGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  describeGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each described group.

  -- Versions: 0+
  describeGroupsResponseGroups :: !(KafkaArray (DescribedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeGroupsResponse.
maxDescribeGroupsResponseVersion :: Int16
maxDescribeGroupsResponseVersion = 6

-- | KafkaMessage instance for DescribeGroupsResponse.
instance KafkaMessage DescribeGroupsResponse where
  messageApiKey = 15
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 5

-- | Worst-case wire size of a DescribedGroupMember.
wireMaxSizeDescribedGroupMember :: Int -> DescribedGroupMember -> Int
wireMaxSizeDescribedGroupMember _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describedGroupMemberMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupMemberGroupInstanceId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupMemberClientId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupMemberClientHost msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (describedGroupMemberMemberMetadata msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (describedGroupMemberMemberAssignment msg))
  + 1

-- | Direct-poke encoder for DescribedGroupMember.
wirePokeDescribedGroupMember :: Int -> Ptr Word8 -> DescribedGroupMember -> IO (Ptr Word8)
wirePokeDescribedGroupMember version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 5 then WP.pokeCompactString p0 (P.toCompactString (describedGroupMemberMemberId msg)) else WP.pokeKafkaString p0 (describedGroupMemberMemberId msg))
  p2 <- (if version >= 4 then (if version >= 5 then WP.pokeCompactString p1 (P.toCompactString (describedGroupMemberGroupInstanceId msg)) else WP.pokeKafkaString p1 (describedGroupMemberGroupInstanceId msg)) else pure p1)
  p3 <- (if version >= 5 then WP.pokeCompactString p2 (P.toCompactString (describedGroupMemberClientId msg)) else WP.pokeKafkaString p2 (describedGroupMemberClientId msg))
  p4 <- (if version >= 5 then WP.pokeCompactString p3 (P.toCompactString (describedGroupMemberClientHost msg)) else WP.pokeKafkaString p3 (describedGroupMemberClientHost msg))
  p5 <- (if version >= 5 then WP.pokeCompactBytes p4 (P.toCompactBytes (describedGroupMemberMemberMetadata msg)) else WP.pokeKafkaBytes p4 (describedGroupMemberMemberMetadata msg))
  p6 <- (if version >= 5 then WP.pokeCompactBytes p5 (P.toCompactBytes (describedGroupMemberMemberAssignment msg)) else WP.pokeKafkaBytes p5 (describedGroupMemberMemberAssignment msg))
  if version >= 5 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for DescribedGroupMember.
wirePeekDescribedGroupMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroupMember, Ptr Word8)
wirePeekDescribedGroupMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_groupinstanceid, p2) <- (if version >= 4 then (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_clientid, p3) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  (f3_clienthost, p4) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_membermetadata, p5) <- (if version >= 5 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p4 endPtr else WP.peekKafkaBytes p4 endPtr)
  (f5_memberassignment, p6) <- (if version >= 5 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p5 endPtr else WP.peekKafkaBytes p5 endPtr)
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (DescribedGroupMember { describedGroupMemberMemberId = f0_memberid, describedGroupMemberGroupInstanceId = f1_groupinstanceid, describedGroupMemberClientId = f2_clientid, describedGroupMemberClientHost = f3_clienthost, describedGroupMemberMemberMetadata = f4_membermetadata, describedGroupMemberMemberAssignment = f5_memberassignment }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribedGroupMember :: DescribedGroupMember
defaultDescribedGroupMember = DescribedGroupMember { describedGroupMemberMemberId = P.KafkaString Null, describedGroupMemberGroupInstanceId = P.KafkaString Null, describedGroupMemberClientId = P.KafkaString Null, describedGroupMemberClientHost = P.KafkaString Null, describedGroupMemberMemberMetadata = P.KafkaBytes Null, describedGroupMemberMemberAssignment = P.KafkaBytes Null }

-- | Worst-case wire size of a DescribedGroup.
wireMaxSizeDescribedGroup :: Int -> DescribedGroup -> Int
wireMaxSizeDescribedGroup _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describedGroupErrorMessage msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupId msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupGroupState msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupProtocolType msg))
  + WP.compactStringMaxSize (P.toCompactString (describedGroupProtocolData msg))
  + (5 + (case P.unKafkaArray (describedGroupMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedGroupMember _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribedGroup.
wirePokeDescribedGroup :: Int -> Ptr Word8 -> DescribedGroup -> IO (Ptr Word8)
wirePokeDescribedGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describedGroupErrorCode msg)
  p2 <- (if version >= 6 then (if version >= 5 then WP.pokeCompactString p1 (P.toCompactString (describedGroupErrorMessage msg)) else WP.pokeKafkaString p1 (describedGroupErrorMessage msg)) else pure p1)
  p3 <- (if version >= 5 then WP.pokeCompactString p2 (P.toCompactString (describedGroupGroupId msg)) else WP.pokeKafkaString p2 (describedGroupGroupId msg))
  p4 <- (if version >= 5 then WP.pokeCompactString p3 (P.toCompactString (describedGroupGroupState msg)) else WP.pokeKafkaString p3 (describedGroupGroupState msg))
  p5 <- (if version >= 5 then WP.pokeCompactString p4 (P.toCompactString (describedGroupProtocolType msg)) else WP.pokeKafkaString p4 (describedGroupProtocolType msg))
  p6 <- (if version >= 5 then WP.pokeCompactString p5 (P.toCompactString (describedGroupProtocolData msg)) else WP.pokeKafkaString p5 (describedGroupProtocolData msg))
  p7 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroupMember version p x) p6 (describedGroupMembers msg)
  p8 <- (if version >= 3 then W.pokeInt32BE p7 (describedGroupAuthorizedOperations msg) else pure p7)
  if version >= 5 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for DescribedGroup.
wirePeekDescribedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroup, Ptr Word8)
wirePeekDescribedGroup version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 6 then (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_groupid, p3) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  (f3_groupstate, p4) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_protocoltype, p5) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
  (f5_protocoldata, p6) <- (if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
  (f6_members, p7) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroupMember version _fp _basePtr p e) p6 endPtr
  (f7_authorizedoperations, p8) <- (if version >= 3 then W.peekInt32BE p7 endPtr else pure (0, p7))
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (DescribedGroup { describedGroupErrorCode = f0_errorcode, describedGroupErrorMessage = f1_errormessage, describedGroupGroupId = f2_groupid, describedGroupGroupState = f3_groupstate, describedGroupProtocolType = f4_protocoltype, describedGroupProtocolData = f5_protocoldata, describedGroupMembers = f6_members, describedGroupAuthorizedOperations = f7_authorizedoperations }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribedGroup :: DescribedGroup
defaultDescribedGroup = DescribedGroup { describedGroupErrorCode = 0, describedGroupErrorMessage = P.KafkaString Null, describedGroupGroupId = P.KafkaString Null, describedGroupGroupState = P.KafkaString Null, describedGroupProtocolType = P.KafkaString Null, describedGroupProtocolData = P.KafkaString Null, describedGroupMembers = P.mkKafkaArray V.empty, describedGroupAuthorizedOperations = 0 }

-- | Worst-case wire size of a DescribeGroupsResponse.
wireMaxSizeDescribeGroupsResponse :: Int -> DescribeGroupsResponse -> Int
wireMaxSizeDescribeGroupsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeGroupsResponse.
wirePokeDescribeGroupsResponse :: Int -> Ptr Word8 -> DescribeGroupsResponse -> IO (Ptr Word8)
wirePokeDescribeGroupsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroup version p x) p0 (describeGroupsResponseGroups msg)
    pure p1
  | version >= 5 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (describeGroupsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroup version p x) p1 (describeGroupsResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (describeGroupsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroup version p x) p1 (describeGroupsResponseGroups msg)
    pure p2
  | otherwise = error $ "wirePoke DescribeGroupsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeGroupsResponse.
wirePeekDescribeGroupsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeGroupsResponse, Ptr Word8)
wirePeekDescribeGroupsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p0 endPtr
    pure (DescribeGroupsResponse { describeGroupsResponseThrottleTimeMs = 0, describeGroupsResponseGroups = f0_groups }, p1)
  | version >= 5 && version <= 6 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_groups, p2) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeGroupsResponse { describeGroupsResponseThrottleTimeMs = f0_throttletimems, describeGroupsResponseGroups = f1_groups }, pTagsEnd)
  | version >= 1 && version <= 4 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_groups, p2) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pure (DescribeGroupsResponse { describeGroupsResponseThrottleTimeMs = f0_throttletimems, describeGroupsResponseGroups = f1_groups }, p2)
  | otherwise = error $ "wirePeek DescribeGroupsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeGroupsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeGroupsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeGroupsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeGroupsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}