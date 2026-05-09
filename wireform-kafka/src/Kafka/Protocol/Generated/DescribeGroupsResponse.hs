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
    encodeDescribeGroupsResponse,
    decodeDescribeGroupsResponse,
    maxDescribeGroupsResponseVersion
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


-- | Encode DescribedGroupMember with version-aware field handling.
encodeDescribedGroupMember :: MonadPut m => E.ApiVersion -> DescribedGroupMember -> m ()
encodeDescribedGroupMember version dmsg =
  do
    if version >= 5 then serialize (toCompactString (describedGroupMemberMemberId dmsg)) else serialize (describedGroupMemberMemberId dmsg)
    when (version >= 4) $
      if version >= 5 then serialize (toCompactString (describedGroupMemberGroupInstanceId dmsg)) else serialize (describedGroupMemberGroupInstanceId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupMemberClientId dmsg)) else serialize (describedGroupMemberClientId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupMemberClientHost dmsg)) else serialize (describedGroupMemberClientHost dmsg)
    if version >= 5 then serialize (toCompactBytes (describedGroupMemberMemberMetadata dmsg)) else serialize (describedGroupMemberMemberMetadata dmsg)
    if version >= 5 then serialize (toCompactBytes (describedGroupMemberMemberAssignment dmsg)) else serialize (describedGroupMemberMemberAssignment dmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroupMember with version-aware field handling.
decodeDescribedGroupMember :: MonadGet m => E.ApiVersion -> m DescribedGroupMember
decodeDescribedGroupMember version =
  do
    fieldmemberid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupinstanceid <- if version >= 4
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldclientid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldclienthost <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldmembermetadata <- if version >= 5 then P.fromCompactBytes <$> deserialize else deserialize
    fieldmemberassignment <- if version >= 5 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedGroupMember
      {
      describedGroupMemberMemberId = fieldmemberid
      ,
      describedGroupMemberGroupInstanceId = fieldgroupinstanceid
      ,
      describedGroupMemberClientId = fieldclientid
      ,
      describedGroupMemberClientHost = fieldclienthost
      ,
      describedGroupMemberMemberMetadata = fieldmembermetadata
      ,
      describedGroupMemberMemberAssignment = fieldmemberassignment
      }


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


-- | Encode DescribedGroup with version-aware field handling.
encodeDescribedGroup :: MonadPut m => E.ApiVersion -> DescribedGroup -> m ()
encodeDescribedGroup version dmsg =
  do
    serialize (describedGroupErrorCode dmsg)
    when (version >= 6) $
      if version >= 5 then serialize (toCompactString (describedGroupErrorMessage dmsg)) else serialize (describedGroupErrorMessage dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupGroupId dmsg)) else serialize (describedGroupGroupId dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupGroupState dmsg)) else serialize (describedGroupGroupState dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupProtocolType dmsg)) else serialize (describedGroupProtocolType dmsg)
    if version >= 5 then serialize (toCompactString (describedGroupProtocolData dmsg)) else serialize (describedGroupProtocolData dmsg)
    E.encodeVersionedArray version 5 encodeDescribedGroupMember (case P.unKafkaArray (describedGroupMembers dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $
      serialize (describedGroupAuthorizedOperations dmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedGroup with version-aware field handling.
decodeDescribedGroup :: MonadGet m => E.ApiVersion -> m DescribedGroup
decodeDescribedGroup version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 6
      then if version >= 5 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgroupid <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupstate <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldprotocoltype <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldprotocoldata <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroupMember
    fieldauthorizedoperations <- if version >= 3
      then deserialize
      else pure ((-2147483648))
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedGroup
      {
      describedGroupErrorCode = fielderrorcode
      ,
      describedGroupErrorMessage = fielderrormessage
      ,
      describedGroupGroupId = fieldgroupid
      ,
      describedGroupGroupState = fieldgroupstate
      ,
      describedGroupProtocolType = fieldprotocoltype
      ,
      describedGroupProtocolData = fieldprotocoldata
      ,
      describedGroupMembers = fieldmembers
      ,
      describedGroupAuthorizedOperations = fieldauthorizedoperations
      }



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

-- | Encode DescribeGroupsResponse with the given API version.
encodeDescribeGroupsResponse :: MonadPut m => E.ApiVersion -> DescribeGroupsResponse -> m ()
encodeDescribeGroupsResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 5 && version <= 6 =
    do
      serialize (describeGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 4 =
    do
      serialize (describeGroupsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 5 encodeDescribedGroup (case P.unKafkaArray (describeGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeGroupsResponse with the given API version.
decodeDescribeGroupsResponse :: MonadGet m => E.ApiVersion -> m DescribeGroupsResponse
decodeDescribeGroupsResponse version
  | version == 0 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = 0
        ,
        describeGroupsResponseGroups = fieldgroups
        }

  | version >= 5 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeGroupsResponseGroups = fieldgroups
        }

  | version >= 1 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeDescribedGroup
      pure DescribeGroupsResponse
        {
        describeGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeGroupsResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describedGroupMemberMemberId msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedGroupMemberGroupInstanceId msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describedGroupMemberClientId msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describedGroupMemberClientHost msg))
  p5 <- WP.pokeCompactBytes p4 (P.toCompactBytes (describedGroupMemberMemberMetadata msg))
  p6 <- WP.pokeCompactBytes p5 (P.toCompactBytes (describedGroupMemberMemberAssignment msg))
  if version >= 5 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for DescribedGroupMember.
wirePeekDescribedGroupMember :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroupMember, Ptr Word8)
wirePeekDescribedGroupMember version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_groupinstanceid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_clientid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_clienthost, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_membermetadata, p5) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p4 endPtr
  (f5_memberassignment, p6) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p5 endPtr
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (DescribedGroupMember { describedGroupMemberMemberId = f0_memberid, describedGroupMemberGroupInstanceId = f1_groupinstanceid, describedGroupMemberClientId = f2_clientid, describedGroupMemberClientHost = f3_clienthost, describedGroupMemberMemberMetadata = f4_membermetadata, describedGroupMemberMemberAssignment = f5_memberassignment }, pTagsEnd)

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
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedGroupErrorMessage msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describedGroupGroupId msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describedGroupGroupState msg))
  p5 <- WP.pokeCompactString p4 (P.toCompactString (describedGroupProtocolType msg))
  p6 <- WP.pokeCompactString p5 (P.toCompactString (describedGroupProtocolData msg))
  p7 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroupMember version p x) p6 (describedGroupMembers msg)
  p8 <- W.pokeInt32BE p7 (describedGroupAuthorizedOperations msg)
  if version >= 5 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for DescribedGroup.
wirePeekDescribedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedGroup, Ptr Word8)
wirePeekDescribedGroup version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_groupid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_groupstate, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_protocoltype, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
  (f5_protocoldata, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
  (f6_members, p7) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroupMember version _fp _basePtr p e) p6 endPtr
  (f7_authorizedoperations, p8) <- W.peekInt32BE p7 endPtr
  pTagsEnd <- if version >= 5 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (DescribedGroup { describedGroupErrorCode = f0_errorcode, describedGroupErrorMessage = f1_errormessage, describedGroupGroupId = f2_groupid, describedGroupGroupState = f3_groupstate, describedGroupProtocolType = f4_protocoltype, describedGroupProtocolData = f5_protocoldata, describedGroupMembers = f6_members, describedGroupAuthorizedOperations = f7_authorizedoperations }, pTagsEnd)

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
    p1 <- W.pokeInt32BE p0 (describeGroupsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 5 (\p x -> wirePokeDescribedGroup version p x) p1 (describeGroupsResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeGroupsResponseThrottleTimeMs msg)
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
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 5 (\p e -> wirePeekDescribedGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeGroupsResponse { describeGroupsResponseThrottleTimeMs = f0_throttletimems, describeGroupsResponseGroups = f1_groups }, pTagsEnd)
  | version >= 1 && version <= 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
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