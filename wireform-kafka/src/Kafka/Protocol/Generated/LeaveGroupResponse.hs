{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaveGroupResponse
Description : Kafka LeaveGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 13.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaveGroupResponse
  (
    LeaveGroupResponse(..),
    MemberResponse(..),
    encodeLeaveGroupResponse,
    decodeLeaveGroupResponse,
    maxLeaveGroupResponseVersion
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


-- | List of leaving member responses.
data MemberResponse = MemberResponse
  {

  -- | The member ID to remove from the group.

  -- Versions: 3+
  memberResponseMemberId :: !(KafkaString)
,

  -- | The group instance ID to remove from the group.

  -- Versions: 3+
  memberResponseGroupInstanceId :: !(KafkaString)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 3+
  memberResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode MemberResponse with version-aware field handling.
encodeMemberResponse :: MonadPut m => E.ApiVersion -> MemberResponse -> m ()
encodeMemberResponse version mmsg =
  do
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberResponseMemberId mmsg)) else serialize (memberResponseMemberId mmsg)
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (memberResponseGroupInstanceId mmsg)) else serialize (memberResponseGroupInstanceId mmsg)
    when (version >= 3) $
      serialize (memberResponseErrorCode mmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MemberResponse with version-aware field handling.
decodeMemberResponse :: MonadGet m => E.ApiVersion -> m MemberResponse
decodeMemberResponse version =
  do
    fieldmemberid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgroupinstanceid <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MemberResponse
      {
      memberResponseMemberId = fieldmemberid
      ,
      memberResponseGroupInstanceId = fieldgroupinstanceid
      ,
      memberResponseErrorCode = fielderrorcode
      }



data LeaveGroupResponse = LeaveGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  leaveGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  leaveGroupResponseErrorCode :: !(Int16)
,

  -- | List of leaving member responses.

  -- Versions: 3+
  leaveGroupResponseMembers :: !(KafkaArray (MemberResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaveGroupResponse.
maxLeaveGroupResponseVersion :: Int16
maxLeaveGroupResponseVersion = 5

-- | KafkaMessage instance for LeaveGroupResponse.
instance KafkaMessage LeaveGroupResponse where
  messageApiKey = 13
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 4

-- | Encode LeaveGroupResponse with the given API version.
encodeLeaveGroupResponse :: MonadPut m => E.ApiVersion -> LeaveGroupResponse -> m ()
encodeLeaveGroupResponse version msg
  | version == 0 =
    do
      serialize (leaveGroupResponseErrorCode msg)


  | version == 3 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)
      E.encodeVersionedArray version 4 encodeMemberResponse (case P.unKafkaArray (leaveGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 1 && version <= 2 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)


  | version >= 4 && version <= 5 =
    do
      serialize (leaveGroupResponseThrottleTimeMs msg)
      serialize (leaveGroupResponseErrorCode msg)
      E.encodeVersionedArray version 4 encodeMemberResponse (case P.unKafkaArray (leaveGroupResponseMembers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode LeaveGroupResponse with the given API version.
decodeLeaveGroupResponse :: MonadGet m => E.ApiVersion -> m LeaveGroupResponse
decodeLeaveGroupResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = 0
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberResponse
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = fieldmembers
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldmembers <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeMemberResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure LeaveGroupResponse
        {
        leaveGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        leaveGroupResponseErrorCode = fielderrorcode
        ,
        leaveGroupResponseMembers = fieldmembers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a MemberResponse.
wireMaxSizeMemberResponse :: Int -> MemberResponse -> Int
wireMaxSizeMemberResponse _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (memberResponseMemberId msg))
  + WP.compactStringMaxSize (P.toCompactString (memberResponseGroupInstanceId msg))
  + 2
  + 1

-- | Direct-poke encoder for MemberResponse.
wirePokeMemberResponse :: Int -> Ptr Word8 -> MemberResponse -> IO (Ptr Word8)
wirePokeMemberResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (memberResponseMemberId msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (memberResponseGroupInstanceId msg))
  p3 <- W.pokeInt16BE p2 (memberResponseErrorCode msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for MemberResponse.
wirePeekMemberResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MemberResponse, Ptr Word8)
wirePeekMemberResponse version _fp _basePtr p0 endPtr = do
  (f0_memberid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_groupinstanceid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (MemberResponse { memberResponseMemberId = f0_memberid, memberResponseGroupInstanceId = f1_groupinstanceid, memberResponseErrorCode = f2_errorcode }, pTagsEnd)

-- | Worst-case wire size of a LeaveGroupResponse.
wireMaxSizeLeaveGroupResponse :: Int -> LeaveGroupResponse -> Int
wireMaxSizeLeaveGroupResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (leaveGroupResponseMembers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMemberResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for LeaveGroupResponse.
wirePokeLeaveGroupResponse :: Int -> Ptr Word8 -> LeaveGroupResponse -> IO (Ptr Word8)
wirePokeLeaveGroupResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (leaveGroupResponseErrorCode msg)
    pure p1
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (leaveGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (leaveGroupResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberResponse version p x) p2 (leaveGroupResponseMembers msg)
    pure p3
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (leaveGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (leaveGroupResponseErrorCode msg)
    pure p2
  | version >= 4 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (leaveGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (leaveGroupResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeMemberResponse version p x) p2 (leaveGroupResponseMembers msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke LeaveGroupResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for LeaveGroupResponse.
wirePeekLeaveGroupResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaveGroupResponse, Ptr Word8)
wirePeekLeaveGroupResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    pure (LeaveGroupResponse { leaveGroupResponseThrottleTimeMs = 0, leaveGroupResponseErrorCode = f0_errorcode, leaveGroupResponseMembers = P.mkKafkaArray V.empty }, p1)
  | version == 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_members, p3) <- WP.peekVersionedArray version 4 (\p e -> wirePeekMemberResponse version _fp _basePtr p e) p2 endPtr
    pure (LeaveGroupResponse { leaveGroupResponseThrottleTimeMs = f0_throttletimems, leaveGroupResponseErrorCode = f1_errorcode, leaveGroupResponseMembers = f2_members }, p3)
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pure (LeaveGroupResponse { leaveGroupResponseThrottleTimeMs = f0_throttletimems, leaveGroupResponseErrorCode = f1_errorcode, leaveGroupResponseMembers = P.mkKafkaArray V.empty }, p2)
  | version >= 4 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_members, p3) <- WP.peekVersionedArray version 4 (\p e -> wirePeekMemberResponse version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (LeaveGroupResponse { leaveGroupResponseThrottleTimeMs = f0_throttletimems, leaveGroupResponseErrorCode = f1_errorcode, leaveGroupResponseMembers = f2_members }, pTagsEnd)
  | otherwise = error $ "wirePeek LeaveGroupResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec LeaveGroupResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaveGroupResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaveGroupResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaveGroupResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}