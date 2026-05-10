{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListGroupsResponse
Description : Kafka ListGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 16.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListGroupsResponse
  (
    ListGroupsResponse(..),
    ListedGroup(..),
    maxListGroupsResponseVersion
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


-- | Each group in the response.
data ListedGroup = ListedGroup
  {

  -- | The group ID.

  -- Versions: 0+
  listedGroupGroupId :: !(KafkaString)
,

  -- | The group protocol type.

  -- Versions: 0+
  listedGroupProtocolType :: !(KafkaString)
,

  -- | The group state name.

  -- Versions: 4+
  listedGroupGroupState :: !(KafkaString)
,

  -- | The group type name.

  -- Versions: 5+
  listedGroupGroupType :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data ListGroupsResponse = ListGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  listGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  listGroupsResponseErrorCode :: !(Int16)
,

  -- | Each group in the response.

  -- Versions: 0+
  listGroupsResponseGroups :: !(KafkaArray (ListedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListGroupsResponse.
maxListGroupsResponseVersion :: Int16
maxListGroupsResponseVersion = 5

-- | KafkaMessage instance for ListGroupsResponse.
instance KafkaMessage ListGroupsResponse where
  messageApiKey = 16
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a ListedGroup.
wireMaxSizeListedGroup :: Int -> ListedGroup -> Int
wireMaxSizeListedGroup _version msg =
  0
  + WP.dualStringMaxSize (listedGroupGroupId msg)
  + WP.dualStringMaxSize (listedGroupProtocolType msg)
  + WP.dualStringMaxSize (listedGroupGroupState msg)
  + WP.dualStringMaxSize (listedGroupGroupType msg)
  + 1

-- | Direct-poke encoder for ListedGroup.
wirePokeListedGroup :: Int -> Ptr Word8 -> ListedGroup -> IO (Ptr Word8)
wirePokeListedGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (listedGroupGroupId msg)) else WP.pokeKafkaString p0 (listedGroupGroupId msg))
  p2 <- (if version >= 3 then WP.pokeCompactString p1 (P.toCompactString (listedGroupProtocolType msg)) else WP.pokeKafkaString p1 (listedGroupProtocolType msg))
  p3 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (listedGroupGroupState msg)) else WP.pokeKafkaString p2 (listedGroupGroupState msg)) else pure p2)
  p4 <- (if version >= 5 then (if version >= 3 then WP.pokeCompactString p3 (P.toCompactString (listedGroupGroupType msg)) else WP.pokeKafkaString p3 (listedGroupGroupType msg)) else pure p3)
  if version >= 3 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for ListedGroup.
wirePeekListedGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListedGroup, Ptr Word8)
wirePeekListedGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_protocoltype, p2) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_groupstate, p3) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
  (f3_grouptype, p4) <- (if version >= 5 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (ListedGroup { listedGroupGroupId = f0_groupid, listedGroupProtocolType = f1_protocoltype, listedGroupGroupState = f2_groupstate, listedGroupGroupType = f3_grouptype }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListedGroup :: ListedGroup
defaultListedGroup = ListedGroup { listedGroupGroupId = P.KafkaString Null, listedGroupProtocolType = P.KafkaString Null, listedGroupGroupState = P.KafkaString Null, listedGroupGroupType = P.KafkaString Null }

-- | Worst-case wire size of a ListGroupsResponse.
wireMaxSizeListGroupsResponse :: Int -> ListGroupsResponse -> Int
wireMaxSizeListGroupsResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (listGroupsResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListedGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListGroupsResponse.
wirePokeListGroupsResponse :: Int -> Ptr Word8 -> ListGroupsResponse -> IO (Ptr Word8)
wirePokeListGroupsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (listGroupsResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeListedGroup version p x) p1 (listGroupsResponseGroups msg)
    pure p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (listGroupsResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (listGroupsResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeListedGroup version p x) p2 (listGroupsResponseGroups msg)
    pure p3
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (listGroupsResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (listGroupsResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeListedGroup version p x) p2 (listGroupsResponseGroups msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ListGroupsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ListGroupsResponse.
wirePeekListGroupsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListGroupsResponse, Ptr Word8)
wirePeekListGroupsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekListedGroup version _fp _basePtr p e) p1 endPtr
    pure (ListGroupsResponse { listGroupsResponseThrottleTimeMs = 0, listGroupsResponseErrorCode = f0_errorcode, listGroupsResponseGroups = f1_groups }, p2)
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_groups, p3) <- WP.peekVersionedArray version 3 (\p e -> wirePeekListedGroup version _fp _basePtr p e) p2 endPtr
    pure (ListGroupsResponse { listGroupsResponseThrottleTimeMs = f0_throttletimems, listGroupsResponseErrorCode = f1_errorcode, listGroupsResponseGroups = f2_groups }, p3)
  | version >= 3 && version <= 5 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_groups, p3) <- WP.peekVersionedArray version 3 (\p e -> wirePeekListedGroup version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ListGroupsResponse { listGroupsResponseThrottleTimeMs = f0_throttletimems, listGroupsResponseErrorCode = f1_errorcode, listGroupsResponseGroups = f2_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek ListGroupsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ListGroupsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListGroupsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListGroupsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListGroupsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}