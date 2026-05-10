{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteAclsRequest
Description : Kafka DeleteAclsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 31.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteAclsRequest
  (
    DeleteAclsRequest(..),
    DeleteAclsFilter(..),
    maxDeleteAclsRequestVersion
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


-- | The filters to use when deleting ACLs.
data DeleteAclsFilter = DeleteAclsFilter
  {

  -- | The resource type.

  -- Versions: 0+
  deleteAclsFilterResourceTypeFilter :: !(Int8)
,

  -- | The resource name, or null to match any resource name.

  -- Versions: 0+
  deleteAclsFilterResourceNameFilter :: !(KafkaString)
,

  -- | The pattern type.

  -- Versions: 1+
  deleteAclsFilterPatternTypeFilter :: !(Int8)
,

  -- | The principal filter, or null to accept all principals.

  -- Versions: 0+
  deleteAclsFilterPrincipalFilter :: !(KafkaString)
,

  -- | The host filter, or null to accept all hosts.

  -- Versions: 0+
  deleteAclsFilterHostFilter :: !(KafkaString)
,

  -- | The ACL operation.

  -- Versions: 0+
  deleteAclsFilterOperation :: !(Int8)
,

  -- | The permission type.

  -- Versions: 0+
  deleteAclsFilterPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


data DeleteAclsRequest = DeleteAclsRequest
  {

  -- | The filters to use when deleting ACLs.

  -- Versions: 0+
  deleteAclsRequestFilters :: !(KafkaArray (DeleteAclsFilter))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteAclsRequest.
maxDeleteAclsRequestVersion :: Int16
maxDeleteAclsRequestVersion = 3

-- | KafkaMessage instance for DeleteAclsRequest.
instance KafkaMessage DeleteAclsRequest where
  messageApiKey = 31
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a DeleteAclsFilter.
wireMaxSizeDeleteAclsFilter :: Int -> DeleteAclsFilter -> Int
wireMaxSizeDeleteAclsFilter _version msg =
  0
  + 1
  + WP.compactStringMaxSize (P.toCompactString (deleteAclsFilterResourceNameFilter msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (deleteAclsFilterPrincipalFilter msg))
  + WP.compactStringMaxSize (P.toCompactString (deleteAclsFilterHostFilter msg))
  + 1
  + 1
  + 1

-- | Direct-poke encoder for DeleteAclsFilter.
wirePokeDeleteAclsFilter :: Int -> Ptr Word8 -> DeleteAclsFilter -> IO (Ptr Word8)
wirePokeDeleteAclsFilter version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (deleteAclsFilterResourceTypeFilter msg))
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (deleteAclsFilterResourceNameFilter msg)) else WP.pokeKafkaString p1 (deleteAclsFilterResourceNameFilter msg))
  p3 <- (if version >= 1 then W.pokeWord8 p2 (fromIntegral (deleteAclsFilterPatternTypeFilter msg)) else pure p2)
  p4 <- (if version >= 2 then WP.pokeCompactString p3 (P.toCompactString (deleteAclsFilterPrincipalFilter msg)) else WP.pokeKafkaString p3 (deleteAclsFilterPrincipalFilter msg))
  p5 <- (if version >= 2 then WP.pokeCompactString p4 (P.toCompactString (deleteAclsFilterHostFilter msg)) else WP.pokeKafkaString p4 (deleteAclsFilterHostFilter msg))
  p6 <- W.pokeWord8 p5 (fromIntegral (deleteAclsFilterOperation msg))
  p7 <- W.pokeWord8 p6 (fromIntegral (deleteAclsFilterPermissionType msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for DeleteAclsFilter.
wirePeekDeleteAclsFilter :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteAclsFilter, Ptr Word8)
wirePeekDeleteAclsFilter version _fp _basePtr p0 endPtr = do
  (f0_resourcetypefilter, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcenamefilter, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_patterntypefilter, p3) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr else pure (0, p2))
  (f3_principalfilter, p4) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_hostfilter, p5) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
  (f5_operation, p6) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr
  (f6_permissiontype, p7) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p6 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (DeleteAclsFilter { deleteAclsFilterResourceTypeFilter = f0_resourcetypefilter, deleteAclsFilterResourceNameFilter = f1_resourcenamefilter, deleteAclsFilterPatternTypeFilter = f2_patterntypefilter, deleteAclsFilterPrincipalFilter = f3_principalfilter, deleteAclsFilterHostFilter = f4_hostfilter, deleteAclsFilterOperation = f5_operation, deleteAclsFilterPermissionType = f6_permissiontype }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeleteAclsFilter :: DeleteAclsFilter
defaultDeleteAclsFilter = DeleteAclsFilter { deleteAclsFilterResourceTypeFilter = 0, deleteAclsFilterResourceNameFilter = P.KafkaString Null, deleteAclsFilterPatternTypeFilter = 0, deleteAclsFilterPrincipalFilter = P.KafkaString Null, deleteAclsFilterHostFilter = P.KafkaString Null, deleteAclsFilterOperation = 0, deleteAclsFilterPermissionType = 0 }

-- | Worst-case wire size of a DeleteAclsRequest.
wireMaxSizeDeleteAclsRequest :: Int -> DeleteAclsRequest -> Int
wireMaxSizeDeleteAclsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (deleteAclsRequestFilters msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteAclsFilter _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteAclsRequest.
wirePokeDeleteAclsRequest :: Int -> Ptr Word8 -> DeleteAclsRequest -> IO (Ptr Word8)
wirePokeDeleteAclsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteAclsFilter version p x) p0 (deleteAclsRequestFilters msg)
    pure p1
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteAclsFilter version p x) p0 (deleteAclsRequestFilters msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DeleteAclsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteAclsRequest.
wirePeekDeleteAclsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteAclsRequest, Ptr Word8)
wirePeekDeleteAclsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_filters, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteAclsFilter version _fp _basePtr p e) p0 endPtr
    pure (DeleteAclsRequest { deleteAclsRequestFilters = f0_filters }, p1)
  | version >= 2 && version <= 3 = do
    (f0_filters, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteAclsFilter version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DeleteAclsRequest { deleteAclsRequestFilters = f0_filters }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteAclsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteAclsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteAclsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteAclsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteAclsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}