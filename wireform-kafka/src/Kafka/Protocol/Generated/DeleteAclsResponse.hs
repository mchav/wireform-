{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteAclsResponse
Description : Kafka DeleteAclsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 31.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteAclsResponse
  (
    DeleteAclsResponse(..),
    DeleteAclsFilterResult(..),
    DeleteAclsMatchingAcl(..),
    maxDeleteAclsResponseVersion
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


-- | The ACLs which matched this filter.
data DeleteAclsMatchingAcl = DeleteAclsMatchingAcl
  {

  -- | The deletion error code, or 0 if the deletion succeeded.

  -- Versions: 0+
  deleteAclsMatchingAclErrorCode :: !(Int16)
,

  -- | The deletion error message, or null if the deletion succeeded.

  -- Versions: 0+
  deleteAclsMatchingAclErrorMessage :: !(KafkaString)
,

  -- | The ACL resource type.

  -- Versions: 0+
  deleteAclsMatchingAclResourceType :: !(Int8)
,

  -- | The ACL resource name.

  -- Versions: 0+
  deleteAclsMatchingAclResourceName :: !(KafkaString)
,

  -- | The ACL resource pattern type.

  -- Versions: 1+
  deleteAclsMatchingAclPatternType :: !(Int8)
,

  -- | The ACL principal.

  -- Versions: 0+
  deleteAclsMatchingAclPrincipal :: !(KafkaString)
,

  -- | The ACL host.

  -- Versions: 0+
  deleteAclsMatchingAclHost :: !(KafkaString)
,

  -- | The ACL operation.

  -- Versions: 0+
  deleteAclsMatchingAclOperation :: !(Int8)
,

  -- | The ACL permission type.

  -- Versions: 0+
  deleteAclsMatchingAclPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)

-- | The results for each filter.
data DeleteAclsFilterResult = DeleteAclsFilterResult
  {

  -- | The error code, or 0 if the filter succeeded.

  -- Versions: 0+
  deleteAclsFilterResultErrorCode :: !(Int16)
,

  -- | The error message, or null if the filter succeeded.

  -- Versions: 0+
  deleteAclsFilterResultErrorMessage :: !(KafkaString)
,

  -- | The ACLs which matched this filter.

  -- Versions: 0+
  deleteAclsFilterResultMatchingAcls :: !(KafkaArray (DeleteAclsMatchingAcl))

  }
  deriving (Eq, Show, Generic)


data DeleteAclsResponse = DeleteAclsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteAclsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each filter.

  -- Versions: 0+
  deleteAclsResponseFilterResults :: !(KafkaArray (DeleteAclsFilterResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteAclsResponse.
maxDeleteAclsResponseVersion :: Int16
maxDeleteAclsResponseVersion = 3

-- | KafkaMessage instance for DeleteAclsResponse.
instance KafkaMessage DeleteAclsResponse where
  messageApiKey = 31
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a DeleteAclsMatchingAcl.
wireMaxSizeDeleteAclsMatchingAcl :: Int -> DeleteAclsMatchingAcl -> Int
wireMaxSizeDeleteAclsMatchingAcl _version msg =
  0
  + 2
  + WP.dualStringMaxSize (deleteAclsMatchingAclErrorMessage msg)
  + 1
  + WP.dualStringMaxSize (deleteAclsMatchingAclResourceName msg)
  + 1
  + WP.dualStringMaxSize (deleteAclsMatchingAclPrincipal msg)
  + WP.dualStringMaxSize (deleteAclsMatchingAclHost msg)
  + 1
  + 1
  + 1

-- | Direct-poke encoder for DeleteAclsMatchingAcl.
wirePokeDeleteAclsMatchingAcl :: Int -> Ptr Word8 -> DeleteAclsMatchingAcl -> IO (Ptr Word8)
wirePokeDeleteAclsMatchingAcl version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (deleteAclsMatchingAclErrorCode msg)
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (deleteAclsMatchingAclErrorMessage msg)) else WP.pokeKafkaString p1 (deleteAclsMatchingAclErrorMessage msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (deleteAclsMatchingAclResourceType msg))
  p4 <- (if version >= 2 then WP.pokeCompactString p3 (P.toCompactString (deleteAclsMatchingAclResourceName msg)) else WP.pokeKafkaString p3 (deleteAclsMatchingAclResourceName msg))
  p5 <- (if version >= 1 then W.pokeWord8 p4 (fromIntegral (deleteAclsMatchingAclPatternType msg)) else pure p4)
  p6 <- (if version >= 2 then WP.pokeCompactString p5 (P.toCompactString (deleteAclsMatchingAclPrincipal msg)) else WP.pokeKafkaString p5 (deleteAclsMatchingAclPrincipal msg))
  p7 <- (if version >= 2 then WP.pokeCompactString p6 (P.toCompactString (deleteAclsMatchingAclHost msg)) else WP.pokeKafkaString p6 (deleteAclsMatchingAclHost msg))
  p8 <- W.pokeWord8 p7 (fromIntegral (deleteAclsMatchingAclOperation msg))
  p9 <- W.pokeWord8 p8 (fromIntegral (deleteAclsMatchingAclPermissionType msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p9 else pure p9

-- | Direct-poke decoder for DeleteAclsMatchingAcl.
wirePeekDeleteAclsMatchingAcl :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteAclsMatchingAcl, Ptr Word8)
wirePeekDeleteAclsMatchingAcl version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_resourcetype, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_resourcename, p4) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_patterntype, p5) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr else pure (3, p4))
  (f5_principal, p6) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
  (f6_host, p7) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr else WP.peekKafkaString p6 endPtr)
  (f7_operation, p8) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p7 endPtr
  (f8_permissiontype, p9) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p8 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p9 endPtr else pure p9
  pure (DeleteAclsMatchingAcl { deleteAclsMatchingAclErrorCode = f0_errorcode, deleteAclsMatchingAclErrorMessage = f1_errormessage, deleteAclsMatchingAclResourceType = f2_resourcetype, deleteAclsMatchingAclResourceName = f3_resourcename, deleteAclsMatchingAclPatternType = f4_patterntype, deleteAclsMatchingAclPrincipal = f5_principal, deleteAclsMatchingAclHost = f6_host, deleteAclsMatchingAclOperation = f7_operation, deleteAclsMatchingAclPermissionType = f8_permissiontype }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeleteAclsMatchingAcl :: DeleteAclsMatchingAcl
defaultDeleteAclsMatchingAcl = DeleteAclsMatchingAcl { deleteAclsMatchingAclErrorCode = 0, deleteAclsMatchingAclErrorMessage = P.KafkaString Null, deleteAclsMatchingAclResourceType = 0, deleteAclsMatchingAclResourceName = P.KafkaString Null, deleteAclsMatchingAclPatternType = 3, deleteAclsMatchingAclPrincipal = P.KafkaString Null, deleteAclsMatchingAclHost = P.KafkaString Null, deleteAclsMatchingAclOperation = 0, deleteAclsMatchingAclPermissionType = 0 }

-- | Worst-case wire size of a DeleteAclsFilterResult.
wireMaxSizeDeleteAclsFilterResult :: Int -> DeleteAclsFilterResult -> Int
wireMaxSizeDeleteAclsFilterResult _version msg =
  0
  + 2
  + WP.dualStringMaxSize (deleteAclsFilterResultErrorMessage msg)
  + (5 + (case P.unKafkaArray (deleteAclsFilterResultMatchingAcls msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteAclsMatchingAcl _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteAclsFilterResult.
wirePokeDeleteAclsFilterResult :: Int -> Ptr Word8 -> DeleteAclsFilterResult -> IO (Ptr Word8)
wirePokeDeleteAclsFilterResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (deleteAclsFilterResultErrorCode msg)
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (deleteAclsFilterResultErrorMessage msg)) else WP.pokeKafkaString p1 (deleteAclsFilterResultErrorMessage msg))
  p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteAclsMatchingAcl version p x) p2 (deleteAclsFilterResultMatchingAcls msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for DeleteAclsFilterResult.
wirePeekDeleteAclsFilterResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteAclsFilterResult, Ptr Word8)
wirePeekDeleteAclsFilterResult version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_matchingacls, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteAclsMatchingAcl version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (DeleteAclsFilterResult { deleteAclsFilterResultErrorCode = f0_errorcode, deleteAclsFilterResultErrorMessage = f1_errormessage, deleteAclsFilterResultMatchingAcls = f2_matchingacls }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeleteAclsFilterResult :: DeleteAclsFilterResult
defaultDeleteAclsFilterResult = DeleteAclsFilterResult { deleteAclsFilterResultErrorCode = 0, deleteAclsFilterResultErrorMessage = P.KafkaString Null, deleteAclsFilterResultMatchingAcls = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DeleteAclsResponse.
wireMaxSizeDeleteAclsResponse :: Int -> DeleteAclsResponse -> Int
wireMaxSizeDeleteAclsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (deleteAclsResponseFilterResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteAclsFilterResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteAclsResponse.
wirePokeDeleteAclsResponse :: Int -> Ptr Word8 -> DeleteAclsResponse -> IO (Ptr Word8)
wirePokeDeleteAclsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteAclsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteAclsFilterResult version p x) p1 (deleteAclsResponseFilterResults msg)
    pure p2
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteAclsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeleteAclsFilterResult version p x) p1 (deleteAclsResponseFilterResults msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DeleteAclsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteAclsResponse.
wirePeekDeleteAclsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteAclsResponse, Ptr Word8)
wirePeekDeleteAclsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_filterresults, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteAclsFilterResult version _fp _basePtr p e) p1 endPtr
    pure (DeleteAclsResponse { deleteAclsResponseThrottleTimeMs = f0_throttletimems, deleteAclsResponseFilterResults = f1_filterresults }, p2)
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_filterresults, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeleteAclsFilterResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteAclsResponse { deleteAclsResponseThrottleTimeMs = f0_throttletimems, deleteAclsResponseFilterResults = f1_filterresults }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteAclsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DeleteAclsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteAclsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteAclsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteAclsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}