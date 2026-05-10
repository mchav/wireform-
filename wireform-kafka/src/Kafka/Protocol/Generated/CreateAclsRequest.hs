{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateAclsRequest
Description : Kafka CreateAclsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 30.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateAclsRequest
  (
    CreateAclsRequest(..),
    AclCreation(..),
    maxCreateAclsRequestVersion
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


-- | The ACLs that we want to create.
data AclCreation = AclCreation
  {

  -- | The type of the resource.

  -- Versions: 0+
  aclCreationResourceType :: !(Int8)
,

  -- | The resource name for the ACL.

  -- Versions: 0+
  aclCreationResourceName :: !(KafkaString)
,

  -- | The pattern type for the ACL.

  -- Versions: 1+
  aclCreationResourcePatternType :: !(Int8)
,

  -- | The principal for the ACL.

  -- Versions: 0+
  aclCreationPrincipal :: !(KafkaString)
,

  -- | The host for the ACL.

  -- Versions: 0+
  aclCreationHost :: !(KafkaString)
,

  -- | The operation type for the ACL (read, write, etc.).

  -- Versions: 0+
  aclCreationOperation :: !(Int8)
,

  -- | The permission type for the ACL (allow, deny, etc.).

  -- Versions: 0+
  aclCreationPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


data CreateAclsRequest = CreateAclsRequest
  {

  -- | The ACLs that we want to create.

  -- Versions: 0+
  createAclsRequestCreations :: !(KafkaArray (AclCreation))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateAclsRequest.
maxCreateAclsRequestVersion :: Int16
maxCreateAclsRequestVersion = 3

-- | KafkaMessage instance for CreateAclsRequest.
instance KafkaMessage CreateAclsRequest where
  messageApiKey = 30
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a AclCreation.
wireMaxSizeAclCreation :: Int -> AclCreation -> Int
wireMaxSizeAclCreation _version msg =
  0
  + 1
  + WP.compactStringMaxSize (P.toCompactString (aclCreationResourceName msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (aclCreationPrincipal msg))
  + WP.compactStringMaxSize (P.toCompactString (aclCreationHost msg))
  + 1
  + 1
  + 1

-- | Direct-poke encoder for AclCreation.
wirePokeAclCreation :: Int -> Ptr Word8 -> AclCreation -> IO (Ptr Word8)
wirePokeAclCreation version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (aclCreationResourceType msg))
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (aclCreationResourceName msg)) else WP.pokeKafkaString p1 (aclCreationResourceName msg))
  p3 <- (if version >= 1 then W.pokeWord8 p2 (fromIntegral (aclCreationResourcePatternType msg)) else pure p2)
  p4 <- (if version >= 2 then WP.pokeCompactString p3 (P.toCompactString (aclCreationPrincipal msg)) else WP.pokeKafkaString p3 (aclCreationPrincipal msg))
  p5 <- (if version >= 2 then WP.pokeCompactString p4 (P.toCompactString (aclCreationHost msg)) else WP.pokeKafkaString p4 (aclCreationHost msg))
  p6 <- W.pokeWord8 p5 (fromIntegral (aclCreationOperation msg))
  p7 <- W.pokeWord8 p6 (fromIntegral (aclCreationPermissionType msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for AclCreation.
wirePeekAclCreation :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AclCreation, Ptr Word8)
wirePeekAclCreation version _fp _basePtr p0 endPtr = do
  (f0_resourcetype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcename, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_resourcepatterntype, p3) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr else pure (0, p2))
  (f3_principal, p4) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_host, p5) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
  (f5_operation, p6) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr
  (f6_permissiontype, p7) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p6 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (AclCreation { aclCreationResourceType = f0_resourcetype, aclCreationResourceName = f1_resourcename, aclCreationResourcePatternType = f2_resourcepatterntype, aclCreationPrincipal = f3_principal, aclCreationHost = f4_host, aclCreationOperation = f5_operation, aclCreationPermissionType = f6_permissiontype }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAclCreation :: AclCreation
defaultAclCreation = AclCreation { aclCreationResourceType = 0, aclCreationResourceName = P.KafkaString Null, aclCreationResourcePatternType = 0, aclCreationPrincipal = P.KafkaString Null, aclCreationHost = P.KafkaString Null, aclCreationOperation = 0, aclCreationPermissionType = 0 }

-- | Worst-case wire size of a CreateAclsRequest.
wireMaxSizeCreateAclsRequest :: Int -> CreateAclsRequest -> Int
wireMaxSizeCreateAclsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (createAclsRequestCreations msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAclCreation _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreateAclsRequest.
wirePokeCreateAclsRequest :: Int -> Ptr Word8 -> CreateAclsRequest -> IO (Ptr Word8)
wirePokeCreateAclsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAclCreation version p x) p0 (createAclsRequestCreations msg)
    pure p1
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAclCreation version p x) p0 (createAclsRequestCreations msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke CreateAclsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateAclsRequest.
wirePeekCreateAclsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateAclsRequest, Ptr Word8)
wirePeekCreateAclsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_creations, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAclCreation version _fp _basePtr p e) p0 endPtr
    pure (CreateAclsRequest { createAclsRequestCreations = f0_creations }, p1)
  | version >= 2 && version <= 3 = do
    (f0_creations, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAclCreation version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (CreateAclsRequest { createAclsRequestCreations = f0_creations }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateAclsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec CreateAclsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateAclsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateAclsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateAclsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}