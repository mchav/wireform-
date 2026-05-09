{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeAclsResponse
Description : Kafka DescribeAclsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 29.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeAclsResponse
  (
    DescribeAclsResponse(..),
    DescribeAclsResource(..),
    AclDescription(..),
    encodeDescribeAclsResponse,
    decodeDescribeAclsResponse,
    maxDescribeAclsResponseVersion
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
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The ACLs.
data AclDescription = AclDescription
  {

  -- | The ACL principal.

  -- Versions: 0+
  aclDescriptionPrincipal :: !(KafkaString)
,

  -- | The ACL host.

  -- Versions: 0+
  aclDescriptionHost :: !(KafkaString)
,

  -- | The ACL operation.

  -- Versions: 0+
  aclDescriptionOperation :: !(Int8)
,

  -- | The ACL permission type.

  -- Versions: 0+
  aclDescriptionPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode AclDescription with version-aware field handling.
encodeAclDescription :: MonadPut m => E.ApiVersion -> AclDescription -> m ()
encodeAclDescription version amsg =
  do
    if version >= 2 then serialize (toCompactString (aclDescriptionPrincipal amsg)) else serialize (aclDescriptionPrincipal amsg)
    if version >= 2 then serialize (toCompactString (aclDescriptionHost amsg)) else serialize (aclDescriptionHost amsg)
    serialize (aclDescriptionOperation amsg)
    serialize (aclDescriptionPermissionType amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AclDescription with version-aware field handling.
decodeAclDescription :: MonadGet m => E.ApiVersion -> m AclDescription
decodeAclDescription version =
  do
    fieldprincipal <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldoperation <- deserialize
    fieldpermissiontype <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AclDescription
      {
      aclDescriptionPrincipal = fieldprincipal
      ,
      aclDescriptionHost = fieldhost
      ,
      aclDescriptionOperation = fieldoperation
      ,
      aclDescriptionPermissionType = fieldpermissiontype
      }


-- | Each Resource that is referenced in an ACL.
data DescribeAclsResource = DescribeAclsResource
  {

  -- | The resource type.

  -- Versions: 0+
  describeAclsResourceResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  describeAclsResourceResourceName :: !(KafkaString)
,

  -- | The resource pattern type.

  -- Versions: 1+
  describeAclsResourcePatternType :: !(Int8)
,

  -- | The ACLs.

  -- Versions: 0+
  describeAclsResourceAcls :: !(KafkaArray (AclDescription))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeAclsResource with version-aware field handling.
encodeDescribeAclsResource :: MonadPut m => E.ApiVersion -> DescribeAclsResource -> m ()
encodeDescribeAclsResource version dmsg =
  do
    serialize (describeAclsResourceResourceType dmsg)
    if version >= 2 then serialize (toCompactString (describeAclsResourceResourceName dmsg)) else serialize (describeAclsResourceResourceName dmsg)
    when (version >= 1) $
      serialize (describeAclsResourcePatternType dmsg)
    E.encodeVersionedArray version 2 encodeAclDescription (case P.unKafkaArray (describeAclsResourceAcls dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeAclsResource with version-aware field handling.
decodeDescribeAclsResource :: MonadGet m => E.ApiVersion -> m DescribeAclsResource
decodeDescribeAclsResource version =
  do
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpatterntype <- if version >= 1
      then deserialize
      else pure (3)
    fieldacls <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAclDescription
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeAclsResource
      {
      describeAclsResourceResourceType = fieldresourcetype
      ,
      describeAclsResourceResourceName = fieldresourcename
      ,
      describeAclsResourcePatternType = fieldpatterntype
      ,
      describeAclsResourceAcls = fieldacls
      }



data DescribeAclsResponse = DescribeAclsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeAclsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  describeAclsResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  describeAclsResponseErrorMessage :: !(KafkaString)
,

  -- | Each Resource that is referenced in an ACL.

  -- Versions: 0+
  describeAclsResponseResources :: !(KafkaArray (DescribeAclsResource))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeAclsResponse.
maxDescribeAclsResponseVersion :: Int16
maxDescribeAclsResponseVersion = 3

-- | KafkaMessage instance for DescribeAclsResponse.
instance KafkaMessage DescribeAclsResponse where
  messageApiKey = 29
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Encode DescribeAclsResponse with the given API version.
encodeDescribeAclsResponse :: MonadPut m => E.ApiVersion -> DescribeAclsResponse -> m ()
encodeDescribeAclsResponse version msg
  | version == 1 =
    do
      serialize (describeAclsResponseThrottleTimeMs msg)
      serialize (describeAclsResponseErrorCode msg)
      serialize (describeAclsResponseErrorMessage msg)
      E.encodeVersionedArray version 2 encodeDescribeAclsResource (case P.unKafkaArray (describeAclsResponseResources msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      serialize (describeAclsResponseThrottleTimeMs msg)
      serialize (describeAclsResponseErrorCode msg)
      serialize (toCompactString (describeAclsResponseErrorMessage msg))
      E.encodeVersionedArray version 2 encodeDescribeAclsResource (case P.unKafkaArray (describeAclsResponseResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeAclsResponse with the given API version.
decodeDescribeAclsResponse :: MonadGet m => E.ApiVersion -> m DescribeAclsResponse
decodeDescribeAclsResponse version
  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeAclsResource
      pure DescribeAclsResponse
        {
        describeAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeAclsResponseErrorCode = fielderrorcode
        ,
        describeAclsResponseErrorMessage = fielderrormessage
        ,
        describeAclsResponseResources = fieldresources
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeAclsResource
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeAclsResponse
        {
        describeAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeAclsResponseErrorCode = fielderrorcode
        ,
        describeAclsResponseErrorMessage = fielderrormessage
        ,
        describeAclsResponseResources = fieldresources
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a AclDescription.
wireMaxSizeAclDescription :: Int -> AclDescription -> Int
wireMaxSizeAclDescription _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (aclDescriptionPrincipal msg))
  + WP.compactStringMaxSize (P.toCompactString (aclDescriptionHost msg))
  + 1
  + 1
  + 1

-- | Direct-poke encoder for AclDescription.
wirePokeAclDescription :: Int -> Ptr Word8 -> AclDescription -> IO (Ptr Word8)
wirePokeAclDescription version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (aclDescriptionPrincipal msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (aclDescriptionHost msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (aclDescriptionOperation msg))
  p4 <- W.pokeWord8 p3 (fromIntegral (aclDescriptionPermissionType msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for AclDescription.
wirePeekAclDescription :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AclDescription, Ptr Word8)
wirePeekAclDescription version _fp _basePtr p0 endPtr = do
  (f0_principal, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_operation, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_permissiontype, p4) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (AclDescription { aclDescriptionPrincipal = f0_principal, aclDescriptionHost = f1_host, aclDescriptionOperation = f2_operation, aclDescriptionPermissionType = f3_permissiontype }, pTagsEnd)

-- | Worst-case wire size of a DescribeAclsResource.
wireMaxSizeDescribeAclsResource :: Int -> DescribeAclsResource -> Int
wireMaxSizeDescribeAclsResource _version msg =
  0
  + 1
  + WP.compactStringMaxSize (P.toCompactString (describeAclsResourceResourceName msg))
  + 1
  + (5 + (case P.unKafkaArray (describeAclsResourceAcls msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAclDescription _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeAclsResource.
wirePokeDescribeAclsResource :: Int -> Ptr Word8 -> DescribeAclsResource -> IO (Ptr Word8)
wirePokeDescribeAclsResource version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (describeAclsResourceResourceType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeAclsResourceResourceName msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (describeAclsResourcePatternType msg))
  p4 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAclDescription version p x) p3 (describeAclsResourceAcls msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DescribeAclsResource.
wirePeekDescribeAclsResource :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeAclsResource, Ptr Word8)
wirePeekDescribeAclsResource version _fp _basePtr p0 endPtr = do
  (f0_resourcetype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcename, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_patterntype, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_acls, p4) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAclDescription version _fp _basePtr p e) p3 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DescribeAclsResource { describeAclsResourceResourceType = f0_resourcetype, describeAclsResourceResourceName = f1_resourcename, describeAclsResourcePatternType = f2_patterntype, describeAclsResourceAcls = f3_acls }, pTagsEnd)

-- | Worst-case wire size of a DescribeAclsResponse.
wireMaxSizeDescribeAclsResponse :: Int -> DescribeAclsResponse -> Int
wireMaxSizeDescribeAclsResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeAclsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (describeAclsResponseResources msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeAclsResource _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeAclsResponse.
wirePokeDescribeAclsResponse :: Int -> Ptr Word8 -> DescribeAclsResponse -> IO (Ptr Word8)
wirePokeDescribeAclsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeAclsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeAclsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (describeAclsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeAclsResource version p x) p3 (describeAclsResponseResources msg)
    pure p4
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeAclsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeAclsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (describeAclsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeAclsResource version p x) p3 (describeAclsResponseResources msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke DescribeAclsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeAclsResponse.
wirePeekDescribeAclsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeAclsResponse, Ptr Word8)
wirePeekDescribeAclsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_resources, p4) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeAclsResource version _fp _basePtr p e) p3 endPtr
    pure (DescribeAclsResponse { describeAclsResponseThrottleTimeMs = f0_throttletimems, describeAclsResponseErrorCode = f1_errorcode, describeAclsResponseErrorMessage = f2_errormessage, describeAclsResponseResources = f3_resources }, p4)
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_resources, p4) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeAclsResource version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DescribeAclsResponse { describeAclsResponseThrottleTimeMs = f0_throttletimems, describeAclsResponseErrorCode = f1_errorcode, describeAclsResponseErrorMessage = f2_errormessage, describeAclsResponseResources = f3_resources }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeAclsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeAclsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeAclsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeAclsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeAclsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}