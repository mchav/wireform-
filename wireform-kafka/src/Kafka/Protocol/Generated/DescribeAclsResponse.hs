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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC


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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeAclsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
