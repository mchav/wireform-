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
    encodeCreateAclsRequest,
    decodeCreateAclsRequest,
    maxCreateAclsRequestVersion
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


-- | Encode AclCreation with version-aware field handling.
encodeAclCreation :: MonadPut m => E.ApiVersion -> AclCreation -> m ()
encodeAclCreation version amsg =
  do
    serialize (aclCreationResourceType amsg)
    if version >= 2 then serialize (toCompactString (aclCreationResourceName amsg)) else serialize (aclCreationResourceName amsg)
    when (version >= 1) $
      serialize (aclCreationResourcePatternType amsg)
    if version >= 2 then serialize (toCompactString (aclCreationPrincipal amsg)) else serialize (aclCreationPrincipal amsg)
    if version >= 2 then serialize (toCompactString (aclCreationHost amsg)) else serialize (aclCreationHost amsg)
    serialize (aclCreationOperation amsg)
    serialize (aclCreationPermissionType amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AclCreation with version-aware field handling.
decodeAclCreation :: MonadGet m => E.ApiVersion -> m AclCreation
decodeAclCreation version =
  do
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcepatterntype <- if version >= 1
      then deserialize
      else pure (3)
    fieldprincipal <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldoperation <- deserialize
    fieldpermissiontype <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AclCreation
      {
      aclCreationResourceType = fieldresourcetype
      ,
      aclCreationResourceName = fieldresourcename
      ,
      aclCreationResourcePatternType = fieldresourcepatterntype
      ,
      aclCreationPrincipal = fieldprincipal
      ,
      aclCreationHost = fieldhost
      ,
      aclCreationOperation = fieldoperation
      ,
      aclCreationPermissionType = fieldpermissiontype
      }



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

-- | Encode CreateAclsRequest with the given API version.
encodeCreateAclsRequest :: MonadPut m => E.ApiVersion -> CreateAclsRequest -> m ()
encodeCreateAclsRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 2 encodeAclCreation (case P.unKafkaArray (createAclsRequestCreations msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      E.encodeVersionedArray version 2 encodeAclCreation (case P.unKafkaArray (createAclsRequestCreations msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateAclsRequest with the given API version.
decodeCreateAclsRequest :: MonadGet m => E.ApiVersion -> m CreateAclsRequest
decodeCreateAclsRequest version
  | version == 1 =
    do
      fieldcreations <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAclCreation
      pure CreateAclsRequest
        {
        createAclsRequestCreations = fieldcreations
        }

  | version >= 2 && version <= 3 =
    do
      fieldcreations <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAclCreation
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateAclsRequest
        {
        createAclsRequestCreations = fieldcreations
        }
  | otherwise = fail $ "Unsupported version: " ++ show version