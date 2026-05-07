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
    encodeDeleteAclsRequest,
    decodeDeleteAclsRequest,
    maxDeleteAclsRequestVersion
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


-- | Encode DeleteAclsFilter with version-aware field handling.
encodeDeleteAclsFilter :: MonadPut m => E.ApiVersion -> DeleteAclsFilter -> m ()
encodeDeleteAclsFilter version dmsg =
  do
    serialize (deleteAclsFilterResourceTypeFilter dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsFilterResourceNameFilter dmsg)) else serialize (deleteAclsFilterResourceNameFilter dmsg)
    when (version >= 1) $
      serialize (deleteAclsFilterPatternTypeFilter dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsFilterPrincipalFilter dmsg)) else serialize (deleteAclsFilterPrincipalFilter dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsFilterHostFilter dmsg)) else serialize (deleteAclsFilterHostFilter dmsg)
    serialize (deleteAclsFilterOperation dmsg)
    serialize (deleteAclsFilterPermissionType dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteAclsFilter with version-aware field handling.
decodeDeleteAclsFilter :: MonadGet m => E.ApiVersion -> m DeleteAclsFilter
decodeDeleteAclsFilter version =
  do
    fieldresourcetypefilter <- deserialize
    fieldresourcenamefilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpatterntypefilter <- if version >= 1
      then deserialize
      else pure (3)
    fieldprincipalfilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldhostfilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldoperation <- deserialize
    fieldpermissiontype <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteAclsFilter
      {
      deleteAclsFilterResourceTypeFilter = fieldresourcetypefilter
      ,
      deleteAclsFilterResourceNameFilter = fieldresourcenamefilter
      ,
      deleteAclsFilterPatternTypeFilter = fieldpatterntypefilter
      ,
      deleteAclsFilterPrincipalFilter = fieldprincipalfilter
      ,
      deleteAclsFilterHostFilter = fieldhostfilter
      ,
      deleteAclsFilterOperation = fieldoperation
      ,
      deleteAclsFilterPermissionType = fieldpermissiontype
      }



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

-- | Encode DeleteAclsRequest with the given API version.
encodeDeleteAclsRequest :: MonadPut m => E.ApiVersion -> DeleteAclsRequest -> m ()
encodeDeleteAclsRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 2 encodeDeleteAclsFilter (case P.unKafkaArray (deleteAclsRequestFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      E.encodeVersionedArray version 2 encodeDeleteAclsFilter (case P.unKafkaArray (deleteAclsRequestFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteAclsRequest with the given API version.
decodeDeleteAclsRequest :: MonadGet m => E.ApiVersion -> m DeleteAclsRequest
decodeDeleteAclsRequest version
  | version == 1 =
    do
      fieldfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteAclsFilter
      pure DeleteAclsRequest
        {
        deleteAclsRequestFilters = fieldfilters
        }

  | version >= 2 && version <= 3 =
    do
      fieldfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteAclsFilter
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteAclsRequest
        {
        deleteAclsRequestFilters = fieldfilters
        }
  | otherwise = fail $ "Unsupported version: " ++ show version