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
    encodeDeleteAclsResponse,
    decodeDeleteAclsResponse,
    maxDeleteAclsResponseVersion
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


-- | Encode DeleteAclsMatchingAcl with version-aware field handling.
encodeDeleteAclsMatchingAcl :: MonadPut m => E.ApiVersion -> DeleteAclsMatchingAcl -> m ()
encodeDeleteAclsMatchingAcl version dmsg =
  do
    serialize (deleteAclsMatchingAclErrorCode dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsMatchingAclErrorMessage dmsg)) else serialize (deleteAclsMatchingAclErrorMessage dmsg)
    serialize (deleteAclsMatchingAclResourceType dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsMatchingAclResourceName dmsg)) else serialize (deleteAclsMatchingAclResourceName dmsg)
    when (version >= 1) $
      serialize (deleteAclsMatchingAclPatternType dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsMatchingAclPrincipal dmsg)) else serialize (deleteAclsMatchingAclPrincipal dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsMatchingAclHost dmsg)) else serialize (deleteAclsMatchingAclHost dmsg)
    serialize (deleteAclsMatchingAclOperation dmsg)
    serialize (deleteAclsMatchingAclPermissionType dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteAclsMatchingAcl with version-aware field handling.
decodeDeleteAclsMatchingAcl :: MonadGet m => E.ApiVersion -> m DeleteAclsMatchingAcl
decodeDeleteAclsMatchingAcl version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpatterntype <- if version >= 1
      then deserialize
      else pure (3)
    fieldprincipal <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldoperation <- deserialize
    fieldpermissiontype <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteAclsMatchingAcl
      {
      deleteAclsMatchingAclErrorCode = fielderrorcode
      ,
      deleteAclsMatchingAclErrorMessage = fielderrormessage
      ,
      deleteAclsMatchingAclResourceType = fieldresourcetype
      ,
      deleteAclsMatchingAclResourceName = fieldresourcename
      ,
      deleteAclsMatchingAclPatternType = fieldpatterntype
      ,
      deleteAclsMatchingAclPrincipal = fieldprincipal
      ,
      deleteAclsMatchingAclHost = fieldhost
      ,
      deleteAclsMatchingAclOperation = fieldoperation
      ,
      deleteAclsMatchingAclPermissionType = fieldpermissiontype
      }


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


-- | Encode DeleteAclsFilterResult with version-aware field handling.
encodeDeleteAclsFilterResult :: MonadPut m => E.ApiVersion -> DeleteAclsFilterResult -> m ()
encodeDeleteAclsFilterResult version dmsg =
  do
    serialize (deleteAclsFilterResultErrorCode dmsg)
    if version >= 2 then serialize (toCompactString (deleteAclsFilterResultErrorMessage dmsg)) else serialize (deleteAclsFilterResultErrorMessage dmsg)
    E.encodeVersionedArray version 2 encodeDeleteAclsMatchingAcl (case P.unKafkaArray (deleteAclsFilterResultMatchingAcls dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteAclsFilterResult with version-aware field handling.
decodeDeleteAclsFilterResult :: MonadGet m => E.ApiVersion -> m DeleteAclsFilterResult
decodeDeleteAclsFilterResult version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldmatchingacls <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteAclsMatchingAcl
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteAclsFilterResult
      {
      deleteAclsFilterResultErrorCode = fielderrorcode
      ,
      deleteAclsFilterResultErrorMessage = fielderrormessage
      ,
      deleteAclsFilterResultMatchingAcls = fieldmatchingacls
      }



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

-- | Encode DeleteAclsResponse with the given API version.
encodeDeleteAclsResponse :: MonadPut m => E.ApiVersion -> DeleteAclsResponse -> m ()
encodeDeleteAclsResponse version msg
  | version == 1 =
    do
      serialize (deleteAclsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteAclsFilterResult (case P.unKafkaArray (deleteAclsResponseFilterResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      serialize (deleteAclsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteAclsFilterResult (case P.unKafkaArray (deleteAclsResponseFilterResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteAclsResponse with the given API version.
decodeDeleteAclsResponse :: MonadGet m => E.ApiVersion -> m DeleteAclsResponse
decodeDeleteAclsResponse version
  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldfilterresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteAclsFilterResult
      pure DeleteAclsResponse
        {
        deleteAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteAclsResponseFilterResults = fieldfilterresults
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldfilterresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteAclsFilterResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteAclsResponse
        {
        deleteAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteAclsResponseFilterResults = fieldfilterresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDeleteAclsResponse' / 'decodeDeleteAclsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DeleteAclsResponse where
  wireCodec = Just (WC.serialShimCodec encodeDeleteAclsResponse decodeDeleteAclsResponse)
  {-# INLINE wireCodec #-}
