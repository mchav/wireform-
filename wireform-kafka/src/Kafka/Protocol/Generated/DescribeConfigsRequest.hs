{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeConfigsRequest
Description : Kafka DescribeConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 32.



Valid versions: 1-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeConfigsRequest
  (
    DescribeConfigsRequest(..),
    DescribeConfigsResource(..),
    encodeDescribeConfigsRequest,
    decodeDescribeConfigsRequest,
    maxDescribeConfigsRequestVersion
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


-- | The resources whose configurations we want to describe.
data DescribeConfigsResource = DescribeConfigsResource
  {

  -- | The resource type.

  -- Versions: 0+
  describeConfigsResourceResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  describeConfigsResourceResourceName :: !(KafkaString)
,

  -- | The configuration keys to list, or null to list all configuration keys.

  -- Versions: 0+
  describeConfigsResourceConfigurationKeys :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsResource with version-aware field handling.
encodeDescribeConfigsResource :: MonadPut m => E.ApiVersion -> DescribeConfigsResource -> m ()
encodeDescribeConfigsResource version dmsg =
  do
    serialize (describeConfigsResourceResourceType dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResourceResourceName dmsg)) else serialize (describeConfigsResourceResourceName dmsg)
    E.encodeVersionedNullableArray version 4 (\v s -> if v >= 4 then serialize (toCompactString s) else serialize s) (describeConfigsResourceConfigurationKeys dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsResource with version-aware field handling.
decodeDescribeConfigsResource :: MonadGet m => E.ApiVersion -> m DescribeConfigsResource
decodeDescribeConfigsResource version =
  do
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigurationkeys <- E.decodeVersionedNullableArray version 4 (\v -> if v >= 4 then P.fromCompactString <$> deserialize else deserialize)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsResource
      {
      describeConfigsResourceResourceType = fieldresourcetype
      ,
      describeConfigsResourceResourceName = fieldresourcename
      ,
      describeConfigsResourceConfigurationKeys = fieldconfigurationkeys
      }



data DescribeConfigsRequest = DescribeConfigsRequest
  {

  -- | The resources whose configurations we want to describe.

  -- Versions: 0+
  describeConfigsRequestResources :: !(KafkaArray (DescribeConfigsResource))
,

  -- | True if we should include all synonyms.

  -- Versions: 1+
  describeConfigsRequestIncludeSynonyms :: !(Bool)
,

  -- | True if we should include configuration documentation.

  -- Versions: 3+
  describeConfigsRequestIncludeDocumentation :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeConfigsRequest.
maxDescribeConfigsRequestVersion :: Int16
maxDescribeConfigsRequestVersion = 4

-- | Encode DescribeConfigsRequest with the given API version.
encodeDescribeConfigsRequest :: MonadPut m => E.ApiVersion -> DescribeConfigsRequest -> m ()
encodeDescribeConfigsRequest version msg
  | version == 3 =
    do
      E.encodeVersionedArray version 4 encodeDescribeConfigsResource (case P.unKafkaArray (describeConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeConfigsRequestIncludeSynonyms msg)
      serialize (describeConfigsRequestIncludeDocumentation msg)


  | version == 4 =
    do
      E.encodeVersionedArray version 4 encodeDescribeConfigsResource (case P.unKafkaArray (describeConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeConfigsRequestIncludeSynonyms msg)
      serialize (describeConfigsRequestIncludeDocumentation msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      E.encodeVersionedArray version 4 encodeDescribeConfigsResource (case P.unKafkaArray (describeConfigsRequestResources msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeConfigsRequestIncludeSynonyms msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeConfigsRequest with the given API version.
decodeDescribeConfigsRequest :: MonadGet m => E.ApiVersion -> m DescribeConfigsRequest
decodeDescribeConfigsRequest version
  | version == 3 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResource
      fieldincludesynonyms <- deserialize
      fieldincludedocumentation <- deserialize
      pure DescribeConfigsRequest
        {
        describeConfigsRequestResources = fieldresources
        ,
        describeConfigsRequestIncludeSynonyms = fieldincludesynonyms
        ,
        describeConfigsRequestIncludeDocumentation = fieldincludedocumentation
        }

  | version == 4 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResource
      fieldincludesynonyms <- deserialize
      fieldincludedocumentation <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeConfigsRequest
        {
        describeConfigsRequestResources = fieldresources
        ,
        describeConfigsRequestIncludeSynonyms = fieldincludesynonyms
        ,
        describeConfigsRequestIncludeDocumentation = fieldincludedocumentation
        }

  | version >= 1 && version <= 2 =
    do
      fieldresources <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResource
      fieldincludesynonyms <- deserialize
      pure DescribeConfigsRequest
        {
        describeConfigsRequestResources = fieldresources
        ,
        describeConfigsRequestIncludeSynonyms = fieldincludesynonyms
        ,
        describeConfigsRequestIncludeDocumentation = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version