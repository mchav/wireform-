{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeConfigsResponse
Description : Kafka DescribeConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 32.



Valid versions: 1-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeConfigsResponse
  (
    DescribeConfigsResponse(..),
    DescribeConfigsResult(..),
    DescribeConfigsResourceResult(..),
    DescribeConfigsSynonym(..),
    encodeDescribeConfigsResponse,
    decodeDescribeConfigsResponse,
    maxDescribeConfigsResponseVersion
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


-- | The synonyms for this configuration key.
data DescribeConfigsSynonym = DescribeConfigsSynonym
  {

  -- | The synonym name.

  -- Versions: 1+
  describeConfigsSynonymName :: !(KafkaString)
,

  -- | The synonym value.

  -- Versions: 1+
  describeConfigsSynonymValue :: !(KafkaString)
,

  -- | The synonym source.

  -- Versions: 1+
  describeConfigsSynonymSource :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsSynonym with version-aware field handling.
encodeDescribeConfigsSynonym :: MonadPut m => E.ApiVersion -> DescribeConfigsSynonym -> m ()
encodeDescribeConfigsSynonym version dmsg =
  do
    when (version >= 1) $
      if version >= 4 then serialize (toCompactString (describeConfigsSynonymName dmsg)) else serialize (describeConfigsSynonymName dmsg)
    when (version >= 1) $
      if version >= 4 then serialize (toCompactString (describeConfigsSynonymValue dmsg)) else serialize (describeConfigsSynonymValue dmsg)
    when (version >= 1) $
      serialize (describeConfigsSynonymSource dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsSynonym with version-aware field handling.
decodeDescribeConfigsSynonym :: MonadGet m => E.ApiVersion -> m DescribeConfigsSynonym
decodeDescribeConfigsSynonym version =
  do
    fieldname <- if version >= 1
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldvalue <- if version >= 1
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldsource <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsSynonym
      {
      describeConfigsSynonymName = fieldname
      ,
      describeConfigsSynonymValue = fieldvalue
      ,
      describeConfigsSynonymSource = fieldsource
      }


-- | Each listed configuration.
data DescribeConfigsResourceResult = DescribeConfigsResourceResult
  {

  -- | The configuration name.

  -- Versions: 0+
  describeConfigsResourceResultName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 0+
  describeConfigsResourceResultValue :: !(KafkaString)
,

  -- | True if the configuration is read-only.

  -- Versions: 0+
  describeConfigsResourceResultReadOnly :: !(Bool)
,

  -- | The configuration source.

  -- Versions: 1+
  describeConfigsResourceResultConfigSource :: !(Int8)
,

  -- | True if this configuration is sensitive.

  -- Versions: 0+
  describeConfigsResourceResultIsSensitive :: !(Bool)
,

  -- | The synonyms for this configuration key.

  -- Versions: 1+
  describeConfigsResourceResultSynonyms :: !(KafkaArray (DescribeConfigsSynonym))
,

  -- | The configuration data type. Type can be one of the following values - BOOLEAN, STRING, INT, SHORT, 

  -- Versions: 3+
  describeConfigsResourceResultConfigType :: !(Int8)
,

  -- | The configuration documentation.

  -- Versions: 3+
  describeConfigsResourceResultDocumentation :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsResourceResult with version-aware field handling.
encodeDescribeConfigsResourceResult :: MonadPut m => E.ApiVersion -> DescribeConfigsResourceResult -> m ()
encodeDescribeConfigsResourceResult version dmsg =
  do
    if version >= 4 then serialize (toCompactString (describeConfigsResourceResultName dmsg)) else serialize (describeConfigsResourceResultName dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResourceResultValue dmsg)) else serialize (describeConfigsResourceResultValue dmsg)
    serialize (describeConfigsResourceResultReadOnly dmsg)
    when (version >= 1) $
      serialize (describeConfigsResourceResultConfigSource dmsg)
    serialize (describeConfigsResourceResultIsSensitive dmsg)
    when (version >= 1) $
      E.encodeVersionedArray version 4 encodeDescribeConfigsSynonym (case P.unKafkaArray (describeConfigsResourceResultSynonyms dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $
      serialize (describeConfigsResourceResultConfigType dmsg)
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (describeConfigsResourceResultDocumentation dmsg)) else serialize (describeConfigsResourceResultDocumentation dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsResourceResult with version-aware field handling.
decodeDescribeConfigsResourceResult :: MonadGet m => E.ApiVersion -> m DescribeConfigsResourceResult
decodeDescribeConfigsResourceResult version =
  do
    fieldname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldreadonly <- deserialize
    fieldconfigsource <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldissensitive <- deserialize
    fieldsynonyms <- if version >= 1
      then P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsSynonym
      else pure (P.mkKafkaArray V.empty)
    fieldconfigtype <- if version >= 3
      then deserialize
      else pure (0)
    fielddocumentation <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsResourceResult
      {
      describeConfigsResourceResultName = fieldname
      ,
      describeConfigsResourceResultValue = fieldvalue
      ,
      describeConfigsResourceResultReadOnly = fieldreadonly
      ,
      describeConfigsResourceResultConfigSource = fieldconfigsource
      ,
      describeConfigsResourceResultIsSensitive = fieldissensitive
      ,
      describeConfigsResourceResultSynonyms = fieldsynonyms
      ,
      describeConfigsResourceResultConfigType = fieldconfigtype
      ,
      describeConfigsResourceResultDocumentation = fielddocumentation
      }


-- | The results for each resource.
data DescribeConfigsResult = DescribeConfigsResult
  {

  -- | The error code, or 0 if we were able to successfully describe the configurations.

  -- Versions: 0+
  describeConfigsResultErrorCode :: !(Int16)
,

  -- | The error message, or null if we were able to successfully describe the configurations.

  -- Versions: 0+
  describeConfigsResultErrorMessage :: !(KafkaString)
,

  -- | The resource type.

  -- Versions: 0+
  describeConfigsResultResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  describeConfigsResultResourceName :: !(KafkaString)
,

  -- | Each listed configuration.

  -- Versions: 0+
  describeConfigsResultConfigs :: !(KafkaArray (DescribeConfigsResourceResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsResult with version-aware field handling.
encodeDescribeConfigsResult :: MonadPut m => E.ApiVersion -> DescribeConfigsResult -> m ()
encodeDescribeConfigsResult version dmsg =
  do
    serialize (describeConfigsResultErrorCode dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResultErrorMessage dmsg)) else serialize (describeConfigsResultErrorMessage dmsg)
    serialize (describeConfigsResultResourceType dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResultResourceName dmsg)) else serialize (describeConfigsResultResourceName dmsg)
    E.encodeVersionedArray version 4 encodeDescribeConfigsResourceResult (case P.unKafkaArray (describeConfigsResultConfigs dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsResult with version-aware field handling.
decodeDescribeConfigsResult :: MonadGet m => E.ApiVersion -> m DescribeConfigsResult
decodeDescribeConfigsResult version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResourceResult
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsResult
      {
      describeConfigsResultErrorCode = fielderrorcode
      ,
      describeConfigsResultErrorMessage = fielderrormessage
      ,
      describeConfigsResultResourceType = fieldresourcetype
      ,
      describeConfigsResultResourceName = fieldresourcename
      ,
      describeConfigsResultConfigs = fieldconfigs
      }



data DescribeConfigsResponse = DescribeConfigsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each resource.

  -- Versions: 0+
  describeConfigsResponseResults :: !(KafkaArray (DescribeConfigsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeConfigsResponse.
maxDescribeConfigsResponseVersion :: Int16
maxDescribeConfigsResponseVersion = 4

-- | Encode DescribeConfigsResponse with the given API version.
encodeDescribeConfigsResponse :: MonadPut m => E.ApiVersion -> DescribeConfigsResponse -> m ()
encodeDescribeConfigsResponse version msg
  | version == 4 =
    do
      serialize (describeConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDescribeConfigsResult (case P.unKafkaArray (describeConfigsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      serialize (describeConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDescribeConfigsResult (case P.unKafkaArray (describeConfigsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeConfigsResponse with the given API version.
decodeDescribeConfigsResponse :: MonadGet m => E.ApiVersion -> m DescribeConfigsResponse
decodeDescribeConfigsResponse version
  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeConfigsResponse
        {
        describeConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeConfigsResponseResults = fieldresults
        }

  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResult
      pure DescribeConfigsResponse
        {
        describeConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeConfigsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeConfigsResponse' / 'decodeDescribeConfigsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeConfigsResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeConfigsResponse decodeDescribeConfigsResponse)
  {-# INLINE wireCodec #-}
