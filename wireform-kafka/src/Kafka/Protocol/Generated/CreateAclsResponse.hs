{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateAclsResponse
Description : Kafka CreateAclsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 30.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateAclsResponse
  (
    CreateAclsResponse(..),
    AclCreationResult(..),
    encodeCreateAclsResponse,
    decodeCreateAclsResponse,
    maxCreateAclsResponseVersion
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


-- | The results for each ACL creation.
data AclCreationResult = AclCreationResult
  {

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  aclCreationResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  aclCreationResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AclCreationResult with version-aware field handling.
encodeAclCreationResult :: MonadPut m => E.ApiVersion -> AclCreationResult -> m ()
encodeAclCreationResult version amsg =
  do
    serialize (aclCreationResultErrorCode amsg)
    if version >= 2 then serialize (toCompactString (aclCreationResultErrorMessage amsg)) else serialize (aclCreationResultErrorMessage amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AclCreationResult with version-aware field handling.
decodeAclCreationResult :: MonadGet m => E.ApiVersion -> m AclCreationResult
decodeAclCreationResult version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AclCreationResult
      {
      aclCreationResultErrorCode = fielderrorcode
      ,
      aclCreationResultErrorMessage = fielderrormessage
      }



data CreateAclsResponse = CreateAclsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  createAclsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each ACL creation.

  -- Versions: 0+
  createAclsResponseResults :: !(KafkaArray (AclCreationResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateAclsResponse.
maxCreateAclsResponseVersion :: Int16
maxCreateAclsResponseVersion = 3

-- | Encode CreateAclsResponse with the given API version.
encodeCreateAclsResponse :: MonadPut m => E.ApiVersion -> CreateAclsResponse -> m ()
encodeCreateAclsResponse version msg
  | version == 1 =
    do
      serialize (createAclsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAclCreationResult (case P.unKafkaArray (createAclsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      serialize (createAclsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAclCreationResult (case P.unKafkaArray (createAclsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateAclsResponse with the given API version.
decodeCreateAclsResponse :: MonadGet m => E.ApiVersion -> m CreateAclsResponse
decodeCreateAclsResponse version
  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAclCreationResult
      pure CreateAclsResponse
        {
        createAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createAclsResponseResults = fieldresults
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAclCreationResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateAclsResponse
        {
        createAclsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createAclsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeCreateAclsResponse' / 'decodeCreateAclsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec CreateAclsResponse where
  wireCodec = Just (WC.serialShimCodec encodeCreateAclsResponse decodeCreateAclsResponse)
  {-# INLINE wireCodec #-}
