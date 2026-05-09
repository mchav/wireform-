{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeUserScramCredentialsResponse
Description : Kafka DescribeUserScramCredentialsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 50.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeUserScramCredentialsResponse
  (
    DescribeUserScramCredentialsResponse(..),
    DescribeUserScramCredentialsResult(..),
    CredentialInfo(..),
    encodeDescribeUserScramCredentialsResponse,
    decodeDescribeUserScramCredentialsResponse,
    maxDescribeUserScramCredentialsResponseVersion
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


-- | The mechanism and related information associated with the user's SCRAM credentials.
data CredentialInfo = CredentialInfo
  {

  -- | The SCRAM mechanism.

  -- Versions: 0+
  credentialInfoMechanism :: !(Int8)
,

  -- | The number of iterations used in the SCRAM credential.

  -- Versions: 0+
  credentialInfoIterations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode CredentialInfo with version-aware field handling.
encodeCredentialInfo :: MonadPut m => E.ApiVersion -> CredentialInfo -> m ()
encodeCredentialInfo version cmsg =
  do
    serialize (credentialInfoMechanism cmsg)
    serialize (credentialInfoIterations cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CredentialInfo with version-aware field handling.
decodeCredentialInfo :: MonadGet m => E.ApiVersion -> m CredentialInfo
decodeCredentialInfo version =
  do
    fieldmechanism <- deserialize
    fielditerations <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CredentialInfo
      {
      credentialInfoMechanism = fieldmechanism
      ,
      credentialInfoIterations = fielditerations
      }


-- | The results for descriptions, one per user.
data DescribeUserScramCredentialsResult = DescribeUserScramCredentialsResult
  {

  -- | The user name.

  -- Versions: 0+
  describeUserScramCredentialsResultUser :: !(KafkaString)
,

  -- | The user-level error code.

  -- Versions: 0+
  describeUserScramCredentialsResultErrorCode :: !(Int16)
,

  -- | The user-level error message, if any.

  -- Versions: 0+
  describeUserScramCredentialsResultErrorMessage :: !(KafkaString)
,

  -- | The mechanism and related information associated with the user's SCRAM credentials.

  -- Versions: 0+
  describeUserScramCredentialsResultCredentialInfos :: !(KafkaArray (CredentialInfo))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeUserScramCredentialsResult with version-aware field handling.
encodeDescribeUserScramCredentialsResult :: MonadPut m => E.ApiVersion -> DescribeUserScramCredentialsResult -> m ()
encodeDescribeUserScramCredentialsResult version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeUserScramCredentialsResultUser dmsg)) else serialize (describeUserScramCredentialsResultUser dmsg)
    serialize (describeUserScramCredentialsResultErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describeUserScramCredentialsResultErrorMessage dmsg)) else serialize (describeUserScramCredentialsResultErrorMessage dmsg)
    E.encodeVersionedArray version 0 encodeCredentialInfo (case P.unKafkaArray (describeUserScramCredentialsResultCredentialInfos dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeUserScramCredentialsResult with version-aware field handling.
decodeDescribeUserScramCredentialsResult :: MonadGet m => E.ApiVersion -> m DescribeUserScramCredentialsResult
decodeDescribeUserScramCredentialsResult version =
  do
    fielduser <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldcredentialinfos <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeCredentialInfo
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeUserScramCredentialsResult
      {
      describeUserScramCredentialsResultUser = fielduser
      ,
      describeUserScramCredentialsResultErrorCode = fielderrorcode
      ,
      describeUserScramCredentialsResultErrorMessage = fielderrormessage
      ,
      describeUserScramCredentialsResultCredentialInfos = fieldcredentialinfos
      }



data DescribeUserScramCredentialsResponse = DescribeUserScramCredentialsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeUserScramCredentialsResponseThrottleTimeMs :: !(Int32)
,

  -- | The message-level error code, 0 except for user authorization or infrastructure issues.

  -- Versions: 0+
  describeUserScramCredentialsResponseErrorCode :: !(Int16)
,

  -- | The message-level error message, if any.

  -- Versions: 0+
  describeUserScramCredentialsResponseErrorMessage :: !(KafkaString)
,

  -- | The results for descriptions, one per user.

  -- Versions: 0+
  describeUserScramCredentialsResponseResults :: !(KafkaArray (DescribeUserScramCredentialsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeUserScramCredentialsResponse.
maxDescribeUserScramCredentialsResponseVersion :: Int16
maxDescribeUserScramCredentialsResponseVersion = 0

-- | Encode DescribeUserScramCredentialsResponse with the given API version.
encodeDescribeUserScramCredentialsResponse :: MonadPut m => E.ApiVersion -> DescribeUserScramCredentialsResponse -> m ()
encodeDescribeUserScramCredentialsResponse version msg
  | version == 0 =
    do
      serialize (describeUserScramCredentialsResponseThrottleTimeMs msg)
      serialize (describeUserScramCredentialsResponseErrorCode msg)
      serialize (toCompactString (describeUserScramCredentialsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeDescribeUserScramCredentialsResult (case P.unKafkaArray (describeUserScramCredentialsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeUserScramCredentialsResponse with the given API version.
decodeDescribeUserScramCredentialsResponse :: MonadGet m => E.ApiVersion -> m DescribeUserScramCredentialsResponse
decodeDescribeUserScramCredentialsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeUserScramCredentialsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeUserScramCredentialsResponse
        {
        describeUserScramCredentialsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeUserScramCredentialsResponseErrorCode = fielderrorcode
        ,
        describeUserScramCredentialsResponseErrorMessage = fielderrormessage
        ,
        describeUserScramCredentialsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeUserScramCredentialsResponse' / 'decodeDescribeUserScramCredentialsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeUserScramCredentialsResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeUserScramCredentialsResponse decodeDescribeUserScramCredentialsResponse)
  {-# INLINE wireCodec #-}
