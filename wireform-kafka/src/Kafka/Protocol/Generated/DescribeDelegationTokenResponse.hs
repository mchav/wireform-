{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeDelegationTokenResponse
Description : Kafka DescribeDelegationTokenResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 41.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeDelegationTokenResponse
  (
    DescribeDelegationTokenResponse(..),
    DescribedDelegationToken(..),
    DescribedDelegationTokenRenewer(..),
    encodeDescribeDelegationTokenResponse,
    decodeDescribeDelegationTokenResponse,
    maxDescribeDelegationTokenResponseVersion
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


-- | Those who are able to renew this token before it expires.
data DescribedDelegationTokenRenewer = DescribedDelegationTokenRenewer
  {

  -- | The renewer principal type.

  -- Versions: 0+
  describedDelegationTokenRenewerPrincipalType :: !(KafkaString)
,

  -- | The renewer principal name.

  -- Versions: 0+
  describedDelegationTokenRenewerPrincipalName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedDelegationTokenRenewer with version-aware field handling.
encodeDescribedDelegationTokenRenewer :: MonadPut m => E.ApiVersion -> DescribedDelegationTokenRenewer -> m ()
encodeDescribedDelegationTokenRenewer version dmsg =
  do
    if version >= 2 then serialize (toCompactString (describedDelegationTokenRenewerPrincipalType dmsg)) else serialize (describedDelegationTokenRenewerPrincipalType dmsg)
    if version >= 2 then serialize (toCompactString (describedDelegationTokenRenewerPrincipalName dmsg)) else serialize (describedDelegationTokenRenewerPrincipalName dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedDelegationTokenRenewer with version-aware field handling.
decodeDescribedDelegationTokenRenewer :: MonadGet m => E.ApiVersion -> m DescribedDelegationTokenRenewer
decodeDescribedDelegationTokenRenewer version =
  do
    fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedDelegationTokenRenewer
      {
      describedDelegationTokenRenewerPrincipalType = fieldprincipaltype
      ,
      describedDelegationTokenRenewerPrincipalName = fieldprincipalname
      }


-- | The tokens.
data DescribedDelegationToken = DescribedDelegationToken
  {

  -- | The token principal type.

  -- Versions: 0+
  describedDelegationTokenPrincipalType :: !(KafkaString)
,

  -- | The token principal name.

  -- Versions: 0+
  describedDelegationTokenPrincipalName :: !(KafkaString)
,

  -- | The principal type of the requester of the token.

  -- Versions: 3+
  describedDelegationTokenTokenRequesterPrincipalType :: !(KafkaString)
,

  -- | The principal type of the requester of the token.

  -- Versions: 3+
  describedDelegationTokenTokenRequesterPrincipalName :: !(KafkaString)
,

  -- | The token issue timestamp in milliseconds.

  -- Versions: 0+
  describedDelegationTokenIssueTimestamp :: !(Int64)
,

  -- | The token expiry timestamp in milliseconds.

  -- Versions: 0+
  describedDelegationTokenExpiryTimestamp :: !(Int64)
,

  -- | The token maximum timestamp length in milliseconds.

  -- Versions: 0+
  describedDelegationTokenMaxTimestamp :: !(Int64)
,

  -- | The token ID.

  -- Versions: 0+
  describedDelegationTokenTokenId :: !(KafkaString)
,

  -- | The token HMAC.

  -- Versions: 0+
  describedDelegationTokenHmac :: !(KafkaBytes)
,

  -- | Those who are able to renew this token before it expires.

  -- Versions: 0+
  describedDelegationTokenRenewers :: !(KafkaArray (DescribedDelegationTokenRenewer))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribedDelegationToken with version-aware field handling.
encodeDescribedDelegationToken :: MonadPut m => E.ApiVersion -> DescribedDelegationToken -> m ()
encodeDescribedDelegationToken version dmsg =
  do
    if version >= 2 then serialize (toCompactString (describedDelegationTokenPrincipalType dmsg)) else serialize (describedDelegationTokenPrincipalType dmsg)
    if version >= 2 then serialize (toCompactString (describedDelegationTokenPrincipalName dmsg)) else serialize (describedDelegationTokenPrincipalName dmsg)
    when (version >= 3) $
      if version >= 2 then serialize (toCompactString (describedDelegationTokenTokenRequesterPrincipalType dmsg)) else serialize (describedDelegationTokenTokenRequesterPrincipalType dmsg)
    when (version >= 3) $
      if version >= 2 then serialize (toCompactString (describedDelegationTokenTokenRequesterPrincipalName dmsg)) else serialize (describedDelegationTokenTokenRequesterPrincipalName dmsg)
    serialize (describedDelegationTokenIssueTimestamp dmsg)
    serialize (describedDelegationTokenExpiryTimestamp dmsg)
    serialize (describedDelegationTokenMaxTimestamp dmsg)
    if version >= 2 then serialize (toCompactString (describedDelegationTokenTokenId dmsg)) else serialize (describedDelegationTokenTokenId dmsg)
    if version >= 2 then serialize (toCompactBytes (describedDelegationTokenHmac dmsg)) else serialize (describedDelegationTokenHmac dmsg)
    E.encodeVersionedArray version 2 encodeDescribedDelegationTokenRenewer (case P.unKafkaArray (describedDelegationTokenRenewers dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribedDelegationToken with version-aware field handling.
decodeDescribedDelegationToken :: MonadGet m => E.ApiVersion -> m DescribedDelegationToken
decodeDescribedDelegationToken version =
  do
    fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldtokenrequesterprincipaltype <- if version >= 3
      then if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtokenrequesterprincipalname <- if version >= 3
      then if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldissuetimestamp <- deserialize
    fieldexpirytimestamp <- deserialize
    fieldmaxtimestamp <- deserialize
    fieldtokenid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldhmac <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
    fieldrenewers <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribedDelegationTokenRenewer
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribedDelegationToken
      {
      describedDelegationTokenPrincipalType = fieldprincipaltype
      ,
      describedDelegationTokenPrincipalName = fieldprincipalname
      ,
      describedDelegationTokenTokenRequesterPrincipalType = fieldtokenrequesterprincipaltype
      ,
      describedDelegationTokenTokenRequesterPrincipalName = fieldtokenrequesterprincipalname
      ,
      describedDelegationTokenIssueTimestamp = fieldissuetimestamp
      ,
      describedDelegationTokenExpiryTimestamp = fieldexpirytimestamp
      ,
      describedDelegationTokenMaxTimestamp = fieldmaxtimestamp
      ,
      describedDelegationTokenTokenId = fieldtokenid
      ,
      describedDelegationTokenHmac = fieldhmac
      ,
      describedDelegationTokenRenewers = fieldrenewers
      }



data DescribeDelegationTokenResponse = DescribeDelegationTokenResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  describeDelegationTokenResponseErrorCode :: !(Int16)
,

  -- | The tokens.

  -- Versions: 0+
  describeDelegationTokenResponseTokens :: !(KafkaArray (DescribedDelegationToken))
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeDelegationTokenResponseThrottleTimeMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeDelegationTokenResponse.
maxDescribeDelegationTokenResponseVersion :: Int16
maxDescribeDelegationTokenResponseVersion = 3

-- | Encode DescribeDelegationTokenResponse with the given API version.
encodeDescribeDelegationTokenResponse :: MonadPut m => E.ApiVersion -> DescribeDelegationTokenResponse -> m ()
encodeDescribeDelegationTokenResponse version msg
  | version == 1 =
    do
      serialize (describeDelegationTokenResponseErrorCode msg)
      E.encodeVersionedArray version 2 encodeDescribedDelegationToken (case P.unKafkaArray (describeDelegationTokenResponseTokens msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeDelegationTokenResponseThrottleTimeMs msg)


  | version >= 2 && version <= 3 =
    do
      serialize (describeDelegationTokenResponseErrorCode msg)
      E.encodeVersionedArray version 2 encodeDescribedDelegationToken (case P.unKafkaArray (describeDelegationTokenResponseTokens msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeDelegationTokenResponse with the given API version.
decodeDescribeDelegationTokenResponse :: MonadGet m => E.ApiVersion -> m DescribeDelegationTokenResponse
decodeDescribeDelegationTokenResponse version
  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldtokens <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribedDelegationToken
      fieldthrottletimems <- deserialize
      pure DescribeDelegationTokenResponse
        {
        describeDelegationTokenResponseErrorCode = fielderrorcode
        ,
        describeDelegationTokenResponseTokens = fieldtokens
        ,
        describeDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version >= 2 && version <= 3 =
    do
      fielderrorcode <- deserialize
      fieldtokens <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribedDelegationToken
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeDelegationTokenResponse
        {
        describeDelegationTokenResponseErrorCode = fielderrorcode
        ,
        describeDelegationTokenResponseTokens = fieldtokens
        ,
        describeDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeDelegationTokenResponse' / 'decodeDescribeDelegationTokenResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeDelegationTokenResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeDelegationTokenResponse decodeDescribeDelegationTokenResponse)
  {-# INLINE wireCodec #-}
