{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateDelegationTokenResponse
Description : Kafka CreateDelegationTokenResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 38.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateDelegationTokenResponse
  (
    CreateDelegationTokenResponse(..),
    encodeCreateDelegationTokenResponse,
    decodeCreateDelegationTokenResponse,
    maxCreateDelegationTokenResponseVersion
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




data CreateDelegationTokenResponse = CreateDelegationTokenResponse
  {

  -- | The top-level error, or zero if there was no error.

  -- Versions: 0+
  createDelegationTokenResponseErrorCode :: !(Int16)
,

  -- | The principal type of the token owner.

  -- Versions: 0+
  createDelegationTokenResponsePrincipalType :: !(KafkaString)
,

  -- | The name of the token owner.

  -- Versions: 0+
  createDelegationTokenResponsePrincipalName :: !(KafkaString)
,

  -- | The principal type of the requester of the token.

  -- Versions: 3+
  createDelegationTokenResponseTokenRequesterPrincipalType :: !(KafkaString)
,

  -- | The principal type of the requester of the token.

  -- Versions: 3+
  createDelegationTokenResponseTokenRequesterPrincipalName :: !(KafkaString)
,

  -- | When this token was generated.

  -- Versions: 0+
  createDelegationTokenResponseIssueTimestampMs :: !(Int64)
,

  -- | When this token expires.

  -- Versions: 0+
  createDelegationTokenResponseExpiryTimestampMs :: !(Int64)
,

  -- | The maximum lifetime of this token.

  -- Versions: 0+
  createDelegationTokenResponseMaxTimestampMs :: !(Int64)
,

  -- | The token UUID.

  -- Versions: 0+
  createDelegationTokenResponseTokenId :: !(KafkaString)
,

  -- | HMAC of the delegation token.

  -- Versions: 0+
  createDelegationTokenResponseHmac :: !(KafkaBytes)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  createDelegationTokenResponseThrottleTimeMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateDelegationTokenResponse.
maxCreateDelegationTokenResponseVersion :: Int16
maxCreateDelegationTokenResponseVersion = 3

-- | Encode CreateDelegationTokenResponse with the given API version.
encodeCreateDelegationTokenResponse :: MonadPut m => E.ApiVersion -> CreateDelegationTokenResponse -> m ()
encodeCreateDelegationTokenResponse version msg
  | version == 1 =
    do
      serialize (createDelegationTokenResponseErrorCode msg)
      serialize (createDelegationTokenResponsePrincipalType msg)
      serialize (createDelegationTokenResponsePrincipalName msg)
      serialize (createDelegationTokenResponseIssueTimestampMs msg)
      serialize (createDelegationTokenResponseExpiryTimestampMs msg)
      serialize (createDelegationTokenResponseMaxTimestampMs msg)
      serialize (createDelegationTokenResponseTokenId msg)
      serialize (createDelegationTokenResponseHmac msg)
      serialize (createDelegationTokenResponseThrottleTimeMs msg)


  | version == 2 =
    do
      serialize (createDelegationTokenResponseErrorCode msg)
      serialize (toCompactString (createDelegationTokenResponsePrincipalType msg))
      serialize (toCompactString (createDelegationTokenResponsePrincipalName msg))
      serialize (createDelegationTokenResponseIssueTimestampMs msg)
      serialize (createDelegationTokenResponseExpiryTimestampMs msg)
      serialize (createDelegationTokenResponseMaxTimestampMs msg)
      serialize (toCompactString (createDelegationTokenResponseTokenId msg))
      serialize (toCompactBytes (createDelegationTokenResponseHmac msg))
      serialize (createDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 3 =
    do
      serialize (createDelegationTokenResponseErrorCode msg)
      serialize (toCompactString (createDelegationTokenResponsePrincipalType msg))
      serialize (toCompactString (createDelegationTokenResponsePrincipalName msg))
      serialize (toCompactString (createDelegationTokenResponseTokenRequesterPrincipalType msg))
      serialize (toCompactString (createDelegationTokenResponseTokenRequesterPrincipalName msg))
      serialize (createDelegationTokenResponseIssueTimestampMs msg)
      serialize (createDelegationTokenResponseExpiryTimestampMs msg)
      serialize (createDelegationTokenResponseMaxTimestampMs msg)
      serialize (toCompactString (createDelegationTokenResponseTokenId msg))
      serialize (toCompactBytes (createDelegationTokenResponseHmac msg))
      serialize (createDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateDelegationTokenResponse with the given API version.
decodeCreateDelegationTokenResponse :: MonadGet m => E.ApiVersion -> m CreateDelegationTokenResponse
decodeCreateDelegationTokenResponse version
  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldprincipaltype <- deserialize
      fieldprincipalname <- deserialize
      fieldissuetimestampms <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldmaxtimestampms <- deserialize
      fieldtokenid <- deserialize
      fieldhmac <- deserialize
      fieldthrottletimems <- deserialize
      pure CreateDelegationTokenResponse
        {
        createDelegationTokenResponseErrorCode = fielderrorcode
        ,
        createDelegationTokenResponsePrincipalType = fieldprincipaltype
        ,
        createDelegationTokenResponsePrincipalName = fieldprincipalname
        ,
        createDelegationTokenResponseTokenRequesterPrincipalType = P.KafkaString Null
        ,
        createDelegationTokenResponseTokenRequesterPrincipalName = P.KafkaString Null
        ,
        createDelegationTokenResponseIssueTimestampMs = fieldissuetimestampms
        ,
        createDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        createDelegationTokenResponseMaxTimestampMs = fieldmaxtimestampms
        ,
        createDelegationTokenResponseTokenId = fieldtokenid
        ,
        createDelegationTokenResponseHmac = fieldhmac
        ,
        createDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version == 2 =
    do
      fielderrorcode <- deserialize
      fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldissuetimestampms <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldmaxtimestampms <- deserialize
      fieldtokenid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldhmac <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateDelegationTokenResponse
        {
        createDelegationTokenResponseErrorCode = fielderrorcode
        ,
        createDelegationTokenResponsePrincipalType = fieldprincipaltype
        ,
        createDelegationTokenResponsePrincipalName = fieldprincipalname
        ,
        createDelegationTokenResponseTokenRequesterPrincipalType = P.KafkaString Null
        ,
        createDelegationTokenResponseTokenRequesterPrincipalName = P.KafkaString Null
        ,
        createDelegationTokenResponseIssueTimestampMs = fieldissuetimestampms
        ,
        createDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        createDelegationTokenResponseMaxTimestampMs = fieldmaxtimestampms
        ,
        createDelegationTokenResponseTokenId = fieldtokenid
        ,
        createDelegationTokenResponseHmac = fieldhmac
        ,
        createDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version == 3 =
    do
      fielderrorcode <- deserialize
      fieldprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldtokenrequesterprincipaltype <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldtokenrequesterprincipalname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldissuetimestampms <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldmaxtimestampms <- deserialize
      fieldtokenid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldhmac <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateDelegationTokenResponse
        {
        createDelegationTokenResponseErrorCode = fielderrorcode
        ,
        createDelegationTokenResponsePrincipalType = fieldprincipaltype
        ,
        createDelegationTokenResponsePrincipalName = fieldprincipalname
        ,
        createDelegationTokenResponseTokenRequesterPrincipalType = fieldtokenrequesterprincipaltype
        ,
        createDelegationTokenResponseTokenRequesterPrincipalName = fieldtokenrequesterprincipalname
        ,
        createDelegationTokenResponseIssueTimestampMs = fieldissuetimestampms
        ,
        createDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        createDelegationTokenResponseMaxTimestampMs = fieldmaxtimestampms
        ,
        createDelegationTokenResponseTokenId = fieldtokenid
        ,
        createDelegationTokenResponseHmac = fieldhmac
        ,
        createDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec CreateDelegationTokenResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
