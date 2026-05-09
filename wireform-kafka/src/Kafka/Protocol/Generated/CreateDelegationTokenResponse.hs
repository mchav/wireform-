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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




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

-- | KafkaMessage instance for CreateDelegationTokenResponse.
instance KafkaMessage CreateDelegationTokenResponse where
  messageApiKey = 38
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

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


-- | Worst-case wire size of a CreateDelegationTokenResponse.
wireMaxSizeCreateDelegationTokenResponse :: Int -> CreateDelegationTokenResponse -> Int
wireMaxSizeCreateDelegationTokenResponse _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenResponsePrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenResponsePrincipalName msg))
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenResponseTokenRequesterPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenResponseTokenRequesterPrincipalName msg))
  + 8
  + 8
  + 8
  + WP.compactStringMaxSize (P.toCompactString (createDelegationTokenResponseTokenId msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (createDelegationTokenResponseHmac msg))
  + 4
  + 1

-- | Direct-poke encoder for CreateDelegationTokenResponse.
wirePokeCreateDelegationTokenResponse :: Int -> Ptr Word8 -> CreateDelegationTokenResponse -> IO (Ptr Word8)
wirePokeCreateDelegationTokenResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (createDelegationTokenResponseErrorCode msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (createDelegationTokenResponsePrincipalType msg))
    p3 <- WP.pokeCompactString p2 (P.toCompactString (createDelegationTokenResponsePrincipalName msg))
    p4 <- W.pokeInt64BE p3 (createDelegationTokenResponseIssueTimestampMs msg)
    p5 <- W.pokeInt64BE p4 (createDelegationTokenResponseExpiryTimestampMs msg)
    p6 <- W.pokeInt64BE p5 (createDelegationTokenResponseMaxTimestampMs msg)
    p7 <- WP.pokeCompactString p6 (P.toCompactString (createDelegationTokenResponseTokenId msg))
    p8 <- WP.pokeCompactBytes p7 (P.toCompactBytes (createDelegationTokenResponseHmac msg))
    p9 <- W.pokeInt32BE p8 (createDelegationTokenResponseThrottleTimeMs msg)
    pure p9
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (createDelegationTokenResponseErrorCode msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (createDelegationTokenResponsePrincipalType msg))
    p3 <- WP.pokeCompactString p2 (P.toCompactString (createDelegationTokenResponsePrincipalName msg))
    p4 <- W.pokeInt64BE p3 (createDelegationTokenResponseIssueTimestampMs msg)
    p5 <- W.pokeInt64BE p4 (createDelegationTokenResponseExpiryTimestampMs msg)
    p6 <- W.pokeInt64BE p5 (createDelegationTokenResponseMaxTimestampMs msg)
    p7 <- WP.pokeCompactString p6 (P.toCompactString (createDelegationTokenResponseTokenId msg))
    p8 <- WP.pokeCompactBytes p7 (P.toCompactBytes (createDelegationTokenResponseHmac msg))
    p9 <- W.pokeInt32BE p8 (createDelegationTokenResponseThrottleTimeMs msg)
    WP.pokeEmptyTaggedFields p9
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (createDelegationTokenResponseErrorCode msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (createDelegationTokenResponsePrincipalType msg))
    p3 <- WP.pokeCompactString p2 (P.toCompactString (createDelegationTokenResponsePrincipalName msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (createDelegationTokenResponseTokenRequesterPrincipalType msg))
    p5 <- WP.pokeCompactString p4 (P.toCompactString (createDelegationTokenResponseTokenRequesterPrincipalName msg))
    p6 <- W.pokeInt64BE p5 (createDelegationTokenResponseIssueTimestampMs msg)
    p7 <- W.pokeInt64BE p6 (createDelegationTokenResponseExpiryTimestampMs msg)
    p8 <- W.pokeInt64BE p7 (createDelegationTokenResponseMaxTimestampMs msg)
    p9 <- WP.pokeCompactString p8 (P.toCompactString (createDelegationTokenResponseTokenId msg))
    p10 <- WP.pokeCompactBytes p9 (P.toCompactBytes (createDelegationTokenResponseHmac msg))
    p11 <- W.pokeInt32BE p10 (createDelegationTokenResponseThrottleTimeMs msg)
    WP.pokeEmptyTaggedFields p11
  | otherwise = error $ "wirePoke CreateDelegationTokenResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for CreateDelegationTokenResponse.
wirePeekCreateDelegationTokenResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreateDelegationTokenResponse, Ptr Word8)
wirePeekCreateDelegationTokenResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_principaltype, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_principalname, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_issuetimestampms, p4) <- W.peekInt64BE p3 endPtr
    (f4_expirytimestampms, p5) <- W.peekInt64BE p4 endPtr
    (f5_maxtimestampms, p6) <- W.peekInt64BE p5 endPtr
    (f6_tokenid, p7) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr
    (f7_hmac, p8) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p7 endPtr
    (f8_throttletimems, p9) <- W.peekInt32BE p8 endPtr
    pure (CreateDelegationTokenResponse { createDelegationTokenResponseErrorCode = f0_errorcode, createDelegationTokenResponsePrincipalType = f1_principaltype, createDelegationTokenResponsePrincipalName = f2_principalname, createDelegationTokenResponseTokenRequesterPrincipalType = P.KafkaString Null, createDelegationTokenResponseTokenRequesterPrincipalName = P.KafkaString Null, createDelegationTokenResponseIssueTimestampMs = f3_issuetimestampms, createDelegationTokenResponseExpiryTimestampMs = f4_expirytimestampms, createDelegationTokenResponseMaxTimestampMs = f5_maxtimestampms, createDelegationTokenResponseTokenId = f6_tokenid, createDelegationTokenResponseHmac = f7_hmac, createDelegationTokenResponseThrottleTimeMs = f8_throttletimems }, p9)
  | version == 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_principaltype, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_principalname, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_issuetimestampms, p4) <- W.peekInt64BE p3 endPtr
    (f4_expirytimestampms, p5) <- W.peekInt64BE p4 endPtr
    (f5_maxtimestampms, p6) <- W.peekInt64BE p5 endPtr
    (f6_tokenid, p7) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p6 endPtr
    (f7_hmac, p8) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p7 endPtr
    (f8_throttletimems, p9) <- W.peekInt32BE p8 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p9 endPtr
    pure (CreateDelegationTokenResponse { createDelegationTokenResponseErrorCode = f0_errorcode, createDelegationTokenResponsePrincipalType = f1_principaltype, createDelegationTokenResponsePrincipalName = f2_principalname, createDelegationTokenResponseTokenRequesterPrincipalType = P.KafkaString Null, createDelegationTokenResponseTokenRequesterPrincipalName = P.KafkaString Null, createDelegationTokenResponseIssueTimestampMs = f3_issuetimestampms, createDelegationTokenResponseExpiryTimestampMs = f4_expirytimestampms, createDelegationTokenResponseMaxTimestampMs = f5_maxtimestampms, createDelegationTokenResponseTokenId = f6_tokenid, createDelegationTokenResponseHmac = f7_hmac, createDelegationTokenResponseThrottleTimeMs = f8_throttletimems }, pTagsEnd)
  | version == 3 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_principaltype, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_principalname, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_tokenrequesterprincipaltype, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_tokenrequesterprincipalname, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_issuetimestampms, p6) <- W.peekInt64BE p5 endPtr
    (f6_expirytimestampms, p7) <- W.peekInt64BE p6 endPtr
    (f7_maxtimestampms, p8) <- W.peekInt64BE p7 endPtr
    (f8_tokenid, p9) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p8 endPtr
    (f9_hmac, p10) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p9 endPtr
    (f10_throttletimems, p11) <- W.peekInt32BE p10 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p11 endPtr
    pure (CreateDelegationTokenResponse { createDelegationTokenResponseErrorCode = f0_errorcode, createDelegationTokenResponsePrincipalType = f1_principaltype, createDelegationTokenResponsePrincipalName = f2_principalname, createDelegationTokenResponseTokenRequesterPrincipalType = f3_tokenrequesterprincipaltype, createDelegationTokenResponseTokenRequesterPrincipalName = f4_tokenrequesterprincipalname, createDelegationTokenResponseIssueTimestampMs = f5_issuetimestampms, createDelegationTokenResponseExpiryTimestampMs = f6_expirytimestampms, createDelegationTokenResponseMaxTimestampMs = f7_maxtimestampms, createDelegationTokenResponseTokenId = f8_tokenid, createDelegationTokenResponseHmac = f9_hmac, createDelegationTokenResponseThrottleTimeMs = f10_throttletimems }, pTagsEnd)
  | otherwise = error $ "wirePeek CreateDelegationTokenResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec CreateDelegationTokenResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreateDelegationTokenResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreateDelegationTokenResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreateDelegationTokenResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}