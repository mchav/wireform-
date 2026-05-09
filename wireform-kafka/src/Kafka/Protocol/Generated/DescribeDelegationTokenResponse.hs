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
    maxDescribeDelegationTokenResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | KafkaMessage instance for DescribeDelegationTokenResponse.
instance KafkaMessage DescribeDelegationTokenResponse where
  messageApiKey = 41
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a DescribedDelegationTokenRenewer.
wireMaxSizeDescribedDelegationTokenRenewer :: Int -> DescribedDelegationTokenRenewer -> Int
wireMaxSizeDescribedDelegationTokenRenewer _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenRenewerPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenRenewerPrincipalName msg))
  + 1

-- | Direct-poke encoder for DescribedDelegationTokenRenewer.
wirePokeDescribedDelegationTokenRenewer :: Int -> Ptr Word8 -> DescribedDelegationTokenRenewer -> IO (Ptr Word8)
wirePokeDescribedDelegationTokenRenewer version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describedDelegationTokenRenewerPrincipalType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedDelegationTokenRenewerPrincipalName msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribedDelegationTokenRenewer.
wirePeekDescribedDelegationTokenRenewer :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedDelegationTokenRenewer, Ptr Word8)
wirePeekDescribedDelegationTokenRenewer version _fp _basePtr p0 endPtr = do
  (f0_principaltype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_principalname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribedDelegationTokenRenewer { describedDelegationTokenRenewerPrincipalType = f0_principaltype, describedDelegationTokenRenewerPrincipalName = f1_principalname }, pTagsEnd)

-- | Worst-case wire size of a DescribedDelegationToken.
wireMaxSizeDescribedDelegationToken :: Int -> DescribedDelegationToken -> Int
wireMaxSizeDescribedDelegationToken _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenPrincipalName msg))
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenTokenRequesterPrincipalType msg))
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenTokenRequesterPrincipalName msg))
  + 8
  + 8
  + 8
  + WP.compactStringMaxSize (P.toCompactString (describedDelegationTokenTokenId msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (describedDelegationTokenHmac msg))
  + (5 + (case P.unKafkaArray (describedDelegationTokenRenewers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedDelegationTokenRenewer _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribedDelegationToken.
wirePokeDescribedDelegationToken :: Int -> Ptr Word8 -> DescribedDelegationToken -> IO (Ptr Word8)
wirePokeDescribedDelegationToken version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describedDelegationTokenPrincipalType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describedDelegationTokenPrincipalName msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describedDelegationTokenTokenRequesterPrincipalType msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describedDelegationTokenTokenRequesterPrincipalName msg))
  p5 <- W.pokeInt64BE p4 (describedDelegationTokenIssueTimestamp msg)
  p6 <- W.pokeInt64BE p5 (describedDelegationTokenExpiryTimestamp msg)
  p7 <- W.pokeInt64BE p6 (describedDelegationTokenMaxTimestamp msg)
  p8 <- WP.pokeCompactString p7 (P.toCompactString (describedDelegationTokenTokenId msg))
  p9 <- WP.pokeCompactBytes p8 (P.toCompactBytes (describedDelegationTokenHmac msg))
  p10 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribedDelegationTokenRenewer version p x) p9 (describedDelegationTokenRenewers msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p10 else pure p10

-- | Direct-poke decoder for DescribedDelegationToken.
wirePeekDescribedDelegationToken :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribedDelegationToken, Ptr Word8)
wirePeekDescribedDelegationToken version _fp _basePtr p0 endPtr = do
  (f0_principaltype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_principalname, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_tokenrequesterprincipaltype, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_tokenrequesterprincipalname, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_issuetimestamp, p5) <- W.peekInt64BE p4 endPtr
  (f5_expirytimestamp, p6) <- W.peekInt64BE p5 endPtr
  (f6_maxtimestamp, p7) <- W.peekInt64BE p6 endPtr
  (f7_tokenid, p8) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr
  (f8_hmac, p9) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p8 endPtr
  (f9_renewers, p10) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribedDelegationTokenRenewer version _fp _basePtr p e) p9 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p10 endPtr else pure p10
  pure (DescribedDelegationToken { describedDelegationTokenPrincipalType = f0_principaltype, describedDelegationTokenPrincipalName = f1_principalname, describedDelegationTokenTokenRequesterPrincipalType = f2_tokenrequesterprincipaltype, describedDelegationTokenTokenRequesterPrincipalName = f3_tokenrequesterprincipalname, describedDelegationTokenIssueTimestamp = f4_issuetimestamp, describedDelegationTokenExpiryTimestamp = f5_expirytimestamp, describedDelegationTokenMaxTimestamp = f6_maxtimestamp, describedDelegationTokenTokenId = f7_tokenid, describedDelegationTokenHmac = f8_hmac, describedDelegationTokenRenewers = f9_renewers }, pTagsEnd)

-- | Worst-case wire size of a DescribeDelegationTokenResponse.
wireMaxSizeDescribeDelegationTokenResponse :: Int -> DescribeDelegationTokenResponse -> Int
wireMaxSizeDescribeDelegationTokenResponse _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (describeDelegationTokenResponseTokens msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribedDelegationToken _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribeDelegationTokenResponse.
wirePokeDescribeDelegationTokenResponse :: Int -> Ptr Word8 -> DescribeDelegationTokenResponse -> IO (Ptr Word8)
wirePokeDescribeDelegationTokenResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (describeDelegationTokenResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribedDelegationToken version p x) p1 (describeDelegationTokenResponseTokens msg)
    p3 <- W.pokeInt32BE p2 (describeDelegationTokenResponseThrottleTimeMs msg)
    pure p3
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (describeDelegationTokenResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribedDelegationToken version p x) p1 (describeDelegationTokenResponseTokens msg)
    p3 <- W.pokeInt32BE p2 (describeDelegationTokenResponseThrottleTimeMs msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DescribeDelegationTokenResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeDelegationTokenResponse.
wirePeekDescribeDelegationTokenResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeDelegationTokenResponse, Ptr Word8)
wirePeekDescribeDelegationTokenResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_tokens, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribedDelegationToken version _fp _basePtr p e) p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pure (DescribeDelegationTokenResponse { describeDelegationTokenResponseErrorCode = f0_errorcode, describeDelegationTokenResponseTokens = f1_tokens, describeDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, p3)
  | version >= 2 && version <= 3 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_tokens, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribedDelegationToken version _fp _basePtr p e) p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeDelegationTokenResponse { describeDelegationTokenResponseErrorCode = f0_errorcode, describeDelegationTokenResponseTokens = f1_tokens, describeDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeDelegationTokenResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeDelegationTokenResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeDelegationTokenResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeDelegationTokenResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeDelegationTokenResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}