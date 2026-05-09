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

-- | KafkaMessage instance for DescribeUserScramCredentialsResponse.
instance KafkaMessage DescribeUserScramCredentialsResponse where
  messageApiKey = 50
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

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

-- | Worst-case wire size of a CredentialInfo.
wireMaxSizeCredentialInfo :: Int -> CredentialInfo -> Int
wireMaxSizeCredentialInfo _version msg =
  0
  + 1
  + 4
  + 1

-- | Direct-poke encoder for CredentialInfo.
wirePokeCredentialInfo :: Int -> Ptr Word8 -> CredentialInfo -> IO (Ptr Word8)
wirePokeCredentialInfo version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (credentialInfoMechanism msg))
  p2 <- W.pokeInt32BE p1 (credentialInfoIterations msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for CredentialInfo.
wirePeekCredentialInfo :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CredentialInfo, Ptr Word8)
wirePeekCredentialInfo version _fp _basePtr p0 endPtr = do
  (f0_mechanism, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_iterations, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (CredentialInfo { credentialInfoMechanism = f0_mechanism, credentialInfoIterations = f1_iterations }, pTagsEnd)

-- | Worst-case wire size of a DescribeUserScramCredentialsResult.
wireMaxSizeDescribeUserScramCredentialsResult :: Int -> DescribeUserScramCredentialsResult -> Int
wireMaxSizeDescribeUserScramCredentialsResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeUserScramCredentialsResultUser msg))
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeUserScramCredentialsResultErrorMessage msg))
  + (5 + (case P.unKafkaArray (describeUserScramCredentialsResultCredentialInfos msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCredentialInfo _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeUserScramCredentialsResult.
wirePokeDescribeUserScramCredentialsResult :: Int -> Ptr Word8 -> DescribeUserScramCredentialsResult -> IO (Ptr Word8)
wirePokeDescribeUserScramCredentialsResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeUserScramCredentialsResultUser msg))
  p2 <- W.pokeInt16BE p1 (describeUserScramCredentialsResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (describeUserScramCredentialsResultErrorMessage msg))
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeCredentialInfo version p x) p3 (describeUserScramCredentialsResultCredentialInfos msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DescribeUserScramCredentialsResult.
wirePeekDescribeUserScramCredentialsResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeUserScramCredentialsResult, Ptr Word8)
wirePeekDescribeUserScramCredentialsResult version _fp _basePtr p0 endPtr = do
  (f0_user, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_credentialinfos, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekCredentialInfo version _fp _basePtr p e) p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DescribeUserScramCredentialsResult { describeUserScramCredentialsResultUser = f0_user, describeUserScramCredentialsResultErrorCode = f1_errorcode, describeUserScramCredentialsResultErrorMessage = f2_errormessage, describeUserScramCredentialsResultCredentialInfos = f3_credentialinfos }, pTagsEnd)

-- | Worst-case wire size of a DescribeUserScramCredentialsResponse.
wireMaxSizeDescribeUserScramCredentialsResponse :: Int -> DescribeUserScramCredentialsResponse -> Int
wireMaxSizeDescribeUserScramCredentialsResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeUserScramCredentialsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (describeUserScramCredentialsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeUserScramCredentialsResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeUserScramCredentialsResponse.
wirePokeDescribeUserScramCredentialsResponse :: Int -> Ptr Word8 -> DescribeUserScramCredentialsResponse -> IO (Ptr Word8)
wirePokeDescribeUserScramCredentialsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeUserScramCredentialsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeUserScramCredentialsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (describeUserScramCredentialsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeUserScramCredentialsResult version p x) p3 (describeUserScramCredentialsResponseResults msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke DescribeUserScramCredentialsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeUserScramCredentialsResponse.
wirePeekDescribeUserScramCredentialsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeUserScramCredentialsResponse, Ptr Word8)
wirePeekDescribeUserScramCredentialsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_results, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeUserScramCredentialsResult version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DescribeUserScramCredentialsResponse { describeUserScramCredentialsResponseThrottleTimeMs = f0_throttletimems, describeUserScramCredentialsResponseErrorCode = f1_errorcode, describeUserScramCredentialsResponseErrorMessage = f2_errormessage, describeUserScramCredentialsResponseResults = f3_results }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeUserScramCredentialsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeUserScramCredentialsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeUserScramCredentialsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeUserScramCredentialsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeUserScramCredentialsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}