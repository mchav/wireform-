{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterUserScramCredentialsResponse
Description : Kafka AlterUserScramCredentialsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 51.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterUserScramCredentialsResponse
  (
    AlterUserScramCredentialsResponse(..),
    AlterUserScramCredentialsResult(..),
    encodeAlterUserScramCredentialsResponse,
    decodeAlterUserScramCredentialsResponse,
    maxAlterUserScramCredentialsResponseVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The results for deletions and alterations, one per affected user.
data AlterUserScramCredentialsResult = AlterUserScramCredentialsResult
  {

  -- | The user name.

  -- Versions: 0+
  alterUserScramCredentialsResultUser :: !(KafkaString)
,

  -- | The error code.

  -- Versions: 0+
  alterUserScramCredentialsResultErrorCode :: !(Int16)
,

  -- | The error message, if any.

  -- Versions: 0+
  alterUserScramCredentialsResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterUserScramCredentialsResult with version-aware field handling.
encodeAlterUserScramCredentialsResult :: MonadPut m => E.ApiVersion -> AlterUserScramCredentialsResult -> m ()
encodeAlterUserScramCredentialsResult version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterUserScramCredentialsResultUser amsg)) else serialize (alterUserScramCredentialsResultUser amsg)
    serialize (alterUserScramCredentialsResultErrorCode amsg)
    if version >= 0 then serialize (toCompactString (alterUserScramCredentialsResultErrorMessage amsg)) else serialize (alterUserScramCredentialsResultErrorMessage amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterUserScramCredentialsResult with version-aware field handling.
decodeAlterUserScramCredentialsResult :: MonadGet m => E.ApiVersion -> m AlterUserScramCredentialsResult
decodeAlterUserScramCredentialsResult version =
  do
    fielduser <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterUserScramCredentialsResult
      {
      alterUserScramCredentialsResultUser = fielduser
      ,
      alterUserScramCredentialsResultErrorCode = fielderrorcode
      ,
      alterUserScramCredentialsResultErrorMessage = fielderrormessage
      }



data AlterUserScramCredentialsResponse = AlterUserScramCredentialsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterUserScramCredentialsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for deletions and alterations, one per affected user.

  -- Versions: 0+
  alterUserScramCredentialsResponseResults :: !(KafkaArray (AlterUserScramCredentialsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterUserScramCredentialsResponse.
maxAlterUserScramCredentialsResponseVersion :: Int16
maxAlterUserScramCredentialsResponseVersion = 0

-- | KafkaMessage instance for AlterUserScramCredentialsResponse.
instance KafkaMessage AlterUserScramCredentialsResponse where
  messageApiKey = 51
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode AlterUserScramCredentialsResponse with the given API version.
encodeAlterUserScramCredentialsResponse :: MonadPut m => E.ApiVersion -> AlterUserScramCredentialsResponse -> m ()
encodeAlterUserScramCredentialsResponse version msg
  | version == 0 =
    do
      serialize (alterUserScramCredentialsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeAlterUserScramCredentialsResult (case P.unKafkaArray (alterUserScramCredentialsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterUserScramCredentialsResponse with the given API version.
decodeAlterUserScramCredentialsResponse :: MonadGet m => E.ApiVersion -> m AlterUserScramCredentialsResponse
decodeAlterUserScramCredentialsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterUserScramCredentialsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterUserScramCredentialsResponse
        {
        alterUserScramCredentialsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterUserScramCredentialsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a AlterUserScramCredentialsResult.
wireMaxSizeAlterUserScramCredentialsResult :: Int -> AlterUserScramCredentialsResult -> Int
wireMaxSizeAlterUserScramCredentialsResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterUserScramCredentialsResultUser msg))
  + 2
  + WP.compactStringMaxSize (P.toCompactString (alterUserScramCredentialsResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for AlterUserScramCredentialsResult.
wirePokeAlterUserScramCredentialsResult :: Int -> Ptr Word8 -> AlterUserScramCredentialsResult -> IO (Ptr Word8)
wirePokeAlterUserScramCredentialsResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterUserScramCredentialsResultUser msg))
  p2 <- W.pokeInt16BE p1 (alterUserScramCredentialsResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (alterUserScramCredentialsResultErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterUserScramCredentialsResult.
wirePeekAlterUserScramCredentialsResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterUserScramCredentialsResult, Ptr Word8)
wirePeekAlterUserScramCredentialsResult version _fp _basePtr p0 endPtr = do
  (f0_user, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterUserScramCredentialsResult { alterUserScramCredentialsResultUser = f0_user, alterUserScramCredentialsResultErrorCode = f1_errorcode, alterUserScramCredentialsResultErrorMessage = f2_errormessage }, pTagsEnd)

-- | Worst-case wire size of a AlterUserScramCredentialsResponse.
wireMaxSizeAlterUserScramCredentialsResponse :: Int -> AlterUserScramCredentialsResponse -> Int
wireMaxSizeAlterUserScramCredentialsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (alterUserScramCredentialsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterUserScramCredentialsResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterUserScramCredentialsResponse.
wirePokeAlterUserScramCredentialsResponse :: Int -> Ptr Word8 -> AlterUserScramCredentialsResponse -> IO (Ptr Word8)
wirePokeAlterUserScramCredentialsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterUserScramCredentialsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeAlterUserScramCredentialsResult version p x) p1 (alterUserScramCredentialsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AlterUserScramCredentialsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterUserScramCredentialsResponse.
wirePeekAlterUserScramCredentialsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterUserScramCredentialsResponse, Ptr Word8)
wirePeekAlterUserScramCredentialsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekAlterUserScramCredentialsResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterUserScramCredentialsResponse { alterUserScramCredentialsResponseThrottleTimeMs = f0_throttletimems, alterUserScramCredentialsResponseResults = f1_results }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterUserScramCredentialsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterUserScramCredentialsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterUserScramCredentialsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterUserScramCredentialsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterUserScramCredentialsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}