{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreatePartitionsResponse
Description : Kafka CreatePartitionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 37.



Valid versions: 0-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreatePartitionsResponse
  (
    CreatePartitionsResponse(..),
    CreatePartitionsTopicResult(..),
    encodeCreatePartitionsResponse,
    decodeCreatePartitionsResponse,
    maxCreatePartitionsResponseVersion
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


-- | The partition creation results for each topic.
data CreatePartitionsTopicResult = CreatePartitionsTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  createPartitionsTopicResultName :: !(KafkaString)
,

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  createPartitionsTopicResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  createPartitionsTopicResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsTopicResult with version-aware field handling.
encodeCreatePartitionsTopicResult :: MonadPut m => E.ApiVersion -> CreatePartitionsTopicResult -> m ()
encodeCreatePartitionsTopicResult version cmsg =
  do
    if version >= 2 then serialize (toCompactString (createPartitionsTopicResultName cmsg)) else serialize (createPartitionsTopicResultName cmsg)
    serialize (createPartitionsTopicResultErrorCode cmsg)
    if version >= 2 then serialize (toCompactString (createPartitionsTopicResultErrorMessage cmsg)) else serialize (createPartitionsTopicResultErrorMessage cmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsTopicResult with version-aware field handling.
decodeCreatePartitionsTopicResult :: MonadGet m => E.ApiVersion -> m CreatePartitionsTopicResult
decodeCreatePartitionsTopicResult version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsTopicResult
      {
      createPartitionsTopicResultName = fieldname
      ,
      createPartitionsTopicResultErrorCode = fielderrorcode
      ,
      createPartitionsTopicResultErrorMessage = fielderrormessage
      }



data CreatePartitionsResponse = CreatePartitionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  createPartitionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The partition creation results for each topic.

  -- Versions: 0+
  createPartitionsResponseResults :: !(KafkaArray (CreatePartitionsTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreatePartitionsResponse.
maxCreatePartitionsResponseVersion :: Int16
maxCreatePartitionsResponseVersion = 3

-- | KafkaMessage instance for CreatePartitionsResponse.
instance KafkaMessage CreatePartitionsResponse where
  messageApiKey = 37
  messageMinVersion = 0
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Encode CreatePartitionsResponse with the given API version.
encodeCreatePartitionsResponse :: MonadPut m => E.ApiVersion -> CreatePartitionsResponse -> m ()
encodeCreatePartitionsResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (createPartitionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopicResult (case P.unKafkaArray (createPartitionsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      serialize (createPartitionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopicResult (case P.unKafkaArray (createPartitionsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreatePartitionsResponse with the given API version.
decodeCreatePartitionsResponse :: MonadGet m => E.ApiVersion -> m CreatePartitionsResponse
decodeCreatePartitionsResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopicResult
      pure CreatePartitionsResponse
        {
        createPartitionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createPartitionsResponseResults = fieldresults
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreatePartitionsResponse
        {
        createPartitionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createPartitionsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a CreatePartitionsTopicResult.
wireMaxSizeCreatePartitionsTopicResult :: Int -> CreatePartitionsTopicResult -> Int
wireMaxSizeCreatePartitionsTopicResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (createPartitionsTopicResultName msg))
  + 2
  + WP.compactStringMaxSize (P.toCompactString (createPartitionsTopicResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for CreatePartitionsTopicResult.
wirePokeCreatePartitionsTopicResult :: Int -> Ptr Word8 -> CreatePartitionsTopicResult -> IO (Ptr Word8)
wirePokeCreatePartitionsTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (createPartitionsTopicResultName msg))
  p2 <- W.pokeInt16BE p1 (createPartitionsTopicResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (createPartitionsTopicResultErrorMessage msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for CreatePartitionsTopicResult.
wirePeekCreatePartitionsTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatePartitionsTopicResult, Ptr Word8)
wirePeekCreatePartitionsTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (CreatePartitionsTopicResult { createPartitionsTopicResultName = f0_name, createPartitionsTopicResultErrorCode = f1_errorcode, createPartitionsTopicResultErrorMessage = f2_errormessage }, pTagsEnd)

-- | Worst-case wire size of a CreatePartitionsResponse.
wireMaxSizeCreatePartitionsResponse :: Int -> CreatePartitionsResponse -> Int
wireMaxSizeCreatePartitionsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (createPartitionsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatePartitionsTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatePartitionsResponse.
wirePokeCreatePartitionsResponse :: Int -> Ptr Word8 -> CreatePartitionsResponse -> IO (Ptr Word8)
wirePokeCreatePartitionsResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (createPartitionsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatePartitionsTopicResult version p x) p1 (createPartitionsResponseResults msg)
    pure p2
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (createPartitionsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatePartitionsTopicResult version p x) p1 (createPartitionsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke CreatePartitionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for CreatePartitionsResponse.
wirePeekCreatePartitionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatePartitionsResponse, Ptr Word8)
wirePeekCreatePartitionsResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatePartitionsTopicResult version _fp _basePtr p e) p1 endPtr
    pure (CreatePartitionsResponse { createPartitionsResponseThrottleTimeMs = f0_throttletimems, createPartitionsResponseResults = f1_results }, p2)
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatePartitionsTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (CreatePartitionsResponse { createPartitionsResponseThrottleTimeMs = f0_throttletimems, createPartitionsResponseResults = f1_results }, pTagsEnd)
  | otherwise = error $ "wirePeek CreatePartitionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec CreatePartitionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreatePartitionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreatePartitionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreatePartitionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}