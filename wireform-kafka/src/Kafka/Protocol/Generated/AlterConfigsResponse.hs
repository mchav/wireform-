{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterConfigsResponse
Description : Kafka AlterConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 33.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterConfigsResponse
  (
    AlterConfigsResponse(..),
    AlterConfigsResourceResponse(..),
    encodeAlterConfigsResponse,
    decodeAlterConfigsResponse,
    maxAlterConfigsResponseVersion
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


-- | The responses for each resource.
data AlterConfigsResourceResponse = AlterConfigsResourceResponse
  {

  -- | The resource error code.

  -- Versions: 0+
  alterConfigsResourceResponseErrorCode :: !(Int16)
,

  -- | The resource error message, or null if there was no error.

  -- Versions: 0+
  alterConfigsResourceResponseErrorMessage :: !(KafkaString)
,

  -- | The resource type.

  -- Versions: 0+
  alterConfigsResourceResponseResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  alterConfigsResourceResponseResourceName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterConfigsResourceResponse with version-aware field handling.
encodeAlterConfigsResourceResponse :: MonadPut m => E.ApiVersion -> AlterConfigsResourceResponse -> m ()
encodeAlterConfigsResourceResponse version amsg =
  do
    serialize (alterConfigsResourceResponseErrorCode amsg)
    if version >= 2 then serialize (toCompactString (alterConfigsResourceResponseErrorMessage amsg)) else serialize (alterConfigsResourceResponseErrorMessage amsg)
    serialize (alterConfigsResourceResponseResourceType amsg)
    if version >= 2 then serialize (toCompactString (alterConfigsResourceResponseResourceName amsg)) else serialize (alterConfigsResourceResponseResourceName amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterConfigsResourceResponse with version-aware field handling.
decodeAlterConfigsResourceResponse :: MonadGet m => E.ApiVersion -> m AlterConfigsResourceResponse
decodeAlterConfigsResourceResponse version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterConfigsResourceResponse
      {
      alterConfigsResourceResponseErrorCode = fielderrorcode
      ,
      alterConfigsResourceResponseErrorMessage = fielderrormessage
      ,
      alterConfigsResourceResponseResourceType = fieldresourcetype
      ,
      alterConfigsResourceResponseResourceName = fieldresourcename
      }



data AlterConfigsResponse = AlterConfigsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  alterConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each resource.

  -- Versions: 0+
  alterConfigsResponseResponses :: !(KafkaArray (AlterConfigsResourceResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterConfigsResponse.
maxAlterConfigsResponseVersion :: Int16
maxAlterConfigsResponseVersion = 2

-- | KafkaMessage instance for AlterConfigsResponse.
instance KafkaMessage AlterConfigsResponse where
  messageApiKey = 33
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode AlterConfigsResponse with the given API version.
encodeAlterConfigsResponse :: MonadPut m => E.ApiVersion -> AlterConfigsResponse -> m ()
encodeAlterConfigsResponse version msg
  | version == 2 =
    do
      serialize (alterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterConfigsResourceResponse (case P.unKafkaArray (alterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (alterConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterConfigsResourceResponse (case P.unKafkaArray (alterConfigsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterConfigsResponse with the given API version.
decodeAlterConfigsResponse :: MonadGet m => E.ApiVersion -> m AlterConfigsResponse
decodeAlterConfigsResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResourceResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterConfigsResponse
        {
        alterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterConfigsResponseResponses = fieldresponses
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterConfigsResourceResponse
      pure AlterConfigsResponse
        {
        alterConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterConfigsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a AlterConfigsResourceResponse.
wireMaxSizeAlterConfigsResourceResponse :: Int -> AlterConfigsResourceResponse -> Int
wireMaxSizeAlterConfigsResourceResponse _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (alterConfigsResourceResponseErrorMessage msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (alterConfigsResourceResponseResourceName msg))
  + 1

-- | Direct-poke encoder for AlterConfigsResourceResponse.
wirePokeAlterConfigsResourceResponse :: Int -> Ptr Word8 -> AlterConfigsResourceResponse -> IO (Ptr Word8)
wirePokeAlterConfigsResourceResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (alterConfigsResourceResponseErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (alterConfigsResourceResponseErrorMessage msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (alterConfigsResourceResponseResourceType msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (alterConfigsResourceResponseResourceName msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for AlterConfigsResourceResponse.
wirePeekAlterConfigsResourceResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsResourceResponse, Ptr Word8)
wirePeekAlterConfigsResourceResponse version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_resourcetype, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_resourcename, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (AlterConfigsResourceResponse { alterConfigsResourceResponseErrorCode = f0_errorcode, alterConfigsResourceResponseErrorMessage = f1_errormessage, alterConfigsResourceResponseResourceType = f2_resourcetype, alterConfigsResourceResponseResourceName = f3_resourcename }, pTagsEnd)

-- | Worst-case wire size of a AlterConfigsResponse.
wireMaxSizeAlterConfigsResponse :: Int -> AlterConfigsResponse -> Int
wireMaxSizeAlterConfigsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (alterConfigsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterConfigsResourceResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterConfigsResponse.
wirePokeAlterConfigsResponse :: Int -> Ptr Word8 -> AlterConfigsResponse -> IO (Ptr Word8)
wirePokeAlterConfigsResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterConfigsResourceResponse version p x) p1 (alterConfigsResponseResponses msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterConfigsResourceResponse version p x) p1 (alterConfigsResponseResponses msg)
    pure p2
  | otherwise = error $ "wirePoke AlterConfigsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterConfigsResponse.
wirePeekAlterConfigsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsResponse, Ptr Word8)
wirePeekAlterConfigsResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterConfigsResourceResponse version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterConfigsResponse { alterConfigsResponseThrottleTimeMs = f0_throttletimems, alterConfigsResponseResponses = f1_responses }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterConfigsResourceResponse version _fp _basePtr p e) p1 endPtr
    pure (AlterConfigsResponse { alterConfigsResponseThrottleTimeMs = f0_throttletimems, alterConfigsResponseResponses = f1_responses }, p2)
  | otherwise = error $ "wirePeek AlterConfigsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterConfigsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterConfigsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterConfigsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterConfigsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}