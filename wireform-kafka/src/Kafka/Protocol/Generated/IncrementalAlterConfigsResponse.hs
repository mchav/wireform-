{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.IncrementalAlterConfigsResponse
Description : Kafka IncrementalAlterConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 44.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.IncrementalAlterConfigsResponse
  (
    IncrementalAlterConfigsResponse(..),
    AlterConfigsResourceResponse(..),
    maxIncrementalAlterConfigsResponseVersion
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


data IncrementalAlterConfigsResponse = IncrementalAlterConfigsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  incrementalAlterConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each resource.

  -- Versions: 0+
  incrementalAlterConfigsResponseResponses :: !(KafkaArray (AlterConfigsResourceResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for IncrementalAlterConfigsResponse.
maxIncrementalAlterConfigsResponseVersion :: Int16
maxIncrementalAlterConfigsResponseVersion = 1

-- | KafkaMessage instance for IncrementalAlterConfigsResponse.
instance KafkaMessage IncrementalAlterConfigsResponse where
  messageApiKey = 44
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a AlterConfigsResourceResponse.
wireMaxSizeAlterConfigsResourceResponse :: Int -> AlterConfigsResourceResponse -> Int
wireMaxSizeAlterConfigsResourceResponse _version msg =
  0
  + 2
  + WP.dualStringMaxSize (alterConfigsResourceResponseErrorMessage msg)
  + 1
  + WP.dualStringMaxSize (alterConfigsResourceResponseResourceName msg)
  + 1

-- | Direct-poke encoder for AlterConfigsResourceResponse.
wirePokeAlterConfigsResourceResponse :: Int -> Ptr Word8 -> AlterConfigsResourceResponse -> IO (Ptr Word8)
wirePokeAlterConfigsResourceResponse version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (alterConfigsResourceResponseErrorCode msg)
  p2 <- (if version >= 1 then WP.pokeCompactString p1 (P.toCompactString (alterConfigsResourceResponseErrorMessage msg)) else WP.pokeKafkaString p1 (alterConfigsResourceResponseErrorMessage msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (alterConfigsResourceResponseResourceType msg))
  p4 <- (if version >= 1 then WP.pokeCompactString p3 (P.toCompactString (alterConfigsResourceResponseResourceName msg)) else WP.pokeKafkaString p3 (alterConfigsResourceResponseResourceName msg))
  if version >= 1 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for AlterConfigsResourceResponse.
wirePeekAlterConfigsResourceResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsResourceResponse, Ptr Word8)
wirePeekAlterConfigsResourceResponse version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_resourcetype, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_resourcename, p4) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (AlterConfigsResourceResponse { alterConfigsResourceResponseErrorCode = f0_errorcode, alterConfigsResourceResponseErrorMessage = f1_errormessage, alterConfigsResourceResponseResourceType = f2_resourcetype, alterConfigsResourceResponseResourceName = f3_resourcename }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterConfigsResourceResponse :: AlterConfigsResourceResponse
defaultAlterConfigsResourceResponse = AlterConfigsResourceResponse { alterConfigsResourceResponseErrorCode = 0, alterConfigsResourceResponseErrorMessage = P.KafkaString Null, alterConfigsResourceResponseResourceType = 0, alterConfigsResourceResponseResourceName = P.KafkaString Null }

-- | Worst-case wire size of a IncrementalAlterConfigsResponse.
wireMaxSizeIncrementalAlterConfigsResponse :: Int -> IncrementalAlterConfigsResponse -> Int
wireMaxSizeIncrementalAlterConfigsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (incrementalAlterConfigsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterConfigsResourceResponse _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for IncrementalAlterConfigsResponse.
wirePokeIncrementalAlterConfigsResponse :: Int -> Ptr Word8 -> IncrementalAlterConfigsResponse -> IO (Ptr Word8)
wirePokeIncrementalAlterConfigsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (incrementalAlterConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeAlterConfigsResourceResponse version p x) p1 (incrementalAlterConfigsResponseResponses msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (incrementalAlterConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeAlterConfigsResourceResponse version p x) p1 (incrementalAlterConfigsResponseResponses msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke IncrementalAlterConfigsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for IncrementalAlterConfigsResponse.
wirePeekIncrementalAlterConfigsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (IncrementalAlterConfigsResponse, Ptr Word8)
wirePeekIncrementalAlterConfigsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekAlterConfigsResourceResponse version _fp _basePtr p e) p1 endPtr
    pure (IncrementalAlterConfigsResponse { incrementalAlterConfigsResponseThrottleTimeMs = f0_throttletimems, incrementalAlterConfigsResponseResponses = f1_responses }, p2)
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekAlterConfigsResourceResponse version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (IncrementalAlterConfigsResponse { incrementalAlterConfigsResponseThrottleTimeMs = f0_throttletimems, incrementalAlterConfigsResponseResponses = f1_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek IncrementalAlterConfigsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec IncrementalAlterConfigsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeIncrementalAlterConfigsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeIncrementalAlterConfigsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekIncrementalAlterConfigsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}