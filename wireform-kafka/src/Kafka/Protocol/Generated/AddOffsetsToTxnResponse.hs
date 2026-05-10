{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddOffsetsToTxnResponse
Description : Kafka AddOffsetsToTxnResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 25.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddOffsetsToTxnResponse
  (
    AddOffsetsToTxnResponse(..),
    maxAddOffsetsToTxnResponseVersion
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




data AddOffsetsToTxnResponse = AddOffsetsToTxnResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  addOffsetsToTxnResponseThrottleTimeMs :: !(Int32)
,

  -- | The response error code, or 0 if there was no error.

  -- Versions: 0+
  addOffsetsToTxnResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddOffsetsToTxnResponse.
maxAddOffsetsToTxnResponseVersion :: Int16
maxAddOffsetsToTxnResponseVersion = 4

-- | KafkaMessage instance for AddOffsetsToTxnResponse.
instance KafkaMessage AddOffsetsToTxnResponse where
  messageApiKey = 25
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 3


-- | Worst-case wire size of a AddOffsetsToTxnResponse.
wireMaxSizeAddOffsetsToTxnResponse :: Int -> AddOffsetsToTxnResponse -> Int
wireMaxSizeAddOffsetsToTxnResponse _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for AddOffsetsToTxnResponse.
wirePokeAddOffsetsToTxnResponse :: Int -> Ptr Word8 -> AddOffsetsToTxnResponse -> IO (Ptr Word8)
wirePokeAddOffsetsToTxnResponse version basePtr msg
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addOffsetsToTxnResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (addOffsetsToTxnResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (addOffsetsToTxnResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (addOffsetsToTxnResponseErrorCode msg)
    pure p2
  | otherwise = error $ "wirePoke AddOffsetsToTxnResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AddOffsetsToTxnResponse.
wirePeekAddOffsetsToTxnResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddOffsetsToTxnResponse, Ptr Word8)
wirePeekAddOffsetsToTxnResponse version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AddOffsetsToTxnResponse { addOffsetsToTxnResponseThrottleTimeMs = f0_throttletimems, addOffsetsToTxnResponseErrorCode = f1_errorcode }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pure (AddOffsetsToTxnResponse { addOffsetsToTxnResponseThrottleTimeMs = f0_throttletimems, addOffsetsToTxnResponseErrorCode = f1_errorcode }, p2)
  | otherwise = error $ "wirePeek AddOffsetsToTxnResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AddOffsetsToTxnResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddOffsetsToTxnResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddOffsetsToTxnResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddOffsetsToTxnResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}