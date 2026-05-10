{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateMetadataResponse
Description : Kafka UpdateMetadataResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 6.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateMetadataResponse
  (
    UpdateMetadataResponse(..),
    maxUpdateMetadataResponseVersion
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




data UpdateMetadataResponse = UpdateMetadataResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateMetadataResponse.
maxUpdateMetadataResponseVersion :: Int16
maxUpdateMetadataResponseVersion = -1 -- No valid versions

-- | KafkaMessage instance for UpdateMetadataResponse.
instance KafkaMessage UpdateMetadataResponse where
  messageApiKey = 6
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a UpdateMetadataResponse.
wireMaxSizeUpdateMetadataResponse :: Int -> UpdateMetadataResponse -> Int
wireMaxSizeUpdateMetadataResponse _version msg =
  0



wirePokeUpdateMetadataResponse :: Int -> Ptr Word8 -> UpdateMetadataResponse -> IO (Ptr Word8)
wirePokeUpdateMetadataResponse _version _basePtr _msg =
  error "wirePoke UpdateMetadataResponse: no valid versions"

wirePeekUpdateMetadataResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateMetadataResponse, Ptr Word8)
wirePeekUpdateMetadataResponse _version _fp _basePtr _p _endPtr =
  error "wirePeek UpdateMetadataResponse: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec UpdateMetadataResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateMetadataResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateMetadataResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateMetadataResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}