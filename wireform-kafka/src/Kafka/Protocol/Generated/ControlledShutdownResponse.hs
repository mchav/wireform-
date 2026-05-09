{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControlledShutdownResponse
Description : Kafka ControlledShutdownResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 7.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControlledShutdownResponse
  (
    ControlledShutdownResponse(..),
    encodeControlledShutdownResponse,
    decodeControlledShutdownResponse,
    maxControlledShutdownResponseVersion
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




data ControlledShutdownResponse = ControlledShutdownResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControlledShutdownResponse.
maxControlledShutdownResponseVersion :: Int16
maxControlledShutdownResponseVersion = -1 -- No valid versions

-- | KafkaMessage instance for ControlledShutdownResponse.
instance KafkaMessage ControlledShutdownResponse where
  messageApiKey = 7
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode ControlledShutdownResponse with the given API version.
encodeControlledShutdownResponse :: MonadPut m => E.ApiVersion -> ControlledShutdownResponse -> m ()
encodeControlledShutdownResponse version msg
  = error "No valid versions"


-- | Decode ControlledShutdownResponse with the given API version.
decodeControlledShutdownResponse :: MonadGet m => E.ApiVersion -> m ControlledShutdownResponse
decodeControlledShutdownResponse version
  = fail "No valid versions"



-- | Worst-case wire size of a ControlledShutdownResponse.
wireMaxSizeControlledShutdownResponse :: Int -> ControlledShutdownResponse -> Int
wireMaxSizeControlledShutdownResponse _version msg =
  0



wirePokeControlledShutdownResponse :: Int -> Ptr Word8 -> ControlledShutdownResponse -> IO (Ptr Word8)
wirePokeControlledShutdownResponse _version _basePtr _msg =
  error "wirePoke ControlledShutdownResponse: no valid versions"

wirePeekControlledShutdownResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ControlledShutdownResponse, Ptr Word8)
wirePeekControlledShutdownResponse _version _fp _basePtr _p _endPtr =
  error "wirePeek ControlledShutdownResponse: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ControlledShutdownResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeControlledShutdownResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeControlledShutdownResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekControlledShutdownResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}