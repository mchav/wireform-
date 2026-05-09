{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaderAndIsrResponse
Description : Kafka LeaderAndIsrResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 4.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaderAndIsrResponse
  (
    LeaderAndIsrResponse(..),
    encodeLeaderAndIsrResponse,
    decodeLeaderAndIsrResponse,
    maxLeaderAndIsrResponseVersion
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




data LeaderAndIsrResponse = LeaderAndIsrResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderAndIsrResponse.
maxLeaderAndIsrResponseVersion :: Int16
maxLeaderAndIsrResponseVersion = -1 -- No valid versions

-- | KafkaMessage instance for LeaderAndIsrResponse.
instance KafkaMessage LeaderAndIsrResponse where
  messageApiKey = 4
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode LeaderAndIsrResponse with the given API version.
encodeLeaderAndIsrResponse :: MonadPut m => E.ApiVersion -> LeaderAndIsrResponse -> m ()
encodeLeaderAndIsrResponse version msg
  = error "No valid versions"


-- | Decode LeaderAndIsrResponse with the given API version.
decodeLeaderAndIsrResponse :: MonadGet m => E.ApiVersion -> m LeaderAndIsrResponse
decodeLeaderAndIsrResponse version
  = fail "No valid versions"



-- | Worst-case wire size of a LeaderAndIsrResponse.
wireMaxSizeLeaderAndIsrResponse :: Int -> LeaderAndIsrResponse -> Int
wireMaxSizeLeaderAndIsrResponse _version msg =
  0



wirePokeLeaderAndIsrResponse :: Int -> Ptr Word8 -> LeaderAndIsrResponse -> IO (Ptr Word8)
wirePokeLeaderAndIsrResponse _version _basePtr _msg =
  error "wirePoke LeaderAndIsrResponse: no valid versions"

wirePeekLeaderAndIsrResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderAndIsrResponse, Ptr Word8)
wirePeekLeaderAndIsrResponse _version _fp _basePtr _p _endPtr =
  error "wirePeek LeaderAndIsrResponse: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec LeaderAndIsrResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeLeaderAndIsrResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeLeaderAndIsrResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekLeaderAndIsrResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}