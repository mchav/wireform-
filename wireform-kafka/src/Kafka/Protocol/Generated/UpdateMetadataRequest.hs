{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateMetadataRequest
Description : Kafka UpdateMetadataRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 6.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateMetadataRequest
  (
    UpdateMetadataRequest(..),
    encodeUpdateMetadataRequest,
    decodeUpdateMetadataRequest,
    maxUpdateMetadataRequestVersion
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




data UpdateMetadataRequest = UpdateMetadataRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateMetadataRequest.
maxUpdateMetadataRequestVersion :: Int16
maxUpdateMetadataRequestVersion = -1 -- No valid versions

-- | KafkaMessage instance for UpdateMetadataRequest.
instance KafkaMessage UpdateMetadataRequest where
  messageApiKey = 6
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode UpdateMetadataRequest with the given API version.
encodeUpdateMetadataRequest :: MonadPut m => E.ApiVersion -> UpdateMetadataRequest -> m ()
encodeUpdateMetadataRequest version msg
  = error "No valid versions"


-- | Decode UpdateMetadataRequest with the given API version.
decodeUpdateMetadataRequest :: MonadGet m => E.ApiVersion -> m UpdateMetadataRequest
decodeUpdateMetadataRequest version
  = fail "No valid versions"



-- | Worst-case wire size of a UpdateMetadataRequest.
wireMaxSizeUpdateMetadataRequest :: Int -> UpdateMetadataRequest -> Int
wireMaxSizeUpdateMetadataRequest _version msg =
  0



wirePokeUpdateMetadataRequest :: Int -> Ptr Word8 -> UpdateMetadataRequest -> IO (Ptr Word8)
wirePokeUpdateMetadataRequest _version _basePtr _msg =
  error "wirePoke UpdateMetadataRequest: no valid versions"

wirePeekUpdateMetadataRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateMetadataRequest, Ptr Word8)
wirePeekUpdateMetadataRequest _version _fp _basePtr _p _endPtr =
  error "wirePeek UpdateMetadataRequest: no valid versions"


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec UpdateMetadataRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateMetadataRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateMetadataRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateMetadataRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}