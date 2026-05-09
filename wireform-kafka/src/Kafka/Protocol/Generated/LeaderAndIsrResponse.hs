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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC




data LeaderAndIsrResponse = LeaderAndIsrResponse
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderAndIsrResponse.
maxLeaderAndIsrResponseVersion :: Int16
maxLeaderAndIsrResponseVersion = -1 -- No valid versions

-- | Encode LeaderAndIsrResponse with the given API version.
encodeLeaderAndIsrResponse :: MonadPut m => E.ApiVersion -> LeaderAndIsrResponse -> m ()
encodeLeaderAndIsrResponse version msg
  = error "No valid versions"


-- | Decode LeaderAndIsrResponse with the given API version.
decodeLeaderAndIsrResponse :: MonadGet m => E.ApiVersion -> m LeaderAndIsrResponse
decodeLeaderAndIsrResponse version
  = fail "No valid versions"

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec LeaderAndIsrResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
