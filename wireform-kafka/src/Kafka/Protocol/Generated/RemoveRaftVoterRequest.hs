{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RemoveRaftVoterRequest
Description : Kafka RemoveRaftVoterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 81.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RemoveRaftVoterRequest
  (
    RemoveRaftVoterRequest(..),
    encodeRemoveRaftVoterRequest,
    decodeRemoveRaftVoterRequest,
    maxRemoveRaftVoterRequestVersion
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




data RemoveRaftVoterRequest = RemoveRaftVoterRequest
  {

  -- | The cluster id of the request.

  -- Versions: 0+
  removeRaftVoterRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter getting removed from the topic partition.

  -- Versions: 0+
  removeRaftVoterRequestVoterId :: !(Int32)
,

  -- | The directory id of the voter getting removed from the topic partition.

  -- Versions: 0+
  removeRaftVoterRequestVoterDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RemoveRaftVoterRequest.
maxRemoveRaftVoterRequestVersion :: Int16
maxRemoveRaftVoterRequestVersion = 0

-- | Encode RemoveRaftVoterRequest with the given API version.
encodeRemoveRaftVoterRequest :: MonadPut m => E.ApiVersion -> RemoveRaftVoterRequest -> m ()
encodeRemoveRaftVoterRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (removeRaftVoterRequestClusterId msg))
      serialize (removeRaftVoterRequestVoterId msg)
      serialize (removeRaftVoterRequestVoterDirectoryId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RemoveRaftVoterRequest with the given API version.
decodeRemoveRaftVoterRequest :: MonadGet m => E.ApiVersion -> m RemoveRaftVoterRequest
decodeRemoveRaftVoterRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RemoveRaftVoterRequest
        {
        removeRaftVoterRequestClusterId = fieldclusterid
        ,
        removeRaftVoterRequestVoterId = fieldvoterid
        ,
        removeRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeRemoveRaftVoterRequest' / 'decodeRemoveRaftVoterRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec RemoveRaftVoterRequest where
  wireCodec = Just (WC.serialShimCodec encodeRemoveRaftVoterRequest decodeRemoveRaftVoterRequest)
  {-# INLINE wireCodec #-}
