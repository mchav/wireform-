{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteGroupsRequest
Description : Kafka DeleteGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 42.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteGroupsRequest
  (
    DeleteGroupsRequest(..),
    encodeDeleteGroupsRequest,
    decodeDeleteGroupsRequest,
    maxDeleteGroupsRequestVersion
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




data DeleteGroupsRequest = DeleteGroupsRequest
  {

  -- | The group names to delete.

  -- Versions: 0+
  deleteGroupsRequestGroupsNames :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteGroupsRequest.
maxDeleteGroupsRequestVersion :: Int16
maxDeleteGroupsRequestVersion = 2

-- | Encode DeleteGroupsRequest with the given API version.
encodeDeleteGroupsRequest :: MonadPut m => E.ApiVersion -> DeleteGroupsRequest -> m ()
encodeDeleteGroupsRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 2 (\v s -> if v >= 2 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (deleteGroupsRequestGroupsNames msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 (\v s -> if v >= 2 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (deleteGroupsRequestGroupsNames msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteGroupsRequest with the given API version.
decodeDeleteGroupsRequest :: MonadGet m => E.ApiVersion -> m DeleteGroupsRequest
decodeDeleteGroupsRequest version
  | version == 2 =
    do
      fieldgroupsnames <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\v -> if v >= 2 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteGroupsRequest
        {
        deleteGroupsRequestGroupsNames = fieldgroupsnames
        }

  | version >= 0 && version <= 1 =
    do
      fieldgroupsnames <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\v -> if v >= 2 then P.fromCompactString <$> deserialize else deserialize)
      pure DeleteGroupsRequest
        {
        deleteGroupsRequestGroupsNames = fieldgroupsnames
        }
  | otherwise = fail $ "Unsupported version: " ++ show version