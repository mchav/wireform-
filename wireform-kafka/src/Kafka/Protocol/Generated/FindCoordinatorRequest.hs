{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FindCoordinatorRequest
Description : Kafka FindCoordinatorRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 10.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FindCoordinatorRequest
  (
    FindCoordinatorRequest(..),
    encodeFindCoordinatorRequest,
    decodeFindCoordinatorRequest,
    maxFindCoordinatorRequestVersion
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




data FindCoordinatorRequest = FindCoordinatorRequest
  {

  -- | The coordinator key.

  -- Versions: 0-3
  findCoordinatorRequestKey :: !(KafkaString)
,

  -- | The coordinator key type. (group, transaction, share).

  -- Versions: 1+
  findCoordinatorRequestKeyType :: !(Int8)
,

  -- | The coordinator keys.

  -- Versions: 4+
  findCoordinatorRequestCoordinatorKeys :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FindCoordinatorRequest.
maxFindCoordinatorRequestVersion :: Int16
maxFindCoordinatorRequestVersion = 6

-- | Encode FindCoordinatorRequest with the given API version.
encodeFindCoordinatorRequest :: MonadPut m => E.ApiVersion -> FindCoordinatorRequest -> m ()
encodeFindCoordinatorRequest version msg
  | version == 0 =
    do
      serialize (findCoordinatorRequestKey msg)


  | version == 3 =
    do
      serialize (toCompactString (findCoordinatorRequestKey msg))
      serialize (findCoordinatorRequestKeyType msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (findCoordinatorRequestKey msg)
      serialize (findCoordinatorRequestKeyType msg)


  | version >= 4 && version <= 6 =
    do
      serialize (findCoordinatorRequestKeyType msg)
      E.encodeVersionedArray version 3 (\v s -> if v >= 3 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (findCoordinatorRequestCoordinatorKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FindCoordinatorRequest with the given API version.
decodeFindCoordinatorRequest :: MonadGet m => E.ApiVersion -> m FindCoordinatorRequest
decodeFindCoordinatorRequest version
  | version == 0 =
    do
      fieldkey <- deserialize
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = 0
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldkey <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldkeytype <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version >= 1 && version <= 2 =
    do
      fieldkey <- deserialize
      fieldkeytype <- deserialize
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = fieldkey
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 6 =
    do
      fieldkeytype <- deserialize
      fieldcoordinatorkeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\v -> if v >= 3 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorRequest
        {
        findCoordinatorRequestKey = P.KafkaString Null
        ,
        findCoordinatorRequestKeyType = fieldkeytype
        ,
        findCoordinatorRequestCoordinatorKeys = fieldcoordinatorkeys
        }
  | otherwise = fail $ "Unsupported version: " ++ show version