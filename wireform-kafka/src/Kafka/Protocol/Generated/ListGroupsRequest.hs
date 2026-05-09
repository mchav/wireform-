{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListGroupsRequest
Description : Kafka ListGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 16.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListGroupsRequest
  (
    ListGroupsRequest(..),
    encodeListGroupsRequest,
    decodeListGroupsRequest,
    maxListGroupsRequestVersion
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




data ListGroupsRequest = ListGroupsRequest
  {

  -- | The states of the groups we want to list. If empty, all groups are returned with their state.

  -- Versions: 4+
  listGroupsRequestStatesFilter :: !(KafkaArray (KafkaString))
,

  -- | The types of the groups we want to list. If empty, all groups are returned with their type.

  -- Versions: 5+
  listGroupsRequestTypesFilter :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListGroupsRequest.
maxListGroupsRequestVersion :: Int16
maxListGroupsRequestVersion = 5

-- | Encode ListGroupsRequest with the given API version.
encodeListGroupsRequest :: MonadPut m => E.ApiVersion -> ListGroupsRequest -> m ()
encodeListGroupsRequest version msg
  | version == 3 =
    do
      
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 4 =
    do
      E.encodeVersionedArray version 3 (\v s -> if v >= 3 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listGroupsRequestStatesFilter msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 5 =
    do
      E.encodeVersionedArray version 3 (\v s -> if v >= 3 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listGroupsRequestStatesFilter msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 3 (\v s -> if v >= 3 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listGroupsRequestTypesFilter msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    pure ()
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListGroupsRequest with the given API version.
decodeListGroupsRequest :: MonadGet m => E.ApiVersion -> m ListGroupsRequest
decodeListGroupsRequest version
  | version == 3 =
    do
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListGroupsRequest
        {
        listGroupsRequestStatesFilter = P.mkKafkaArray V.empty
        ,
        listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
        }

  | version == 4 =
    do
      fieldstatesfilter <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\v -> if v >= 3 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListGroupsRequest
        {
        listGroupsRequestStatesFilter = fieldstatesfilter
        ,
        listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
        }

  | version == 5 =
    do
      fieldstatesfilter <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\v -> if v >= 3 then P.fromCompactString <$> deserialize else deserialize)
      fieldtypesfilter <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 (\v -> if v >= 3 then P.fromCompactString <$> deserialize else deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListGroupsRequest
        {
        listGroupsRequestStatesFilter = fieldstatesfilter
        ,
        listGroupsRequestTypesFilter = fieldtypesfilter
        }

  | version >= 0 && version <= 2 =
    do

      pure ListGroupsRequest
        {
        listGroupsRequestStatesFilter = P.mkKafkaArray V.empty
        ,
        listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version