{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeGroupsRequest
Description : Kafka DescribeGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 15.



Valid versions: 0-6
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeGroupsRequest
  (
    DescribeGroupsRequest(..),
    encodeDescribeGroupsRequest,
    decodeDescribeGroupsRequest,
    maxDescribeGroupsRequestVersion
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




data DescribeGroupsRequest = DescribeGroupsRequest
  {

  -- | The names of the groups to describe.

  -- Versions: 0+
  describeGroupsRequestGroups :: !(KafkaArray (KafkaString))
,

  -- | Whether to include authorized operations.

  -- Versions: 3+
  describeGroupsRequestIncludeAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeGroupsRequest.
maxDescribeGroupsRequestVersion :: Int16
maxDescribeGroupsRequestVersion = 6

-- | Encode DescribeGroupsRequest with the given API version.
encodeDescribeGroupsRequest :: MonadPut m => E.ApiVersion -> DescribeGroupsRequest -> m ()
encodeDescribeGroupsRequest version msg
  | version >= 3 && version <= 4 =
    do
      E.encodeVersionedArray version 5 (\v s -> if v >= 5 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (describeGroupsRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeGroupsRequestIncludeAuthorizedOperations msg)


  | version >= 5 && version <= 6 =
    do
      E.encodeVersionedArray version 5 (\v s -> if v >= 5 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (describeGroupsRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeGroupsRequestIncludeAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      E.encodeVersionedArray version 5 (\v s -> if v >= 5 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (describeGroupsRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeGroupsRequest with the given API version.
decodeDescribeGroupsRequest :: MonadGet m => E.ApiVersion -> m DescribeGroupsRequest
decodeDescribeGroupsRequest version
  | version >= 3 && version <= 4 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 (\v -> if v >= 5 then P.fromCompactString <$> deserialize else deserialize)
      fieldincludeauthorizedoperations <- deserialize
      pure DescribeGroupsRequest
        {
        describeGroupsRequestGroups = fieldgroups
        ,
        describeGroupsRequestIncludeAuthorizedOperations = fieldincludeauthorizedoperations
        }

  | version >= 5 && version <= 6 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 (\v -> if v >= 5 then P.fromCompactString <$> deserialize else deserialize)
      fieldincludeauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeGroupsRequest
        {
        describeGroupsRequestGroups = fieldgroups
        ,
        describeGroupsRequestIncludeAuthorizedOperations = fieldincludeauthorizedoperations
        }

  | version >= 0 && version <= 2 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 (\v -> if v >= 5 then P.fromCompactString <$> deserialize else deserialize)
      pure DescribeGroupsRequest
        {
        describeGroupsRequestGroups = fieldgroups
        ,
        describeGroupsRequestIncludeAuthorizedOperations = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version