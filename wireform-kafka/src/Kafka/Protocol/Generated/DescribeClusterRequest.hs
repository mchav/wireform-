{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClusterRequest
Description : Kafka DescribeClusterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 60.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClusterRequest
  (
    DescribeClusterRequest(..),
    encodeDescribeClusterRequest,
    decodeDescribeClusterRequest,
    maxDescribeClusterRequestVersion
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




data DescribeClusterRequest = DescribeClusterRequest
  {

  -- | Whether to include cluster authorized operations.

  -- Versions: 0+
  describeClusterRequestIncludeClusterAuthorizedOperations :: !(Bool)
,

  -- | The endpoint type to describe. 1=brokers, 2=controllers.

  -- Versions: 1+
  describeClusterRequestEndpointType :: !(Int8)
,

  -- | Whether to include fenced brokers when listing brokers.

  -- Versions: 2+
  describeClusterRequestIncludeFencedBrokers :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClusterRequest.
maxDescribeClusterRequestVersion :: Int16
maxDescribeClusterRequestVersion = 2

-- | Encode DescribeClusterRequest with the given API version.
encodeDescribeClusterRequest :: MonadPut m => E.ApiVersion -> DescribeClusterRequest -> m ()
encodeDescribeClusterRequest version msg
  | version == 0 =
    do
      serialize (describeClusterRequestIncludeClusterAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (describeClusterRequestIncludeClusterAuthorizedOperations msg)
      serialize (describeClusterRequestEndpointType msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      serialize (describeClusterRequestIncludeClusterAuthorizedOperations msg)
      serialize (describeClusterRequestEndpointType msg)
      serialize (describeClusterRequestIncludeFencedBrokers msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeClusterRequest with the given API version.
decodeDescribeClusterRequest :: MonadGet m => E.ApiVersion -> m DescribeClusterRequest
decodeDescribeClusterRequest version
  | version == 0 =
    do
      fieldincludeclusterauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClusterRequest
        {
        describeClusterRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        describeClusterRequestEndpointType = 1
        ,
        describeClusterRequestIncludeFencedBrokers = False
        }

  | version == 1 =
    do
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldendpointtype <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClusterRequest
        {
        describeClusterRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        describeClusterRequestEndpointType = fieldendpointtype
        ,
        describeClusterRequestIncludeFencedBrokers = False
        }

  | version == 2 =
    do
      fieldincludeclusterauthorizedoperations <- deserialize
      fieldendpointtype <- deserialize
      fieldincludefencedbrokers <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClusterRequest
        {
        describeClusterRequestIncludeClusterAuthorizedOperations = fieldincludeclusterauthorizedoperations
        ,
        describeClusterRequestEndpointType = fieldendpointtype
        ,
        describeClusterRequestIncludeFencedBrokers = fieldincludefencedbrokers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeClusterRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
