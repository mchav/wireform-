{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClusterResponse
Description : Kafka DescribeClusterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 60.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClusterResponse
  (
    DescribeClusterResponse(..),
    DescribeClusterBroker(..),
    encodeDescribeClusterResponse,
    decodeDescribeClusterResponse,
    maxDescribeClusterResponseVersion
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


-- | Each broker in the response.
data DescribeClusterBroker = DescribeClusterBroker
  {

  -- | The broker ID.

  -- Versions: 0+
  describeClusterBrokerBrokerId :: !(Int32)
,

  -- | The broker hostname.

  -- Versions: 0+
  describeClusterBrokerHost :: !(KafkaString)
,

  -- | The broker port.

  -- Versions: 0+
  describeClusterBrokerPort :: !(Int32)
,

  -- | The rack of the broker, or null if it has not been assigned to a rack.

  -- Versions: 0+
  describeClusterBrokerRack :: !(KafkaString)
,

  -- | Whether the broker is fenced

  -- Versions: 2+
  describeClusterBrokerIsFenced :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeClusterBroker with version-aware field handling.
encodeDescribeClusterBroker :: MonadPut m => E.ApiVersion -> DescribeClusterBroker -> m ()
encodeDescribeClusterBroker version dmsg =
  do
    serialize (describeClusterBrokerBrokerId dmsg)
    if version >= 0 then serialize (toCompactString (describeClusterBrokerHost dmsg)) else serialize (describeClusterBrokerHost dmsg)
    serialize (describeClusterBrokerPort dmsg)
    if version >= 0 then serialize (toCompactString (describeClusterBrokerRack dmsg)) else serialize (describeClusterBrokerRack dmsg)
    when (version >= 2) $
      serialize (describeClusterBrokerIsFenced dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeClusterBroker with version-aware field handling.
decodeDescribeClusterBroker :: MonadGet m => E.ApiVersion -> m DescribeClusterBroker
decodeDescribeClusterBroker version =
  do
    fieldbrokerid <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldisfenced <- if version >= 2
      then deserialize
      else pure (False)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeClusterBroker
      {
      describeClusterBrokerBrokerId = fieldbrokerid
      ,
      describeClusterBrokerHost = fieldhost
      ,
      describeClusterBrokerPort = fieldport
      ,
      describeClusterBrokerRack = fieldrack
      ,
      describeClusterBrokerIsFenced = fieldisfenced
      }



data DescribeClusterResponse = DescribeClusterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeClusterResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeClusterResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  describeClusterResponseErrorMessage :: !(KafkaString)
,

  -- | The endpoint type that was described. 1=brokers, 2=controllers.

  -- Versions: 1+
  describeClusterResponseEndpointType :: !(Int8)
,

  -- | The cluster ID that responding broker belongs to.

  -- Versions: 0+
  describeClusterResponseClusterId :: !(KafkaString)
,

  -- | The ID of the controller. When handled by a controller, returns the current voter leader ID. When ha

  -- Versions: 0+
  describeClusterResponseControllerId :: !(Int32)
,

  -- | Each broker in the response.

  -- Versions: 0+
  describeClusterResponseBrokers :: !(KafkaArray (DescribeClusterBroker))
,

  -- | 32-bit bitfield to represent authorized operations for this cluster.

  -- Versions: 0+
  describeClusterResponseClusterAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClusterResponse.
maxDescribeClusterResponseVersion :: Int16
maxDescribeClusterResponseVersion = 2

-- | Encode DescribeClusterResponse with the given API version.
encodeDescribeClusterResponse :: MonadPut m => E.ApiVersion -> DescribeClusterResponse -> m ()
encodeDescribeClusterResponse version msg
  | version == 0 =
    do
      serialize (describeClusterResponseThrottleTimeMs msg)
      serialize (describeClusterResponseErrorCode msg)
      serialize (toCompactString (describeClusterResponseErrorMessage msg))
      serialize (toCompactString (describeClusterResponseClusterId msg))
      serialize (describeClusterResponseControllerId msg)
      E.encodeVersionedArray version 0 encodeDescribeClusterBroker (case P.unKafkaArray (describeClusterResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeClusterResponseClusterAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (describeClusterResponseThrottleTimeMs msg)
      serialize (describeClusterResponseErrorCode msg)
      serialize (toCompactString (describeClusterResponseErrorMessage msg))
      serialize (describeClusterResponseEndpointType msg)
      serialize (toCompactString (describeClusterResponseClusterId msg))
      serialize (describeClusterResponseControllerId msg)
      E.encodeVersionedArray version 0 encodeDescribeClusterBroker (case P.unKafkaArray (describeClusterResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (describeClusterResponseClusterAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeClusterResponse with the given API version.
decodeDescribeClusterResponse :: MonadGet m => E.ApiVersion -> m DescribeClusterResponse
decodeDescribeClusterResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeClusterBroker
      fieldclusterauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClusterResponse
        {
        describeClusterResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeClusterResponseErrorCode = fielderrorcode
        ,
        describeClusterResponseErrorMessage = fielderrormessage
        ,
        describeClusterResponseEndpointType = 1
        ,
        describeClusterResponseClusterId = fieldclusterid
        ,
        describeClusterResponseControllerId = fieldcontrollerid
        ,
        describeClusterResponseBrokers = fieldbrokers
        ,
        describeClusterResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldendpointtype <- deserialize
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeClusterBroker
      fieldclusterauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeClusterResponse
        {
        describeClusterResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeClusterResponseErrorCode = fielderrorcode
        ,
        describeClusterResponseErrorMessage = fielderrormessage
        ,
        describeClusterResponseEndpointType = fieldendpointtype
        ,
        describeClusterResponseClusterId = fieldclusterid
        ,
        describeClusterResponseControllerId = fieldcontrollerid
        ,
        describeClusterResponseBrokers = fieldbrokers
        ,
        describeClusterResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeClusterResponse' / 'decodeDescribeClusterResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeClusterResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeClusterResponse decodeDescribeClusterResponse)
  {-# INLINE wireCodec #-}
