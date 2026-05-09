{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FindCoordinatorResponse
Description : Kafka FindCoordinatorResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 10.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FindCoordinatorResponse
  (
    FindCoordinatorResponse(..),
    Coordinator(..),
    encodeFindCoordinatorResponse,
    decodeFindCoordinatorResponse,
    maxFindCoordinatorResponseVersion
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


-- | Each coordinator result in the response.
data Coordinator = Coordinator
  {

  -- | The coordinator key.

  -- Versions: 4+
  coordinatorKey :: !(KafkaString)
,

  -- | The node id.

  -- Versions: 4+
  coordinatorNodeId :: !(Int32)
,

  -- | The host name.

  -- Versions: 4+
  coordinatorHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 4+
  coordinatorPort :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 4+
  coordinatorErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 4+
  coordinatorErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode Coordinator with version-aware field handling.
encodeCoordinator :: MonadPut m => E.ApiVersion -> Coordinator -> m ()
encodeCoordinator version cmsg =
  do
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorKey cmsg)) else serialize (coordinatorKey cmsg)
    when (version >= 4) $
      serialize (coordinatorNodeId cmsg)
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorHost cmsg)) else serialize (coordinatorHost cmsg)
    when (version >= 4) $
      serialize (coordinatorPort cmsg)
    when (version >= 4) $
      serialize (coordinatorErrorCode cmsg)
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorErrorMessage cmsg)) else serialize (coordinatorErrorMessage cmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Coordinator with version-aware field handling.
decodeCoordinator :: MonadGet m => E.ApiVersion -> m Coordinator
decodeCoordinator version =
  do
    fieldkey <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldnodeid <- if version >= 4
      then deserialize
      else pure (0)
    fieldhost <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 4
      then deserialize
      else pure (0)
    fielderrorcode <- if version >= 4
      then deserialize
      else pure (0)
    fielderrormessage <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Coordinator
      {
      coordinatorKey = fieldkey
      ,
      coordinatorNodeId = fieldnodeid
      ,
      coordinatorHost = fieldhost
      ,
      coordinatorPort = fieldport
      ,
      coordinatorErrorCode = fielderrorcode
      ,
      coordinatorErrorMessage = fielderrormessage
      }



data FindCoordinatorResponse = FindCoordinatorResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  findCoordinatorResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0-3
  findCoordinatorResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 1-3
  findCoordinatorResponseErrorMessage :: !(KafkaString)
,

  -- | The node id.

  -- Versions: 0-3
  findCoordinatorResponseNodeId :: !(Int32)
,

  -- | The host name.

  -- Versions: 0-3
  findCoordinatorResponseHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 0-3
  findCoordinatorResponsePort :: !(Int32)
,

  -- | Each coordinator result in the response.

  -- Versions: 4+
  findCoordinatorResponseCoordinators :: !(KafkaArray (Coordinator))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FindCoordinatorResponse.
maxFindCoordinatorResponseVersion :: Int16
maxFindCoordinatorResponseVersion = 6

-- | Encode FindCoordinatorResponse with the given API version.
encodeFindCoordinatorResponse :: MonadPut m => E.ApiVersion -> FindCoordinatorResponse -> m ()
encodeFindCoordinatorResponse version msg
  | version == 0 =
    do
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (findCoordinatorResponseNodeId msg)
      serialize (findCoordinatorResponseHost msg)
      serialize (findCoordinatorResponsePort msg)


  | version == 3 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (toCompactString (findCoordinatorResponseErrorMessage msg))
      serialize (findCoordinatorResponseNodeId msg)
      serialize (toCompactString (findCoordinatorResponseHost msg))
      serialize (findCoordinatorResponsePort msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (findCoordinatorResponseErrorMessage msg)
      serialize (findCoordinatorResponseNodeId msg)
      serialize (findCoordinatorResponseHost msg)
      serialize (findCoordinatorResponsePort msg)


  | version >= 4 && version <= 6 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeCoordinator (case P.unKafkaArray (findCoordinatorResponseCoordinators msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FindCoordinatorResponse with the given API version.
decodeFindCoordinatorResponse :: MonadGet m => E.ApiVersion -> m FindCoordinatorResponse
decodeFindCoordinatorResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldnodeid <- deserialize
      fieldhost <- deserialize
      fieldport <- deserialize
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = 0
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = P.KafkaString Null
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldnodeid <- deserialize
      fieldhost <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldport <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = fielderrormessage
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldnodeid <- deserialize
      fieldhost <- deserialize
      fieldport <- deserialize
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = fielderrormessage
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldcoordinators <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeCoordinator
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = 0
        ,
        findCoordinatorResponseErrorMessage = P.KafkaString Null
        ,
        findCoordinatorResponseNodeId = 0
        ,
        findCoordinatorResponseHost = P.KafkaString Null
        ,
        findCoordinatorResponsePort = 0
        ,
        findCoordinatorResponseCoordinators = fieldcoordinators
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec FindCoordinatorResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
