{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.HeartbeatRequest
Description : Kafka HeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 12.



Valid versions: 0-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.HeartbeatRequest
  (
    HeartbeatRequest(..),
    encodeHeartbeatRequest,
    decodeHeartbeatRequest,
    maxHeartbeatRequestVersion
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




data HeartbeatRequest = HeartbeatRequest
  {

  -- | The group id.

  -- Versions: 0+
  heartbeatRequestGroupId :: !(KafkaString)
,

  -- | The generation of the group.

  -- Versions: 0+
  heartbeatRequestGenerationId :: !(Int32)
,

  -- | The member ID.

  -- Versions: 0+
  heartbeatRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 3+
  heartbeatRequestGroupInstanceId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for HeartbeatRequest.
maxHeartbeatRequestVersion :: Int16
maxHeartbeatRequestVersion = 4

-- | Encode HeartbeatRequest with the given API version.
encodeHeartbeatRequest :: MonadPut m => E.ApiVersion -> HeartbeatRequest -> m ()
encodeHeartbeatRequest version msg
  | version == 3 =
    do
      serialize (heartbeatRequestGroupId msg)
      serialize (heartbeatRequestGenerationId msg)
      serialize (heartbeatRequestMemberId msg)
      serialize (heartbeatRequestGroupInstanceId msg)


  | version == 4 =
    do
      serialize (toCompactString (heartbeatRequestGroupId msg))
      serialize (heartbeatRequestGenerationId msg)
      serialize (toCompactString (heartbeatRequestMemberId msg))
      serialize (toCompactString (heartbeatRequestGroupInstanceId msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (heartbeatRequestGroupId msg)
      serialize (heartbeatRequestGenerationId msg)
      serialize (heartbeatRequestMemberId msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode HeartbeatRequest with the given API version.
decodeHeartbeatRequest :: MonadGet m => E.ApiVersion -> m HeartbeatRequest
decodeHeartbeatRequest version
  | version == 3 =
    do
      fieldgroupid <- deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- deserialize
      fieldgroupinstanceid <- deserialize
      pure HeartbeatRequest
        {
        heartbeatRequestGroupId = fieldgroupid
        ,
        heartbeatRequestGenerationId = fieldgenerationid
        ,
        heartbeatRequestMemberId = fieldmemberid
        ,
        heartbeatRequestGroupInstanceId = fieldgroupinstanceid
        }

  | version == 4 =
    do
      fieldgroupid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure HeartbeatRequest
        {
        heartbeatRequestGroupId = fieldgroupid
        ,
        heartbeatRequestGenerationId = fieldgenerationid
        ,
        heartbeatRequestMemberId = fieldmemberid
        ,
        heartbeatRequestGroupInstanceId = fieldgroupinstanceid
        }

  | version >= 0 && version <= 2 =
    do
      fieldgroupid <- deserialize
      fieldgenerationid <- deserialize
      fieldmemberid <- deserialize
      pure HeartbeatRequest
        {
        heartbeatRequestGroupId = fieldgroupid
        ,
        heartbeatRequestGenerationId = fieldgenerationid
        ,
        heartbeatRequestMemberId = fieldmemberid
        ,
        heartbeatRequestGroupInstanceId = P.KafkaString Null
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec HeartbeatRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
