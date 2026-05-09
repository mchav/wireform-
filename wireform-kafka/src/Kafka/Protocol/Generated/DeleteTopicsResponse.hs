{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteTopicsResponse
Description : Kafka DeleteTopicsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 20.



Valid versions: 1-6
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteTopicsResponse
  (
    DeleteTopicsResponse(..),
    DeletableTopicResult(..),
    encodeDeleteTopicsResponse,
    decodeDeleteTopicsResponse,
    maxDeleteTopicsResponseVersion
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


-- | The results for each topic we tried to delete.
data DeletableTopicResult = DeletableTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  deletableTopicResultName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 6+
  deletableTopicResultTopicId :: !(KafkaUuid)
,

  -- | The deletion error, or 0 if the deletion succeeded.

  -- Versions: 0+
  deletableTopicResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 5+
  deletableTopicResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeletableTopicResult with version-aware field handling.
encodeDeletableTopicResult :: MonadPut m => E.ApiVersion -> DeletableTopicResult -> m ()
encodeDeletableTopicResult version dmsg =
  do
    if version >= 4 then serialize (toCompactString (deletableTopicResultName dmsg)) else serialize (deletableTopicResultName dmsg)
    when (version >= 6) $
      serialize (deletableTopicResultTopicId dmsg)
    serialize (deletableTopicResultErrorCode dmsg)
    when (version >= 5) $
      if version >= 4 then serialize (toCompactString (deletableTopicResultErrorMessage dmsg)) else serialize (deletableTopicResultErrorMessage dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeletableTopicResult with version-aware field handling.
decodeDeletableTopicResult :: MonadGet m => E.ApiVersion -> m DeletableTopicResult
decodeDeletableTopicResult version =
  do
    fieldname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- if version >= 6
      then deserialize
      else pure (P.nullUuid)
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 5
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeletableTopicResult
      {
      deletableTopicResultName = fieldname
      ,
      deletableTopicResultTopicId = fieldtopicid
      ,
      deletableTopicResultErrorCode = fielderrorcode
      ,
      deletableTopicResultErrorMessage = fielderrormessage
      }



data DeleteTopicsResponse = DeleteTopicsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  deleteTopicsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each topic we tried to delete.

  -- Versions: 0+
  deleteTopicsResponseResponses :: !(KafkaArray (DeletableTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteTopicsResponse.
maxDeleteTopicsResponseVersion :: Int16
maxDeleteTopicsResponseVersion = 6

-- | Encode DeleteTopicsResponse with the given API version.
encodeDeleteTopicsResponse :: MonadPut m => E.ApiVersion -> DeleteTopicsResponse -> m ()
encodeDeleteTopicsResponse version msg
  | version >= 1 && version <= 3 =
    do
      serialize (deleteTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDeletableTopicResult (case P.unKafkaArray (deleteTopicsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 4 && version <= 6 =
    do
      serialize (deleteTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDeletableTopicResult (case P.unKafkaArray (deleteTopicsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteTopicsResponse with the given API version.
decodeDeleteTopicsResponse :: MonadGet m => E.ApiVersion -> m DeleteTopicsResponse
decodeDeleteTopicsResponse version
  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDeletableTopicResult
      pure DeleteTopicsResponse
        {
        deleteTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteTopicsResponseResponses = fieldresponses
        }

  | version >= 4 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDeletableTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteTopicsResponse
        {
        deleteTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteTopicsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DeleteTopicsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
