{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterUserScramCredentialsResponse
Description : Kafka AlterUserScramCredentialsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 51.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterUserScramCredentialsResponse
  (
    AlterUserScramCredentialsResponse(..),
    AlterUserScramCredentialsResult(..),
    encodeAlterUserScramCredentialsResponse,
    decodeAlterUserScramCredentialsResponse,
    maxAlterUserScramCredentialsResponseVersion
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


-- | The results for deletions and alterations, one per affected user.
data AlterUserScramCredentialsResult = AlterUserScramCredentialsResult
  {

  -- | The user name.

  -- Versions: 0+
  alterUserScramCredentialsResultUser :: !(KafkaString)
,

  -- | The error code.

  -- Versions: 0+
  alterUserScramCredentialsResultErrorCode :: !(Int16)
,

  -- | The error message, if any.

  -- Versions: 0+
  alterUserScramCredentialsResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterUserScramCredentialsResult with version-aware field handling.
encodeAlterUserScramCredentialsResult :: MonadPut m => E.ApiVersion -> AlterUserScramCredentialsResult -> m ()
encodeAlterUserScramCredentialsResult version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterUserScramCredentialsResultUser amsg)) else serialize (alterUserScramCredentialsResultUser amsg)
    serialize (alterUserScramCredentialsResultErrorCode amsg)
    if version >= 0 then serialize (toCompactString (alterUserScramCredentialsResultErrorMessage amsg)) else serialize (alterUserScramCredentialsResultErrorMessage amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterUserScramCredentialsResult with version-aware field handling.
decodeAlterUserScramCredentialsResult :: MonadGet m => E.ApiVersion -> m AlterUserScramCredentialsResult
decodeAlterUserScramCredentialsResult version =
  do
    fielduser <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterUserScramCredentialsResult
      {
      alterUserScramCredentialsResultUser = fielduser
      ,
      alterUserScramCredentialsResultErrorCode = fielderrorcode
      ,
      alterUserScramCredentialsResultErrorMessage = fielderrormessage
      }



data AlterUserScramCredentialsResponse = AlterUserScramCredentialsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterUserScramCredentialsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for deletions and alterations, one per affected user.

  -- Versions: 0+
  alterUserScramCredentialsResponseResults :: !(KafkaArray (AlterUserScramCredentialsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterUserScramCredentialsResponse.
maxAlterUserScramCredentialsResponseVersion :: Int16
maxAlterUserScramCredentialsResponseVersion = 0

-- | Encode AlterUserScramCredentialsResponse with the given API version.
encodeAlterUserScramCredentialsResponse :: MonadPut m => E.ApiVersion -> AlterUserScramCredentialsResponse -> m ()
encodeAlterUserScramCredentialsResponse version msg
  | version == 0 =
    do
      serialize (alterUserScramCredentialsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeAlterUserScramCredentialsResult (case P.unKafkaArray (alterUserScramCredentialsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterUserScramCredentialsResponse with the given API version.
decodeAlterUserScramCredentialsResponse :: MonadGet m => E.ApiVersion -> m AlterUserScramCredentialsResponse
decodeAlterUserScramCredentialsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterUserScramCredentialsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterUserScramCredentialsResponse
        {
        alterUserScramCredentialsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterUserScramCredentialsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec AlterUserScramCredentialsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
