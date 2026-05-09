{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListTransactionsRequest
Description : Kafka ListTransactionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 66.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListTransactionsRequest
  (
    ListTransactionsRequest(..),
    encodeListTransactionsRequest,
    decodeListTransactionsRequest,
    maxListTransactionsRequestVersion
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




data ListTransactionsRequest = ListTransactionsRequest
  {

  -- | The transaction states to filter by: if empty, all transactions are returned; if non-empty, then onl

  -- Versions: 0+
  listTransactionsRequestStateFilters :: !(KafkaArray (KafkaString))
,

  -- | The producerIds to filter by: if empty, all transactions will be returned; if non-empty, only transa

  -- Versions: 0+
  listTransactionsRequestProducerIdFilters :: !(KafkaArray (Int64))
,

  -- | Duration (in millis) to filter by: if < 0, all transactions will be returned; otherwise, only transa

  -- Versions: 1+
  listTransactionsRequestDurationFilter :: !(Int64)
,

  -- | The transactional ID regular expression pattern to filter by: if it is empty or null, all transactio

  -- Versions: 2+
  listTransactionsRequestTransactionalIdPattern :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListTransactionsRequest.
maxListTransactionsRequestVersion :: Int16
maxListTransactionsRequestVersion = 2

-- | Encode ListTransactionsRequest with the given API version.
encodeListTransactionsRequest :: MonadPut m => E.ApiVersion -> ListTransactionsRequest -> m ()
encodeListTransactionsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (listTransactionsRequestDurationFilter msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 2 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (listTransactionsRequestStateFilters msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listTransactionsRequestProducerIdFilters msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int64"
      serialize (listTransactionsRequestDurationFilter msg)
      serialize (toCompactString (listTransactionsRequestTransactionalIdPattern msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListTransactionsRequest with the given API version.
decodeListTransactionsRequest :: MonadGet m => E.ApiVersion -> m ListTransactionsRequest
decodeListTransactionsRequest version
  | version == 0 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = (-1)
        ,
        listTransactionsRequestTransactionalIdPattern = P.KafkaString Null
        }

  | version == 1 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fielddurationfilter <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = fielddurationfilter
        ,
        listTransactionsRequestTransactionalIdPattern = P.KafkaString Null
        }

  | version == 2 =
    do
      fieldstatefilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldproduceridfilters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      fielddurationfilter <- deserialize
      fieldtransactionalidpattern <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListTransactionsRequest
        {
        listTransactionsRequestStateFilters = fieldstatefilters
        ,
        listTransactionsRequestProducerIdFilters = fieldproduceridfilters
        ,
        listTransactionsRequestDurationFilter = fielddurationfilter
        ,
        listTransactionsRequestTransactionalIdPattern = fieldtransactionalidpattern
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ListTransactionsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
