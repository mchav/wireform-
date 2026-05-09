{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.SerdeContext
Description : KIP-492 — metadata context passed to serializer / deserializer

KIP-492 added an optional /serializer-context/ argument so a
@Serializer<K,V>@ can see which @(topic, partition, headers,
isKey)@ it's about to encode for. Useful when one logical type
is encoded differently per topic (Confluent SR's subject naming
strategy is the classic case) or when the serializer needs to
mutate headers.

This module defines the 'SerdeCtx' record + a typeclass-style
'CtxSerde' that consumers / producers route through. Callers
that don't need the context use 'liftSerdeCtx' to lift a plain
'Kafka.Streams.Serde.Serde'.
-}
module Kafka.Client.SerdeContext
  ( SerdeCtx (..)
  , CtxSerde (..)
  , liftSerdeCtx
  , withTopic
  , withHeaders
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The /context/ a serializer / deserializer sees on each
-- per-record encode. Mirrors the Java @SerializationContext@.
data SerdeCtx = SerdeCtx
  { scTopic       :: !Text
  , scPartition   :: !(Maybe Int32)
    -- ^ 'Nothing' on the consumer side when the partition isn't
    --   yet assigned (e.g. the deserializer is invoked before
    --   the metadata refresh completes).
  , scIsKey       :: !Bool
  , scHeaders     :: ![(Text, ByteString)]
  }
  deriving stock (Eq, Show, Generic)

-- | Context-aware serializer / deserializer.
data CtxSerde a = CtxSerde
  { csSerialize   :: !(SerdeCtx -> a -> ByteString)
  , csDeserialize :: !(SerdeCtx -> ByteString -> Either String a)
  }

-- | Lift a context-free encoder + decoder into a 'CtxSerde'.
liftSerdeCtx
  :: (a -> ByteString)
  -> (ByteString -> Either String a)
  -> CtxSerde a
liftSerdeCtx enc dec = CtxSerde
  { csSerialize   = \_ -> enc
  , csDeserialize = \_ -> dec
  }

-- | Convenience: focus a context on a particular topic +
-- isKey tag (the most common case).
withTopic :: Text -> Bool -> SerdeCtx
withTopic t isKey = SerdeCtx
  { scTopic     = t
  , scPartition = Nothing
  , scIsKey     = isKey
  , scHeaders   = []
  }

withHeaders :: SerdeCtx -> [(Text, ByteString)] -> SerdeCtx
withHeaders ctx hs = ctx { scHeaders = hs }
