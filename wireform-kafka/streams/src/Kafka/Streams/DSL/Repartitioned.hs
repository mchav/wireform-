{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.DSL.Repartitioned
-- Description : KIP-559 @Repartitioned<K, V>@ config record
--
-- Carries the user-supplied options for an explicit
-- 'Kafka.Streams.DSL.KStream.repartition' call:
--
--   * an internal-topic name override (otherwise the DSL
--     synthesises one),
--   * key + value serdes for the wire format on the internal
--     topic (otherwise the upstream stream's serdes are
--     reused),
--   * a partition count for the internal topic,
--   * an optional custom partitioner.
--
-- Construct via 'repartitioned' and refine with @withX@
-- combinators, mirroring Java's @Repartitioned.as(\"name\")
-- .withNumberOfPartitions(n).withKeySerde(...).withValueSerde(...).withStreamPartitioner(...)@.
module Kafka.Streams.DSL.Repartitioned
  ( Repartitioned (..)
  , repartitioned
  , withRepartitionName
  , withNumberOfPartitions
  , withRepartitionKeySerde
  , withRepartitionValueSerde
  , withRepartitionStreamPartitioner
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.DSL.Produced (StreamPartitioner)
import Kafka.Streams.Serde (Serde)

data Repartitioned k v = Repartitioned
  { rpName             :: !(Maybe Text)
  , rpNumberOfPartitions :: !(Maybe Int32)
  , rpKeySerde         :: !(Maybe (Serde k))
  , rpValueSerde       :: !(Maybe (Serde v))
  , rpStreamPartitioner :: !(Maybe (StreamPartitioner k v))
  }
  deriving stock Generic

-- | An anonymous 'Repartitioned' — every override is unset, so
-- the DSL synthesises sensible defaults.
repartitioned :: Repartitioned k v
repartitioned = Repartitioned
  { rpName               = Nothing
  , rpNumberOfPartitions = Nothing
  , rpKeySerde           = Nothing
  , rpValueSerde         = Nothing
  , rpStreamPartitioner  = Nothing
  }

withRepartitionName :: Text -> Repartitioned k v -> Repartitioned k v
withRepartitionName n r = r { rpName = Just n }

withNumberOfPartitions :: Int32 -> Repartitioned k v -> Repartitioned k v
withNumberOfPartitions n r = r { rpNumberOfPartitions = Just n }

withRepartitionKeySerde :: Serde k -> Repartitioned k v -> Repartitioned k v
withRepartitionKeySerde s r = r { rpKeySerde = Just s }

withRepartitionValueSerde :: Serde v -> Repartitioned k v -> Repartitioned k v
withRepartitionValueSerde s r = r { rpValueSerde = Just s }

withRepartitionStreamPartitioner
  :: StreamPartitioner k v -> Repartitioned k v -> Repartitioned k v
withRepartitionStreamPartitioner p r = r { rpStreamPartitioner = Just p }

-- Silence the otherwise-unused 'ByteString' bring-in.
_keep :: ByteString -> ByteString
_keep = id
