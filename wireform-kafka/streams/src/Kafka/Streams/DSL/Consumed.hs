-- |
-- Module      : Kafka.Streams.DSL.Consumed
-- Description : @Consumed<K,V>@ DSL config — how to read a topic
module Kafka.Streams.DSL.Consumed
  ( Consumed (..)
  , consumed
  , withTimestampExtractor
  , withName
  , withOffsetResetPolicy
  , AutoOffsetReset (..)
  ) where

import Data.Text (Text)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.Time (TimestampExtractor, recordTimestampExtractor)

-- | Auto-offset-reset policy for a 'Consumed' source. Mirrors
-- Java's @Topology.AutoOffsetReset@.
data AutoOffsetReset = OffsetEarliest | OffsetLatest | OffsetNone
  deriving (Eq, Show)

-- | Mirrors @org.apache.kafka.streams.kstream.Consumed<K,V>@.
data Consumed k v = Consumed
  { consumedKeySerde       :: !(Serde k)
  , consumedValueSerde     :: !(Serde v)
  , consumedExtractor      :: !(TimestampExtractor k v)
  , consumedNodeName       :: !(Maybe Text)
  , consumedOffsetReset    :: !AutoOffsetReset
  }

-- | Default 'Consumed' that uses the embedded record timestamp.
consumed :: Serde k -> Serde v -> Consumed k v
consumed ks vs = Consumed
  { consumedKeySerde    = ks
  , consumedValueSerde  = vs
  , consumedExtractor   = recordTimestampExtractor
  , consumedNodeName    = Nothing
  , consumedOffsetReset = OffsetEarliest
  }

-- | Set the auto-offset-reset policy on a 'Consumed'. Mirrors
-- Java's @Consumed.withOffsetResetPolicy@.
withOffsetResetPolicy
  :: AutoOffsetReset -> Consumed k v -> Consumed k v
withOffsetResetPolicy r c = c { consumedOffsetReset = r }

withTimestampExtractor
  :: TimestampExtractor k v -> Consumed k v -> Consumed k v
withTimestampExtractor ex c = c { consumedExtractor = ex }

withName :: Text -> Consumed k v -> Consumed k v
withName n c = c { consumedNodeName = Just n }
