-- |
-- Module      : Kafka.Streams.Consumed
-- Description : @Consumed<K,V>@ DSL config — how to read a topic
module Kafka.Streams.Consumed
  ( Consumed (..)
  , consumed
  , withTimestampExtractor
  , withName
  , withOffsetResetPolicy
  , withWatermarkStrategy
  , AutoOffsetReset (..)
  ) where

import Data.Text (Text)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.Time (TimestampExtractor, recordTimestampExtractor)
import Kafka.Streams.Watermark (WatermarkStrategy)

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
  , consumedWatermark      :: !(Maybe WatermarkStrategy)
    -- ^ Riffle \xc2\xa75: optional cross-source watermark strategy.
    -- 'Nothing' (the default) preserves the legacy per-task
    -- 'StreamTime' behaviour. 'Just s' opts the source into
    -- the 'Kafka.Streams.Watermark.WatermarkCoordinator'.
  }

-- | Default 'Consumed' that uses the embedded record timestamp.
consumed :: Serde k -> Serde v -> Consumed k v
consumed ks vs = Consumed
  { consumedKeySerde    = ks
  , consumedValueSerde  = vs
  , consumedExtractor   = recordTimestampExtractor
  , consumedNodeName    = Nothing
  , consumedOffsetReset = OffsetEarliest
  , consumedWatermark   = Nothing
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

-- | Attach a watermark strategy to a 'Consumed'. The source
-- built with this 'Consumed' registers with the
-- 'Kafka.Streams.Watermark.WatermarkCoordinator' at startup and
-- reports every record's timestamp via 'reportRecord'.
withWatermarkStrategy
  :: WatermarkStrategy -> Consumed k v -> Consumed k v
withWatermarkStrategy s c = c { consumedWatermark = Just s }
