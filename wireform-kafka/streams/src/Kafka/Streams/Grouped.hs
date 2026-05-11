-- |
-- Module      : Kafka.Streams.Grouped
-- Description : @Grouped<K,V>@ DSL config
module Kafka.Streams.Grouped
  ( Grouped (..)
  , grouped
  , withGroupedName
  ) where

import Data.Text (Text)
import Kafka.Streams.Serde (Serde)

data Grouped k v = Grouped
  { groupedKeySerde   :: !(Serde k)
  , groupedValueSerde :: !(Serde v)
  , groupedName       :: !(Maybe Text)
  }

grouped :: Serde k -> Serde v -> Grouped k v
grouped ks vs = Grouped
  { groupedKeySerde   = ks
  , groupedValueSerde = vs
  , groupedName       = Nothing
  }

withGroupedName :: Text -> Grouped k v -> Grouped k v
withGroupedName n g = g { groupedName = Just n }
