-- |
-- Module      : Kafka.Streams.DSL.Produced
-- Description : @Produced<K,V>@ DSL config — how to write a topic
module Kafka.Streams.DSL.Produced
  ( Produced (..)
  , produced
  , withProducedName
  ) where

import Data.Text (Text)
import Kafka.Streams.Serde (Serde)

data Produced k v = Produced
  { producedKeySerde   :: !(Serde k)
  , producedValueSerde :: !(Serde v)
  , producedName       :: !(Maybe Text)
  }

produced :: Serde k -> Serde v -> Produced k v
produced ks vs = Produced
  { producedKeySerde   = ks
  , producedValueSerde = vs
  , producedName       = Nothing
  }

withProducedName :: Text -> Produced k v -> Produced k v
withProducedName n p = p { producedName = Just n }
