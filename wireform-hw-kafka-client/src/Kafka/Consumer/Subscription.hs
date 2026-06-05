{-|
Module      : Kafka.Consumer.Subscription
Description : Legacy topic subscription builders.

The @hw-kafka-client@ API represents a subscription as a set of topic
names plus topic-level properties. This compatibility module keeps that
shape while feeding topic names and offset-reset policy into the native
wireform consumer.
-}
module Kafka.Consumer.Subscription
  ( Subscription (..)
  , topics
  , offsetReset
  , extraSubscriptionProps
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Kafka.Consumer.Types (OffsetReset (..))
import Kafka.Types (TopicName (..))
import qualified Data.Map as M
import qualified Data.Set as Set

-- | A consumer subscription to topics plus subscription properties.
--
-- Typically callers combine settings:
--
-- @
-- consumerSub = 'topics' ['TopicName' "events"] <> 'offsetReset' 'Earliest'
-- @
data Subscription = Subscription (Set TopicName) (Map Text Text)

instance Semigroup Subscription where
  Subscription ts1 m1 <> Subscription ts2 m2 =
    Subscription (Set.union ts1 ts2) (M.union m1 m2)

instance Monoid Subscription where
  mempty = Subscription Set.empty M.empty

-- | Build a subscription from topic names.
topics :: [TopicName] -> Subscription
topics ts = Subscription (Set.fromList ts) M.empty

-- | Set the @auto.offset.reset@ subscription parameter.
offsetReset :: OffsetReset -> Subscription
offsetReset o =
  Subscription Set.empty (M.singleton "auto.offset.reset" value)
  where
    value = case o of
      Earliest -> "earliest"
      Latest -> "latest"

-- | Set arbitrary subscription properties.
extraSubscriptionProps :: Map Text Text -> Subscription
extraSubscriptionProps = Subscription Set.empty
