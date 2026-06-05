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

data Subscription = Subscription (Set TopicName) (Map Text Text)

instance Semigroup Subscription where
  Subscription ts1 m1 <> Subscription ts2 m2 =
    Subscription (Set.union ts1 ts2) (M.union m1 m2)

instance Monoid Subscription where
  mempty = Subscription Set.empty M.empty

topics :: [TopicName] -> Subscription
topics ts = Subscription (Set.fromList ts) M.empty

offsetReset :: OffsetReset -> Subscription
offsetReset o =
  Subscription Set.empty (M.singleton "auto.offset.reset" value)
  where
    value = case o of
      Earliest -> "earliest"
      Latest -> "latest"

extraSubscriptionProps :: Map Text Text -> Subscription
extraSubscriptionProps = Subscription Set.empty
