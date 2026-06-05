module Kafka.Producer.Callbacks
  ( deliveryCallback
  , module X
  ) where

import Kafka.Callbacks as X
import Kafka.Internal.Compat (Callback (..))
import Kafka.Producer.Types (DeliveryReport)

deliveryCallback :: (DeliveryReport -> IO ()) -> Callback
deliveryCallback _ = Callback
