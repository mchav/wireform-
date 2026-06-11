{- |
Module      : Kafka.Producer.Callbacks
Description : Producer callback compatibility helpers.

This module keeps the @hw-kafka-client@ producer callback API available
for transitional configuration. Per-message callbacks supplied to
'Kafka.Producer.produceMessage'' are invoked by the facade; global
delivery callbacks registered with 'deliveryCallback' are invoked via
the native wireform producer acknowledgement hook.
-}
module Kafka.Producer.Callbacks (
  deliveryCallback,
  module X,
) where

import Kafka.Callbacks as X
import Kafka.Internal.Callbacks (Callback (..))
import Kafka.Producer.Types (DeliveryReport)


-- | Set the legacy delivery-report callback token.
deliveryCallback :: (DeliveryReport -> IO ()) -> Callback
deliveryCallback = DeliveryCallback
