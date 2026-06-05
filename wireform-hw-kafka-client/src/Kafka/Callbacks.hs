{-|
Module      : Kafka.Callbacks
Description : Legacy callback tokens for compatibility.

@hw-kafka-client@ exposed callbacks for librdkafka errors, logs, and
statistics. This transitional facade preserves those builders. Error
callbacks are invoked for facade-level create/send failures; log and
statistics callbacks are retained as configuration values because the
native wireform client does not use librdkafka callback queues.
-}
module Kafka.Callbacks
  ( errorCallback
  , logCallback
  , statsCallback
  , Callback
  ) where

import Data.ByteString (ByteString)
import Kafka.Internal.Callbacks (Callback (..))
import Kafka.Types (KafkaError, KafkaLogLevel)

-- | Add a callback for errors in legacy configuration.
--
-- The facade invokes this for failures it observes directly, such as
-- producer or consumer creation errors.
errorCallback :: (KafkaError -> String -> IO ()) -> Callback
errorCallback = ErrorCallback

-- | Add a callback for logs in legacy configuration.
--
-- The native wireform client does not emit librdkafka log callbacks.
logCallback :: (KafkaLogLevel -> String -> String -> IO ()) -> Callback
logCallback = LogCallback

-- | Add a callback for statistics in legacy configuration.
--
-- The native wireform client does not emit librdkafka statistics
-- callbacks.
statsCallback :: (ByteString -> IO ()) -> Callback
statsCallback = StatsCallback
