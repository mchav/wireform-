{-|
Module      : Kafka.Callbacks
Description : Legacy callback tokens for compatibility.

@hw-kafka-client@ exposed callbacks for librdkafka errors, logs, and
statistics. The native wireform client does not use librdkafka callback
queues, so this transitional facade accepts the same callback builders
as source-compatible configuration tokens.
-}
module Kafka.Callbacks
  ( errorCallback
  , logCallback
  , statsCallback
  , Callback
  ) where

import Data.ByteString (ByteString)
import Kafka.Internal.Compat (Callback (..))
import Kafka.Types (KafkaError, KafkaLogLevel)

-- | Add a callback for errors in legacy configuration.
errorCallback :: (KafkaError -> String -> IO ()) -> Callback
errorCallback _ = Callback

-- | Add a callback for logs in legacy configuration.
logCallback :: (KafkaLogLevel -> String -> String -> IO ()) -> Callback
logCallback _ = Callback

-- | Add a callback for statistics in legacy configuration.
statsCallback :: (ByteString -> IO ()) -> Callback
statsCallback _ = Callback
