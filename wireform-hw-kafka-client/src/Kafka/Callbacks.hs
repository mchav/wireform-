module Kafka.Callbacks
  ( errorCallback
  , logCallback
  , statsCallback
  , Callback
  ) where

import Data.ByteString (ByteString)
import Kafka.Internal.Compat (Callback (..))
import Kafka.Types (KafkaError, KafkaLogLevel)

errorCallback :: (KafkaError -> String -> IO ()) -> Callback
errorCallback _ = Callback

logCallback :: (KafkaLogLevel -> String -> String -> IO ()) -> Callback
logCallback _ = Callback

statsCallback :: (ByteString -> IO ()) -> Callback
statsCallback _ = Callback
