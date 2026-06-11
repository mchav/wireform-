{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Shared types for client-side config validation (KIP-360).

We model invalid configs as a list of 'ConfigError' so the
caller can surface every problem in one shot instead of the
usual "fail at the first one" style. The actual rules live next
to their config types in "Kafka.Client.Producer" and
"Kafka.Client.Consumer" because they need access to the record's
private constructors and dragging them into a separate module
creates an import cycle (Producer already imports Consumer).

The rules mirror what the JVM client
(@org.apache.kafka.clients.producer.ProducerConfig@,
@org.apache.kafka.clients.consumer.ConsumerConfig@) checks at
construction time and that we previously deferred to the broker.
-}
module Kafka.Client.ConfigValidation (
  ConfigError (..),
  renderConfigErrors,
  check,
) where

import Data.Text (Text)
import Data.Text qualified as T


{- | A single failed validation rule. @configErrorField@ uses the
librdkafka / JVM property name (e.g. @batch.size@) so a Haskell
caller and an ops engineer reading logs can talk about the same
knob.
-}
data ConfigError = ConfigError
  { configErrorField :: !Text
  , configErrorMessage :: !Text
  }
  deriving (Eq, Show)


{- | Render a list of 'ConfigError' as a human-readable multi-line
string suitable for embedding in a @Left@ or an exception body.
-}
renderConfigErrors :: [ConfigError] -> String
renderConfigErrors errs =
  T.unpack $
    T.intercalate "\n" $
      "wireform-kafka: invalid configuration:"
        : fmap
          ( \ConfigError {..} ->
              "  - " <> configErrorField <> ": " <> configErrorMessage
          )
          errs


{- | Tiny DSL helper used by 'validateProducerConfig' /
'validateConsumerConfig' to keep each rule on a single line.
-}
check :: Bool -> Text -> Text -> [ConfigError]
check True field msg = [ConfigError field msg]
check False _ _ = []
