module Kafka.Streams.Config
  ( StreamsConfig(..)
  ) where

import Data.Text (Text)

data StreamsConfig = StreamsConfig
  { applicationId    :: !Text
  , bootstrapServers :: ![Text]
  } deriving (Eq, Show)


