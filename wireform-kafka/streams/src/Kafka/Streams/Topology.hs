module Kafka.Streams.Topology
  ( SourceTopic(..)
  , SinkTopic(..)
  , Topology(..)
  , emptyTopology
  , addSource
  , addSink
  ) where

import Data.Text (Text)

newtype SourceTopic = SourceTopic { unSourceTopic :: Text } deriving (Eq, Ord, Show)
newtype SinkTopic   = SinkTopic   { unSinkTopic   :: Text } deriving (Eq, Ord, Show)

data Topology m = Topology
  { topologySources :: [SourceTopic]
  , topologySinks   :: [SinkTopic]
  }

emptyTopology :: Topology m
emptyTopology = Topology [] []

addSource :: SourceTopic -> Topology m -> Topology m
addSource s t = t { topologySources = topologySources t ++ [s] }

addSink :: SinkTopic -> Topology m -> Topology m
addSink s t = t { topologySinks = topologySinks t ++ [s] }


