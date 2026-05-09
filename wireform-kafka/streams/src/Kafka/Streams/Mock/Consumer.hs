{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Consumer
-- Description : Streams-side wrappers for 'Kafka.Client.Mock.Consumer'
module Kafka.Streams.Mock.Consumer
  ( CC.MockConsumer
  , CC.newMockConsumer
  , CC.newMockConsumerWithId
  , CC.consumerMemberId
  , subscribeMC
  , refreshAssignment
  , assignedPartitions
  , topicAssignment
  , PollResult (..)
  , pollMC
  , commitOffsetsMC
  , seekMC
  , currentPosition
  , CC.IsolationLevel (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)

import qualified Kafka.Client.Mock.Consumer as CC
import Kafka.Streams.Mock.Cluster (StoredRecord, fromCoreCompat)
import Kafka.Streams.Mock.Fault ()
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, topicName, unTopicName)

import qualified Kafka.Client.Mock.Cluster as C
import qualified Kafka.Client.Mock.Fault as F

-- | Streams-side poll result with 'TopicName'-shaped records.
data PollResult = PollResult
  { prRecords :: ![(TopicName, Int32, StoredRecord)]
  , prErrors  :: ![(TopicName, Int32, F.MockError)]
  }
  deriving Show

subscribeMC :: CC.MockConsumer -> [TopicName] -> IO ()
subscribeMC c ts = CC.subscribeMC c (map unTopicName ts)

refreshAssignment :: CC.MockConsumer -> IO ()
refreshAssignment = CC.refreshAssignment

assignedPartitions :: CC.MockConsumer -> IO [(TopicName, Int32)]
assignedPartitions c =
  map (\(t, p) -> (topicName t, p)) <$> CC.assignedPartitions c

topicAssignment :: CC.MockConsumer -> IO [(TopicName, Int32)]
topicAssignment = assignedPartitions

pollMC :: CC.MockConsumer -> IO PollResult
pollMC c = do
  CC.PollResult rs errs <- CC.pollMC c
  pure PollResult
    { prRecords =
        map (\(t, p, r) -> (topicName t, p, fromCoreCompat r)) rs
    , prErrors  =
        map (\(t, p, e) -> (topicName t, p, e)) errs
    }

commitOffsetsMC
  :: CC.MockConsumer
  -> [(TopicName, Int32, Int64)]
  -> IO (Either F.MockError ())
commitOffsetsMC c xs =
  CC.commitOffsetsMC c [(unTopicName t, p, o) | (t, p, o) <- xs]

seekMC :: CC.MockConsumer -> TopicName -> Int32 -> Int64 -> IO ()
seekMC c t p off = CC.seekMC c (unTopicName t) p off

currentPosition
  :: CC.MockConsumer -> TopicName -> Int32 -> IO (Maybe Int64)
currentPosition c t p = CC.currentPosition c (unTopicName t) p
