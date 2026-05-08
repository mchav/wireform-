{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Cluster
-- Description : Streams-side wrappers around 'Kafka.Client.Mock.Cluster'
--
-- The mock broker lives in the core @wireform-kafka@ library so that
-- both the streams runtime and the core client tests can share it.
-- This module re-exports it under the streams namespace, with thin
-- wrappers for the few signatures that take a 'TopicName' or
-- 'Timestamp' instead of the core's 'Text' / 'Int64'.
--
-- Existing streams call sites that pass @topicName "foo"@ /
-- @Timestamp 0@ keep working unchanged.
module Kafka.Streams.Mock.Cluster
  ( -- * Cluster
    C.MockCluster
  , C.newMockCluster
  , clusterClockNow
  , tickClock
    -- * Topology
  , createTopic
  , listTopics
  , partitionCount
    -- * Brokers
  , C.BrokerId (..)
  , C.addBroker
  , C.markBrokerDown
  , C.markBrokerUp
  , C.isBrokerUp
  , C.downedBrokers
    -- * Append + fetch
  , StoredRecord (..)
  , C.ProducerStamp (..)
  , appendToPartition
  , fetchSlice
  , partitionHWM
  , partitionLastStableOffset
    -- * Consumer-group offsets
  , C.GroupId (..)
  , commitGroupOffsets
  , groupOffsetsFor
    -- * Transaction markers
  , C.TxnId (..)
  , C.TxnState (..)
  , C.beginTxn
  , C.commitTxn
  , C.abortTxn
  , C.txnState
  , C.currentTxnEpoch
    -- * Group rebalance / assignment
  , C.MemberId (..)
  , joinGroup
  , C.leaveGroup
  , C.membersOf
  , assignmentFor
    -- * Inspection
  , dumpPartition
  , partitionLogSize
  , fromCoreCompat
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict
import Data.Map.Strict (Map)
import qualified Data.Text

import qualified Kafka.Client.Mock.Cluster as C
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, topicName, unTopicName)

----------------------------------------------------------------------
-- StoredRecord (TopicName + Timestamp friendly view)
----------------------------------------------------------------------

-- | Streams-side view: same payload as 'C.StoredRecord' but with
-- 'Timestamp' instead of 'Int64'. Built lazily by 'appendToPartition'
-- / 'fetchSlice' / 'dumpPartition' so streams tests don't have to
-- unwrap.
data StoredRecord = StoredRecord
  { srOffset    :: !Int64
  , srKey       :: !(Maybe ByteString)
  , srValue     :: !ByteString
  , srTimestamp :: !Timestamp
  , srHeaders   :: ![(Data.Text.Text, ByteString)]
  , srProducer  :: !(Maybe C.ProducerStamp)
  }
  deriving (Eq, Show)

-- | Convert a core-shaped record to the streams-shaped one. Public
-- helper so the Mock.Consumer wrapper can reuse it without
-- duplicating the field projection.
fromCoreCompat :: C.StoredRecord -> StoredRecord
fromCoreCompat = fromCore

fromCore :: C.StoredRecord -> StoredRecord
fromCore r = StoredRecord
  { srOffset    = C.srOffset r
  , srKey       = C.srKey r
  , srValue     = C.srValue r
  , srTimestamp = Timestamp (C.srTimestamp r)
  , srHeaders   = C.srHeaders r
  , srProducer  = C.srProducer r
  }

----------------------------------------------------------------------
-- Topic / fetch / append wrappers
----------------------------------------------------------------------

clusterClockNow :: C.MockCluster -> IO Timestamp
clusterClockNow c = Timestamp <$> C.clusterClockNow c

tickClock :: C.MockCluster -> Int64 -> IO ()
tickClock = C.tickClock

createTopic :: C.MockCluster -> TopicName -> Int -> IO ()
createTopic c t n = C.createTopic c (unTopicName t) n

listTopics :: C.MockCluster -> IO [TopicName]
listTopics c = map topicName <$> C.listTopics c

partitionCount :: C.MockCluster -> TopicName -> IO (Maybe Int)
partitionCount c t = C.partitionCount c (unTopicName t)

appendToPartition
  :: C.MockCluster
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> [(Data.Text.Text, ByteString)]
  -> Maybe C.ProducerStamp
  -> IO (Either String Int64)
appendToPartition c t p k v (Timestamp ts) hdrs stamp =
  C.appendToPartition c (unTopicName t) p k v ts hdrs stamp

fetchSlice
  :: C.MockCluster
  -> TopicName
  -> Int32
  -> Int64
  -> Int
  -> Bool
  -> IO (Either String ([StoredRecord], Int64))
fetchSlice c t p from maxN stable = do
  r <- C.fetchSlice c (unTopicName t) p from maxN stable
  pure $ case r of
    Left  e         -> Left e
    Right (rs, nxt) -> Right (map fromCore rs, nxt)

partitionHWM :: C.MockCluster -> TopicName -> Int32 -> IO (Maybe Int64)
partitionHWM c t p = C.partitionHWM c (unTopicName t) p

partitionLastStableOffset
  :: C.MockCluster -> TopicName -> Int32 -> IO (Maybe Int64)
partitionLastStableOffset c t p =
  C.partitionLastStableOffset c (unTopicName t) p

commitGroupOffsets
  :: C.MockCluster
  -> C.GroupId
  -> [(TopicName, Int32, Int64)]
  -> IO ()
commitGroupOffsets c g xs =
  C.commitGroupOffsets c g [ (unTopicName t, p, o) | (t, p, o) <- xs ]

groupOffsetsFor
  :: C.MockCluster -> C.GroupId -> IO (Map (TopicName, Int32) Int64)
groupOffsetsFor c g = do
  m <- C.groupOffsetsFor c g
  pure $ Data.Map.Strict.mapKeys (\(t, p) -> (topicName t, p)) m

joinGroup
  :: C.MockCluster
  -> C.GroupId
  -> C.MemberId
  -> [TopicName]
  -> IO ()
joinGroup c g m ts = C.joinGroup c g m (map unTopicName ts)

assignmentFor
  :: C.MockCluster
  -> C.GroupId
  -> C.MemberId
  -> IO [(TopicName, Int32)]
assignmentFor c g m = do
  xs <- C.assignmentFor c g m
  pure [ (topicName t, p) | (t, p) <- xs ]

dumpPartition :: C.MockCluster -> TopicName -> Int32 -> IO [StoredRecord]
dumpPartition c t p = map fromCore <$> C.dumpPartition c (unTopicName t) p

partitionLogSize :: C.MockCluster -> TopicName -> Int32 -> IO Int
partitionLogSize c t p = C.partitionLogSize c (unTopicName t) p
