{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Consumer
-- Description : Consumer view over a 'MockCluster'
--
-- A 'MockConsumer' joins a consumer group, subscribes to a list of
-- topics, and polls fresh records from each subscribed (topic,
-- partition). On each poll the consumer:
--
--   1. Consults the 'FaultPolicy' for an injected fetch error;
--      if one is present and is retriable, the test should treat
--      the next poll as a retry. Fatal fetch errors propagate via
--      the result type.
--   2. Fetches up to @fetchBatchSize@ records starting at the
--      current per-partition offset.
--   3. Advances the in-memory offset cursor (the consumer's
--      position) past the returned records — actual offset commit
--      happens later via 'commitOffsetsMC'.
module Kafka.Streams.Mock.Consumer
  ( MockConsumer
  , newMockConsumer
  , subscribeMC
  , assignedPartitions
  , topicAssignment
  , PollResult (..)
  , pollMC
  , commitOffsetsMC
  , seekMC
  , currentPosition
  , IsolationLevel (..)
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, when)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Kafka.Streams.Mock.Cluster
  ( GroupId
  , MockCluster
  , StoredRecord (..)
  , commitGroupOffsets
  , fetchSlice
  , groupOffsetsFor
  , listTopics
  , partitionCount
  )
import qualified Kafka.Streams.Mock.Fault
import Kafka.Streams.Mock.Fault
  ( FaultPolicy
  , MockError
  , takeFetchFault
  , takeCommitFault
  )
import qualified Data.Text
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Isolation level
----------------------------------------------------------------------

data IsolationLevel = ReadUncommitted | ReadCommitted
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Consumer
----------------------------------------------------------------------

data MockConsumer = MockConsumer
  { mcCluster      :: !MockCluster
  , mcFaults       :: !FaultPolicy
  , mcGroupId      :: !GroupId
  , mcAssignment   :: !(TVar (Set (TopicName, Int32)))
  , mcPositions    :: !(TVar (Map (TopicName, Int32) Int64))
    -- ^ The consumer's /current/ read cursor per partition. Distinct
    -- from the committed offset (group offset) because the consumer
    -- might have read records but not yet committed.
  , mcSubscribed   :: !(TVar (Set TopicName))
  , mcIsolation    :: !IsolationLevel
  , mcFetchBatch   :: !Int
  }

-- | Build a fresh consumer. The /assignment/ is initially empty;
-- call 'subscribeMC' to subscribe to topics, which auto-assigns
-- every partition of every subscribed topic to this consumer.
-- (The mock collapses the rebalance protocol to a single-member
-- group for simplicity; multi-member groups would use the
-- 'Kafka.Streams.Runtime.Assignor' pure assigner.)
newMockConsumer
  :: MockCluster
  -> FaultPolicy
  -> GroupId
  -> IsolationLevel
  -> Int                             -- ^ fetch batch size
  -> IO MockConsumer
newMockConsumer c fp gid iso n = do
  asg <- newTVarIO Set.empty
  pos <- newTVarIO Map.empty
  sub <- newTVarIO Set.empty
  pure MockConsumer
    { mcCluster    = c
    , mcFaults     = fp
    , mcGroupId    = gid
    , mcAssignment = asg
    , mcPositions  = pos
    , mcSubscribed = sub
    , mcIsolation  = iso
    , mcFetchBatch = n
    }

-- | Subscribe to a list of topics, replacing any prior subscription.
-- Re-fetches the latest committed offsets for each (topic, partition)
-- and uses them as the starting position; partitions with no
-- committed offset start at 0 (earliest).
subscribeMC :: MockConsumer -> [TopicName] -> IO ()
subscribeMC mc topics = do
  -- Compute the new assignment.
  newAsg <- fmap concat . forM topics $ \t -> do
    mp <- partitionCount (mcCluster mc) t
    case mp of
      Nothing -> pure []
      Just n  -> pure [(t, fromIntegral i) | i <- [0 .. n - 1]]
  -- Fetch committed offsets and seed positions.
  committed <- groupOffsetsFor (mcCluster mc) (mcGroupId mc)
  let !positions = Map.fromList
        [ (tp, Map.findWithDefault 0 tp committed)
        | tp <- newAsg
        ]
  atomically $ do
    writeTVar (mcSubscribed mc) (Set.fromList topics)
    writeTVar (mcAssignment mc) (Set.fromList newAsg)
    writeTVar (mcPositions mc) positions

assignedPartitions :: MockConsumer -> IO [(TopicName, Int32)]
assignedPartitions mc = Set.toList <$> readTVarIO (mcAssignment mc)

-- | Alias: Java's @KafkaConsumer.assignment@ vs the field accessor
-- naming we use elsewhere. Keeps imports unambiguous.
topicAssignment :: MockConsumer -> IO [(TopicName, Int32)]
topicAssignment = assignedPartitions

----------------------------------------------------------------------
-- Poll
----------------------------------------------------------------------

-- | One poll's worth of records, plus per-partition fetch errors.
data PollResult = PollResult
  { prRecords :: ![(TopicName, Int32, StoredRecord)]
  , prErrors  :: ![(TopicName, Int32, MockError)]
  }
  deriving Show

-- | Poll every assigned partition once. Returns up to
-- @fetchBatchSize@ records /per partition/; the caller is expected
-- to call 'pollMC' in a loop. Records are emitted in partition
-- order, then offset order within partition.
pollMC :: MockConsumer -> IO PollResult
pollMC mc = do
  asg <- readTVarIO (mcAssignment mc)
  positions <- readTVarIO (mcPositions mc)
  results <- forM (Set.toList asg) $ \(t, p) -> do
    let !pos = Map.findWithDefault 0 (t, p) positions
    mFault <- takeFetchFault (mcFaults mc) t p
    case mFault of
      Just e -> pure (Left (t, p, e))
      Nothing -> do
        slice <- fetchSlice (mcCluster mc) t p pos
                            (mcFetchBatch mc)
                            (mcIsolation mc == ReadCommitted)
        case slice of
          Left  err -> pure (Left (t, p, errFromText err))
          Right (rs, next) -> pure (Right (t, p, rs, next))
  let !errs    = [ (t, p, e) | Left (t, p, e) <- results ]
      !success = [ (t, p, rs, next) | Right (t, p, rs, next) <- results ]
      !flat    = concatMap (\(t, p, rs, _) ->
                              [ (t, p, r) | r <- rs ]) success
  -- Advance positions.
  atomically $
    forM_ success $ \(t, p, _rs, next) ->
      modifyTVar' (mcPositions mc) (Map.insert (t, p) next)
  pure PollResult
    { prRecords = flat
    , prErrors  = errs
    }
  where
    forM_ xs f = mapM_ f xs
    errFromText = Kafka.Streams.Mock.Fault.ErrCustom . Data.Text.pack

----------------------------------------------------------------------
-- Commit
----------------------------------------------------------------------

-- | Commit a batch of (topic, partition, offset) entries to the
-- group's coordinator. Mirrors @KafkaConsumer.commitSync@.
commitOffsetsMC
  :: MockConsumer
  -> [(TopicName, Int32, Int64)]
  -> IO (Either MockError ())
commitOffsetsMC mc offs = do
  mFault <- takeCommitFault (mcFaults mc) (mcGroupId mc)
  case mFault of
    Just e  -> pure (Left e)
    Nothing -> do
      commitGroupOffsets (mcCluster mc) (mcGroupId mc) offs
      pure (Right ())

-- | Move the consumer's read cursor on a partition. Mirrors
-- @KafkaConsumer.seek@.
seekMC :: MockConsumer -> TopicName -> Int32 -> Int64 -> IO ()
seekMC mc t p off = atomically $
  modifyTVar' (mcPositions mc) (Map.insert (t, p) off)

currentPosition
  :: MockConsumer -> TopicName -> Int32 -> IO (Maybe Int64)
currentPosition mc t p = do
  m <- readTVarIO (mcPositions mc)
  pure (Map.lookup (t, p) m)

