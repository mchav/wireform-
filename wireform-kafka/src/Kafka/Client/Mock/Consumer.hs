{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Client.Mock.Consumer
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
module Kafka.Client.Mock.Consumer
  ( MockConsumer
  , newMockConsumer
  , newMockConsumerWithId
  , consumerMemberId
  , subscribeMC
  , refreshAssignment
  , assignedPartitions
  , topicAssignment
  , PollResult (..)
  , pollMC
  , commitOffsetsMC
  , commitOffsetsWithMetadataMC
  , seekMC
  , currentPosition
  , IsolationLevel (..)
    -- * Pause / resume (KIP-41)
  , pausePartitions
  , resumePartitions
  , pausedPartitions
    -- * Manual offset store (KIP-392 store_offsets / 0130)
  , storeOffsetMC
  , storedOffsets
  , commitStoredOffsetsMC
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, when)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text
import System.IO.Unsafe (unsafePerformIO)

import Kafka.Client.Mock.Cluster
  ( GroupId
  , MemberId (..)
  , MockCluster
  , OffsetAndMetadata (..)
  , StoredRecord (..)
  , assignmentFor
  , commitGroupOffsets
  , commitGroupOffsetsWithMetadata
  , fetchSlice
  , groupOffsetsFor
  , joinGroup
  , listTopics
  , partitionCount
  )
import qualified Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Fault
  ( FaultPolicy
  , MockError
  , takeFetchFault
  , takeCommitFault
  )
import qualified Data.Text


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
  , mcMemberId     :: !MemberId
  , mcAssignment   :: !(TVar (Set (Text, Int32)))
  , mcPositions    :: !(TVar (Map (Text, Int32) Int64))
    -- ^ The consumer's /current/ read cursor per partition. Distinct
    -- from the committed offset (group offset) because the consumer
    -- might have read records but not yet committed.
  , mcSubscribed   :: !(TVar (Set Text))
  , mcPaused       :: !(TVar (Set (Text, Int32)))
  , mcStored       :: !(TVar (Map (Text, Int32) Int64))
    -- ^ Manual offset store: 'storeOffsetMC' writes here without
    -- committing to the cluster; 'commitStoredOffsetsMC' drains
    -- the store into a single commit. Mirrors the
    -- @auto.offset.store / store_offsets@ semantics in the
    -- librdkafka client.
    -- ^ Partitions on which 'pollMC' should currently skip the fetch.
    -- Mirrors @KafkaConsumer.pause(Collection<TopicPartition>)@.
  , mcIsolation    :: !IsolationLevel
  , mcFetchBatch   :: !Int
  }

-- | Build a fresh consumer. The /assignment/ is initially empty;
-- call 'subscribeMC' to subscribe to topics, which auto-assigns
-- every partition of every subscribed topic to this consumer.
-- (The mock collapses the rebalance protocol to a single-member
-- group for simplicity; multi-member groups would use the
-- 'Kafka.Streams.Runtime.Assignor (in the streams package)' pure assigner.)
-- | Build a fresh consumer with an auto-generated member id. Use
-- the explicit-id variant if you need stable identifiers across
-- runs (e.g. for sticky-assignment tests).
newMockConsumer
  :: MockCluster
  -> FaultPolicy
  -> GroupId
  -> IsolationLevel
  -> Int                             -- ^ fetch batch size
  -> IO MockConsumer
newMockConsumer c fp gid iso n = do
  ctr <- nextConsumerCounter
  let !mid = MemberId (Data.Text.pack ("consumer-" <> show ctr))
  newMockConsumerWithId c fp gid mid iso n

-- | Build a consumer with an explicit member id.
newMockConsumerWithId
  :: MockCluster
  -> FaultPolicy
  -> GroupId
  -> MemberId
  -> IsolationLevel
  -> Int
  -> IO MockConsumer
newMockConsumerWithId c fp gid mid iso n = do
  asg <- newTVarIO Set.empty
  pos <- newTVarIO Map.empty
  sub <- newTVarIO Set.empty
  pa  <- newTVarIO Set.empty
  st  <- newTVarIO Map.empty
  pure MockConsumer
    { mcCluster    = c
    , mcFaults     = fp
    , mcGroupId    = gid
    , mcMemberId   = mid
    , mcAssignment = asg
    , mcPositions  = pos
    , mcSubscribed = sub
    , mcPaused     = pa
    , mcStored     = st
    , mcIsolation  = iso
    , mcFetchBatch = n
    }

consumerMemberId :: MockConsumer -> MemberId
consumerMemberId = mcMemberId

----------------------------------------------------------------------
-- Member id counter
----------------------------------------------------------------------

{-# NOINLINE consumerCounter #-}
consumerCounter :: TVar Int
consumerCounter = unsafePerformIO (newTVarIO 0)

nextConsumerCounter :: IO Int
nextConsumerCounter = atomically $ do
  n <- readTVar consumerCounter
  writeTVar consumerCounter (n + 1)
  pure n

-- | Subscribe to a list of topics, replacing any prior
-- subscription. Joins the consumer to its group; the cluster runs
-- a deterministic round-robin assignor over every member's union
-- of subscribed topics. Use 'refreshAssignment' to re-run the
-- assignor after a sibling consumer joins or leaves.
subscribeMC :: MockConsumer -> [Text] -> IO ()
subscribeMC mc topics = do
  joinGroup (mcCluster mc) (mcGroupId mc) (mcMemberId mc) topics
  atomically $ writeTVar (mcSubscribed mc) (Set.fromList topics)
  refreshAssignment mc

-- | Re-run the group assignor and update this consumer's
-- assignment. Mirrors what the JVM client does after receiving an
-- @onPartitionsAssigned@ callback from the consumer coordinator.
refreshAssignment :: MockConsumer -> IO ()
refreshAssignment mc = do
  newAsg <- assignmentFor (mcCluster mc) (mcGroupId mc) (mcMemberId mc)
  committed <- groupOffsetsFor (mcCluster mc) (mcGroupId mc)
  let positionsList :: [((Text, Int32), Int64)]
      positionsList =
        map (\tp -> (tp, Map.findWithDefault 0 tp committed)) newAsg
      !positions = Map.fromList positionsList
  atomically $ do
    writeTVar (mcAssignment mc) (Set.fromList newAsg)
    writeTVar (mcPositions mc) positions

assignedPartitions :: MockConsumer -> IO [(Text, Int32)]
assignedPartitions mc = Set.toList <$> readTVarIO (mcAssignment mc)

-- | Alias: Java's @KafkaConsumer.assignment@ vs the field accessor
-- naming we use elsewhere. Keeps imports unambiguous.
topicAssignment :: MockConsumer -> IO [(Text, Int32)]
topicAssignment = assignedPartitions

----------------------------------------------------------------------
-- Poll
----------------------------------------------------------------------

-- | One poll's worth of records, plus per-partition fetch errors.
data PollResult = PollResult
  { prRecords :: ![(Text, Int32, StoredRecord)]
  , prErrors  :: ![(Text, Int32, MockError)]
  }
  deriving Show

-- | Poll every assigned partition once. Returns up to
-- @fetchBatchSize@ records /per partition/; the caller is expected
-- to call 'pollMC' in a loop. Records are emitted in partition
-- order, then offset order within partition.
pollMC :: MockConsumer -> IO PollResult
pollMC mc = do
  asg <- readTVarIO (mcAssignment mc)
  paused <- readTVarIO (mcPaused mc)
  positions <- readTVarIO (mcPositions mc)
  -- Skip paused partitions entirely: no records, no fault, no
  -- position advance. Mirrors KafkaConsumer.pause semantics.
  let !active = Set.toList (Set.difference asg paused)
  results <- forM active $ \(t, p) -> do
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
    errFromText = Kafka.Client.Mock.Fault.ErrCustom . Data.Text.pack

----------------------------------------------------------------------
-- Commit
----------------------------------------------------------------------

-- | Commit a batch of (topic, partition, offset) entries to the
-- group's coordinator. Mirrors @KafkaConsumer.commitSync@.
commitOffsetsMC
  :: MockConsumer
  -> [(Text, Int32, Int64)]
  -> IO (Either MockError ())
commitOffsetsMC mc offs = do
  mFault <- takeCommitFault (mcFaults mc) (mcGroupId mc)
  case mFault of
    Just e  -> pure (Left e)
    Nothing -> do
      commitGroupOffsets (mcCluster mc) (mcGroupId mc) offs
      pure (Right ())

-- | Like 'commitOffsetsMC' but records full
-- 'OffsetAndMetadata' (including KIP-320 leaderEpoch + per-commit
-- metadata bytes). Mirrors
-- @KafkaConsumer.commitSync(Map<TopicPartition, OffsetAndMetadata>)@.
commitOffsetsWithMetadataMC
  :: MockConsumer
  -> [((Text, Int32), OffsetAndMetadata)]
  -> IO (Either MockError ())
commitOffsetsWithMetadataMC mc offs = do
  mFault <- takeCommitFault (mcFaults mc) (mcGroupId mc)
  case mFault of
    Just e  -> pure (Left e)
    Nothing -> do
      commitGroupOffsetsWithMetadata
        (mcCluster mc) (mcGroupId mc) offs
      pure (Right ())

----------------------------------------------------------------------
-- Pause / resume (KIP-41)
----------------------------------------------------------------------

-- | Mark a set of partitions paused on this consumer; subsequent
-- 'pollMC' calls skip them entirely. Idempotent. Pause-state is
-- consumer-local (not stored on the cluster), so a second
-- consumer in the same group is unaffected.
pausePartitions
  :: MockConsumer -> [(Text, Int32)] -> IO ()
pausePartitions mc tps = atomically $
  modifyTVar' (mcPaused mc)
    (Set.union (Set.fromList tps))

resumePartitions
  :: MockConsumer -> [(Text, Int32)] -> IO ()
resumePartitions mc tps = atomically $
  modifyTVar' (mcPaused mc)
    (\s -> Set.difference s (Set.fromList tps))

pausedPartitions :: MockConsumer -> IO [(Text, Int32)]
pausedPartitions mc = Set.toList <$> readTVarIO (mcPaused mc)

----------------------------------------------------------------------
-- Manual offset store
----------------------------------------------------------------------

-- | Store an offset locally without committing it to the cluster.
-- Mirrors librdkafka's @rd_kafka_offset_store / store_offsets@:
-- the application is in full control of /when/ those local stores
-- become commits via 'commitStoredOffsetsMC'.
storeOffsetMC
  :: MockConsumer -> Text -> Int32 -> Int64 -> IO ()
storeOffsetMC mc t p o = atomically $
  modifyTVar' (mcStored mc) (Map.insert (t, p) o)

storedOffsets :: MockConsumer -> IO (Map (Text, Int32) Int64)
storedOffsets = readTVarIO . mcStored

-- | Commit every offset currently in the local store, then clear
-- it. The commit goes through the regular 'takeCommitFault' check
-- so test scenarios can still inject coordinator errors.
commitStoredOffsetsMC :: MockConsumer -> IO (Either MockError ())
commitStoredOffsetsMC mc = do
  m <- readTVarIO (mcStored mc)
  let !batch = [ (t, p, o) | ((t, p), o) <- Map.toList m ]
  if null batch
    then pure (Right ())
    else do
      r <- commitOffsetsMC mc batch
      case r of
        Right () -> do
          atomically (writeTVar (mcStored mc) Map.empty)
          pure (Right ())
        Left e -> pure (Left e)

-- | Move the consumer's read cursor on a partition. Mirrors
-- @KafkaConsumer.seek@.
seekMC :: MockConsumer -> Text -> Int32 -> Int64 -> IO ()
seekMC mc t p off = atomically $
  modifyTVar' (mcPositions mc) (Map.insert (t, p) off)

currentPosition
  :: MockConsumer -> Text -> Int32 -> IO (Maybe Int64)
currentPosition mc t p = do
  m <- readTVarIO (mcPositions mc)
  pure (Map.lookup (t, p) m)

