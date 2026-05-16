{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.MultiInstanceMockHarness
-- Description : Live multi-instance rebalance harness over the
--               in-process @MockCluster@
--
-- "Kafka.Streams.Runtime.MultiInstanceHarness" is a /pure/
-- abstract model of N instances, failure events, and a
-- "would-have-processed" trace. It's useful for property tests
-- but it doesn't exercise the actual JoinGroup / SyncGroup /
-- assignment / poll cycle.
--
-- This module fills that gap. It spins up N
-- 'Kafka.Streams.Mock.StreamsDriver.MockStreamsDriver' instances
-- against the same 'MockCluster' with the same application id,
-- and exposes a small step-driven API so tests can drive a
-- realistic two-instance assignment dance:
--
-- @
-- harness <- newMockSet topology 4 2     -- 4 partitions, 2 instances
-- send harness \"in\" 0 (Just \"k0\") \"v0\" (ts 0)
-- send harness \"in\" 1 (Just \"k1\") \"v1\" (ts 0)
-- tickAll harness
-- asg <- instanceAssignments harness
-- print asg     -- e.g. [(\"i0\", [(\"in\",0),(\"in\",2)]), (\"i1\", [(\"in\",1),(\"in\",3)])]
-- crashInstance harness 0
-- refreshAll harness                     -- surviving instance picks up the orphans
-- @
--
-- This is the "multi-process rebalance verification" path the
-- streams @README.md@ called out, with one caveat: each
-- /instance/ here is still a Haskell value in the same process
-- (a separate 'MockStreamsDriver' against the shared mock
-- cluster). For a real multi-OS-process test you'd swap
-- 'MockCluster' for 'Kafka.Streams.Runtime.NativeDriver'
-- against a live broker — the harness shape, the assignment
-- semantics, and the failure responses are identical.
module Kafka.Streams.Runtime.MultiInstanceMockHarness
  ( -- * Harness
    MockSet
  , newMockSet
  , closeMockSet
    -- * Inspection
  , setInstances
  , instanceAssignments
  , setCluster
    -- * Drive
  , tickAll
  , tickAllUntilQuiet
  , refreshAll
    -- * Manipulate
  , send
  , crashInstance
    -- * Output
  , readSink
  ) where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import qualified Kafka.Streams.Mock.Cluster as Cluster
import Kafka.Streams.Mock.Cluster (MockCluster, StoredRecord)
import qualified Kafka.Streams.Mock.Consumer as MC
import qualified Kafka.Streams.Mock.Fault as Fault
import Kafka.Streams.Mock.StreamsDriver
  ( MockStreamsDriver
  , closeMockDriver
  , driverConsumer
  , externalSend
  , newMockStreamsDriver
  , tickDriver
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Harness
----------------------------------------------------------------------

-- | A live multi-instance harness. The 'MockCluster' is shared
-- across every driver so they meet on @JoinGroup@. Each driver
-- has its own consumer / producer / engine / state stores —
-- exactly like a multi-process deployment of the runtime.
data MockSet = MockSet
  { msCluster   :: !MockCluster
  , msAppId     :: !Text
  , msTopology  :: !Topo.TopologyValid
  , msPartCount :: !Int
  , msInstances :: !(IORef (Map.Map InstanceLabel MockStreamsDriver))
  }

-- | A stable identifier for an instance in the harness. Used to
-- crash / inspect a particular member of the group.
type InstanceLabel = Text

----------------------------------------------------------------------
-- Setup
----------------------------------------------------------------------

-- | Create a fresh 'MockCluster', spin up @numInstances@
-- drivers against it, all subscribed under the same
-- application id, and wait for the assignor to deal partitions
-- across them.
--
-- Returns the harness with @numInstances@ instances labelled
-- @\"i0\"@ … @\"i(N-1)\"@; tests can address them by label
-- through 'crashInstance' / 'instanceAssignments' / 'readSink'.
newMockSet
  :: Topo.TopologyValid
  -> Int                                  -- ^ partitions per topic
  -> Text                                 -- ^ application id
  -> Int                                  -- ^ number of instances
  -> IO MockSet
newMockSet topo partsPerTopic appId numInstances = do
  -- Spin the cluster up with a single broker; the assignor +
  -- group-coordinator paths we exercise don't multiplex across
  -- brokers, but 'createTopic' refuses to register a topic on
  -- an empty broker set.
  cluster <- Cluster.newMockCluster 1
  faults  <- Fault.noFaults
  ref     <- newIORef Map.empty
  let set = MockSet
        { msCluster   = cluster
        , msAppId     = appId
        , msTopology  = topo
        , msPartCount = partsPerTopic
        , msInstances = ref
        }
  -- Spin up the instances one by one. After each join, refresh
  -- every existing driver's assignment so they shed partitions
  -- to the newcomer — matches what JoinGroup does on the wire.
  forM_ [0 .. numInstances - 1] $ \i -> do
    let label = T.pack ("i" <> show i)
    d <- newMockStreamsDriver
            cluster
            faults
            topo
            appId
            partsPerTopic
    atomicModifyIORef' ref $ \m ->
      (Map.insert label d m, ())
    refreshAll set
  pure set

-- | Close every driver in the set. Idempotent.
closeMockSet :: MockSet -> IO ()
closeMockSet set = do
  m <- readIORef (msInstances set)
  forM_ (Map.elems m) closeMockDriver
  writeIORef (msInstances set) Map.empty

----------------------------------------------------------------------
-- Inspection
----------------------------------------------------------------------

-- | Every live instance's label.
setInstances :: MockSet -> IO [InstanceLabel]
setInstances set = Map.keys <$> readIORef (msInstances set)

-- | Every live instance, paired with its current assignment.
-- The union of these lists should cover every partition of
-- every subscribed source topic, exactly once, after the
-- assignor has stabilised.
instanceAssignments
  :: MockSet -> IO [(InstanceLabel, [(TopicName, Int32)])]
instanceAssignments set = do
  m <- readIORef (msInstances set)
  forM (Map.toAscList m) $ \(label, d) -> do
    asg <- MC.assignedPartitions (driverConsumer d)
    pure (label, asg)

-- | The 'MockCluster' the set is wired against. Exposed so
-- tests can call lower-level @Mock.Cluster@ helpers
-- (e.g. @dumpPartition@) directly.
setCluster :: MockSet -> MockCluster
setCluster = msCluster

----------------------------------------------------------------------
-- Drive
----------------------------------------------------------------------

-- | Run one poll/process/commit cycle on every live instance,
-- in label order. Returns 'True' if /any/ instance consumed
-- at least one record.
tickAll :: MockSet -> IO Bool
tickAll set = do
  m <- readIORef (msInstances set)
  rs <- forM (Map.elems m) tickDriver
  pure (or rs)

-- | Loop 'tickAll' until every instance reports a quiet tick.
-- Bounded so a pathological topology can't hang the suite.
tickAllUntilQuiet :: MockSet -> IO ()
tickAllUntilQuiet set = go (64 :: Int)
  where
    go 0 = pure ()
    go n = do
      progressed <- tickAll set
      if progressed then go (n - 1) else pure ()

-- | Ask every live instance to re-run the assignor against the
-- cluster's current membership. Matches the JVM client calling
-- @rebalance@ after a member joined or left.
refreshAll :: MockSet -> IO ()
refreshAll set = do
  m <- readIORef (msInstances set)
  forM_ (Map.elems m) $ \d ->
    MC.refreshAssignment (driverConsumer d)

----------------------------------------------------------------------
-- Manipulate
----------------------------------------------------------------------

-- | Send a record into a source topic from /outside/ the
-- harness (an upstream service in real life; an @externalSend@
-- against the mock cluster here). The harness routes the
-- record through whichever instance currently owns the
-- partition.
send
  :: MockSet
  -> TopicName
  -> Int32                                -- ^ partition
  -> Maybe ByteString                     -- ^ key
  -> ByteString                           -- ^ value
  -> Timestamp
  -> IO (Either String Int64)
send set topic part k v ts = do
  m <- readIORef (msInstances set)
  -- Any instance can publish; we pick the first one
  -- arbitrarily. The mock producer talks to the shared cluster.
  case Map.elems m of
    (d : _) -> externalSend d topic part k v ts
    []      -> pure (Left "MockSet: no instances")

-- | Pretend an instance crashed: close it, drop its handle
-- from the set, and call 'Cluster.leaveGroup' so the cluster
-- evicts its membership. Surviving instances pick up the
-- orphaned partitions after the next 'refreshAll'.
--
-- Idempotent: crashing an instance that isn't there is a
-- no-op.
crashInstance :: MockSet -> InstanceLabel -> IO ()
crashInstance set label = do
  mbDriver <- atomicModifyIORef' (msInstances set) $ \m ->
    case Map.lookup label m of
      Nothing -> (m, Nothing)
      Just d  -> (Map.delete label m, Just d)
  case mbDriver of
    Nothing -> pure ()
    Just d  -> do
      let mid = MC.consumerMemberId (driverConsumer d)
      Cluster.leaveGroup
        (msCluster set)
        (Cluster.GroupId (msAppId set))
        mid
      closeMockDriver d

----------------------------------------------------------------------
-- Output
----------------------------------------------------------------------

-- | Read every record sitting in @topic@ partition @part@ at
-- this point in time. Useful to assert the union of records
-- both instances produced into a sink topic.
readSink
  :: MockSet -> TopicName -> Int32 -> IO [StoredRecord]
readSink set topic part =
  Cluster.dumpPartition (msCluster set) topic part
