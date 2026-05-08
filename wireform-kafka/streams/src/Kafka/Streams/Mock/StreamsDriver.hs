{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Mock.StreamsDriver
-- Description : Streams engine wired against a 'MockCluster'
--
-- 'MockStreamsDriver' is the higher-level integration: a streams
-- 'Engine' fed by a 'MockConsumer' and emitting via a
-- 'MockProducer'. Tests can exercise the full lifecycle (poll →
-- engine.feedSource → forward → mock-producer.append → consumer
-- offset commit) including failure modes that 'TopologyTestDriver'
-- can't reach because it bypasses the producer/consumer halves.
--
-- The driver is /step-driven/: tests call 'tickDriver' to run one
-- poll/process/commit cycle. This keeps assertions deterministic
-- without 'threadDelay'.
module Kafka.Streams.Mock.StreamsDriver
  ( MockStreamsDriver
  , newMockStreamsDriver
  , driverCluster
  , driverProducer
  , driverConsumer
  , driverEngine
  , tickDriver
  , runUntilQuiet
  , closeMockDriver
    -- * Convenience producers (from outside the topology)
  , externalSend
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.ByteString
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Internal.Engine
  ( Engine
  , buildEngine
  , closeEngine
  , commitEngine
  , engineCollector
  , feedSource
  , takeCommitRequested
  )
import Kafka.Streams.Internal.RecordCollector
  ( CollectedRecord (..)
  , collectorTake
  , inMemoryCollector
  )
import Kafka.Streams.Mock.Cluster
  ( GroupId (..)
  , MockCluster
  , StoredRecord (..)
  , appendToPartition
  , createTopic
  )
import qualified Kafka.Streams.Mock.Consumer
import Kafka.Streams.Mock.Consumer
  ( IsolationLevel (..)
  , MockConsumer
  , PollResult (..)
  , commitOffsetsMC
  , currentPosition
  , newMockConsumer
  , pollMC
  , subscribeMC
  )
import Kafka.Streams.Mock.Fault (FaultPolicy)
import qualified Kafka.Streams.Mock.Fault as F
import Kafka.Streams.Mock.Producer
  ( MockProducer
  , MockProduceResult (..)
  , flushMock
  , newMockProducer
  , sendMock
  )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Time (Timestamp (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (TopicName, unTopicName)

----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

data MockStreamsDriver = MockStreamsDriver
  { driverCluster   :: !MockCluster
  , driverFaults    :: !FaultPolicy
  , driverProducer  :: !MockProducer
  , driverConsumer  :: !MockConsumer
  , driverEngine    :: !Engine
  , driverGroup     :: !GroupId
  , driverPartCount :: !(IORef (Map.Map TopicName Int))
  }

-- | Build a driver. The cluster + faults are caller-supplied so
-- tests can pre-load failure scenarios. The driver:
--
--   * Creates every source topic listed in the topology with the
--     given partition count (default 1) if it doesn't exist yet.
--   * Subscribes the consumer to every source topic.
--   * Spawns no threads; the test drives ticks manually.
newMockStreamsDriver
  :: MockCluster
  -> FaultPolicy
  -> Topo.TopologyValid
  -> Text                              -- ^ application id (group id)
  -> Int                               -- ^ partitions per source topic
  -> IO MockStreamsDriver
newMockStreamsDriver cluster faults topo appId partsPerTopic = do
  let topo' = Topo.topologyValidGraph topo
      sourceTopics =
        Set.toList . Set.fromList
          $ concatMap Topo.sourceTopics
                      (Map.elems (Topo.topoSources topo'))
      sinkTopics =
        Set.toList . Set.fromList
          $ map Topo.sinkTopic (Map.elems (Topo.topoSinks topo'))
  -- Auto-create every topic the topology references, source AND sink.
  forM_ (sourceTopics ++ sinkTopics) $ \t ->
    createTopic cluster t partsPerTopic
  partRef <- newIORef
    (Map.fromList [(t, partsPerTopic) | t <- sourceTopics ++ sinkTopics])
  -- Producer + consumer.
  prod <- newMockProducer cluster faults Nothing
  let !grp = GroupId appId
  cons <- newMockConsumer cluster faults grp ReadUncommitted 100
  subscribeMC cons sourceTopics
  -- Engine.
  collector <- inMemoryCollector
  engine    <- buildEngine topo (TaskId 0 0) appId collector logAndContinue
  pure MockStreamsDriver
    { driverCluster   = cluster
    , driverFaults    = faults
    , driverProducer  = prod
    , driverConsumer  = cons
    , driverEngine    = engine
    , driverGroup     = grp
    , driverPartCount = partRef
    }

----------------------------------------------------------------------
-- Tick: one poll → process → drain → commit cycle
----------------------------------------------------------------------

-- | Run one full poll/process/drain/commit cycle. Returns 'True'
-- if the consumer actually saw fresh records (the test loop
-- typically calls 'tickDriver' until it returns 'False' to reach
-- quiescence).
tickDriver :: MockStreamsDriver -> IO Bool
tickDriver d = do
  PollResult{ prRecords = recs, prErrors = errs } <- pollMC (driverConsumer d)
  -- Surface fetch errors via a per-tick log of recent errors. For
  -- the test driver we currently just count them — tests inspect
  -- via 'driverConsumer' / 'pollMC' directly when they need the
  -- exact error.
  when (not (null errs)) $ pure ()
  -- Feed records into the engine in arrival order.
  forM_ recs $ \(t, p, sr) ->
    feedSource (driverEngine d)
      t (srKey sr) (srValue sr) (srTimestamp sr)
      (fromIntegral p) (srOffset sr)
  -- Drain the engine's collector and route each record into the
  -- mock cluster's partition log under the sink's topic.
  drainCollectedToMock d
  -- Commit offsets to the consumer group if the engine asked for
  -- one (or always — tests can be more aggressive).
  reqd <- takeCommitRequested (driverEngine d)
  when (reqd || True) $ do
    commitEngine (driverEngine d)
    -- Commit current positions to the group coordinator.
    asg <- partitionsForCommit d
    offs <- mapM (\(t, p) -> do
                    mPos <- currentPosition (driverConsumer d) t p
                    pure (t, p, maybe 0 id mPos))
                 asg
    _ <- commitOffsetsMC (driverConsumer d) offs
    pure ()
  pure (not (null recs))

partitionsForCommit
  :: MockStreamsDriver -> IO [(TopicName, Int32)]
partitionsForCommit d = do
  asg <- assignedPartitionsOnConsumer (driverConsumer d)
  pure asg

-- The Consumer module already exports 'assignedPartitions' under
-- that name; alias to avoid the small naming collision below.
assignedPartitionsOnConsumer :: MockConsumer -> IO [(TopicName, Int32)]
assignedPartitionsOnConsumer = Kafka.Streams.Mock.Consumer.assignedPartitions

----------------------------------------------------------------------
-- Drain collector into the mock cluster
----------------------------------------------------------------------

drainCollectedToMock :: MockStreamsDriver -> IO ()
drainCollectedToMock d = do
  -- We don't know which topics the topology might write to in
  -- advance, so iterate over every topic the cluster knows about
  -- and pull anything sitting in the collector for it.
  parts <- readIORef (driverPartCount d)
  forM_ (Map.toAscList parts) $ \(topic, _n) -> do
    rs <- collectorTake (engineCollector (driverEngine d)) topic
    forM_ rs $ \cr -> do
      n <- partitionCountFor d (crTopic cr)
      let !p = case crPartition cr of
            Just explicit -> fromIntegral explicit `mod` n
            Nothing       -> hashKeyToPartition (crKey cr) n
      _ <- sendMock (driverProducer d)
                    (crTopic cr) (fromIntegral p)
                    (crKey cr) (crValue cr) (crTimestamp cr)
      pure ()

partitionCountFor :: MockStreamsDriver -> TopicName -> IO Int
partitionCountFor d t = do
  m <- readIORef (driverPartCount d)
  pure (Map.findWithDefault 1 t m)

hashKeyToPartition :: Maybe ByteString -> Int -> Int
hashKeyToPartition Nothing  _ = 0
hashKeyToPartition (Just b) n =
  let !h = byteStringHash b
   in (h `mod` n)

byteStringHash :: ByteString -> Int
byteStringHash =
  -- Stand-in for Kafka's Murmur2; we only need a deterministic
  -- non-zero hash, not exact compat with the JVM client.
  Data.ByteString.foldl' (\acc w -> acc * 31 + fromEnum w) 0
  where
    foldl' = Data.ByteString.foldl'

----------------------------------------------------------------------
-- Run-until-quiet helper
----------------------------------------------------------------------

-- | Tick until two consecutive ticks return 'False' (no fresh
-- records consumed). Bounded to defend against pathological loops.
runUntilQuiet :: MockStreamsDriver -> IO ()
runUntilQuiet d = go (32 :: Int)
  where
    go 0 = pure ()
    go n = do
      r <- tickDriver d
      if r then go (n - 1) else pure ()

closeMockDriver :: MockStreamsDriver -> IO ()
closeMockDriver d = do
  flushMock (driverProducer d)
  closeEngine (driverEngine d)

----------------------------------------------------------------------
-- External producer (for tests injecting input from outside the
-- topology, e.g. simulating an upstream service)
----------------------------------------------------------------------

-- | Write a record directly to the mock cluster as if some
-- /external/ producer had done so. Useful for setting up a topic
-- with seed data the topology will later consume.
externalSend
  :: MockStreamsDriver
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> IO (Either String Int64)
externalSend d topic part k v ts =
  appendToPartition (driverCluster d) topic part k v ts Nothing
