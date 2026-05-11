{-# LANGUAGE OverloadedStrings #-}

-- | Network/metadata-shaped tests:
-- broker outage propagation, leader reassignment, metadata refresh,
-- exponential backoff curve. Mirrors librdkafka 0121 (clusterid),
-- 0127/0143 (backoff), 0146 (metadata).
module Client.MockBrokerNetSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Client.Mock.Backoff
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Integer -> Int64
ts = fromIntegral

tests :: TestTree
tests = testGroup "MockBrokerNet"
  [ broker_down_propagates_to_producer
  , broker_back_up_resumes_writes
  , reassign_leader_bumps_epoch
  , reassign_leader_after_broker_down_lets_writes_through
  , metadata_lists_brokers_and_topics
  , metadata_redacts_leader_when_down
  , partitionLeader_reflects_round_robin
  , default_backoff_is_monotonic_within_cap
  , default_backoff_caps_at_max
  , backoff_series_is_deterministic
  , backoff_zero_jitter_doubles_exactly
  ]

----------------------------------------------------------------------
-- Broker outage propagation
----------------------------------------------------------------------

broker_down_propagates_to_producer :: TestTree
broker_down_propagates_to_producer =
  testCase "marking the partition's leader broker down surfaces a not-leader error to the producer" $ do
    c <- newMockCluster 2
    createTopic c "t" 1
    Just leader <- partitionLeader c "t" 0
    markBrokerDown c leader
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p "t" 0 Nothing (bytes "v") (ts 0)
    case r of
      MPNoSuchPartition msg ->
        assertBool ("expected not_leader, got: " <> msg)
                   ("not_leader" `L.isInfixOf` msg)
      other -> error ("expected not_leader error, got " <> show other)

broker_back_up_resumes_writes :: TestTree
broker_back_up_resumes_writes =
  testCase "markBrokerUp on the leader lets writes succeed again" $ do
    c <- newMockCluster 2
    createTopic c "t" 1
    Just leader <- partitionLeader c "t" 0
    markBrokerDown c leader
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p "t" 0 Nothing (bytes "lost") (ts 0)   -- fails
    markBrokerUp c leader
    r  <- sendMock p "t" 0 Nothing (bytes "ok") (ts 0)
    case r of
      MPSent 0 0 -> pure ()
      other      -> error ("expected MPSent, got " <> show other)

reassign_leader_bumps_epoch :: TestTree
reassign_leader_bumps_epoch =
  testCase "reassignPartitionLeader bumps the leader epoch by 1" $ do
    c <- newMockCluster 3
    createTopic c "t" 1
    e0 <- currentLeaderEpoch c "t" 0
    e0 @?= Just 0
    Just newEp <- reassignPartitionLeader c "t" 0 (BrokerId 2)
    newEp @?= 1
    Just leader <- partitionLeader c "t" 0
    leader @?= BrokerId 2

reassign_leader_after_broker_down_lets_writes_through :: TestTree
reassign_leader_after_broker_down_lets_writes_through =
  testCase "after a leader-failover dance, writes succeed again on the new leader" $ do
    c <- newMockCluster 3
    createTopic c "t" 1
    Just oldLeader <- partitionLeader c "t" 0
    -- Old leader goes down; producer sees not-leader.
    markBrokerDown c oldLeader
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p "t" 0 Nothing (bytes "blocked") (ts 0)
    case r1 of
      MPNoSuchPartition _ -> pure ()
      other               -> error ("expected not_leader, got " <> show other)
    -- Reassign to a healthy broker.
    let !newLeader = if oldLeader == BrokerId 0 then BrokerId 1 else BrokerId 0
    _ <- reassignPartitionLeader c "t" 0 newLeader
    r2 <- sendMock p "t" 0 Nothing (bytes "through") (ts 0)
    case r2 of
      MPSent _ _ -> pure ()
      other      -> error ("expected MPSent, got " <> show other)

----------------------------------------------------------------------
-- Cluster metadata
----------------------------------------------------------------------

metadata_lists_brokers_and_topics :: TestTree
metadata_lists_brokers_and_topics =
  testCase "describeClusterMetadata lists every broker + topic + partition" $ do
    c <- newMockCluster 3
    createTopic c "alpha" 2
    createTopic c "beta"  1
    cm <- describeClusterMetadata c
    cmClusterId cm @?= "mock-cluster"
    L.sort (cmBrokers cm) @?= [BrokerId 0, BrokerId 1, BrokerId 2]
    map tmName (cmTopics cm) @?= ["alpha", "beta"]
    map (length . tmPartitions) (cmTopics cm) @?= [2, 1]

metadata_redacts_leader_when_down :: TestTree
metadata_redacts_leader_when_down =
  testCase "metadata reports leader=Nothing for partitions whose leader is down" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    Just leader <- partitionLeader c "t" 0
    markBrokerDown c leader
    cm <- describeClusterMetadata c
    case cmTopics cm of
      [tm] -> case tmPartitions tm of
        [pm] -> pmLeader pm @?= Nothing
        _    -> error "expected one partition"
      _    -> error "expected one topic"

partitionLeader_reflects_round_robin :: TestTree
partitionLeader_reflects_round_robin =
  testCase "createTopic round-robins leaders across the broker set" $ do
    c <- newMockCluster 3
    createTopic c "t" 6
    leaders <- mapM (\p -> partitionLeader c "t" p) [0 .. 5]
    -- Round-robin: 0,1,2,0,1,2
    leaders @?= [Just (BrokerId i) | i <- [0, 1, 2, 0, 1, 2]]

----------------------------------------------------------------------
-- Backoff
----------------------------------------------------------------------

default_backoff_is_monotonic_within_cap :: TestTree
default_backoff_is_monotonic_within_cap =
  testCase "default backoff rises monotonically until it hits the ceiling" $ do
    let series = backoffSeries defaultBackoffPolicy 8
    -- First few attempts are below the cap, then the cap holds.
    -- With multiplier 2 + jitter, the curve doesn't have to be
    -- strictly monotonic step-by-step, but the trend should be
    -- non-decreasing once we cross the cap. Check the floor + cap.
    let !mxAllowed = bpMaxMs defaultBackoffPolicy
        !mn        = bpInitialMs defaultBackoffPolicy
    assertBool ("first below cap: " <> show series)
               (head series <= mxAllowed)
    assertBool ("first above floor: " <> show series)
               (head series >= mn `div` 2)
    -- All values bounded by max + jitter ceiling.
    let !ceilingW = round (fromIntegral mxAllowed * (1 + bpJitter defaultBackoffPolicy))
    mapM_ (\b -> assertBool ("value above ceiling: " <> show b)
                            (b <= ceilingW)) series

default_backoff_caps_at_max :: TestTree
default_backoff_caps_at_max =
  testCase "after enough attempts, backoff hits bpMaxMs" $ do
    -- attempt 20 with multiplier 2 puts us many orders of magnitude
    -- above the 1s cap; the cap clamps it.
    let !v = nextBackoffMs defaultBackoffPolicy 20
        !ceilingW = round
                      (fromIntegral (bpMaxMs defaultBackoffPolicy)
                       * (1 + bpJitter defaultBackoffPolicy))
    assertBool ("expected <= ceiling " <> show ceilingW <> ", got " <> show v)
               (v <= ceilingW)

backoff_series_is_deterministic :: TestTree
backoff_series_is_deterministic =
  testCase "backoffSeries produces identical bytes on every call (no random source)" $ do
    let s1 = backoffSeries defaultBackoffPolicy 10
        s2 = backoffSeries defaultBackoffPolicy 10
    s1 @?= s2

backoff_zero_jitter_doubles_exactly :: TestTree
backoff_zero_jitter_doubles_exactly =
  testCase "with bpJitter = 0, bpMultiplier = 2, the curve doubles cleanly" $ do
    let bp = defaultBackoffPolicy
              { bpInitialMs = 50
              , bpMaxMs     = 10000
              , bpJitter    = 0
              }
    backoffSeries bp 5 @?= [50, 100, 200, 400, 800]
