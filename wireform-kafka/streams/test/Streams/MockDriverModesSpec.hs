{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the 'MockStreamsDriver' in its various modes:
-- multi-partition, exactly-once, mid-flight rebalance.
module Streams.MockDriverModesSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams
import qualified Kafka.Streams.Mock.Cluster as MC
import Kafka.Streams.Mock.Cluster
  hiding (leaveGroup)
import Kafka.Streams.Mock.Consumer
import Kafka.Streams.Mock.Fault
import Kafka.Streams.Mock.Producer
import Kafka.Streams.Mock.StreamsDriver

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "MockDriverModes"
  [ multi_partition_input
  , key_hashed_routing_to_output_partitions
  , eos_mode_round_trip_visible_to_read_committed
  , eos_mode_commit_fault_aborts_records
  , eos_mode_recovers_after_aborted_tick
  , two_drivers_split_partitions_same_group
  , driver_picks_up_partitions_after_sibling_leaves
  ]

passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

----------------------------------------------------------------------
-- Multi-partition routing
----------------------------------------------------------------------

multi_partition_input :: TestTree
multi_partition_input =
  testCase "driver consumes from every partition of a 4-partition input" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 4

    -- Seed each input partition with a distinct value.
    mapM_
      (\(p, v) -> externalSend d (topicName "in") p Nothing (bytes v) (t 0))
      [(0, "p0"), (1, "p1"), (2, "p2"), (3, "p3")]
    runUntilQuiet d

    -- The pass-through topology preserves keys; when there's no
    -- key, the driver hashes 'Nothing' to partition 0. So every
    -- output record lands on partition 0 in this test.
    out0 <- map (unbytes . srValue)
              <$> dumpPartition cluster (topicName "out") 0
    Set.fromList out0 @?= Set.fromList ["p0", "p1", "p2", "p3"]
    closeMockDriver d

key_hashed_routing_to_output_partitions :: TestTree
key_hashed_routing_to_output_partitions =
  testCase "key-hashed routing distributes records across N output partitions" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 3

    -- Send several records, each with a distinct key, to a single
    -- input partition. The pass-through preserves keys; the driver
    -- hashes them on the way out so output should spread across
    -- multiple partitions.
    mapM_
      (\(k, v) -> externalSend d (topicName "in") 0
                                (Just (bytes k)) (bytes v) (t 0))
      [ ("k1", "v1"), ("k2", "v2"), ("k3", "v3")
      , ("k4", "v4"), ("k5", "v5"), ("k6", "v6")
      ]
    runUntilQuiet d

    -- Sum the per-partition output counts. Every input record
    -- should appear in exactly one output partition.
    counts <- mapM
      (\p -> length <$> dumpPartition cluster (topicName "out") p)
      [0, 1, 2]
    sum counts @?= 6
    -- And at least two partitions should be non-empty (the hash
    -- function we use is FNV-style and very unlikely to map all
    -- six keys to the same bucket).
    let !nonEmpty = length (filter (> 0) counts)
    assertBool ("expected >= 2 non-empty output partitions, got " <> show counts)
               (nonEmpty >= 2)
    closeMockDriver d

----------------------------------------------------------------------
-- EOS mode
----------------------------------------------------------------------

eos_mode_round_trip_visible_to_read_committed :: TestTree
eos_mode_round_trip_visible_to_read_committed =
  testCase "EOS mode: per-tick txn commit; read-committed consumer sees the output" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriverEOS cluster fp topo "app" (TxnId "app-tx") 1

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "alpha") (t 0)
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "bravo") (t 1)
    runUntilQuiet d

    cc <- newMockConsumer cluster fp (GroupId "ext") ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult rs _ <- pollMC cc
    map (\(_, _, sr) -> unbytes (srValue sr)) rs @?= ["alpha", "bravo"]
    closeMockDriver d

eos_mode_commit_fault_aborts_records :: TestTree
eos_mode_commit_fault_aborts_records =
  testCase "EOS mode: a commitTxn fault aborts the tick; nothing is read-committed visible" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    let txn = TxnId "app-tx"
    d <- newMockStreamsDriverEOS cluster fp topo "app" txn 1

    -- Inject a commit-only fault so the next commit fails.
    addTxnCommitFault fp txn ErrCoordinatorNotAvailable

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "lost") (t 0)
    _ <- tickDriver d   -- tick begins txn, sends, commit faults, abort

    cc <- newMockConsumer cluster fp (GroupId "ext") ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult rs _ <- pollMC cc
    rs @?= []
    -- The txn is aborted, not committed.
    txnState cluster txn >>= (@?= Just TxnAborted)
    closeMockDriver d

eos_mode_recovers_after_aborted_tick :: TestTree
eos_mode_recovers_after_aborted_tick =
  testCase "EOS mode: after an aborted tick, the next tick recommits cleanly" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    let txn = TxnId "app-tx"
    d <- newMockStreamsDriverEOS cluster fp topo "app" txn 1

    -- First tick: arrange a commit-only fault.
    addTxnCommitFault fp txn ErrCoordinatorNotAvailable
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "first") (t 0)
    _ <- tickDriver d

    -- The first record was aborted but the consumer's offset
    -- advanced past it (we don't have rollback semantics in this
    -- minimal model). Send another and tick cleanly.
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "second") (t 1)
    -- Second tick: no faults; commit succeeds.
    runUntilQuiet d

    cc <- newMockConsumer cluster fp (GroupId "ext") ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult rs _ <- pollMC cc
    -- Read-committed sees only the successfully-committed record.
    map (\(_, _, sr) -> unbytes (srValue sr)) rs @?= ["second"]
    closeMockDriver d

----------------------------------------------------------------------
-- Multi-driver rebalance
----------------------------------------------------------------------

two_drivers_split_partitions_same_group :: TestTree
two_drivers_split_partitions_same_group =
  testCase "two drivers in the same group split a 4-partition input" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    -- Both drivers use application id "shared" → same group.
    a <- newMockStreamsDriver cluster fp topo "shared" 4
    b <- newMockStreamsDriver cluster fp topo "shared" 4
    -- Driver A's consumer joined first and saw a single-member
    -- group; refresh now that B is also in.
    refreshAssignment (driverConsumer a)

    -- Seed each input partition.
    mapM_
      (\(p, v) -> externalSend a (topicName "in") p Nothing (bytes v) (t 0))
      [(0, "p0"), (1, "p1"), (2, "p2"), (3, "p3")]
    -- Drive both drivers to quiescence.
    runUntilQuiet a
    runUntilQuiet b

    aAsg <- L.sort <$> assignedPartitions (driverConsumer a)
    bAsg <- L.sort <$> assignedPartitions (driverConsumer b)
    -- The deterministic round-robin assignor sorts members by id;
    -- since both auto-generated ids are "consumer-N" with N
    -- monotonically increasing, the lower-N driver gets even
    -- partitions and the higher-N driver gets odd ones.
    map snd aAsg @?= [0, 2]
    map snd bAsg @?= [1, 3]
    -- Together they cover every partition.
    Set.fromList (aAsg ++ bAsg)
      @?= Set.fromList [(topicName "in", p) | p <- [0, 1, 2, 3]]
    closeMockDriver a
    closeMockDriver b

driver_picks_up_partitions_after_sibling_leaves :: TestTree
driver_picks_up_partitions_after_sibling_leaves =
  testCase "after a sibling driver leaves, the remaining driver gets all partitions" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    a <- newMockStreamsDriver cluster fp topo "shared" 4
    b <- newMockStreamsDriver cluster fp topo "shared" 4
    refreshAssignment (driverConsumer a)
    -- Both have a partition slice. B leaves the group; A refreshes.
    MC.leaveGroup cluster (GroupId "shared")
                   (consumerMemberId (driverConsumer b))
    refreshAssignment (driverConsumer a)
    asg <- L.sort <$> assignedPartitions (driverConsumer a)
    map snd asg @?= [0, 1, 2, 3]
    closeMockDriver a
    closeMockDriver b
