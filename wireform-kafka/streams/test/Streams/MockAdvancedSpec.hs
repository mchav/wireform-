{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Advanced failure-mode tests for the mock broker.
module Streams.MockAdvancedSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative
import qualified Kafka.Streams.Mock.Cluster as MC
import Kafka.Streams.Mock.Cluster
  hiding (leaveGroup)
import Kafka.Streams.Mock.Consumer
import Kafka.Streams.Mock.Fault
import Kafka.Streams.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "MockAdvanced"
  [ headers_round_trip
  , empty_headers_round_trip
  , epoch_bumps_on_commit
  , producer_fenced_after_concurrent_commit
  , multi_partition_txn_commit_advances_lso
  , two_consumers_split_partitions_round_robin
  , three_consumers_one_leaves_partitions_redistribute
  , coordinator_retry_then_success
  , several_topics_one_consumer
  ]

----------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------

headers_round_trip :: TestTree
headers_round_trip =
  testCase "sendMockH stores headers; pollMC returns them on the StoredRecord" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    let hdrs = [("trace-id", bytes "abc"), ("source", bytes "test")]
    _  <- sendMockH p (topicName "out") 0 (Just (bytes "k")) (bytes "v") (t 0) hdrs
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons [topicName "out"]
    PollResult rs _ <- pollMC cons
    case rs of
      [(_, _, sr)] -> srHeaders sr @?= hdrs
      _            -> error "expected exactly one record"

empty_headers_round_trip :: TestTree
empty_headers_round_trip =
  testCase "sendMock without headers stores an empty header list" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p (topicName "out") 0 Nothing (bytes "v") (t 0)
    [sr] <- dumpPartition c (topicName "out") 0
    srHeaders sr @?= []

----------------------------------------------------------------------
-- Epoch fencing
----------------------------------------------------------------------

epoch_bumps_on_commit :: TestTree
epoch_bumps_on_commit =
  testCase "commitTxn / abortTxn bump the cluster's per-txn-id epoch" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p1 <- newMockProducer c fp (Just (TxnId "tx"))
    Right () <- beginTxnMP p1
    currentTxnEpoch c (TxnId "tx") >>= (@?= 0)
    Right () <- commitTxnMP p1
    currentTxnEpoch c (TxnId "tx") >>= (@?= 1)
    -- Re-using the same txn id bumps again on the next commit.
    p2 <- newMockProducer c fp (Just (TxnId "tx"))
    Right () <- beginTxnMP p2
    Right () <- abortTxnMP p2
    currentTxnEpoch c (TxnId "tx") >>= (@?= 2)

producer_fenced_after_concurrent_commit :: TestTree
producer_fenced_after_concurrent_commit =
  testCase "stale-epoch producer is fenced when a sibling commits the same txn id" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    -- Both producers attach to the same txn id "tx-stale".
    pStale  <- newMockProducer c fp (Just (TxnId "tx-stale"))
    Right () <- beginTxnMP pStale       -- pStale's epoch = 0
    -- A concurrent restarted producer takes over the same txn id,
    -- begins, sends, and commits -> the broker bumps to epoch 1.
    pNew    <- newMockProducer c fp (Just (TxnId "tx-stale"))
    Right () <- beginTxnMP pNew
    _       <- sendMock pNew (topicName "out") 0 Nothing (bytes "fresh") (t 0)
    Right () <- commitTxnMP pNew        -- cluster epoch = 1
    -- Now the stale producer tries to send with its old epoch.
    r <- sendMock pStale (topicName "out") 0 Nothing (bytes "stale") (t 1)
    case r of
      MPFenced -> pure ()
      other    -> error ("expected MPFenced, got " <> show other)
    -- The stale send's payload never landed.
    log_ <- dumpPartition c (topicName "out") 0
    map (unbytes . srValue) log_ @?= ["fresh"]

----------------------------------------------------------------------
-- Multi-partition txn
----------------------------------------------------------------------

multi_partition_txn_commit_advances_lso :: TestTree
multi_partition_txn_commit_advances_lso =
  testCase "commitTxn advances LSO on every partition the txn touched" $ do
    c <- newMockCluster 1
    createTopic c (topicName "ledger") 3
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "ledger-tx"))
    Right () <- beginTxnMP p
    _ <- sendMock p (topicName "ledger") 0 Nothing (bytes "a") (t 0)
    _ <- sendMock p (topicName "ledger") 1 Nothing (bytes "b") (t 0)
    _ <- sendMock p (topicName "ledger") 2 Nothing (bytes "c") (t 0)
    -- Pre-commit: every partition has HWM = 1 but LSO = 0.
    mapM_ (\part -> do
              Just hwm <- partitionHWM c (topicName "ledger") part
              Just lso <- partitionLastStableOffset c (topicName "ledger") part
              hwm @?= 1
              lso @?= 0)
          [0, 1, 2]
    Right () <- commitTxnMP p
    -- Post-commit: LSO catches up to HWM on all three.
    mapM_ (\part -> do
              Just hwm <- partitionHWM c (topicName "ledger") part
              Just lso <- partitionLastStableOffset c (topicName "ledger") part
              hwm @?= 1
              lso @?= 1)
          [0, 1, 2]

----------------------------------------------------------------------
-- Multi-consumer rebalance
----------------------------------------------------------------------

two_consumers_split_partitions_round_robin :: TestTree
two_consumers_split_partitions_round_robin =
  testCase "two consumers in one group split partitions deterministically" $ do
    c <- newMockCluster 1
    createTopic c (topicName "in") 4
    fp <- noFaults
    let g = GroupId "shared"
    c1 <- newMockConsumerWithId c fp g (MemberId "c1") ReadUncommitted 100
    c2 <- newMockConsumerWithId c fp g (MemberId "c2") ReadUncommitted 100
    subscribeMC c1 [topicName "in"]
    subscribeMC c2 [topicName "in"]
    -- After both have subscribed, re-run the assignor on c1 so it
    -- sees the new member set.
    refreshAssignment c1

    a1 <- L.sort <$> assignedPartitions c1
    a2 <- L.sort <$> assignedPartitions c2
    -- The deterministic round-robin assignor (sorted by member id,
    -- partitions sorted by topic + index) gives:
    --   c1 -> partitions 0, 2
    --   c2 -> partitions 1, 3
    map snd a1 @?= [0, 2]
    map snd a2 @?= [1, 3]
    -- Together they cover every partition exactly once.
    Set.fromList (a1 ++ a2)
      @?= Set.fromList [(topicName "in", p) | p <- [0, 1, 2, 3]]

three_consumers_one_leaves_partitions_redistribute :: TestTree
three_consumers_one_leaves_partitions_redistribute =
  testCase "leaving the group triggers re-assignment on the survivors" $ do
    c <- newMockCluster 1
    createTopic c (topicName "in") 6
    fp <- noFaults
    let g = GroupId "rb"
    a <- newMockConsumerWithId c fp g (MemberId "a") ReadUncommitted 100
    b <- newMockConsumerWithId c fp g (MemberId "b") ReadUncommitted 100
    cConsumer <- newMockConsumerWithId c fp g (MemberId "c") ReadUncommitted 100
    mapM_ (\m -> subscribeMC m [topicName "in"]) [a, b, cConsumer]
    mapM_ refreshAssignment [a, b, cConsumer]

    aBefore <- L.sort <$> assignedPartitions a
    bBefore <- L.sort <$> assignedPartitions b
    cBefore <- L.sort <$> assignedPartitions cConsumer
    map snd aBefore @?= [0, 3]
    map snd bBefore @?= [1, 4]
    map snd cBefore @?= [2, 5]

    -- 'b' leaves; 'a' and 'c' refresh.
    MC.leaveGroup c g (MemberId "b")
    refreshAssignment a
    refreshAssignment cConsumer

    aAfter <- L.sort <$> assignedPartitions a
    cAfter <- L.sort <$> assignedPartitions cConsumer
    -- Two members + six partitions = 3 each. Sorted alphabetically:
    --   a -> partitions 0, 2, 4
    --   c -> partitions 1, 3, 5
    map snd aAfter @?= [0, 2, 4]
    map snd cAfter @?= [1, 3, 5]

----------------------------------------------------------------------
-- Coordinator retry
----------------------------------------------------------------------

coordinator_retry_then_success :: TestTree
coordinator_retry_then_success =
  testCase "two queued NotCoordinator errors fail commits 1 + 2; commit 3 succeeds" $ do
    c <- newMockCluster 1
    createTopic c (topicName "in") 1
    let g = GroupId "g"
    fp <- noFaults
    queueCommitErrors fp g [ErrNotCoordinator, ErrCoordinatorNotAvailable]
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons [topicName "in"]
    r1 <- commitOffsetsMC cons [(topicName "in", 0, 1)]
    r2 <- commitOffsetsMC cons [(topicName "in", 0, 1)]
    r3 <- commitOffsetsMC cons [(topicName "in", 0, 1)]
    case (r1, r2, r3) of
      (Left e1, Left e2, Right ()) -> do
        isRetriable e1 @?= True
        isRetriable e2 @?= True
      _ -> error "expected 2 failures, then success"
    -- Final committed offset is what r3 wrote.
    m <- groupOffsetsFor c g
    Map.lookup (topicName "in", 0) m @?= Just 1

----------------------------------------------------------------------
-- Multi-topic single-consumer assignment
----------------------------------------------------------------------

several_topics_one_consumer :: TestTree
several_topics_one_consumer =
  testCase "subscribing to N topics assigns every partition of every topic" $ do
    c <- newMockCluster 1
    createTopic c (topicName "alpha") 2
    createTopic c (topicName "beta")  3
    createTopic c (topicName "gamma") 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons [topicName "alpha", topicName "beta", topicName "gamma"]
    asg <- L.sort <$> assignedPartitions cons
    L.sort asg @?=
      L.sort
        [ (topicName "alpha", 0), (topicName "alpha", 1)
        , (topicName "beta",  0), (topicName "beta",  1), (topicName "beta", 2)
        , (topicName "gamma", 0)
        ]
