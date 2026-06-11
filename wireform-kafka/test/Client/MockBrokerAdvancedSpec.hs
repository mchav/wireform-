{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Advanced failure-mode coverage at the core client layer:
epoch fencing, headers round-trip, multi-consumer rebalance,
coordinator retry sequences.
-}
module Client.MockBrokerAdvancedSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int32, Int64)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


ts :: Integer -> Int64
ts = fromIntegral


tests :: Spec
tests =
  describe "MockBrokerAdvanced" $
    sequence_
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


headers_round_trip :: Spec
headers_round_trip =
  it "sendMockH stores headers; pollMC returns them on the StoredRecord" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p <- newMockProducer c fp Nothing
    let hdrs = [("trace-id", bytes "abc"), ("source", bytes "test")]
    _ <- sendMockH p "out" 0 (Just (bytes "k")) (bytes "v") (ts 0) hdrs
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["out"]
    PollResult rs _ <- pollMC cons
    case rs of
      [(_, _, sr)] -> srHeaders sr `shouldBe` hdrs
      _ -> error "expected exactly one record"


empty_headers_round_trip :: Spec
empty_headers_round_trip =
  it "sendMock without headers stores an empty header list" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p <- newMockProducer c fp Nothing
    _ <- sendMock p "out" 0 Nothing (bytes "v") (ts 0)
    [sr] <- dumpPartition c "out" 0
    srHeaders sr `shouldBe` []


epoch_bumps_on_commit :: Spec
epoch_bumps_on_commit =
  it "commitTxn / abortTxn bump the cluster's per-txn-id epoch" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p1 <- newMockProducer c fp (Just (TxnId "tx"))
    Right () <- beginTxnMP p1
    currentTxnEpoch c (TxnId "tx") >>= (`shouldBe` 0)
    Right () <- commitTxnMP p1
    currentTxnEpoch c (TxnId "tx") >>= (`shouldBe` 1)
    p2 <- newMockProducer c fp (Just (TxnId "tx"))
    Right () <- beginTxnMP p2
    Right () <- abortTxnMP p2
    currentTxnEpoch c (TxnId "tx") >>= (`shouldBe` 2)


producer_fenced_after_concurrent_commit :: Spec
producer_fenced_after_concurrent_commit =
  it "stale-epoch producer is fenced when a sibling commits the same txn id" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    pStale <- newMockProducer c fp (Just (TxnId "tx-stale"))
    Right () <- beginTxnMP pStale
    pNew <- newMockProducer c fp (Just (TxnId "tx-stale"))
    Right () <- beginTxnMP pNew
    _ <- sendMock pNew "out" 0 Nothing (bytes "fresh") (ts 0)
    Right () <- commitTxnMP pNew
    r <- sendMock pStale "out" 0 Nothing (bytes "stale") (ts 1)
    case r of
      MPFenced -> pure ()
      other -> error ("expected MPFenced, got " <> show other)
    log_ <- dumpPartition c "out" 0
    map (unbytes . srValue) log_ `shouldBe` ["fresh"]


multi_partition_txn_commit_advances_lso :: Spec
multi_partition_txn_commit_advances_lso =
  it "commitTxn advances LSO on every partition the txn touched" $ do
    c <- newMockCluster 1
    createTopic c "ledger" 3
    fp <- noFaults
    p <- newMockProducer c fp (Just (TxnId "ledger-tx"))
    Right () <- beginTxnMP p
    _ <- sendMock p "ledger" 0 Nothing (bytes "a") (ts 0)
    _ <- sendMock p "ledger" 1 Nothing (bytes "b") (ts 0)
    _ <- sendMock p "ledger" 2 Nothing (bytes "c") (ts 0)
    mapM_
      ( \part -> do
          Just hwm <- partitionHWM c "ledger" part
          Just lso <- partitionLastStableOffset c "ledger" part
          hwm `shouldBe` 1
          lso `shouldBe` 0
      )
      [0, 1, 2]
    Right () <- commitTxnMP p
    mapM_
      ( \part -> do
          Just hwm <- partitionHWM c "ledger" part
          Just lso <- partitionLastStableOffset c "ledger" part
          hwm `shouldBe` 1
          lso `shouldBe` 1
      )
      [0, 1, 2]


two_consumers_split_partitions_round_robin :: Spec
two_consumers_split_partitions_round_robin =
  it "two consumers in one group split partitions deterministically" $ do
    c <- newMockCluster 1
    createTopic c "in" 4
    fp <- noFaults
    let g = GroupId "shared"
    c1 <- newMockConsumerWithId c fp g (MemberId "c1") ReadUncommitted 100
    c2 <- newMockConsumerWithId c fp g (MemberId "c2") ReadUncommitted 100
    subscribeMC c1 ["in"]
    subscribeMC c2 ["in"]
    refreshAssignment c1
    a1 <- L.sort <$> assignedPartitions c1
    a2 <- L.sort <$> assignedPartitions c2
    map snd a1 `shouldBe` [0, 2]
    map snd a2 `shouldBe` [1, 3]
    Set.fromList (a1 ++ a2)
      `shouldBe` Set.fromList [("in", p) | p <- [0, 1, 2, 3]]


three_consumers_one_leaves_partitions_redistribute :: Spec
three_consumers_one_leaves_partitions_redistribute =
  it "leaving the group triggers re-assignment on the survivors" $ do
    c <- newMockCluster 1
    createTopic c "in" 6
    fp <- noFaults
    let g = GroupId "rb"
    a <- newMockConsumerWithId c fp g (MemberId "a") ReadUncommitted 100
    b <- newMockConsumerWithId c fp g (MemberId "b") ReadUncommitted 100
    cConsumer <- newMockConsumerWithId c fp g (MemberId "c") ReadUncommitted 100
    mapM_ (\m -> subscribeMC m ["in"]) [a, b, cConsumer]
    mapM_ refreshAssignment [a, b, cConsumer]
    aBefore <- L.sort <$> assignedPartitions a
    bBefore <- L.sort <$> assignedPartitions b
    cBefore <- L.sort <$> assignedPartitions cConsumer
    map snd aBefore `shouldBe` [0, 3]
    map snd bBefore `shouldBe` [1, 4]
    map snd cBefore `shouldBe` [2, 5]

    leaveGroup c g (MemberId "b")
    refreshAssignment a
    refreshAssignment cConsumer
    aAfter <- L.sort <$> assignedPartitions a
    cAfter <- L.sort <$> assignedPartitions cConsumer
    map snd aAfter `shouldBe` [0, 2, 4]
    map snd cAfter `shouldBe` [1, 3, 5]


coordinator_retry_then_success :: Spec
coordinator_retry_then_success =
  it "two queued NotCoordinator errors fail commits 1 + 2; commit 3 succeeds" $ do
    c <- newMockCluster 1
    createTopic c "in" 1
    let g = GroupId "g"
    fp <- noFaults
    queueCommitErrors fp g [ErrNotCoordinator, ErrCoordinatorNotAvailable]
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["in"]
    r1 <- commitOffsetsMC cons [("in", 0, 1)]
    r2 <- commitOffsetsMC cons [("in", 0, 1)]
    r3 <- commitOffsetsMC cons [("in", 0, 1)]
    case (r1, r2, r3) of
      (Left e1, Left e2, Right ()) -> do
        isRetriable e1 `shouldBe` True
        isRetriable e2 `shouldBe` True
      _ -> error "expected 2 failures, then success"
    m <- groupOffsetsFor c g
    Map.lookup ("in", 0) m `shouldBe` Just 1


several_topics_one_consumer :: Spec
several_topics_one_consumer =
  it "subscribing to N topics assigns every partition of every topic" $ do
    c <- newMockCluster 1
    createTopic c "alpha" 2
    createTopic c "beta" 3
    createTopic c "gamma" 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["alpha", "beta", "gamma"]
    asg <- L.sort <$> assignedPartitions cons
    L.sort asg
      `shouldBe` L.sort
        [ ("alpha", 0)
        , ("alpha", 1)
        , ("beta", 0)
        , ("beta", 1)
        , ("beta", 2)
        , ("gamma", 0)
        ]
