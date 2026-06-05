{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Failure-mode coverage at the core client layer. Each scenario
-- maps directly to a librdkafka mock-cluster test port.
module Client.MockBrokerFailureModesSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

ts :: Integer -> Int64
ts = fromIntegral

tests :: Spec
tests = describe "MockBrokerFailureModes" $ sequence_
  [ producer_drains_queued_retriable_errors_in_order
  , producer_sticky_fault_blocks_all_writes
  , producer_clear_sticky_unblocks_writes
  , producer_alternating_partitions_drain_independently
  , producer_no_such_partition_returns_explanatory_error
  , consumer_seek_overrides_committed_offset
  , consumer_offset_out_of_range_is_retriable
  , consumer_subscribe_to_unknown_topic_yields_empty_assignment
  , consumer_commit_fault_returns_left_without_committing
  , transaction_interleaved_one_committed_one_aborted
  , transaction_begin_fault_keeps_producer_out_of_txn
  , transaction_commit_fault_keeps_records_in_open_state
  , markBroker_down_does_not_break_local_appends
  , clock_advances_monotonically
  ]

----------------------------------------------------------------------
-- Producer
----------------------------------------------------------------------

producer_drains_queued_retriable_errors_in_order :: Spec
producer_drains_queued_retriable_errors_in_order =
  it "queued errors drain FIFO; subsequent sends succeed" $ do
    c  <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    queueProduceErrors fp "out" 0
      [ ErrLeaderNotAvailable, ErrNotLeaderForPartition, ErrRequestTimedOut ]
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p "out" 0 Nothing (bytes "a") (ts 0)
    r2 <- sendMock p "out" 0 Nothing (bytes "b") (ts 0)
    r3 <- sendMock p "out" 0 Nothing (bytes "c") (ts 0)
    r4 <- sendMock p "out" 0 Nothing (bytes "d") (ts 0)
    case (r1, r2, r3, r4) of
      (MPFault e1, MPFault e2, MPFault e3, MPSent _ _) -> do
        isRetriable e1 `shouldBe` True
        isRetriable e2 `shouldBe` True
        isRetriable e3 `shouldBe` True
      _ -> error $ "unexpected: "
                    <> show r1 <> " " <> show r2 <> " "
                    <> show r3 <> " " <> show r4

producer_sticky_fault_blocks_all_writes :: Spec
producer_sticky_fault_blocks_all_writes =
  it "sticky produce fault keeps every send returning the same fault" $ do
    c  <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    setStickyProduce fp "out" 0 ErrCoordinatorNotAvailable
    p  <- newMockProducer c fp Nothing
    rs <- mapM
            (\v -> sendMock p "out" 0 Nothing (bytes v) (ts 0))
            ["1", "2", "3", "4", "5"]
    let faulted = [ () | MPFault _ <- rs ]
    length faulted `shouldBe` 5
    partitionLogSize c "out" 0 >>= (`shouldBe` 0)

producer_clear_sticky_unblocks_writes :: Spec
producer_clear_sticky_unblocks_writes =
  it "clearStickyProduce lets the next send succeed" $ do
    c  <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    setStickyProduce fp "out" 0 ErrNetworkException
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p "out" 0 Nothing (bytes "a") (ts 0)
    case r1 of
      MPFault _ -> pure ()
      other     -> error ("expected fault, got " <> show other)
    clearStickyProduce fp "out" 0
    r2 <- sendMock p "out" 0 Nothing (bytes "b") (ts 0)
    case r2 of
      MPSent 0 0 -> pure ()
      other      -> error ("expected success, got " <> show other)
    partitionLogSize c "out" 0 >>= (`shouldBe` 1)

producer_alternating_partitions_drain_independently :: Spec
producer_alternating_partitions_drain_independently =
  it "fault queues per partition don't bleed into siblings" $ do
    c  <- newMockCluster 1
    createTopic c "out" 2
    fp <- noFaults
    queueProduceErrors fp "out" 0 [ErrLeaderNotAvailable]
    p  <- newMockProducer c fp Nothing
    r0 <- sendMock p "out" 0 Nothing (bytes "p0") (ts 0)
    r1 <- sendMock p "out" 1 Nothing (bytes "p1") (ts 0)
    case r0 of
      MPFault _ -> pure ()
      other     -> error ("partition 0 should fault: " <> show other)
    case r1 of
      MPSent 1 0 -> pure ()
      other      -> error ("partition 1 should succeed: " <> show other)

producer_no_such_partition_returns_explanatory_error :: Spec
producer_no_such_partition_returns_explanatory_error =
  it "send to a non-existent (topic, partition) returns MPNoSuchPartition" $ do
    c  <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p "out" 7 Nothing (bytes "x") (ts 0)
    case r of
      MPNoSuchPartition msg ->
        (not (null msg)) `shouldBe` True
      other -> error ("expected MPNoSuchPartition, got " <> show other)

----------------------------------------------------------------------
-- Consumer
----------------------------------------------------------------------

consumer_seek_overrides_committed_offset :: Spec
consumer_seek_overrides_committed_offset =
  it "seekMC moves the in-memory cursor; commit doesn't auto-rewind" $ do
    c <- newMockCluster 1
    createTopic c "in" 1
    mapM_ (\v -> appendToPartition c "in" 0 Nothing (bytes v) (ts 0) [] Nothing)
      ["a", "b", "c", "d", "e"]
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["in"]
    seekMC cons "in" 0 3
    PollResult rs _ <- pollMC cons
    map (\(_, _, sr) -> unbytes (srValue sr)) rs `shouldBe` ["d", "e"]

consumer_offset_out_of_range_is_retriable :: Spec
consumer_offset_out_of_range_is_retriable =
  it "OffsetOutOfRange is reported as retriable" $ do
    isRetriable ErrOffsetOutOfRange `shouldBe` True
    isFatal     ErrOffsetOutOfRange `shouldBe` False

consumer_subscribe_to_unknown_topic_yields_empty_assignment :: Spec
consumer_subscribe_to_unknown_topic_yields_empty_assignment =
  it "subscribing to a topic that doesn't exist yields an empty assignment" $ do
    c <- newMockCluster 1
    fp <- noFaults
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["ghost"]
    asg <- assignedPartitions cons
    asg `shouldBe` []

consumer_commit_fault_returns_left_without_committing :: Spec
consumer_commit_fault_returns_left_without_committing =
  it "commit fault propagates as Left and the cluster offset is unchanged" $ do
    c <- newMockCluster 1
    createTopic c "in" 1
    let g = GroupId "g"
    fp <- noFaults
    addCommitFault fp g ErrNotCoordinator
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["in"]
    r <- commitOffsetsMC cons [("in", 0, 42)]
    case r of
      Left e  -> isRetriable e `shouldBe` True
      Right _ -> error "expected Left fault"
    m <- groupOffsetsFor c g
    Map.lookup ("in", 0) m `shouldBe` Nothing
    r2 <- commitOffsetsMC cons [("in", 0, 42)]
    case r2 of
      Right () -> pure ()
      Left e   -> error ("unexpected " <> show e)
    m2 <- groupOffsetsFor c g
    Map.lookup ("in", 0) m2 `shouldBe` Just 42

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

transaction_interleaved_one_committed_one_aborted :: Spec
transaction_interleaved_one_committed_one_aborted =
  it "two txns on the same partition: only the committed one is read-committed visible" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    pA <- newMockProducer c fp (Just (TxnId "tx-A"))
    Right () <- beginTxnMP pA
    _ <- sendMock pA "out" 0 Nothing (bytes "A1") (ts 0)
    Right () <- commitTxnMP pA
    pB <- newMockProducer c fp (Just (TxnId "tx-B"))
    Right () <- beginTxnMP pB
    _ <- sendMock pB "out" 0 Nothing (bytes "B1") (ts 1)
    Right () <- abortTxnMP pB
    _ <- appendToPartition c "out" 0 Nothing (bytes "plain") (ts 2) [] Nothing
    cc <- newMockConsumer c fp (GroupId "g") ReadCommitted 100
    subscribeMC cc ["out"]
    PollResult rs _ <- pollMC cc
    map (\(_, _, sr) -> unbytes (srValue sr)) rs `shouldBe` ["A1", "plain"]

transaction_begin_fault_keeps_producer_out_of_txn :: Spec
transaction_begin_fault_keeps_producer_out_of_txn =
  it "beginTxn fault keeps the producer in non-txn state" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    addTxnBeginFault fp (TxnId "tx") ErrInvalidTxnState
    p <- newMockProducer c fp (Just (TxnId "tx"))
    r <- beginTxnMP p
    case r of
      Left e  -> isFatal e `shouldBe` True
      Right _ -> error "expected Left"
    isInTxnMP p >>= (`shouldBe` False)

transaction_commit_fault_keeps_records_in_open_state :: Spec
transaction_commit_fault_keeps_records_in_open_state =
  it "commit fault leaves the txn in TxnOpen" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p <- newMockProducer c fp (Just (TxnId "tx-keep"))
    Right () <- beginTxnMP p
    _ <- sendMock p "out" 0 Nothing (bytes "x") (ts 0)
    addTxnCommitFault fp (TxnId "tx-keep") ErrCoordinatorNotAvailable
    r <- commitTxnMP p
    case r of
      Left e  -> isRetriable e `shouldBe` True
      Right _ -> error "expected Left"
    txnState c (TxnId "tx-keep") >>= (`shouldBe` Just TxnOpen)
    Right () <- commitTxnMP p
    txnState c (TxnId "tx-keep") >>= (`shouldBe` Just TxnCommitted)

----------------------------------------------------------------------
-- Cluster
----------------------------------------------------------------------

markBroker_down_does_not_break_local_appends :: Spec
markBroker_down_does_not_break_local_appends =
  it "broker-down on a partition's leader propagates as not_leader" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    markBrokerDown c (BrokerId 0)
    p <- newMockProducer c fp Nothing
    r <- sendMock p "out" 0 Nothing (bytes "v") (ts 0)
    case r of
      MPNoSuchPartition msg | "not_leader" `T.isInfixOf` T.pack msg -> pure ()
      other -> error ("expected not_leader error, got " <> show other)

clock_advances_monotonically :: Spec
clock_advances_monotonically =
  it "tickClock advances the cluster clock" $ do
    c <- newMockCluster 1
    t0 <- clusterClockNow c
    t0 `shouldBe` 0
    tickClock c 100
    t1 <- clusterClockNow c
    t1 `shouldBe` 100
    tickClock c 50
    t2 <- clusterClockNow c
    t2 `shouldBe` 150
