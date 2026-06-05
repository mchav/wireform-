{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Failure-mode coverage for the mock broker. Mirrors the
-- specific scenarios librdkafka exercises with
-- @rd_kafka_mock_push_request_errors@ and friends.
module Streams.MockFailureModesSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Streams.Imperative
import Kafka.Streams.Mock.Cluster
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

tests :: Spec
tests = describe "MockFailureModes" $ sequence_
  [ -- Producer
    producer_drains_queued_retriable_errors_in_order
  , producer_sticky_fault_blocks_all_writes
  , producer_clear_sticky_unblocks_writes
  , producer_alternating_partitions_drain_independently
  , producer_no_such_partition_returns_explanatory_error
    -- Consumer
  , consumer_seek_overrides_committed_offset
  , consumer_offset_out_of_range_is_retriable
  , consumer_subscribe_to_unknown_topic_yields_empty_assignment
  , consumer_commit_fault_returns_left_without_committing
    -- Transactions
  , transaction_interleaved_one_committed_one_aborted
  , transaction_begin_fault_keeps_producer_out_of_txn
  , transaction_commit_fault_keeps_records_in_open_state
    -- Broker / cluster
  , markBroker_down_does_not_break_local_appends
  , clock_advances_monotonically
    -- StreamsDriver end-to-end failure paths
  , driver_sticky_fetch_fault_blocks_progress
  , driver_sticky_fetch_then_clear_resumes
  , driver_recovers_from_multiple_consecutive_fetch_faults
  , driver_lso_observed_via_partitionLastStableOffset
  ]

----------------------------------------------------------------------
-- Producer
----------------------------------------------------------------------

producer_drains_queued_retriable_errors_in_order :: Spec
producer_drains_queued_retriable_errors_in_order =
  it "queued errors drain FIFO; subsequent sends succeed" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    queueProduceErrors fp (topicName "out") 0
      [ ErrLeaderNotAvailable, ErrNotLeaderForPartition, ErrRequestTimedOut ]
    p  <- newMockProducer c fp Nothing
    -- Three retriable faults pop in order, then the fourth send
    -- actually succeeds.
    r1 <- sendMock p (topicName "out") 0 Nothing (bytes "a") (t 0)
    r2 <- sendMock p (topicName "out") 0 Nothing (bytes "b") (t 0)
    r3 <- sendMock p (topicName "out") 0 Nothing (bytes "c") (t 0)
    r4 <- sendMock p (topicName "out") 0 Nothing (bytes "d") (t 0)
    case (r1, r2, r3, r4) of
      (MPFault e1, MPFault e2, MPFault e3, MPSent _ _) ->
        do isRetriable e1 `shouldBe` True
           isRetriable e2 `shouldBe` True
           isRetriable e3 `shouldBe` True
      _ -> error $ "unexpected: "
                    <> show r1 <> " "
                    <> show r2 <> " "
                    <> show r3 <> " "
                    <> show r4

producer_sticky_fault_blocks_all_writes :: Spec
producer_sticky_fault_blocks_all_writes =
  it "sticky produce fault keeps every send returning the same fault" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    setStickyProduce fp (topicName "out") 0 ErrCoordinatorNotAvailable
    p  <- newMockProducer c fp Nothing
    -- Five sends; all five faulted.
    rs <- mapM (\v -> sendMock p (topicName "out") 0
                                Nothing (bytes v) (t 0))
               ["1", "2", "3", "4", "5"]
    let faulted = [ () | MPFault _ <- rs ]
    length faulted `shouldBe` 5
    -- Nothing made it to the log.
    partitionLogSize c (topicName "out") 0 >>= (`shouldBe` 0)

producer_clear_sticky_unblocks_writes :: Spec
producer_clear_sticky_unblocks_writes =
  it "clearStickyProduce lets the next send succeed" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    setStickyProduce fp (topicName "out") 0 ErrNetworkException
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p (topicName "out") 0 Nothing (bytes "a") (t 0)
    case r1 of
      MPFault _ -> pure ()
      other     -> error ("expected fault, got " <> show other)
    clearStickyProduce fp (topicName "out") 0
    r2 <- sendMock p (topicName "out") 0 Nothing (bytes "b") (t 0)
    case r2 of
      MPSent 0 0 -> pure ()
      other      -> error ("expected success, got " <> show other)
    partitionLogSize c (topicName "out") 0 >>= (`shouldBe` 1)

producer_alternating_partitions_drain_independently :: Spec
producer_alternating_partitions_drain_independently =
  it "fault queues per partition don't bleed into siblings" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 2
    fp <- noFaults
    queueProduceErrors fp (topicName "out") 0 [ErrLeaderNotAvailable]
    -- Partition 1 has no faults; should succeed even on the first try.
    p  <- newMockProducer c fp Nothing
    r0 <- sendMock p (topicName "out") 0 Nothing (bytes "p0") (t 0)
    r1 <- sendMock p (topicName "out") 1 Nothing (bytes "p1") (t 0)
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
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    -- Topic 'out' has 1 partition; sending to partition 7 hits the error path.
    r <- sendMock p (topicName "out") 7 Nothing (bytes "x") (t 0)
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
    createTopic c (topicName "in") 1
    -- Seed five records.
    mapM_ (\v -> appendToPartition c (topicName "in") 0 Nothing (bytes v) (t 0) [] Nothing)
      ["a", "b", "c", "d", "e"]
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons [topicName "in"]
    -- Seek past the first three.
    seekMC cons (topicName "in") 0 3
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
    subscribeMC cons [topicName "ghost"]
    asg <- assignedPartitions cons
    asg `shouldBe` []

consumer_commit_fault_returns_left_without_committing :: Spec
consumer_commit_fault_returns_left_without_committing =
  it "commit fault propagates as Left and the cluster offset is unchanged" $ do
    c <- newMockCluster 1
    createTopic c (topicName "in") 1
    let g = GroupId "g"
    fp <- noFaults
    addCommitFault fp g ErrNotCoordinator
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons [topicName "in"]
    r <- commitOffsetsMC cons [(topicName "in", 0, 42)]
    case r of
      Left e  -> isRetriable e `shouldBe` True
      Right _ -> error "expected Left fault"
    -- Offset is unchanged in the group store.
    m <- groupOffsetsFor c g
    Map.lookup (topicName "in", 0) m `shouldBe` Nothing
    -- A second commit (no fault queued) succeeds.
    r2 <- commitOffsetsMC cons [(topicName "in", 0, 42)]
    case r2 of
      Right () -> pure ()
      Left e   -> error ("unexpected " <> show e)
    m2 <- groupOffsetsFor c g
    Map.lookup (topicName "in", 0) m2 `shouldBe` Just 42

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

transaction_interleaved_one_committed_one_aborted :: Spec
transaction_interleaved_one_committed_one_aborted =
  it "two txns on the same partition: only the committed one is read-committed visible" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    -- Producer 1: tx-A, will commit.
    pA <- newMockProducer c fp (Just (TxnId "tx-A"))
    Right () <- beginTxnMP pA
    _ <- sendMock pA (topicName "out") 0 Nothing (bytes "A1") (t 0)
    Right () <- commitTxnMP pA
    -- Producer 2: tx-B, will abort. Independent producer + epoch.
    pB <- newMockProducer c fp (Just (TxnId "tx-B"))
    Right () <- beginTxnMP pB
    _ <- sendMock pB (topicName "out") 0 Nothing (bytes "B1") (t 1)
    Right () <- abortTxnMP pB
    -- A non-transactional record outside any txn.
    _ <- appendToPartition c (topicName "out") 0 Nothing (bytes "plain") (t 2) [] Nothing

    cc <- newMockConsumer c fp (GroupId "g") ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult rs _ <- pollMC cc
    map (\(_, _, sr) -> unbytes (srValue sr)) rs `shouldBe` ["A1", "plain"]

transaction_begin_fault_keeps_producer_out_of_txn :: Spec
transaction_begin_fault_keeps_producer_out_of_txn =
  it "beginTxn fault keeps the producer in non-txn state" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    addTxnFault fp (TxnId "tx") ErrInvalidTxnState
    p <- newMockProducer c fp (Just (TxnId "tx"))
    r <- beginTxnMP p
    case r of
      Left e -> isFatal e `shouldBe` True
      Right _ -> error "expected Left"
    isInTxnMP p >>= (`shouldBe` False)

transaction_commit_fault_keeps_records_in_open_state :: Spec
transaction_commit_fault_keeps_records_in_open_state =
  it "commit fault leaves the txn in TxnOpen" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p <- newMockProducer c fp (Just (TxnId "tx-keep"))
    Right () <- beginTxnMP p
    _ <- sendMock p (topicName "out") 0 Nothing (bytes "x") (t 0)
    addTxnFault fp (TxnId "tx-keep") ErrCoordinatorNotAvailable
    r <- commitTxnMP p
    case r of
      Left e -> isRetriable e `shouldBe` True
      Right _ -> error "expected Left"
    -- Txn state unchanged: still TxnOpen.
    txnState c (TxnId "tx-keep") >>= (`shouldBe` Just TxnOpen)
    -- A second commit (no fault queued) succeeds and bumps to Committed.
    Right () <- commitTxnMP p
    txnState c (TxnId "tx-keep") >>= (`shouldBe` Just TxnCommitted)

----------------------------------------------------------------------
-- Broker / cluster
----------------------------------------------------------------------

markBroker_down_does_not_break_local_appends :: Spec
markBroker_down_does_not_break_local_appends =
  it "broker-down on a partition's leader propagates as not_leader" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    markBrokerDown c (BrokerId 0)
    p <- newMockProducer c fp Nothing
    r <- sendMock p (topicName "out") 0 Nothing (bytes "v") (t 0)
    case r of
      MPNoSuchPartition msg | "not_leader" `T.isInfixOf` T.pack msg -> pure ()
      other -> error ("expected not_leader error, got " <> show other)

clock_advances_monotonically :: Spec
clock_advances_monotonically =
  it "tickClock advances the cluster clock" $ do
    c <- newMockCluster 1
    Timestamp t0 <- clusterClockNow c
    t0 `shouldBe` 0
    tickClock c 100
    Timestamp t1 <- clusterClockNow c
    t1 `shouldBe` 100
    tickClock c 50
    Timestamp t2 <- clusterClockNow c
    t2 `shouldBe` 150

----------------------------------------------------------------------
-- StreamsDriver end-to-end failure paths
----------------------------------------------------------------------

passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left  err -> error (show err)
    Right v   -> pure v

driver_sticky_fetch_fault_blocks_progress :: Spec
driver_sticky_fetch_fault_blocks_progress =
  it "a sticky fetch fault on the input topic blocks all output" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    setStickyFetch fp (topicName "in") 0 ErrCoordinatorLoadInProgress

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "v1") (t 0)
    -- Run several ticks; nothing should make it through.
    mapM_ (\_ -> tickDriver d) [1 .. 5 :: Int]
    out <- dumpPartition cluster (topicName "out") 0
    out `shouldBe` []
    closeMockDriver d

driver_sticky_fetch_then_clear_resumes :: Spec
driver_sticky_fetch_then_clear_resumes =
  it "clearing a sticky fetch fault lets buffered records flow through" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    setStickyFetch fp (topicName "in") 0 ErrCoordinatorLoadInProgress
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "v1") (t 0)
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "v2") (t 1)
    mapM_ (\_ -> tickDriver d) [1 .. 3 :: Int]
    out0 <- dumpPartition cluster (topicName "out") 0
    out0 `shouldBe` []

    clearStickyFetch fp (topicName "in") 0
    runUntilQuiet d
    out1 <- dumpPartition cluster (topicName "out") 0
    map (unbytes . srValue) out1 `shouldBe` ["v1", "v2"]
    closeMockDriver d

driver_recovers_from_multiple_consecutive_fetch_faults :: Spec
driver_recovers_from_multiple_consecutive_fetch_faults =
  it "three queued fetch faults consume three ticks; the fourth tick processes the record" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    queueFetchErrors fp (topicName "in") 0
      [ ErrLeaderNotAvailable
      , ErrNotLeaderForPartition
      , ErrRequestTimedOut
      ]
    topo <- passthroughTopo
    d    <- newMockStreamsDriver cluster fp topo "app" 1

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "v") (t 0)
    -- Ticks 1..3 each consume one queued fault.
    mapM_ (\_ -> tickDriver d) [1 .. 3 :: Int]
    out0 <- dumpPartition cluster (topicName "out") 0
    out0 `shouldBe` []
    -- Tick 4 sees no fault and processes the record.
    _ <- tickDriver d
    out1 <- dumpPartition cluster (topicName "out") 0
    map (unbytes . srValue) out1 `shouldBe` ["v"]
    closeMockDriver d

driver_lso_observed_via_partitionLastStableOffset :: Spec
driver_lso_observed_via_partitionLastStableOffset =
  it "non-transactional sink writes advance LSO step-for-step with HWM" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    mapM_ (\v -> externalSend d (topicName "in") 0
                                Nothing (bytes v) (t 0))
          ["a", "b", "c"]
    runUntilQuiet d

    Just hwm <- partitionHWM cluster (topicName "out") 0
    Just lso <- partitionLastStableOffset cluster (topicName "out") 0
    hwm `shouldBe` 3
    lso `shouldBe` 3
    closeMockDriver d
