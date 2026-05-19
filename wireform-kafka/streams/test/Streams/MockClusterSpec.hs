{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the in-process mock broker (cluster + producer +
-- consumer + streams driver). Mirrors the failure-mode coverage
-- librdkafka exercises through @rd_kafka_mock_cluster_t@.
module Streams.MockClusterSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32, Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative
import Kafka.Streams.Mock.Cluster
import qualified Kafka.Streams.Mock.Cluster as MC
import Kafka.Streams.Mock.Consumer
import qualified Kafka.Streams.Mock.Consumer as MCons
import Kafka.Streams.Mock.Fault
import qualified Kafka.Streams.Mock.Fault as MF
import Kafka.Streams.Mock.Producer
import qualified Kafka.Streams.Mock.Producer as MP
import Kafka.Streams.Mock.StreamsDriver

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "MockCluster"
  [ cluster_basics
  , append_and_fetch
  , consumer_groups_remember_offsets
  , producer_round_trip
  , producer_retriable_then_succeeds
  , producer_fatal_propagates
  , consumer_fetch_retriable_isolated_to_partition
  , read_committed_filters_open_txn_records
  , transaction_commit_advances_lso
  , transaction_abort_makes_records_invisible
  , broker_marked_down_is_observable
  , streams_driver_round_trip
  , streams_driver_with_filter_topology
  , streams_driver_handles_fetch_fault_then_recovers
  ]

----------------------------------------------------------------------
-- Cluster primitives
----------------------------------------------------------------------

cluster_basics :: TestTree
cluster_basics = testCase "createTopic / listTopics / partitionCount" $ do
  c <- newMockCluster 3
  createTopic c (topicName "x") 4
  createTopic c (topicName "y") 1
  ts <- listTopics c
  Set.fromList ts @?= Set.fromList [topicName "x", topicName "y"]
  partitionCount c (topicName "x") >>= (@?= Just 4)
  partitionCount c (topicName "y") >>= (@?= Just 1)
  partitionCount c (topicName "z") >>= (@?= Nothing)

append_and_fetch :: TestTree
append_and_fetch = testCase "appendToPartition assigns offsets; fetchSlice reads them back" $ do
  c <- newMockCluster 1
  createTopic c (topicName "log") 1
  Right o0 <- appendToPartition c (topicName "log") 0
                (Just (bytes "k")) (bytes "a") (t 0) [] Nothing
  Right o1 <- appendToPartition c (topicName "log") 0
                (Just (bytes "k")) (bytes "b") (t 1) [] Nothing
  o0 @?= 0
  o1 @?= 1
  Right (rs, next) <- fetchSlice c (topicName "log") 0 0 100 False
  map (unbytes . srValue) rs @?= ["a", "b"]
  next @?= 2

consumer_groups_remember_offsets :: TestTree
consumer_groups_remember_offsets =
  testCase "commitGroupOffsets / groupOffsetsFor round-trips per (topic, partition)" $ do
    c <- newMockCluster 1
    createTopic c (topicName "log") 2
    let g = GroupId "consumers"
    commitGroupOffsets c g
      [ (topicName "log", 0, 7)
      , (topicName "log", 1, 12)
      ]
    m <- groupOffsetsFor c g
    Map.lookup (topicName "log", 0) m @?= Just 7
    Map.lookup (topicName "log", 1) m @?= Just 12
    Map.lookup (topicName "log", 2) m @?= Nothing

----------------------------------------------------------------------
-- Producer round trip + faults
----------------------------------------------------------------------

producer_round_trip :: TestTree
producer_round_trip =
  testCase "MockProducer.send appends to the cluster log" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p (topicName "out") 0 (Just (bytes "k")) (bytes "v") (t 0)
    case r of
      MPSent 0 0 -> pure ()
      other      -> error ("unexpected " <> show other)
    sz <- partitionLogSize c (topicName "out") 0
    sz @?= 1

producer_retriable_then_succeeds :: TestTree
producer_retriable_then_succeeds =
  testCase "queued retriable produce error fires once, then the next send succeeds" $ do
    c  <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    addProduceFault fp (topicName "out") 0 ErrLeaderNotAvailable
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p (topicName "out") 0 Nothing (bytes "v1") (t 0)
    case r1 of
      MPFault e -> isRetriable e @?= True
      other     -> error ("expected fault, got " <> show other)
    r2 <- sendMock p (topicName "out") 0 Nothing (bytes "v2") (t 1)
    case r2 of
      MPSent 0 0 -> pure ()
      other      -> error ("unexpected " <> show other)
    -- Only the second send made it into the log.
    log_ <- dumpPartition c (topicName "out") 0
    map (unbytes . srValue) log_ @?= ["v2"]

producer_fatal_propagates :: TestTree
producer_fatal_propagates =
  testCase "fatal produce error doesn't append" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    addProduceFault fp (topicName "out") 0 ErrAuthorizationFailed
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p (topicName "out") 0 Nothing (bytes "v") (t 0)
    case r of
      MPFault e -> isFatal e @?= True
      other     -> error ("expected fault, got " <> show other)
    partitionLogSize c (topicName "out") 0 >>= (@?= 0)

----------------------------------------------------------------------
-- Consumer faults
----------------------------------------------------------------------

consumer_fetch_retriable_isolated_to_partition :: TestTree
consumer_fetch_retriable_isolated_to_partition =
  testCase "fetch fault on one partition does not block siblings" $ do
    c <- newMockCluster 1
    createTopic c (topicName "in") 2
    let g = GroupId "g"
    -- Seed both partitions with one record.
    _ <- appendToPartition c (topicName "in") 0 Nothing (bytes "p0") (t 0) [] Nothing
    _ <- appendToPartition c (topicName "in") 1 Nothing (bytes "p1") (t 0) [] Nothing
    fp <- noFaults
    -- Fault on partition 0 only.
    addFetchFault fp (topicName "in") 0 ErrCoordinatorLoadInProgress
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons [topicName "in"]
    PollResult{ prRecords = recs, prErrors = errs } <- pollMC cons
    -- Partition 0 surfaced an error; partition 1 returned its record.
    map (\(_, p, _) -> p) errs @?= [0]
    map (\(_, p, sr) -> (p, unbytes (srValue sr))) recs
      @?= [(1, "p1")]

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

read_committed_filters_open_txn_records :: TestTree
read_committed_filters_open_txn_records =
  testCase "read-committed consumers don't see records inside an open txn" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx1"))
    Right () <- beginTxnMP p
    _ <- sendMock p (topicName "out") 0 Nothing (bytes "in-txn") (t 0)
    -- Read-uncommitted sees the record:
    let g = GroupId "g"
    cu <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cu [topicName "out"]
    PollResult uncommitted _ <- pollMC cu
    map (\(_, _, sr) -> unbytes (srValue sr)) uncommitted
      @?= ["in-txn"]
    -- Read-committed does NOT:
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult committed _ <- pollMC cc
    committed @?= []

transaction_commit_advances_lso :: TestTree
transaction_commit_advances_lso =
  testCase "commitTxn advances LSO so read-committed consumers catch up" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx2"))
    Right () <- beginTxnMP p
    _ <- sendMock p (topicName "out") 0 Nothing (bytes "v") (t 0)
    Right () <- commitTxnMP p
    let g = GroupId "g"
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult committed _ <- pollMC cc
    map (\(_, _, sr) -> unbytes (srValue sr)) committed @?= ["v"]
    txnState c (TxnId "tx2") >>= (@?= Just TxnCommitted)

transaction_abort_makes_records_invisible :: TestTree
transaction_abort_makes_records_invisible =
  testCase "abortTxn keeps read-committed consumers from ever seeing the records" $ do
    c <- newMockCluster 1
    createTopic c (topicName "out") 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx3"))
    Right () <- beginTxnMP p
    _ <- sendMock p (topicName "out") 0 Nothing (bytes "v") (t 0)
    Right () <- abortTxnMP p
    let g = GroupId "g"
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc [topicName "out"]
    PollResult committed _ <- pollMC cc
    committed @?= []
    txnState c (TxnId "tx3") >>= (@?= Just TxnAborted)

----------------------------------------------------------------------
-- Brokers
----------------------------------------------------------------------

broker_marked_down_is_observable :: TestTree
broker_marked_down_is_observable =
  testCase "markBrokerDown / isBrokerUp / downedBrokers form a consistent state" $ do
    c <- newMockCluster 3
    isBrokerUp c (BrokerId 0) >>= (@?= True)
    markBrokerDown c (BrokerId 0)
    isBrokerUp c (BrokerId 0) >>= (@?= False)
    downedBrokers c >>= (@?= [BrokerId 0])
    markBrokerUp c (BrokerId 0)
    isBrokerUp c (BrokerId 0) >>= (@?= True)
    downedBrokers c >>= (@?= [])

----------------------------------------------------------------------
-- StreamsDriver wiring (engine + producer + consumer)
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

filterTopo :: IO TopologyValid
filterTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <- filterStream (\r -> recordValue r /= "skip") s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  case validateTopology topo of
    Left  err -> error (show err)
    Right v   -> pure v

streams_driver_round_trip :: TestTree
streams_driver_round_trip =
  testCase "MockStreamsDriver: pass-through topology delivers in -> out" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "alpha") (t 0)
    _ <- externalSend d (topicName "in") 0 Nothing (bytes "bravo") (t 1)
    runUntilQuiet d

    out <- dumpPartition cluster (topicName "out") 0
    map (unbytes . srValue) out @?= ["alpha", "bravo"]
    closeMockDriver d

streams_driver_with_filter_topology :: TestTree
streams_driver_with_filter_topology =
  testCase "MockStreamsDriver: filter topology drops 'skip' records" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    topo    <- filterTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    mapM_
      (\(v, ts) -> externalSend d (topicName "in") 0 Nothing (bytes v) ts)
      [("a", t 0), ("skip", t 1), ("b", t 2)]
    runUntilQuiet d

    out <- dumpPartition cluster (topicName "out") 0
    map (unbytes . srValue) out @?= ["a", "b"]
    closeMockDriver d

streams_driver_handles_fetch_fault_then_recovers :: TestTree
streams_driver_handles_fetch_fault_then_recovers =
  testCase "MockStreamsDriver: a queued fetch fault skips one tick, then recovers" $ do
    cluster <- newMockCluster 1
    fp      <- noFaults
    -- Inject one retriable fetch error; subsequent polls succeed.
    addFetchFault fp (topicName "in") 0 ErrCoordinatorLoadInProgress
    topo    <- passthroughTopo
    d       <- newMockStreamsDriver cluster fp topo "app" 1

    _ <- externalSend d (topicName "in") 0 Nothing (bytes "v1") (t 0)
    -- First tick: fetch fault fires; consumer sees the error and
    -- doesn't advance.
    _ <- tickDriver d
    out0 <- dumpPartition cluster (topicName "out") 0
    out0 @?= []

    -- Second tick: fault drained; record flows through.
    _ <- tickDriver d
    out1 <- dumpPartition cluster (topicName "out") 0
    map (unbytes . srValue) out1 @?= ["v1"]
    closeMockDriver d
