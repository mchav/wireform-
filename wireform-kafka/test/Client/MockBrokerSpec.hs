{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the in-process mock broker at the core
-- @wireform-kafka@ client layer. Mirrors librdkafka's
-- @rd_kafka_mock_cluster_t@ test ports.
module Client.MockBrokerSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
tests = describe "MockBroker" $ sequence_
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
  ]

----------------------------------------------------------------------
-- Cluster primitives
----------------------------------------------------------------------

cluster_basics :: Spec
cluster_basics = it "createTopic / listTopics / partitionCount" $ do
  c <- newMockCluster 3
  createTopic c "x" 4
  createTopic c "y" 1
  ls <- listTopics c
  Set.fromList ls `shouldBe` Set.fromList ["x", "y" :: Text]
  partitionCount c "x" >>= (`shouldBe` Just 4)
  partitionCount c "y" >>= (`shouldBe` Just 1)
  partitionCount c "z" >>= (`shouldBe` Nothing)

append_and_fetch :: Spec
append_and_fetch = it "appendToPartition assigns offsets; fetchSlice reads them back" $ do
  c <- newMockCluster 1
  createTopic c "log" 1
  Right o0 <- appendToPartition c "log" 0
                (Just (bytes "k")) (bytes "a") (ts 0) [] Nothing
  Right o1 <- appendToPartition c "log" 0
                (Just (bytes "k")) (bytes "b") (ts 1) [] Nothing
  o0 `shouldBe` 0
  o1 `shouldBe` 1
  Right (rs, next) <- fetchSlice c "log" 0 0 100 False
  map (unbytes . srValue) rs `shouldBe` ["a", "b"]
  next `shouldBe` 2

consumer_groups_remember_offsets :: Spec
consumer_groups_remember_offsets =
  it "commitGroupOffsets / groupOffsetsFor round-trip per (topic, partition)" $ do
    c <- newMockCluster 1
    createTopic c "log" 2
    let g = GroupId "consumers"
    commitGroupOffsets c g
      [ ("log", 0, 7)
      , ("log", 1, 12)
      ]
    m <- groupOffsetsFor c g
    Map.lookup ("log", 0) m `shouldBe` Just 7
    Map.lookup ("log", 1) m `shouldBe` Just 12
    Map.lookup ("log", 2) m `shouldBe` Nothing

----------------------------------------------------------------------
-- Producer
----------------------------------------------------------------------

producer_round_trip :: Spec
producer_round_trip =
  it "MockProducer.send appends to the cluster log" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p "out" 0 (Just (bytes "k")) (bytes "v") (ts 0)
    case r of
      MPSent 0 0 -> pure ()
      other      -> error ("unexpected " <> show other)
    sz <- partitionLogSize c "out" 0
    sz `shouldBe` 1

producer_retriable_then_succeeds :: Spec
producer_retriable_then_succeeds =
  it "queued retriable produce error fires once, then the next send succeeds" $ do
    c  <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    addProduceFault fp "out" 0 ErrLeaderNotAvailable
    p  <- newMockProducer c fp Nothing
    r1 <- sendMock p "out" 0 Nothing (bytes "v1") (ts 0)
    case r1 of
      MPFault e -> isRetriable e `shouldBe` True
      other     -> error ("expected fault, got " <> show other)
    r2 <- sendMock p "out" 0 Nothing (bytes "v2") (ts 1)
    case r2 of
      MPSent 0 0 -> pure ()
      other      -> error ("unexpected " <> show other)
    log_ <- dumpPartition c "out" 0
    map (unbytes . srValue) log_ `shouldBe` ["v2"]

producer_fatal_propagates :: Spec
producer_fatal_propagates =
  it "fatal produce error doesn't append" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    addProduceFault fp "out" 0 ErrAuthorizationFailed
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p "out" 0 Nothing (bytes "v") (ts 0)
    case r of
      MPFault e -> isFatal e `shouldBe` True
      other     -> error ("expected fault, got " <> show other)
    partitionLogSize c "out" 0 >>= (`shouldBe` 0)

----------------------------------------------------------------------
-- Consumer
----------------------------------------------------------------------

consumer_fetch_retriable_isolated_to_partition :: Spec
consumer_fetch_retriable_isolated_to_partition =
  it "fetch fault on one partition does not block siblings" $ do
    c <- newMockCluster 1
    createTopic c "in" 2
    let g = GroupId "g"
    _ <- appendToPartition c "in" 0 Nothing (bytes "p0") (ts 0) [] Nothing
    _ <- appendToPartition c "in" 1 Nothing (bytes "p1") (ts 0) [] Nothing
    fp <- noFaults
    addFetchFault fp "in" 0 ErrCoordinatorLoadInProgress
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["in"]
    PollResult{ prRecords = recs, prErrors = errs } <- pollMC cons
    map (\(_, p, _) -> p) errs `shouldBe` [0]
    map (\(_, p, sr) -> (p, unbytes (srValue sr))) recs
      `shouldBe` [(1, "p1")]

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

read_committed_filters_open_txn_records :: Spec
read_committed_filters_open_txn_records =
  it "read-committed consumers don't see records inside an open txn" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx1"))
    Right () <- beginTxnMP p
    _ <- sendMock p "out" 0 Nothing (bytes "in-txn") (ts 0)
    let g = GroupId "g"
    cu <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cu ["out"]
    PollResult uncommitted _ <- pollMC cu
    map (\(_, _, sr) -> unbytes (srValue sr)) uncommitted
      `shouldBe` ["in-txn"]
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc ["out"]
    PollResult committed _ <- pollMC cc
    committed `shouldBe` []

transaction_commit_advances_lso :: Spec
transaction_commit_advances_lso =
  it "commitTxn advances LSO so read-committed consumers catch up" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx2"))
    Right () <- beginTxnMP p
    _ <- sendMock p "out" 0 Nothing (bytes "v") (ts 0)
    Right () <- commitTxnMP p
    let g = GroupId "g"
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc ["out"]
    PollResult committed _ <- pollMC cc
    map (\(_, _, sr) -> unbytes (srValue sr)) committed `shouldBe` ["v"]
    txnState c (TxnId "tx2") >>= (`shouldBe` Just TxnCommitted)

transaction_abort_makes_records_invisible :: Spec
transaction_abort_makes_records_invisible =
  it "abortTxn keeps read-committed consumers from ever seeing the records" $ do
    c <- newMockCluster 1
    createTopic c "out" 1
    fp <- noFaults
    p  <- newMockProducer c fp (Just (TxnId "tx3"))
    Right () <- beginTxnMP p
    _ <- sendMock p "out" 0 Nothing (bytes "v") (ts 0)
    Right () <- abortTxnMP p
    let g = GroupId "g"
    cc <- newMockConsumer c fp g ReadCommitted 100
    subscribeMC cc ["out"]
    PollResult committed _ <- pollMC cc
    committed `shouldBe` []
    txnState c (TxnId "tx3") >>= (`shouldBe` Just TxnAborted)

----------------------------------------------------------------------
-- Brokers
----------------------------------------------------------------------

broker_marked_down_is_observable :: Spec
broker_marked_down_is_observable =
  it "markBrokerDown / isBrokerUp / downedBrokers form a consistent state" $ do
    c <- newMockCluster 3
    isBrokerUp c (BrokerId 0) >>= (`shouldBe` True)
    markBrokerDown c (BrokerId 0)
    isBrokerUp c (BrokerId 0) >>= (`shouldBe` False)
    downedBrokers c >>= (`shouldBe` [BrokerId 0])
    markBrokerUp c (BrokerId 0)
    isBrokerUp c (BrokerId 0) >>= (`shouldBe` True)
    downedBrokers c >>= (`shouldBe` [])
