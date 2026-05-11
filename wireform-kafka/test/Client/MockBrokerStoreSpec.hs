{-# LANGUAGE OverloadedStrings #-}

-- | Manual offset store + immediate flush + barrier-batch tests.
-- Mirrors librdkafka 0125 (immediate flush), 0130 (store offsets),
-- 0137 (barrier batch).
module Client.MockBrokerStoreSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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

tests :: TestTree
tests = testGroup "MockBrokerStore"
  [ store_offset_does_not_commit
  , commit_stored_drains_local_store
  , store_then_commit_then_offset_visible
  , commit_stored_with_fault_keeps_local
  , flush_sync_returns_right
  , send_batch_round_trip
  , send_batch_assigns_increasing_offsets
  , send_batch_partition_routing_per_record
  ]

----------------------------------------------------------------------
-- Manual offset store
----------------------------------------------------------------------

store_offset_does_not_commit :: TestTree
store_offset_does_not_commit =
  testCase "storeOffsetMC writes locally without touching the cluster" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    storeOffsetMC cons "t" 0 42
    -- Local store has it.
    sm <- storedOffsets cons
    Map.lookup ("t", 0) sm @?= Just 42
    -- Cluster offset store does NOT.
    gm <- groupOffsetsFor c g
    Map.lookup ("t", 0) gm @?= Nothing

commit_stored_drains_local_store :: TestTree
commit_stored_drains_local_store =
  testCase "commitStoredOffsetsMC commits + clears the local store" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    storeOffsetMC cons "t" 0 5
    storeOffsetMC cons "t" 1 9
    Right () <- commitStoredOffsetsMC cons
    -- Cluster sees both.
    gm <- groupOffsetsFor c g
    Map.lookup ("t", 0) gm @?= Just 5
    Map.lookup ("t", 1) gm @?= Just 9
    -- Local store is empty.
    sm <- storedOffsets cons
    Map.size sm @?= 0

store_then_commit_then_offset_visible :: TestTree
store_then_commit_then_offset_visible =
  testCase "store + commit reflects in the cluster's group offset store" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    let g = GroupId "shared"
    fp <- noFaults
    a <- newMockConsumerWithId c fp g (MemberId "a") ReadUncommitted 100
    subscribeMC a ["t"]
    storeOffsetMC a "t" 0 17
    storeOffsetMC a "t" 1 23
    Right () <- commitStoredOffsetsMC a
    -- The cluster's group offset store has both entries.
    gm <- groupOffsetsFor c g
    Map.lookup ("t", 0) gm @?= Just 17
    Map.lookup ("t", 1) gm @?= Just 23
    -- A second consumer joining the same group sees the offset
    -- for whichever partition the assignor gave it. With members
    -- sorted [a, b] and 2 partitions, b gets partition 1.
    b <- newMockConsumerWithId c fp g (MemberId "b") ReadUncommitted 100
    subscribeMC b ["t"]
    refreshAssignment a   -- a refreshes too, may revoke partition 1
    pos <- currentPosition b "t" 1
    pos @?= Just 23

commit_stored_with_fault_keeps_local :: TestTree
commit_stored_with_fault_keeps_local =
  testCase "commit fault leaves the local store intact for retry" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    let g = GroupId "g"
    fp <- noFaults
    addCommitFault fp g ErrCoordinatorNotAvailable
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    storeOffsetMC cons "t" 0 99
    r <- commitStoredOffsetsMC cons
    case r of
      Left e -> isRetriable e @?= True
      Right _ -> error "expected Left"
    -- Local store is unchanged so the next attempt re-tries the same offsets.
    sm <- storedOffsets cons
    Map.lookup ("t", 0) sm @?= Just 99
    -- Second commit (no fault) succeeds and clears the store.
    Right () <- commitStoredOffsetsMC cons
    sm2 <- storedOffsets cons
    Map.size sm2 @?= 0

----------------------------------------------------------------------
-- Immediate flush
----------------------------------------------------------------------

flush_sync_returns_right :: TestTree
flush_sync_returns_right =
  testCase "flushMockSync returns Right immediately (synchronous in-memory mock)" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p "t" 0 Nothing (bytes "v") (ts 0)
    r  <- flushMockSync p 5000
    r @?= Right ()
    -- 'producerPendingCount' is always zero since sends are sync.
    n <- producerPendingCount p
    n @?= 0

----------------------------------------------------------------------
-- Send batch (barrier batch)
----------------------------------------------------------------------

send_batch_round_trip :: TestTree
send_batch_round_trip =
  testCase "sendBatchMock produces one result per record, in input order" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    rs <- sendBatchMock p
      [ ("t", 0, Just (bytes "k1"), bytes "v1", ts 0)
      , ("t", 0, Just (bytes "k2"), bytes "v2", ts 1)
      , ("t", 0, Just (bytes "k3"), bytes "v3", ts 2)
      ]
    case rs of
      [MPSent 0 0, MPSent 0 1, MPSent 0 2] -> pure ()
      other -> error ("unexpected " <> show other)

send_batch_assigns_increasing_offsets :: TestTree
send_batch_assigns_increasing_offsets =
  testCase "every record in the batch lands at a contiguous offset" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    let !batch = [ ("t", 0, Nothing, bytes ("v" <> T.pack (show i)), ts 0)
                 | i <- [0 .. 9 :: Int] ]
    rs <- sendBatchMock p batch
    let !offsets = [ off | MPSent _ off <- rs ]
    offsets @?= [0 .. 9]

send_batch_partition_routing_per_record :: TestTree
send_batch_partition_routing_per_record =
  testCase "sendBatchMock honours each record's partition argument" $ do
    c <- newMockCluster 1
    createTopic c "t" 3
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    rs <- sendBatchMock p
      [ ("t", 0, Nothing, bytes "p0a", ts 0)
      , ("t", 1, Nothing, bytes "p1a", ts 0)
      , ("t", 2, Nothing, bytes "p2a", ts 0)
      , ("t", 0, Nothing, bytes "p0b", ts 1)
      ]
    case rs of
      [MPSent 0 0, MPSent 1 0, MPSent 2 0, MPSent 0 1] -> pure ()
      other -> error ("unexpected " <> show other)
    -- Per-partition logs reflect the routing.
    p0 <- map (unbytes . srValue) <$> dumpPartition c "t" 0
    p1 <- map (unbytes . srValue) <$> dumpPartition c "t" 1
    p2 <- map (unbytes . srValue) <$> dumpPartition c "t" 2
    p0 @?= ["p0a", "p0b"]
    p1 @?= ["p1a"]
    p2 @?= ["p2a"]
