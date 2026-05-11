{-# LANGUAGE OverloadedStrings #-}

-- | Round 1 of librdkafka mock-test ports:
-- auto-create topics (0109), null/empty key/value (0070),
-- multi-header preservation (0085), pause/resume (0145),
-- commit metadata (0140), delete topic.
module Client.MockBrokerExtSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

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
tests = testGroup "MockBrokerExt"
  [ -- Auto-create
    auto_create_disabled_returns_no_partition
  , auto_create_creates_topic_on_first_send
  , auto_create_uses_configured_partition_count
    -- null vs empty
  , null_vs_empty_key_distinct
  , empty_value_round_trips
    -- multi-header
  , multi_header_preserves_order
  , multi_header_duplicate_keys_kept
    -- pause / resume
  , pause_skips_partition
  , resume_restores_partition
  , pause_one_partition_does_not_block_siblings
  , pausedPartitions_lists_paused_set
    -- commit metadata
  , commit_metadata_round_trips
  , commit_metadata_default_is_empty
  , commit_metadata_carries_leader_epoch
    -- delete topic
  , delete_topic_clears_partitions
  , delete_topic_unknown_returns_false
  ]

----------------------------------------------------------------------
-- Auto-create
----------------------------------------------------------------------

auto_create_disabled_returns_no_partition :: TestTree
auto_create_disabled_returns_no_partition =
  testCase "with auto-create disabled, sending to a missing topic returns Left" $ do
    c <- newMockCluster 1
    fp <- noFaults
    p <- newMockProducer c fp Nothing
    r <- sendMock p "missing" 0 Nothing (bytes "v") (ts 0)
    case r of
      MPNoSuchPartition _ -> pure ()
      other               -> error ("expected MPNoSuchPartition, got " <> show other)

auto_create_creates_topic_on_first_send :: TestTree
auto_create_creates_topic_on_first_send =
  testCase "setAutoCreateTopics (Just 1) creates the topic on first send" $ do
    c <- newMockCluster 1
    setAutoCreateTopics c (Just 1)
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    r  <- sendMock p "auto-topic" 0 Nothing (bytes "v") (ts 0)
    case r of
      MPSent 0 0 -> pure ()
      other      -> error ("expected MPSent, got " <> show other)
    partitionCount c "auto-topic" >>= (@?= Just 1)

auto_create_uses_configured_partition_count :: TestTree
auto_create_uses_configured_partition_count =
  testCase "auto-created topic gets the configured partition count" $ do
    c <- newMockCluster 1
    setAutoCreateTopics c (Just 4)
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p "spread" 3 Nothing (bytes "v") (ts 0)
    partitionCount c "spread" >>= (@?= Just 4)

----------------------------------------------------------------------
-- null vs empty
----------------------------------------------------------------------

null_vs_empty_key_distinct :: TestTree
null_vs_empty_key_distinct =
  testCase "null key vs empty-bytes key are stored distinctly" $ do
    c  <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p "t" 0 Nothing               (bytes "v1") (ts 0)
    _  <- sendMock p "t" 0 (Just BS.empty)       (bytes "v2") (ts 1)
    log_ <- dumpPartition c "t" 0
    map srKey log_ @?= [Nothing, Just BS.empty]

empty_value_round_trips :: TestTree
empty_value_round_trips =
  testCase "empty value bytes round-trip; not the same as null" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    _  <- sendMock p "t" 0 Nothing BS.empty (ts 0)
    log_ <- dumpPartition c "t" 0
    map srValue log_ @?= [BS.empty]

----------------------------------------------------------------------
-- Multi-header
----------------------------------------------------------------------

multi_header_preserves_order :: TestTree
multi_header_preserves_order =
  testCase "headers are stored in submission order" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    let hdrs =
          [ ("z", bytes "1")
          , ("a", bytes "2")
          , ("m", bytes "3")
          , ("a", bytes "4")
          ]
    _  <- sendMockH p "t" 0 Nothing (bytes "v") (ts 0) hdrs
    [sr] <- dumpPartition c "t" 0
    srHeaders sr @?= hdrs

multi_header_duplicate_keys_kept :: TestTree
multi_header_duplicate_keys_kept =
  testCase "duplicate header keys are NOT collapsed" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    let hdrs = [("k", bytes "v1"), ("k", bytes "v2"), ("k", bytes "v3")]
    _  <- sendMockH p "t" 0 Nothing (bytes "v") (ts 0) hdrs
    [sr] <- dumpPartition c "t" 0
    srHeaders sr @?= hdrs

----------------------------------------------------------------------
-- pause / resume
----------------------------------------------------------------------

pause_skips_partition :: TestTree
pause_skips_partition =
  testCase "pausePartitions: pollMC skips paused partitions entirely" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    _ <- appendToPartition c "t" 0 Nothing (bytes "p0") (ts 0) [] Nothing
    _ <- appendToPartition c "t" 1 Nothing (bytes "p1") (ts 0) [] Nothing
    fp <- noFaults
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["t"]
    pausePartitions cons [("t", 0)]
    PollResult rs _ <- pollMC cons
    map (\(_, p, _) -> p) rs @?= [1]

resume_restores_partition :: TestTree
resume_restores_partition =
  testCase "resumePartitions reactivates the partition" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    _ <- appendToPartition c "t" 0 Nothing (bytes "v") (ts 0) [] Nothing
    fp <- noFaults
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["t"]
    pausePartitions cons [("t", 0)]
    PollResult rs0 _ <- pollMC cons
    rs0 @?= []
    resumePartitions cons [("t", 0)]
    PollResult rs1 _ <- pollMC cons
    map (\(_, _, sr) -> unbytes (srValue sr)) rs1 @?= ["v"]

pause_one_partition_does_not_block_siblings :: TestTree
pause_one_partition_does_not_block_siblings =
  testCase "pausing one partition doesn't affect siblings" $ do
    c <- newMockCluster 1
    createTopic c "t" 3
    _ <- appendToPartition c "t" 0 Nothing (bytes "a") (ts 0) [] Nothing
    _ <- appendToPartition c "t" 1 Nothing (bytes "b") (ts 0) [] Nothing
    _ <- appendToPartition c "t" 2 Nothing (bytes "c") (ts 0) [] Nothing
    fp <- noFaults
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["t"]
    pausePartitions cons [("t", 1)]
    PollResult rs _ <- pollMC cons
    map (\(_, p, _) -> p) rs @?= [0, 2]

pausedPartitions_lists_paused_set :: TestTree
pausedPartitions_lists_paused_set =
  testCase "pausedPartitions returns the current paused set" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    fp <- noFaults
    cons <- newMockConsumer c fp (GroupId "g") ReadUncommitted 100
    subscribeMC cons ["t"]
    pausePartitions cons [("t", 1), ("t", 3)]
    ps <- pausedPartitions cons
    ps @?= [("t", 1), ("t", 3)]
    resumePartitions cons [("t", 1)]
    pausedPartitions cons >>= (@?= [("t", 3)])

----------------------------------------------------------------------
-- Commit metadata
----------------------------------------------------------------------

commit_metadata_round_trips :: TestTree
commit_metadata_round_trips =
  testCase "OffsetAndMetadata: commit + read back the metadata bytes" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    Right () <- commitOffsetsWithMetadataMC cons
      [(("t", 0), OffsetAndMetadata 42 (Just (bytes "host=worker-7")) Nothing)]
    m <- groupOffsetsWithMetadataFor c g
    case Map.lookup ("t", 0) m of
      Just oam -> do
        oamOffset oam   @?= 42
        oamMetadata oam @?= Just (bytes "host=worker-7")
      Nothing -> error "missing committed entry"

commit_metadata_default_is_empty :: TestTree
commit_metadata_default_is_empty =
  testCase "commitOffsetsMC stores Nothing metadata + Nothing leader epoch" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    Right () <- commitOffsetsMC cons [("t", 0, 9)]
    m <- groupOffsetsWithMetadataFor c g
    case Map.lookup ("t", 0) m of
      Just oam -> do
        oamOffset oam      @?= 9
        oamMetadata oam    @?= Nothing
        oamLeaderEpoch oam @?= Nothing
      Nothing -> error "missing committed entry"

commit_metadata_carries_leader_epoch :: TestTree
commit_metadata_carries_leader_epoch =
  testCase "OffsetAndMetadata includes the leader epoch when set" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    let g = GroupId "g"
    fp <- noFaults
    cons <- newMockConsumer c fp g ReadUncommitted 100
    subscribeMC cons ["t"]
    Right () <- commitOffsetsWithMetadataMC cons
      [(("t", 0), OffsetAndMetadata 5 Nothing (Just 12))]
    m <- groupOffsetsWithMetadataFor c g
    case Map.lookup ("t", 0) m of
      Just oam -> oamLeaderEpoch oam @?= Just 12
      Nothing  -> error "missing"

----------------------------------------------------------------------
-- Delete topic
----------------------------------------------------------------------

delete_topic_clears_partitions :: TestTree
delete_topic_clears_partitions =
  testCase "deleteTopic clears the topic and its partitions" $ do
    c <- newMockCluster 1
    createTopic c "doomed" 3
    partitionCount c "doomed" >>= (@?= Just 3)
    ok <- deleteTopic c "doomed"
    ok @?= True
    partitionCount c "doomed" >>= (@?= Nothing)

delete_topic_unknown_returns_false :: TestTree
delete_topic_unknown_returns_false =
  testCase "deleteTopic on a non-existent topic returns False" $ do
    c <- newMockCluster 1
    ok <- deleteTopic c "ghost"
    ok @?= False
