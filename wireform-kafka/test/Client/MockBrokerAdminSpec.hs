{-# LANGUAGE OverloadedStrings #-}

{- | Admin-client mock test port — librdkafka 0138_admin_mock.c
equivalents.
-}
module Client.MockBrokerAdminSpec (tests) where

import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Mock.Admin
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Fault
import Test.Syd


tests :: Spec
tests =
  describe "MockBrokerAdmin" $
    sequence_
      [ create_topics_basic
      , create_topics_idempotent_at_same_partition_count
      , create_topics_rejects_replication_above_broker_count
      , delete_topics_known_and_unknown_mixed
      , describe_topic_returns_partition_info
      , describe_topic_unknown_returns_error
      , list_consumer_groups_includes_active
      , describe_cluster_returns_broker_count_and_topics
      ]


create_topics_basic :: Spec
create_topics_basic =
  it "createTopicsAdmin creates new topics" $ do
    c <- newMockCluster 3
    rs <-
      createTopicsAdmin
        c
        [ defaultNewTopic "alpha" 4
        , defaultNewTopic "beta" 1
        ]
    rs `shouldBe` [("alpha", Right ()), ("beta", Right ())]
    partitionCount c "alpha" >>= (`shouldBe` Just 4)
    partitionCount c "beta" >>= (`shouldBe` Just 1)


create_topics_idempotent_at_same_partition_count :: Spec
create_topics_idempotent_at_same_partition_count =
  it "createTopicsAdmin is idempotent for same-partition-count topics" $ do
    c <- newMockCluster 1
    _ <- createTopicsAdmin c [defaultNewTopic "t" 2]
    rs <- createTopicsAdmin c [defaultNewTopic "t" 2]
    rs `shouldBe` [("t", Right ())]
    partitionCount c "t" >>= (`shouldBe` Just 2)


create_topics_rejects_replication_above_broker_count :: Spec
create_topics_rejects_replication_above_broker_count =
  it "asking for replication > broker count returns INVALID_REPLICATION_FACTOR" $ do
    c <- newMockCluster 2
    rs <-
      createTopicsAdmin
        c
        [(defaultNewTopic "rf" 1) {ntReplication = 5}]
    case rs of
      [("rf", Left e)] -> case e of
        ErrCustom msg ->
          (if ("INVALID_REPLICATION_FACTOR" `T.isPrefixOf` msg) then pure () else expectationFailure ("got: " <> show msg))
        _ -> error ("unexpected error: " <> show e)
      _ -> error ("unexpected results: " <> show rs)
    partitionCount c "rf" >>= (`shouldBe` Nothing)


delete_topics_known_and_unknown_mixed :: Spec
delete_topics_known_and_unknown_mixed =
  it "deleteTopicsAdmin: known -> Right (), unknown -> Left UNKNOWN_TOPIC_OR_PARTITION" $ do
    c <- newMockCluster 1
    _ <- createTopicsAdmin c [defaultNewTopic "t1" 1, defaultNewTopic "t2" 1]
    rs <- deleteTopicsAdmin c ["t1", "ghost", "t2"]
    rs `shouldBe` [("t1", Right ()), ("ghost", Left ErrUnknownTopicOrPartition), ("t2", Right ())]
    listTopics c >>= (`shouldBe` [])


describe_topic_returns_partition_info :: Spec
describe_topic_returns_partition_info =
  it "describeTopicAdmin returns one PartitionInfo per partition" $ do
    c <- newMockCluster 1
    _ <- createTopicsAdmin c [defaultNewTopic "t" 3]
    Right td <- describeTopicAdmin c "t"
    tdName td `shouldBe` "t"
    map piPartitionId (tdPartitions td) `shouldBe` [0, 1, 2]
    -- All partitions start with HWM=0, LSO=0.
    map piHwm (tdPartitions td) `shouldBe` [0, 0, 0]
    map piLso (tdPartitions td) `shouldBe` [0, 0, 0]


describe_topic_unknown_returns_error :: Spec
describe_topic_unknown_returns_error =
  it "describeTopicAdmin on unknown topic returns Left UnknownTopicOrPartition" $ do
    c <- newMockCluster 1
    r <- describeTopicAdmin c "ghost"
    case r of
      Left ErrUnknownTopicOrPartition -> pure ()
      other -> error ("unexpected " <> show other)


list_consumer_groups_includes_active :: Spec
list_consumer_groups_includes_active =
  it "listConsumerGroupsAdmin surfaces every group with members or commits" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    -- A group with offset commits but no active members.
    commitGroupOffsets c (GroupId "old") [("t", 0, 5)]
    -- A group with members but no commits.
    joinGroup c (GroupId "live") (MemberId "m1") ["t"]
    Right gs <- listConsumerGroupsAdmin c
    Set.fromList gs `shouldBe` Set.fromList ["old", "live"]


describe_cluster_returns_broker_count_and_topics :: Spec
describe_cluster_returns_broker_count_and_topics =
  it "describeClusterAdmin: broker count + topic list + cluster id" $ do
    c <- newMockCluster 4
    _ <- createTopicsAdmin c [defaultNewTopic "x" 1, defaultNewTopic "y" 2]
    Right cd <- describeClusterAdmin c
    cdBrokerCount cd `shouldBe` 4
    Set.fromList (cdTopics cd) `shouldBe` Set.fromList ["x", "y"]
    cdClusterId cd `shouldBe` "mock-cluster"
