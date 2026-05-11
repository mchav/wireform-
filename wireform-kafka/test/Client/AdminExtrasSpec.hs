{-# LANGUAGE OverloadedStrings #-}

module Client.AdminExtrasSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.AdminExtras as AE

tests :: TestTree
tests = testGroup "AdminExtras (KIP-464 / 484 / 524 / 967 / 1107 / 1153 / 1170)"
  [ testCase "defaultAdminApiTimeoutMs matches the JVM client (60s)"
      timeout_default
  , testCase "defaultTopicCreateDefaults: 1 partition, 1 replica, no overrides"
      topic_defaults
  , testCase "defaultNullKeyCompactionPolicy = Reject"
      null_key_default
  , testCase "metric names follow the kafka.admin.* convention"
      metric_names
  ]

timeout_default :: IO ()
timeout_default = AE.defaultAdminApiTimeoutMs @?= 60_000

topic_defaults :: IO ()
topic_defaults = do
  AE.tcdReplicationFactor AE.defaultTopicCreateDefaults @?= 1
  AE.tcdNumPartitions     AE.defaultTopicCreateDefaults @?= 1
  AE.tcdConfigOverrides   AE.defaultTopicCreateDefaults @?= Map.empty

null_key_default :: IO ()
null_key_default = AE.defaultNullKeyCompactionPolicy @?= AE.NkcReject

metric_names :: IO ()
metric_names = do
  AE.adminListTopicsLatencyMs     @?= "kafka.admin.list-topics.latency.ms"
  AE.adminCreateTopicsLatencyMs   @?= "kafka.admin.create-topics.latency.ms"
  AE.adminDescribeGroupsLatencyMs @?= "kafka.admin.describe-groups.latency.ms"
  AE.adminAlterConfigsLatencyMs   @?= "kafka.admin.alter-configs.latency.ms"
  AE.adminDeleteRecordsLatencyMs  @?= "kafka.admin.delete-records.latency.ms"
