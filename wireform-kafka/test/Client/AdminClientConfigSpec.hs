{-# LANGUAGE OverloadedStrings #-}

module Client.AdminClientConfigSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Syd

import qualified Kafka.Client.AdminClient as AE

tests :: Spec
tests = describe "AdminClient configuration helpers" $ sequence_
  [ it "defaultAdminApiTimeoutMs matches the JVM client (60s)"
      timeout_default
  , it "defaultTopicCreateDefaults: 1 partition, 1 replica, no overrides"
      topic_defaults
  , it "defaultNullKeyCompactionPolicy = Reject"
      null_key_default
  , it "metric names follow the kafka.admin.* convention"
      metric_names
  ]

timeout_default :: IO ()
timeout_default = AE.defaultAdminApiTimeoutMs `shouldBe` 60_000

topic_defaults :: IO ()
topic_defaults = do
  AE.tcdReplicationFactor AE.defaultTopicCreateDefaults `shouldBe` 1
  AE.tcdNumPartitions     AE.defaultTopicCreateDefaults `shouldBe` 1
  AE.tcdConfigOverrides   AE.defaultTopicCreateDefaults `shouldBe` Map.empty

null_key_default :: IO ()
null_key_default = AE.defaultNullKeyCompactionPolicy `shouldBe` AE.NkcReject

metric_names :: IO ()
metric_names = do
  AE.adminListTopicsLatencyMs     `shouldBe` "kafka.admin.list-topics.latency.ms"
  AE.adminCreateTopicsLatencyMs   `shouldBe` "kafka.admin.create-topics.latency.ms"
  AE.adminDescribeGroupsLatencyMs `shouldBe` "kafka.admin.describe-groups.latency.ms"
  AE.adminAlterConfigsLatencyMs   `shouldBe` "kafka.admin.alter-configs.latency.ms"
  AE.adminDeleteRecordsLatencyMs  `shouldBe` "kafka.admin.delete-records.latency.ms"
