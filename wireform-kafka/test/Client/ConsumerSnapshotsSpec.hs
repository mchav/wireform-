{-# LANGUAGE OverloadedStrings #-}

module Client.ConsumerSnapshotsSpec (tests) where

import Test.Syd

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Consumer as CE

tests :: Spec
tests = describe "Consumer snapshots + helpers" $ sequence_
  [ it "effectiveConsumerSnapshot reflects defaults"
      snapshot
  , it "withReadOnly clears auto-commit"
      readOnly
  , it "planRewind: Earliest -> low watermark"
      rewind_earliest
  , it "planRewind: Latest -> high watermark"
      rewind_latest
  , it "planRewind: Fail -> Nothing"
      rewind_fail
  , it "recordRebalanceTrigger surfaces canonical strings"
      trigger_strings
  , it "shutdownReasonText covers every constructor"
      shutdown_strings
  , it "assignorHintText canonical names"
      assignor_strings
  ]

snapshot :: IO ()
snapshot = do
  let s = CE.effectiveConsumerSnapshot KC.defaultConsumerConfig
  CE.ecsAutoCommit s        `shouldBe` True
  CE.ecsSessionTimeoutMs s  `shouldBe` 45_000
  CE.ecsMaxPollRecords s    `shouldBe` 500

readOnly :: IO ()
readOnly = do
  let c = CE.withReadOnly KC.defaultConsumerConfig
  KC.consumerAutoCommit c `shouldBe` False
  CE.isReadOnlyMode c    `shouldBe` True

rewind_earliest :: IO ()
rewind_earliest = CE.planRewind CE.RewindToEarliest 5 100 `shouldBe` Just 5

rewind_latest :: IO ()
rewind_latest = CE.planRewind CE.RewindToLatest 5 100 `shouldBe` Just 100

rewind_fail :: IO ()
rewind_fail = CE.planRewind CE.RewindFail 5 100 `shouldBe` Nothing

trigger_strings :: IO ()
trigger_strings = do
  CE.recordRebalanceTrigger CE.TriggerSubscriptionChange `shouldBe` "subscription-changed"
  CE.recordRebalanceTrigger CE.TriggerExplicitEnforce    `shouldBe` "enforce-rebalance-called"

shutdown_strings :: IO ()
shutdown_strings = do
  CE.shutdownReasonText CE.ShutdownExplicit `shouldBe` "explicit-close"
  CE.shutdownReasonText CE.ShutdownLost     `shouldBe` "consumer-lost-partitions"
  CE.shutdownReasonText CE.ShutdownFenced   `shouldBe` "consumer-fenced-by-coordinator"

assignor_strings :: IO ()
assignor_strings = do
  CE.assignorHintText CE.HintRangeAssignor             `shouldBe` "range"
  CE.assignorHintText CE.HintCooperativeStickyAssignor `shouldBe` "cooperative-sticky"
  CE.assignorHintText (CE.HintCustomAssignor "my")     `shouldBe` "my"
