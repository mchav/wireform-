{-# LANGUAGE OverloadedStrings #-}

module Client.ConsumerSnapshotsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Consumer as CE

tests :: TestTree
tests = testGroup "Consumer snapshots + helpers"
  [ testCase "effectiveConsumerSnapshot reflects defaults"
      snapshot
  , testCase "withReadOnly clears auto-commit"
      readOnly
  , testCase "planRewind: Earliest -> low watermark"
      rewind_earliest
  , testCase "planRewind: Latest -> high watermark"
      rewind_latest
  , testCase "planRewind: Fail -> Nothing"
      rewind_fail
  , testCase "recordRebalanceTrigger surfaces canonical strings"
      trigger_strings
  , testCase "shutdownReasonText covers every constructor"
      shutdown_strings
  , testCase "assignorHintText canonical names"
      assignor_strings
  ]

snapshot :: IO ()
snapshot = do
  let s = CE.effectiveConsumerSnapshot KC.defaultConsumerConfig
  CE.ecsAutoCommit s        @?= True
  CE.ecsSessionTimeoutMs s  @?= 45_000
  CE.ecsMaxPollRecords s    @?= 500

readOnly :: IO ()
readOnly = do
  let c = CE.withReadOnly KC.defaultConsumerConfig
  KC.consumerAutoCommit c @?= False
  CE.isReadOnlyMode c    @?= True

rewind_earliest :: IO ()
rewind_earliest = CE.planRewind CE.RewindToEarliest 5 100 @?= Just 5

rewind_latest :: IO ()
rewind_latest = CE.planRewind CE.RewindToLatest 5 100 @?= Just 100

rewind_fail :: IO ()
rewind_fail = CE.planRewind CE.RewindFail 5 100 @?= Nothing

trigger_strings :: IO ()
trigger_strings = do
  CE.recordRebalanceTrigger CE.TriggerSubscriptionChange @?= "subscription-changed"
  CE.recordRebalanceTrigger CE.TriggerExplicitEnforce    @?= "enforce-rebalance-called"

shutdown_strings :: IO ()
shutdown_strings = do
  CE.shutdownReasonText CE.ShutdownExplicit @?= "explicit-close"
  CE.shutdownReasonText CE.ShutdownLost     @?= "consumer-lost-partitions"
  CE.shutdownReasonText CE.ShutdownFenced   @?= "consumer-fenced-by-coordinator"

assignor_strings :: IO ()
assignor_strings = do
  CE.assignorHintText CE.HintRangeAssignor             @?= "range"
  CE.assignorHintText CE.HintCooperativeStickyAssignor @?= "cooperative-sticky"
  CE.assignorHintText (CE.HintCustomAssignor "my")     @?= "my"
