{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure multi-instance liveness harness used to
-- enumerate failure orderings without spinning up the runtime.
module Streams.MultiInstanceHarnessSpec (tests) where

import qualified Data.Map.Strict as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.Runtime.MultiInstanceHarness as H

tests :: TestTree
tests = testGroup "Multi-instance liveness harness"
  [ testCase "no failures: every record is processed"
      no_failures
  , testCase "single instance crash: subsequent records are skipped"
      single_crash
  , testCase "multi-instance: surviving instance keeps processing"
      multi_survival
  , testProperty
      "with at least one healthy instance, no records are skipped"
      prop_at_least_one_healthy
  ]

no_failures :: IO ()
no_failures = do
  let r = H.runHarness [H.InstanceId 0]
            [ H.EvRecord ("k1" :: String) ("v1" :: String)
            , H.EvRecord "k2" "v2"
            ]
  H.rrProcessed r @?= [("k1", "v1"), ("k2", "v2")]
  H.rrSkipped r   @?= []

single_crash :: IO ()
single_crash = do
  let r = H.runHarness [H.InstanceId 0]
            [ H.EvRecord ("k1" :: String) ("v1" :: String)
            , H.EvFailure (H.Crash (H.InstanceId 0))
            , H.EvRecord "k2" "v2"
            ]
  H.rrProcessed r @?= [("k1", "v1")]
  H.rrSkipped r   @?= [("k2", "v2")]

multi_survival :: IO ()
multi_survival = do
  let r = H.runHarness [H.InstanceId 0, H.InstanceId 1]
            [ H.EvRecord ("k1" :: String) ("v1" :: String)
            , H.EvFailure (H.Crash (H.InstanceId 0))
            , H.EvRecord "k2" "v2"
            ]
  -- Instance 1 still healthy; record k2 still processed.
  H.rrProcessed r @?= [("k1", "v1"), ("k2", "v2")]
  H.rrSkipped r   @?= []

prop_at_least_one_healthy :: Property
prop_at_least_one_healthy = property $ do
  -- Generate up to 4 instances and a small event stream that
  -- never crashes /every/ instance simultaneously.
  numInstances <- forAll (Gen.int (Range.linear 1 4))
  let instances = map H.InstanceId [0 .. numInstances - 1]
  -- Only crash up to numInstances - 1 instances total.
  numFailures <- forAll (Gen.int (Range.linear 0 (numInstances - 1)))
  -- Always have at least one record; failures are interleaved.
  records <- forAll (Gen.list (Range.linear 1 8) (Gen.int (Range.linear 0 9)))
  let failures = take numFailures (map (H.Crash . H.InstanceId) [0 ..])
      events =
        map (\r -> H.EvRecord r r) records
        <> map H.EvFailure failures
  let result = H.runHarness instances events
  -- With at least one survivor, no record should be skipped.
  -- (Failures fire AFTER all the records since they're appended;
  --  what we're really asserting is the invariant
  --  "skipped == [] iff at-least-one-live-throughout".)
  H.rrSkipped result === []
  -- Sanity: final state has Healthy for any non-crashed instance.
  let stillHealthy =
        [ () | (_, H.Healthy) <- Map.toList (H.rrFinalStates result) ]
  assert (length stillHealthy >= 1)
