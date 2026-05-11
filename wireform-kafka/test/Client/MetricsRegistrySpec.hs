{-# LANGUAGE OverloadedStrings #-}

module Client.MetricsRegistrySpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Telemetry.Metrics as M

tests :: TestTree
tests = testGroup "Telemetry metrics registry"
  [ testCase "recordCount accumulates"
      counts_accumulate
  , testCase "recordValue overwrites"
      values_overwrite
  , testCase "recordHistogram tracks count + sum + min + max"
      histogram_tracking
  , testCase "tag-set is part of the key"
      tags_partition
  ]

counts_accumulate :: IO ()
counts_accumulate = do
  r <- M.newMetricsRegistry
  M.recordCount r "x" mempty 1
  M.recordCount r "x" mempty 2
  M.recordCount r "x" mempty 4
  s <- M.snapshotMetrics r
  Map.lookup ("x", []) (M.snapshotCounters s) @?= Just 7

values_overwrite :: IO ()
values_overwrite = do
  r <- M.newMetricsRegistry
  M.recordValue r "g" mempty 5
  M.recordValue r "g" mempty 3
  s <- M.snapshotMetrics r
  Map.lookup ("g", []) (M.snapshotCounters s) @?= Just 3

histogram_tracking :: IO ()
histogram_tracking = do
  r <- M.newMetricsRegistry
  M.recordHistogram r "h" mempty 1
  M.recordHistogram r "h" mempty 5
  M.recordHistogram r "h" mempty 3
  s <- M.snapshotMetrics r
  case Map.lookup ("h", []) (M.snapshotHistograms s) of
    Nothing -> error "no histogram"
    Just h -> do
      M.hCount h @?= 3
      M.hSum h   @?= 9
      M.hMin h   @?= 1
      M.hMax h   @?= 5

tags_partition :: IO ()
tags_partition = do
  r <- M.newMetricsRegistry
  M.recordCount r "x" (Map.fromList [("topic", "a")]) 1
  M.recordCount r "x" (Map.fromList [("topic", "b")]) 1
  s <- M.snapshotMetrics r
  Map.lookup ("x", [("topic", "a")]) (M.snapshotCounters s) @?= Just 1
  Map.lookup ("x", [("topic", "b")]) (M.snapshotCounters s) @?= Just 1
