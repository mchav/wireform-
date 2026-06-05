{-# LANGUAGE OverloadedStrings #-}

module Client.MetricsRegistrySpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Syd

import qualified Kafka.Telemetry.Metrics as M

tests :: Spec
tests = describe "Telemetry metrics registry" $ sequence_
  [ it "recordCount accumulates"
      counts_accumulate
  , it "recordValue overwrites"
      values_overwrite
  , it "recordHistogram tracks count + sum + min + max"
      histogram_tracking
  , it "tag-set is part of the key"
      tags_partition
  ]

counts_accumulate :: IO ()
counts_accumulate = do
  r <- M.newMetricsRegistry
  M.recordCount r "x" mempty 1
  M.recordCount r "x" mempty 2
  M.recordCount r "x" mempty 4
  s <- M.snapshotMetrics r
  Map.lookup ("x", []) (M.snapshotCounters s) `shouldBe` Just 7

values_overwrite :: IO ()
values_overwrite = do
  r <- M.newMetricsRegistry
  M.recordValue r "g" mempty 5
  M.recordValue r "g" mempty 3
  s <- M.snapshotMetrics r
  Map.lookup ("g", []) (M.snapshotCounters s) `shouldBe` Just 3

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
      M.hCount h `shouldBe` 3
      M.hSum h   `shouldBe` 9
      M.hMin h   `shouldBe` 1
      M.hMax h   `shouldBe` 5

tags_partition :: IO ()
tags_partition = do
  r <- M.newMetricsRegistry
  M.recordCount r "x" (Map.fromList [("topic", "a")]) 1
  M.recordCount r "x" (Map.fromList [("topic", "b")]) 1
  s <- M.snapshotMetrics r
  Map.lookup ("x", [("topic", "a")]) (M.snapshotCounters s) `shouldBe` Just 1
  Map.lookup ("x", [("topic", "b")]) (M.snapshotCounters s) `shouldBe` Just 1
