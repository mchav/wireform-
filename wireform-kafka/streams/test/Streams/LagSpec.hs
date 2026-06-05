{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Streams.LagSpec
-- Description : Tests for the lag report utility
module Streams.LagSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Metrics
  ( newMetricsRegistry
  , readGauge
  )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime (LagInfo (..))

import Kafka.Streams.Observability.Lag

tests :: TestTree
tests = testGroup "Lag"
  [ task_lag_clamps_negative
  , report_aggregates
  , report_sorted_by_task
  , classify_thresholds
  , record_lag_sets_gauges
  ]

sampleLags :: [LagInfo]
sampleLags =
  [ LagInfo (TaskId 1 0) 500 1500     -- behind 1000
  , LagInfo (TaskId 0 0) 980 1000     -- behind 20
  , LagInfo (TaskId 0 1) 1000 1000    -- caught up
  ]

task_lag_clamps_negative :: TestTree
task_lag_clamps_negative =
  testCase "taskLagOf clamps a current ahead of end to zero behind" $ do
    let tl = taskLagOf (LagInfo (TaskId 0 0) 1200 1000)
    taskLagBehind tl @?= 0

report_aggregates :: TestTree
report_aggregates =
  testCase "lagReport sums, maxes, and counts caught-up tasks" $ do
    let rep = lagReport sampleLags
    lagReportTaskCount rep   @?= 3
    lagReportCaughtUp rep    @?= 1
    lagReportTotalBehind rep @?= 1020
    lagReportMaxBehind rep   @?= 1000

report_sorted_by_task :: TestTree
report_sorted_by_task =
  testCase "lagReport sorts tasks by TaskId" $ do
    let rep = lagReport sampleLags
    map taskLagId (lagReportTasks rep)
      @?= [TaskId 0 0, TaskId 0 1, TaskId 1 0]

classify_thresholds :: TestTree
classify_thresholds =
  testCase "classifyLag respects the threshold" $ do
    let rep = lagReport sampleLags
    classifyLag 2000 rep @?= LagWithinThreshold
    classifyLag 500 rep  @?= LagExceeded
    classifyLag 0 (lagReport [LagInfo (TaskId 0 0) 10 10]) @?= LagCaughtUp

record_lag_sets_gauges :: TestTree
record_lag_sets_gauges =
  testCase "recordLagReport publishes per-task and aggregate gauges" $ do
    reg <- newMetricsRegistry
    let rep = lagReport sampleLags
    recordLagReport reg rep
    mMax   <- readGauge reg lagMaxGaugeName
    mTotal <- readGauge reg lagTotalGaugeName
    mTask  <- readGauge reg (lagGaugeName (TaskId 1 0))
    mMax   @?= Just 1000
    mTotal @?= Just 1020
    mTask  @?= Just 1000
