{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Streams.HealthSpec
-- Description : Tests for the health/readiness report
module Streams.HealthSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime
  ( LocalThreadMetadata (..)
  , StreamsStatus (..)
  )

import Kafka.Streams.Observability.Health
import Kafka.Streams.Observability.Lag (lagReport)
import Kafka.Streams.Runtime (LagInfo (..))

tests :: TestTree
tests = testGroup "Health"
  [ running_within_budget_is_healthy
  , running_over_budget_is_degraded
  , error_state_is_unhealthy
  , created_state_not_ready
  , aggregates_thread_metadata
  ]

threads :: [LocalThreadMetadata]
threads =
  [ LocalThreadMetadata { threadId = 0, assigned = [], processedRecs = 100 }
  , LocalThreadMetadata { threadId = 1, assigned = [], processedRecs = 250 }
  ]

running_within_budget_is_healthy :: TestTree
running_within_budget_is_healthy =
  testCase "running with lag within budget is healthy + ready" $ do
    let lag = lagReport [LagInfo (TaskId 0 0) 990 1000]  -- behind 10
        rep = healthReportFrom defaultHealthConfig StreamsRunning threads (Just lag)
    healthStatus rep @?= Healthy
    healthReady rep  @?= True

running_over_budget_is_degraded :: TestTree
running_over_budget_is_degraded =
  testCase "running with lag over budget is degraded + not ready" $ do
    let lag = lagReport [LagInfo (TaskId 0 0) 0 1000000]  -- behind 1e6
        rep = healthReportFrom defaultHealthConfig StreamsRunning threads (Just lag)
    healthStatus rep @?= Degraded
    healthReady rep  @?= False

error_state_is_unhealthy :: TestTree
error_state_is_unhealthy =
  testCase "error state is unhealthy + not ready" $ do
    let rep = healthReportFrom defaultHealthConfig (StreamsError "boom") threads Nothing
    healthStatus rep @?= Unhealthy
    healthReady rep  @?= False

created_state_not_ready :: TestTree
created_state_not_ready =
  testCase "created-but-not-started is degraded + not ready" $ do
    let rep = healthReportFrom defaultHealthConfig StreamsCreated threads Nothing
    healthStatus rep @?= Degraded
    healthReady rep  @?= False

aggregates_thread_metadata :: TestTree
aggregates_thread_metadata =
  testCase "thread count and processed records are summed" $ do
    let rep = healthReportFrom defaultHealthConfig StreamsRunning threads Nothing
    healthThreads rep          @?= 2
    healthProcessedRecords rep @?= 350
    assertBool "ready when running without a lag report"
      (healthReady rep)
