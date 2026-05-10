{-# LANGUAGE OverloadedStrings #-}

module Streams.RevocationGraceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Processor (TaskId (..))
import qualified Kafka.Streams.Runtime.RevocationGrace as RG

tests :: TestTree
tests = testGroup "KIP-869 task revocation grace"
  [ testCase "grace = 0 -> RevokeImmediate"
      grace_zero_immediate
  , testCase "grace > 0 -> KeepAsStandby with deadline"
      grace_keeps_standby
  , testCase "negative grace -> RevokeImmediate"
      grace_negative
  , testCase "planRevocation maps over a list of tasks"
      plan_maps
  ]

grace_zero_immediate :: IO ()
grace_zero_immediate =
  RG.classifyRevocation 1000 0 @?= RG.RevokeImmediate

grace_keeps_standby :: IO ()
grace_keeps_standby =
  RG.classifyRevocation 1000 5000 @?= RG.KeepAsStandby 6000

grace_negative :: IO ()
grace_negative =
  RG.classifyRevocation 1000 (-1) @?= RG.RevokeImmediate

plan_maps :: IO ()
plan_maps =
  RG.planRevocation 1000 5000 [TaskId 0 0, TaskId 1 0]
    @?= [ RG.RevocationPlan (TaskId 0 0) (RG.KeepAsStandby 6000)
        , RG.RevocationPlan (TaskId 1 0) (RG.KeepAsStandby 6000)
        ]
