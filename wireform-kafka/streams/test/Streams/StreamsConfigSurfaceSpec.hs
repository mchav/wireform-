{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the new fields on 'StreamsConfig' added to honour
-- the JVM client's full configuration surface (KIP-892 +
-- KIP-441 family).
module Streams.StreamsConfigSurfaceSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.Config as C

tests :: TestTree
tests = testGroup "StreamsConfig: new full-surface fields"
  [ testCase "defaults match the JVM 3.x defaults"
      defaults_match
  , testCase "task.timeout.ms is honoured by streamsConfigFromMap"
      ttl_parsed
  , testCase "acceptable.recovery.lag is honoured"
      arl_parsed
  , testCase "max.warmup.replicas is honoured"
      mwr_parsed
  , testCase "probing.rebalance.interval.ms is honoured"
      pri_parsed
  , testCase "task.assignor.class is honoured"
      tac_parsed
  ]

defaults_match :: IO ()
defaults_match = do
  C.taskTimeoutMs              C.defaultStreamsConfig @?= 300_000
  C.acceptableRecoveryLag      C.defaultStreamsConfig @?= 10_000
  C.maxWarmupReplicas          C.defaultStreamsConfig @?= 2
  C.probingRebalanceIntervalMs C.defaultStreamsConfig @?= 600_000
  C.taskAssignorClass          C.defaultStreamsConfig @?= Nothing

ttl_parsed :: IO ()
ttl_parsed =
  C.taskTimeoutMs (C.streamsConfigFromMap (Map.singleton "task.timeout.ms" "120000"))
    @?= 120_000

arl_parsed :: IO ()
arl_parsed =
  C.acceptableRecoveryLag
    (C.streamsConfigFromMap (Map.singleton "acceptable.recovery.lag" "5000"))
    @?= 5_000

mwr_parsed :: IO ()
mwr_parsed =
  C.maxWarmupReplicas
    (C.streamsConfigFromMap (Map.singleton "max.warmup.replicas" "8"))
    @?= 8

pri_parsed :: IO ()
pri_parsed =
  C.probingRebalanceIntervalMs
    (C.streamsConfigFromMap (Map.singleton "probing.rebalance.interval.ms" "30000"))
    @?= 30_000

tac_parsed :: IO ()
tac_parsed =
  C.taskAssignorClass
    (C.streamsConfigFromMap
       (Map.singleton "task.assignor.class" "com.acme.MyAssignor"))
    @?= Just "com.acme.MyAssignor"
