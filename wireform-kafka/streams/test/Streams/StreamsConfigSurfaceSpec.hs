{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the new fields on 'StreamsConfig' added to honour
-- the JVM client's full configuration surface (KIP-892 +
-- KIP-441 family).
module Streams.StreamsConfigSurfaceSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Syd

import qualified Kafka.Streams.Config as C

tests :: Spec
tests = describe "StreamsConfig: new full-surface fields" $ sequence_
  [ it "defaults match the JVM 3.x defaults"
      defaults_match
  , it "task.timeout.ms is honoured by streamsConfigFromMap"
      ttl_parsed
  , it "acceptable.recovery.lag is honoured"
      arl_parsed
  , it "max.warmup.replicas is honoured"
      mwr_parsed
  , it "probing.rebalance.interval.ms is honoured"
      pri_parsed
  , it "task.assignor.class is honoured"
      tac_parsed
  ]

defaults_match :: IO ()
defaults_match = do
  C.taskTimeoutMs              C.defaultStreamsConfig `shouldBe` 300_000
  C.acceptableRecoveryLag      C.defaultStreamsConfig `shouldBe` 10_000
  C.maxWarmupReplicas          C.defaultStreamsConfig `shouldBe` 2
  C.probingRebalanceIntervalMs C.defaultStreamsConfig `shouldBe` 600_000
  C.taskAssignorClass          C.defaultStreamsConfig `shouldBe` Nothing

ttl_parsed :: IO ()
ttl_parsed =
  C.taskTimeoutMs (C.streamsConfigFromMap (Map.singleton "task.timeout.ms" "120000"))
    `shouldBe` 120_000

arl_parsed :: IO ()
arl_parsed =
  C.acceptableRecoveryLag
    (C.streamsConfigFromMap (Map.singleton "acceptable.recovery.lag" "5000"))
    `shouldBe` 5_000

mwr_parsed :: IO ()
mwr_parsed =
  C.maxWarmupReplicas
    (C.streamsConfigFromMap (Map.singleton "max.warmup.replicas" "8"))
    `shouldBe` 8

pri_parsed :: IO ()
pri_parsed =
  C.probingRebalanceIntervalMs
    (C.streamsConfigFromMap (Map.singleton "probing.rebalance.interval.ms" "30000"))
    `shouldBe` 30_000

tac_parsed :: IO ()
tac_parsed =
  C.taskAssignorClass
    (C.streamsConfigFromMap
       (Map.singleton "task.assignor.class" "com.acme.MyAssignor"))
    `shouldBe` Just "com.acme.MyAssignor"
