{-# LANGUAGE OverloadedStrings #-}

module Client.ConsumerConfigSpec where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Text (Text)
import qualified Data.Text as T

import Kafka.Client.Consumer

-- | Test suite for consumer configuration (KIP-256, KIP-392)
consumerConfigSpec :: TestTree
consumerConfigSpec = testGroup "Consumer Configuration"
  [ testGroup "KIP-256: Max Poll Interval"
      [ testCase "unit_defaultMaxPollInterval" unit_defaultMaxPollInterval
      , testCase "unit_maxPollIntervalSeparateFromSessionTimeout" unit_maxPollIntervalSeparateFromSessionTimeout
      , testProperty "prop_maxPollIntervalConfigurable" prop_maxPollIntervalConfigurable
      ]
  , testGroup "KIP-392: Rack-Aware Fetching"
      [ testCase "unit_defaultRackIdIsNothing" unit_defaultRackIdIsNothing
      , testCase "unit_canSetRackId" unit_canSetRackId
      , testProperty "prop_rackIdConfigurable" prop_rackIdConfigurable
      ]
  ]

-- | KIP-256: Default max poll interval should be 300000ms (5 minutes)
unit_defaultMaxPollInterval :: Assertion
unit_defaultMaxPollInterval = do
  let config = defaultConsumerConfig
  consumerMaxPollIntervalMs config @?= 300000

-- | KIP-256: Max poll interval should be separate from session timeout.
unit_maxPollIntervalSeparateFromSessionTimeout :: Assertion
unit_maxPollIntervalSeparateFromSessionTimeout = do
  let config = defaultConsumerConfig
      maxPollInterval = consumerMaxPollIntervalMs config
      sessionTimeout = consumerSessionTimeoutMs config

  -- Max poll interval (5 min) should be longer than session timeout
  -- (45s, post-KIP-735).
  assertBool "Max poll interval should be longer than session timeout"
    (maxPollInterval > sessionTimeout)

  -- Default max poll interval is still 300000ms.
  maxPollInterval @?= 300000

  -- KIP-735 widened the default session timeout from 10000ms to
  -- 45000ms (Kafka 3.0). We track the JVM client's default.
  sessionTimeout @?= 45000

-- | KIP-256: Max poll interval should be configurable
prop_maxPollIntervalConfigurable :: H.Property
prop_maxPollIntervalConfigurable = H.property $ do
  customInterval <- H.forAll $ Gen.int (Range.linear 10000 600000)
  
  let config = defaultConsumerConfig { consumerMaxPollIntervalMs = customInterval }
  
  H.annotate $ "Custom max poll interval: " ++ show customInterval
  consumerMaxPollIntervalMs config H.=== customInterval

-- | KIP-392: Default rack ID should be Nothing (disabled by default)
unit_defaultRackIdIsNothing :: Assertion
unit_defaultRackIdIsNothing = do
  let config = defaultConsumerConfig
  consumerRackId config @?= Nothing

-- | KIP-392: Should be able to set rack ID
unit_canSetRackId :: Assertion
unit_canSetRackId = do
  let rackId = "us-east-1a"
      config = defaultConsumerConfig { consumerRackId = Just rackId }
  
  consumerRackId config @?= Just rackId

-- | KIP-392: Rack ID should be configurable
prop_rackIdConfigurable :: H.Property
prop_rackIdConfigurable = H.property $ do
  rackId <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  
  let config = defaultConsumerConfig { consumerRackId = Just rackId }
  
  H.annotate $ "Rack ID: " ++ T.unpack rackId
  consumerRackId config H.=== Just rackId
  
  -- Also test that Nothing works
  let configNoRack = defaultConsumerConfig { consumerRackId = Nothing }
  consumerRackId configNoRack H.=== Nothing

