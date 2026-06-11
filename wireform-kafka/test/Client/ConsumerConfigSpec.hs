{-# LANGUAGE OverloadedStrings #-}

module Client.ConsumerConfigSpec where

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Consumer
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Test suite for consumer configuration (KIP-256, KIP-392)
consumerConfigSpec :: Spec
consumerConfigSpec =
  describe "Consumer Configuration" $
    sequence_
      [ describe "KIP-256: Max Poll Interval" $
          sequence_
            [ it "unit_defaultMaxPollInterval" unit_defaultMaxPollInterval
            , it "unit_maxPollIntervalSeparateFromSessionTimeout" unit_maxPollIntervalSeparateFromSessionTimeout
            , it "prop_maxPollIntervalConfigurable" prop_maxPollIntervalConfigurable
            ]
      , describe "KIP-392: Rack-Aware Fetching" $
          sequence_
            [ it "unit_defaultRackIdIsNothing" unit_defaultRackIdIsNothing
            , it "unit_canSetRackId" unit_canSetRackId
            , it "prop_rackIdConfigurable" prop_rackIdConfigurable
            ]
      ]


-- | KIP-256: Default max poll interval should be 300000ms (5 minutes)
unit_defaultMaxPollInterval :: IO ()
unit_defaultMaxPollInterval = do
  let config = defaultConsumerConfig
  consumerMaxPollIntervalMs config `shouldBe` 300000


-- | KIP-256: Max poll interval should be separate from session timeout.
unit_maxPollIntervalSeparateFromSessionTimeout :: IO ()
unit_maxPollIntervalSeparateFromSessionTimeout = do
  let config = defaultConsumerConfig
      maxPollInterval = consumerMaxPollIntervalMs config
      sessionTimeout = consumerSessionTimeoutMs config

  -- Max poll interval (5 min) should be longer than session timeout
  -- (45s, post-KIP-735).
  (maxPollInterval > sessionTimeout) `shouldBe` True

  -- Default max poll interval is still 300000ms.
  maxPollInterval `shouldBe` 300000

  -- KIP-735 widened the default session timeout from 10000ms to
  -- 45000ms (Kafka 3.0). We track the JVM client's default.
  sessionTimeout `shouldBe` 45000


-- | KIP-256: Max poll interval should be configurable
prop_maxPollIntervalConfigurable :: H.Property
prop_maxPollIntervalConfigurable = H.property $ do
  customInterval <- H.forAll $ Gen.int (Range.linear 10000 600000)

  let config = defaultConsumerConfig {consumerMaxPollIntervalMs = customInterval}

  H.annotate $ "Custom max poll interval: " ++ show customInterval
  consumerMaxPollIntervalMs config H.=== customInterval


-- | KIP-392: Default rack ID should be Nothing (disabled by default)
unit_defaultRackIdIsNothing :: IO ()
unit_defaultRackIdIsNothing = do
  let config = defaultConsumerConfig
  consumerRackId config `shouldBe` Nothing


-- | KIP-392: Should be able to set rack ID
unit_canSetRackId :: IO ()
unit_canSetRackId = do
  let rackId = "us-east-1a"
      config = defaultConsumerConfig {consumerRackId = Just rackId}

  consumerRackId config `shouldBe` Just rackId


-- | KIP-392: Rack ID should be configurable
prop_rackIdConfigurable :: H.Property
prop_rackIdConfigurable = H.property $ do
  rackId <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum

  let config = defaultConsumerConfig {consumerRackId = Just rackId}

  H.annotate $ "Rack ID: " ++ T.unpack rackId
  consumerRackId config H.=== Just rackId

  -- Also test that Nothing works
  let configNoRack = defaultConsumerConfig {consumerRackId = Nothing}
  consumerRackId configNoRack H.=== Nothing
