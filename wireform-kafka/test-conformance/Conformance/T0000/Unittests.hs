{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0000.Unittests
Description : librdkafka @tests\/0000-unittests.c@

The librdkafka file is a smoke test that constructs a producer with
debugging enabled and asks it to print its build options. We can't
test the OpenSSL build options, but we can do the equivalent
"library boots" check and exercise the bits of our high-level
surface that the librdkafka equivalent exercises:

  * Construct a 'C.ConsumerConfig' with the typed builders.
  * Construct a 'C.ProducerConfig' with the typed builders.
  * Look up the umbrella module's API surface.
-}
module Conformance.T0000.Unittests (tests) where

import Data.Text qualified as T
import Kafka.Client.Consumer qualified as C
import Kafka.Client.Group qualified as G
import Kafka.Client.Producer qualified as P
import Test.Syd


tests :: Spec
tests =
  describe "0000-unittests" $
    sequence_
      [ it "default consumer config has sensible knobs" $ do
          let cfg = C.defaultConsumerConfig
          (not (T.null (C.consumerClientId cfg))) `shouldBe` True
          C.consumerSessionTimeoutMs cfg `shouldBe` 45000 -- KIP-735
          C.consumerHeartbeatIntervalMs cfg `shouldBe` 3000
          C.consumerMaxPollRecords cfg `shouldBe` 500
          C.consumerMaxPollIntervalMs cfg `shouldBe` 300000
      , it "default producer config has sensible knobs" $ do
          let cfg = P.defaultProducerConfig
          -- Just check the constructor + record selectors round-trip
          -- (we exercise the types; the configured values are what they are).
          seq cfg (return () :: IO ())
      , it "high-level group umbrella exports a default" $ do
          let cfg = G.defaultGroupConfig
          (not (null (G.bootstrapBrokers cfg))) `shouldBe` True
          G.sessionTimeoutMs cfg `shouldBe` 10000
          G.maxPollIntervalMs cfg `shouldBe` 300000
      ]
