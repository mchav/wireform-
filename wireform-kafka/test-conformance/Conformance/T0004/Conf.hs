{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0004.Conf
Description : librdkafka @tests\/0004-conf.c@ — configuration validation

librdkafka's @0004-conf@ exercises @rd_kafka_conf_set@ + @rd_kafka_topic_conf_set@
with valid and invalid keys / values, and asserts that defaults
round-trip via @rd_kafka_conf_get@.

Our analogue: 'Kafka.Client.Group.GroupConfig' is a typed record so
type errors are compile-time. The runtime checks that survive are:

  * Empty bootstrap broker list rejected.
  * Empty group id rejected.
  * Empty topic list rejected.
  * Defaults make sense.
-}
module Conformance.T0004.Conf (tests) where

import Control.Exception (try)
import Kafka.Client.Group qualified as G
import Kafka.Errors (KafkaException)
import Test.Syd


tests :: Spec
tests =
  describe "0004-conf" $
    sequence_
      [ it "empty bootstrap brokers rejected" $
          rejects
            G.defaultGroupConfig
              { G.bootstrapBrokers = []
              , G.groupId = "g"
              , G.topics = ["t"]
              }
      , it "empty group id rejected" $
          rejects
            G.defaultGroupConfig
              { G.groupId = ""
              , G.topics = ["t"]
              }
      , it "empty topics list rejected" $
          rejects
            G.defaultGroupConfig
              { G.groupId = "g"
              , G.topics = []
              }
      , it "default knobs round-trip" $ do
          let cfg = G.defaultGroupConfig
          G.sessionTimeoutMs cfg `shouldBe` 10000
          G.maxPollIntervalMs cfg `shouldBe` 300000
          G.maxPollRecords cfg `shouldBe` 500
          G.pollTimeoutMs cfg `shouldBe` 1000
          G.closeTimeoutMs cfg `shouldBe` 30000
      ]
  where
    rejects cfg = do
      r <- try (G.runConsumer cfg (\_ -> pure ()))
      case (r :: Either KafkaException ()) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected validation failure, got success"
