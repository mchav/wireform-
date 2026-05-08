{-# LANGUAGE OverloadedStrings #-}

{-|
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

import Control.Exception (try, IOException)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Client.Group as G

tests :: TestTree
tests = testGroup "0004-conf"
  [ testCase "empty bootstrap brokers rejected" $ rejects
      G.defaultGroupConfig
        { G.gcBootstrapBrokers = []
        , G.gcGroupId = "g"
        , G.gcTopics  = ["t"]
        }

  , testCase "empty group id rejected" $ rejects
      G.defaultGroupConfig
        { G.gcGroupId = ""
        , G.gcTopics  = ["t"]
        }

  , testCase "empty topics list rejected" $ rejects
      G.defaultGroupConfig
        { G.gcGroupId = "g"
        , G.gcTopics  = []
        }

  , testCase "default knobs round-trip" $ do
      let cfg = G.defaultGroupConfig
      G.gcSessionTimeoutMs   cfg @?= 10000
      G.gcMaxPollIntervalMs  cfg @?= 300000
      G.gcMaxPollRecords     cfg @?= 500
      G.gcPollTimeoutMs      cfg @?= 1000
      G.gcCloseTimeoutMs     cfg @?= 30000
  ]
  where
    rejects cfg = do
      r <- try (G.runConsumer cfg (\_ -> pure ()))
      case (r :: Either IOException ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected validation failure, got success"
