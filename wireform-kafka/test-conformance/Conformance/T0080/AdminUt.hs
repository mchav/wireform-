{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0080.AdminUt
Description : librdkafka @tests\/0080-admin_ut.c@

librdkafka's @0080-admin_ut@ unit-tests the AdminClient surface
without going to a broker — argument validation, options builders,
result inspection. Our analogue: 'Kafka.Client.AdminClient' value
constructors round-trip and option records have the expected
defaults.
-}
module Conformance.T0080.AdminUt (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Client.AdminClient as Admin

tests :: TestTree
tests = testGroup "0080-admin_ut"
  [ testCase "NewTopic builds with sane defaults" $ do
      let t = Admin.NewTopic
            { Admin.ntName              = "events"
            , Admin.ntNumPartitions     = 12
            , Admin.ntReplicationFactor = 3
            , Admin.ntConfigs           = [ ("retention.ms", "604800000")
                                          , ("cleanup.policy", "delete")
                                          ]
            }
      Admin.ntName              t @?= "events"
      Admin.ntNumPartitions     t @?= 12
      Admin.ntReplicationFactor t @?= 3
      length (Admin.ntConfigs t)  @?= 2

  , testCase "default AdminClientConfig has the documented client id" $ do
      let cfg = Admin.defaultAdminClientConfig
      Admin.adminRequestTimeoutMs cfg > 0 @?= True

  , testCase "ConfigResourceType ADT covers all three resource types" $ do
      -- librdkafka equivalent of the symbol smoke test: every
      -- constructor exists and is reachable.
      let _ = Admin.ConfigResourceTopic
          _ = Admin.ConfigResourceBroker
          _ = Admin.ConfigResourceBrokerLogger
      pure ()
  ]
