{-# LANGUAGE OverloadedStrings #-}

module Client.AdminTimeoutsSpec (tests) where

import Test.Syd

import qualified Kafka.Client.AdminTimeouts as A

tests :: Spec
tests = describe "AdminClient timeouts + routing (KIP-540 / 918 / 919)" $ sequence_
  [ it "AdminUseDefault uses default.api.timeout.ms"
      use_default
  , it "AdminTimeoutMs overrides default"
      explicit_timeout
  , it "AdminNoDeadline returns Nothing"
      no_deadline
  , it "metadata reads -> RouteAnyBroker"
      metadata_route
  , it "topic / config / acl mutations -> RouteControllerBroker"
      mutation_route
  , it "broker / quorum lifecycle -> RouteKRaftQuorum"
      kraft_route
  ]

use_default :: IO ()
use_default = A.effectiveDeadlineMs 1000 30_000 A.AdminUseDefault `shouldBe` Just 31_000

explicit_timeout :: IO ()
explicit_timeout = A.effectiveDeadlineMs 1000 30_000 (A.AdminTimeoutMs 5_000) `shouldBe` Just 6_000

no_deadline :: IO ()
no_deadline = A.effectiveDeadlineMs 1000 30_000 A.AdminNoDeadline `shouldBe` Nothing

metadata_route :: IO ()
metadata_route = A.routeOperation A.AdminMetadataRead `shouldBe` A.RouteAnyBroker

mutation_route :: IO ()
mutation_route = do
  A.routeOperation A.AdminTopicMutation  `shouldBe` A.RouteControllerBroker
  A.routeOperation A.AdminConfigMutation `shouldBe` A.RouteControllerBroker
  A.routeOperation A.AdminAclMutation    `shouldBe` A.RouteControllerBroker

kraft_route :: IO ()
kraft_route = do
  A.routeOperation A.AdminBrokerLifecycle  `shouldBe` A.RouteKRaftQuorum
  A.routeOperation A.AdminQuorumManagement `shouldBe` A.RouteKRaftQuorum
