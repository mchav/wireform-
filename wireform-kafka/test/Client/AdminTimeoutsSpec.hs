{-# LANGUAGE OverloadedStrings #-}

module Client.AdminTimeoutsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.AdminTimeouts as A

tests :: TestTree
tests = testGroup "AdminClient timeouts + routing (KIP-540 / 918 / 919)"
  [ testCase "AdminUseDefault uses default.api.timeout.ms"
      use_default
  , testCase "AdminTimeoutMs overrides default"
      explicit_timeout
  , testCase "AdminNoDeadline returns Nothing"
      no_deadline
  , testCase "metadata reads -> RouteAnyBroker"
      metadata_route
  , testCase "topic / config / acl mutations -> RouteControllerBroker"
      mutation_route
  , testCase "broker / quorum lifecycle -> RouteKRaftQuorum"
      kraft_route
  ]

use_default :: IO ()
use_default = A.effectiveDeadlineMs 1000 30_000 A.AdminUseDefault @?= Just 31_000

explicit_timeout :: IO ()
explicit_timeout = A.effectiveDeadlineMs 1000 30_000 (A.AdminTimeoutMs 5_000) @?= Just 6_000

no_deadline :: IO ()
no_deadline = A.effectiveDeadlineMs 1000 30_000 A.AdminNoDeadline @?= Nothing

metadata_route :: IO ()
metadata_route = A.routeOperation A.AdminMetadataRead @?= A.RouteAnyBroker

mutation_route :: IO ()
mutation_route = do
  A.routeOperation A.AdminTopicMutation  @?= A.RouteControllerBroker
  A.routeOperation A.AdminConfigMutation @?= A.RouteControllerBroker
  A.routeOperation A.AdminAclMutation    @?= A.RouteControllerBroker

kraft_route :: IO ()
kraft_route = do
  A.routeOperation A.AdminBrokerLifecycle  @?= A.RouteKRaftQuorum
  A.routeOperation A.AdminQuorumManagement @?= A.RouteKRaftQuorum
