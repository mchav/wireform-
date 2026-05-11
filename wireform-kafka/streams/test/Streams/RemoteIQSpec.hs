{-# LANGUAGE OverloadedStrings #-}

-- | KIP-535 cross-instance IQ: subscription metadata
-- round-trip + the routing-decision helper.
module Streams.RemoteIQSpec (tests) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Consumer as KC
import Kafka.Streams.Discovery
  ( HostInfo (..)
  , KeyQueryMetadata (..)
  )
import Kafka.Streams.Discovery.RemoteIQ
import Kafka.Streams.Discovery.Subscription

tests :: TestTree
tests = testGroup "Cross-instance IQ (KIP-535)"
  [ subscription_info_round_trip
  , subscription_info_rejects_unknown_version
  , route_query_local_when_active
  , route_query_local_when_standby
  , route_query_remote_otherwise
  , route_query_missing_when_nobody_owns
  ]

----------------------------------------------------------------------
-- Subscription wire format
----------------------------------------------------------------------

sampleSI :: SubscriptionInfo
sampleSI = SubscriptionInfo
  { siHost = HostInfo "instance-1.example.com" 9091
  , siStoreNames = Set.fromList ["orders", "customers"]
  , siSourceTopics = Set.fromList ["t1", "t2"]
  , siActive = Set.fromList
      [ KC.TopicPartition "t1" 0
      , KC.TopicPartition "t1" 1
      ]
  , siStandby = Set.singleton (KC.TopicPartition "t2" 0)
  }

subscription_info_round_trip :: TestTree
subscription_info_round_trip =
  testCase "SubscriptionInfo: encode . decode = id" $ do
    let !bs   = encodeSubscriptionInfo sampleSI
    case decodeSubscriptionInfo bs of
      Right si -> si @?= sampleSI
      Left e   -> error e

subscription_info_rejects_unknown_version :: TestTree
subscription_info_rejects_unknown_version =
  testCase "SubscriptionInfo: unknown version returns Left" $ do
    -- A version=99 prefix followed by garbage should not be
    -- silently accepted.
    let !bad = "\99\x00\x00garbage"
    case decodeSubscriptionInfo bad of
      Left _  -> pure ()
      Right _ -> error "expected Left on unknown version"

----------------------------------------------------------------------
-- routeQuery
----------------------------------------------------------------------

route_query_local_when_active :: TestTree
route_query_local_when_active =
  testCase "routeQuery: local instance owns the active => RouteLocal" $ do
    let local = HostInfo "self" 9099
        kqm = KeyQueryMetadata local [] 0
    routeQuery local (Just kqm) @?= RouteLocal

route_query_local_when_standby :: TestTree
route_query_local_when_standby =
  testCase "routeQuery: active remote but standby local => RouteLocal" $ do
    let local = HostInfo "self" 9099
        kqm = KeyQueryMetadata
                (HostInfo "remote" 9090) [local] 0
    routeQuery local (Just kqm) @?= RouteLocal

route_query_remote_otherwise :: TestTree
route_query_remote_otherwise =
  testCase "routeQuery: nothing local => RouteRemote with active host" $ do
    let local  = HostInfo "self" 9099
        active = HostInfo "owner" 9090
        kqm    = KeyQueryMetadata active [] 0
    routeQuery local (Just kqm) @?= RouteRemote active

route_query_missing_when_nobody_owns :: TestTree
route_query_missing_when_nobody_owns =
  testCase "routeQuery: no KeyQueryMetadata => RouteMissing" $ do
    routeQuery (HostInfo "self" 9099) Nothing @?= RouteMissing
