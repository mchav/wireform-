{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | KIP-535 cross-instance IQ: subscription metadata
-- round-trip + the routing-decision helper.
module Streams.RemoteIQSpec (tests) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import qualified Kafka.Client.Consumer as KC
import Kafka.Streams.Discovery
  ( HostInfo (..)
  , KeyQueryMetadata (..)
  )
import Kafka.Client.Internal.Subscribe
  ( encodeSubscriptionWithOwned
  , decodeSubscriptionFull
  )
import Kafka.Streams.Discovery.RemoteIQ
import Kafka.Streams.Discovery.Subscription

tests :: Spec
tests = describe "Cross-instance IQ (KIP-535)" $ sequence_
  [ subscription_info_round_trip
  , subscription_info_rejects_unknown_version
  , route_query_local_when_active
  , route_query_local_when_standby
  , route_query_remote_otherwise
  , route_query_missing_when_nobody_owns
  , subscription_info_userdata_round_trips_through_joingroup
  ]

----------------------------------------------------------------------
-- Subscription wire format
----------------------------------------------------------------------

sampleSI :: SubscriptionInfo
sampleSI = SubscriptionInfo
  { host = HostInfo "instance-1.example.com" 9091
  , storeNames = Set.fromList ["orders", "customers"]
  , sourceTopics = Set.fromList ["t1", "t2"]
  , active = Set.fromList
      [ KC.TopicPartition "t1" 0
      , KC.TopicPartition "t1" 1
      ]
  , standby = Set.singleton (KC.TopicPartition "t2" 0)
  }

subscription_info_round_trip :: Spec
subscription_info_round_trip =
  it "SubscriptionInfo: encode . decode = id" $ do
    let !bs   = encodeSubscriptionInfo sampleSI
    case decodeSubscriptionInfo bs of
      Right si -> si `shouldBe` sampleSI
      Left e   -> expectationFailure e

subscription_info_rejects_unknown_version :: Spec
subscription_info_rejects_unknown_version =
  it "SubscriptionInfo: unknown version returns Left" $ do
    -- A version=99 prefix followed by garbage should not be
    -- silently accepted.
    let !bad = "\99\x00\x00garbage"
    case decodeSubscriptionInfo bad of
      Left _  -> pure ()
      Right _ -> expectationFailure "expected Left on unknown version"

----------------------------------------------------------------------
-- routeQuery
----------------------------------------------------------------------

route_query_local_when_active :: Spec
route_query_local_when_active =
  it "routeQuery: local instance owns the active => RouteLocal" $ do
    let local = HostInfo "self" 9099
        kqm = KeyQueryMetadata local [] 0
    routeQuery local (Just kqm) `shouldBe` RouteLocal

route_query_local_when_standby :: Spec
route_query_local_when_standby =
  it "routeQuery: active remote but standby local => RouteLocal" $ do
    let local = HostInfo "self" 9099
        kqm = KeyQueryMetadata
                (HostInfo "remote" 9090) [local] 0
    routeQuery local (Just kqm) `shouldBe` RouteLocal

route_query_remote_otherwise :: Spec
route_query_remote_otherwise =
  it "routeQuery: nothing local => RouteRemote with active host" $ do
    let local  = HostInfo "self" 9099
        active = HostInfo "owner" 9090
        kqm    = KeyQueryMetadata active [] 0
    routeQuery local (Just kqm) `shouldBe` RouteRemote active

route_query_missing_when_nobody_owns :: Spec
route_query_missing_when_nobody_owns =
  it "routeQuery: no KeyQueryMetadata => RouteMissing" $ do
    routeQuery (HostInfo "self" 9099) Nothing `shouldBe` RouteMissing

----------------------------------------------------------------------
-- Subscription userdata flows end-to-end through the JoinGroup
-- subscription codec.
--
-- Verifies the wire-shape we depend on for the assignor-side
-- exchange: encode -> wrap-into-subscription -> decode back
-- -> recover the SubscriptionInfo. This is the byte-level
-- contract the streams assignor uses to pull peer metadata
-- off the leader-side JoinGroup view.
----------------------------------------------------------------------

subscription_info_userdata_round_trips_through_joingroup :: Spec
subscription_info_userdata_round_trips_through_joingroup =
  it "SubscriptionInfo embedded in JoinGroup userdata round-trips" $ do
    -- Encode our streams 'SubscriptionInfo' to bytes.
    let userdataBytes = encodeSubscriptionInfo sampleSI
    -- Wrap into a full consumer-protocol subscription with
    -- topic list + the userdata bytes (matches what
    -- subscribeFlow now puts on the wire).
    let topics = ["t1", "t2"]
    let !wire = encodeSubscriptionWithOwned topics userdataBytes []
    -- Decode the full subscription and pull out the userdata
    -- bytes — they must match what we put in.
    case decodeSubscriptionFull wire of
      Right (decodedTopics, decodedUserdata, decodedOwned) -> do
        decodedTopics `shouldBe` topics
        decodedUserdata `shouldBe` userdataBytes
        decodedOwned `shouldBe` []
        -- And the userdata bytes still decode back to the
        -- original SubscriptionInfo.
        case decodeSubscriptionInfo decodedUserdata of
          Right si -> si `shouldBe` sampleSI
          Left e   -> expectationFailure ("inner decode: " <> e)
      Left e -> expectationFailure ("outer decode: " <> e)
