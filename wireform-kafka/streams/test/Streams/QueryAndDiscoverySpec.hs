{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the new typed Query API extensions (KIP-805 /
-- KIP-889 / KIP-796) + the KIP-535 discovery helpers.
module Streams.QueryAndDiscoverySpec (tests) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Consumer as KC
import Kafka.Streams.Imperative
import qualified Kafka.Streams.Query as Q
import qualified Kafka.Streams.State.KeyValue.Versioned as V
import qualified Kafka.Streams.State.Window.InMemory as WS

tests :: TestTree
tests = testGroup "Query API + Discovery"
  [ window_key_query_fetches_single_window
  , versioned_key_query_returns_as_of
  , position_advance_is_monotone
  , host_info_parses
  , key_query_metadata_picks_owner
  ]

----------------------------------------------------------------------
-- WindowKeyQuery
----------------------------------------------------------------------

window_key_query_fetches_single_window :: TestTree
window_key_query_fetches_single_window =
  testCase "WindowKeyQuery: point lookup at window-start ts" $ do
    ws <- WS.inMemoryWindowStore @Text @Int (storeName "w") 1000 60_000
    wsPut ws "alice" 5 (Timestamp 100)
    wsPut ws "alice" 9 (Timestamp 1500)
    r <- Q.executeWindowKeyQuery ws "alice" (Timestamp 100)
    case r of
      Q.QuerySuccess (Just v) -> v @?= 5
      other                   -> error ("got " <> show other)

----------------------------------------------------------------------
-- VersionedKeyQuery
----------------------------------------------------------------------

versioned_key_query_returns_as_of :: TestTree
versioned_key_query_returns_as_of =
  testCase "VersionedKeyQuery: asOfTimestamp returns the right version" $ do
    s <- V.inMemoryVersionedKeyValueStore @Text @Int
            (storeName "v") V.defaultVersionedConfig
    V.vkvPut s "k" 1 (Timestamp 100)
    V.vkvPut s "k" 2 (Timestamp 200)
    V.vkvPut s "k" 3 (Timestamp 300)
    r <- Q.executeVersionedKeyQuery s "k" (Timestamp 250)
    case Q.queryValue r of
      Just (Just (V.VersionedRecord v _)) -> v @?= 2
      _ -> error "expected v=2 at ts=250"

----------------------------------------------------------------------
-- Position
----------------------------------------------------------------------

position_advance_is_monotone :: TestTree
position_advance_is_monotone =
  testCase "Position.advance is monotone (older offsets don't regress)" $ do
    let p0 = Q.emptyPosition
        p1 = Q.positionAdvance "in" 0 100 p0
        p2 = Q.positionAdvance "in" 0 50  p1  -- older, must NOT override
        p3 = Q.positionAdvance "in" 0 200 p2
    Q.positionAt "in" 0 p1 @?= Just 100
    Q.positionAt "in" 0 p2 @?= Just 100
    Q.positionAt "in" 0 p3 @?= Just 200
    Q.positionAt "out" 0 p3 @?= Nothing

----------------------------------------------------------------------
-- HostInfo / KeyQueryMetadata
----------------------------------------------------------------------

host_info_parses :: TestTree
host_info_parses =
  testCase "parseHostInfo: host:port round-trips" $ do
    case parseHostInfo "kafka-app-1.example.com:9909" of
      Right (HostInfo h p) -> do
        h @?= "kafka-app-1.example.com"
        p @?= 9909
      Left e   -> error e
    -- And rejects garbage.
    case parseHostInfo "no-port-here" of
      Left _  -> pure ()
      Right _ -> error "expected parse failure"

key_query_metadata_picks_owner :: TestTree
key_query_metadata_picks_owner =
  testCase "makeKeyQueryMetadata: active owner + standby owners" $ do
    let h1 = HostInfo "host-1" 9090
        h2 = HostInfo "host-2" 9090
        h3 = HostInfo "host-3" 9090
        tp0 = KC.TopicPartition "in" 0
        tp1 = KC.TopicPartition "in" 1
        m1 = StreamsMetadata h1
               (Set.fromList ["s"])
               (Set.fromList [tp0])
               Set.empty
        m2 = StreamsMetadata h2
               (Set.fromList ["s"])
               (Set.fromList [tp1])
               (Set.fromList [tp0])  -- standby for tp0
        m3 = StreamsMetadata h3
               (Set.fromList ["s"])
               Set.empty
               (Set.fromList [tp0])  -- another standby for tp0
    case makeKeyQueryMetadata [m1, m2, m3] "in" 0 of
      Just kqm -> do
        kqm.activeHost @?= h1
        Set.fromList kqm.standbyHosts @?= Set.fromList [h2, h3]
        kqm.partition @?= 0
      Nothing -> error "expected an active owner"
