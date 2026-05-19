{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Streams.Properties.OrphanTopicsSpec
-- Description : Detector tests for orphaned internal topics
module Streams.Properties.OrphanTopicsSpec (tests) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative
import qualified Kafka.Streams.State.Store as Store
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Observability.OrphanTopics
  ( OrphanInternalTopic (..)
  , OrphanReason (..)
  , changelogTopic
  , detectOrphans
  , expectedInternalTopics
  , isInternalTopicName
  , repartitionTopic
  )

----------------------------------------------------------------------
-- Topologies
----------------------------------------------------------------------

-- | A topology with one logged KV store under the standard
-- changelog convention.
loggedStoreTopology :: IO Topo.Topology
loggedStoreTopology = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde int64Serde)
  let g  = grouped textSerde int64Serde
      ks = groupByKey g s
  _ <- countStream
         (materializedAs (Store.storeName "logged-store"))
         ks
  buildTopology b

-- | A topology with no stores at all (passthrough). Used to
-- check that 'expectedInternalTopics' yields an empty set when
-- there's nothing logged.
storelessTopology :: IO Topo.Topology
storelessTopology = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in")
         (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  buildTopology b

-- | A topology that uses 'repartition' on the stream — produces
-- an internal repartition node.
repartitionTopology :: IO Topo.Topology
repartitionTopology = do
  b <- newStreamsBuilder
  s  <- streamFromTopic b (topicName "in") (consumed textSerde int64Serde)
  s' <- selectKey (\r -> T.reverse (case recordKey r of
                                      Just k -> k
                                      Nothing -> ""))
                  s
  _  <- repartition "prefix" s'
  buildTopology b

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "OrphanTopics"
  [ expected_set_contains_changelog_for_logged_store
  , expected_set_empty_for_storeless
  , detect_orphan_changelog_from_old_deploy
  , detect_does_not_flag_legit_topics
  , detect_orphan_repartition
  , is_internal_topic_name_matches_convention
  ]

----------------------------------------------------------------------

expected_set_contains_changelog_for_logged_store :: TestTree
expected_set_contains_changelog_for_logged_store =
  testCase "expectedInternalTopics names the logged store's changelog" $ do
    topo <- loggedStoreTopology
    let s = expectedInternalTopics topo "app1"
    assertBool
      ("expected to find app1-logged-store-changelog; got "
        <> show s)
      (Set.member (changelogTopic "app1" (Store.storeName "logged-store")) s)

expected_set_empty_for_storeless :: TestTree
expected_set_empty_for_storeless =
  testCase "expectedInternalTopics is empty for a storeless passthrough" $ do
    topo <- storelessTopology
    expectedInternalTopics topo "app-empty" @?= Set.empty

detect_orphan_changelog_from_old_deploy :: TestTree
detect_orphan_changelog_from_old_deploy =
  testCase "detectOrphans flags a leftover changelog from a renamed store" $ do
    topo <- loggedStoreTopology
    let broker =
          [ topicName "in"                                  -- input topic
          , changelogTopic "app1" (Store.storeName "logged-store")
              -- still in the current topology
          , changelogTopic "app1" (Store.storeName "removed-store")
              -- left over from a previous deploy
          ]
        orphans = detectOrphans topo "app1" broker
    map orphanTopic orphans
      @?= [changelogTopic "app1" (Store.storeName "removed-store")]
    map orphanReason orphans @?= [OrphanChangelog]

detect_does_not_flag_legit_topics :: TestTree
detect_does_not_flag_legit_topics =
  testCase "detectOrphans ignores topics that don't match the internal-topic naming scheme" $ do
    topo <- loggedStoreTopology
    let broker =
          [ topicName "in"
          , topicName "out"
          , topicName "user-events"                 -- not prefixed by app id
          , topicName "app1-some-business-topic"    -- prefixed but not -changelog/-repartition
          ]
        orphans = detectOrphans topo "app1" broker
    orphans @?= []

detect_orphan_repartition :: TestTree
detect_orphan_repartition =
  testCase "detectOrphans flags a leftover repartition topic" $ do
    topo <- repartitionTopology
    let broker =
          [ topicName "in"
          , topicName "app3-KSTREAM-REPARTITION-stale-0000000099-repartition"
          ]
        orphans = detectOrphans topo "app3" broker
    -- The current topology owns its own repartition node, which
    -- is in 'expectedInternalTopics'; the stale name is orphan.
    map orphanReason orphans @?= [OrphanRepartition]

is_internal_topic_name_matches_convention :: TestTree
is_internal_topic_name_matches_convention =
  testCase "isInternalTopicName recognises -changelog and -repartition suffixes" $ do
    isInternalTopicName "app" (topicName "app-store-changelog")     @?= True
    isInternalTopicName "app" (topicName "app-node-repartition")    @?= True
    isInternalTopicName "app" (topicName "app-business-data")       @?= False
    isInternalTopicName "app" (topicName "other-store-changelog")   @?= False
    -- Also check the conventional helpers round-trip:
    let t = changelogTopic "app" (Store.storeName "s")
    isInternalTopicName "app" t @?= True
    -- Repartition:
    isInternalTopicName "app" (repartitionTopic "app" (Topo.NodeName "n"))
      @?= True
