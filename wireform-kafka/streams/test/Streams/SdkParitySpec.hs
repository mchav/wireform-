{-# LANGUAGE OverloadedStrings #-}

-- | Smoke tests for the JVM-parity shims added in the SDK_PARITY
-- audit pass: 'Kafka.Client.ConsumerSdk' (ConsumerRecords,
-- OffsetAndMetadata, ConsumerGroupMetadata, OffsetCommitCallback,
-- SubscriptionPattern) and 'Kafka.Streams.Processor.Mock' (the
-- mock processor context for unit-testing user processors).
module Streams.SdkParitySpec (tests) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.ConsumerSdk as SDK
import qualified Kafka.Common as Common
import qualified Kafka.Common.Acl as Acl
import qualified Kafka.Common.Quota as Quota
import qualified Kafka.Common.Resource as Resource
import qualified Kafka.Streams.Processor as P
import qualified Kafka.Streams.Processor.Mock as M
import Kafka.Streams.Types (Record (..), mkRecord)
import qualified Kafka.Streams.Time
import Kafka.Streams.Time
  ( StreamTime (..)
  , Timestamp (..)
  , initialStreamTime
  , runTimestampExtractor
  , usePartitionTimeOnInvalidTimestamp
  )

tests :: TestTree
tests = testGroup "SDK parity shims (audit pass)"
  [ testGroup "Kafka.Client.ConsumerSdk"
      [ consumerRecords_groupings
      , offsetAndMetadata_builder
      , subscriptionPattern_match
      , offsetCommitCallback_compiles
      ]
  , testGroup "Kafka.Streams.Processor.Mock"
      [ mock_captures_forwards
      , mock_captures_punctuators
      , mock_commit_request_flag
      ]
  , testGroup "Kafka.Common (v2 audit additions)"
      [ common_node_endpoint_smoke
      , common_acl_wildcard
      , common_quota_helpers
      , timeextractor_use_partition_time
      ]
  ]

----------------------------------------------------------------------
-- ConsumerSdk
----------------------------------------------------------------------

mkRec :: T.Text -> Int -> Int -> C.ConsumerRecord
mkRec t p o = C.ConsumerRecord
  { C.topic     = t
  , C.partition = fromIntegral p
  , C.offset    = fromIntegral o
  , C.timestamp = 0
  , C.key       = Nothing
  , C.value     = ""
  , C.headers   = []
  }

consumerRecords_groupings :: TestTree
consumerRecords_groupings =
  testCase "ConsumerRecords: partition/topic/next-offset projections" $ do
    let rs = SDK.ConsumerRecords
          [ mkRec "events" 0 10
          , mkRec "events" 0 11
          , mkRec "events" 1 20
          , mkRec "audit"  0 30
          ]
    SDK.consumerRecordsCount rs @?= 4
    SDK.consumerRecordsPartitions rs @?=
      Set.fromList
        [ C.TopicPartition "events" 0
        , C.TopicPartition "events" 1
        , C.TopicPartition "audit"  0
        ]
    map C.offset (SDK.recordsByPartition (C.TopicPartition "events" 0) rs)
      @?= [10, 11]
    Map.keys (SDK.recordsByTopic rs)
      @?= ["audit", "events"]
    SDK.consumerRecordsNextOffsets rs @?=
      HashMap.fromList
        [ (C.TopicPartition "events" 0, 12)
        , (C.TopicPartition "events" 1, 21)
        , (C.TopicPartition "audit"  0, 31)
        ]

offsetAndMetadata_builder :: TestTree
offsetAndMetadata_builder =
  testCase "OffsetAndMetadata: builder applies metadata + leader epoch" $ do
    let oam0 = SDK.offsetAndMetadata 42
        oam  = SDK.withLeaderEpoch 7 (SDK.withMetadata "ckpt" oam0)
    SDK.oamOffset oam      @?= 42
    SDK.oamMetadata oam    @?= "ckpt"
    SDK.oamLeaderEpoch oam @?= Just 7

subscriptionPattern_match :: TestTree
subscriptionPattern_match =
  testCase "SubscriptionPattern: matches by regex" $ do
    case SDK.subscriptionPattern "events\\.[a-z]+" of
      Left e   -> error ("regex compile failed: " <> e)
      Right sp -> do
        assertBool "matches events.user"
          (SDK.matchesSubscriptionPattern sp "events.user")
        assertBool "does not match events.UPPER"
          (not (SDK.matchesSubscriptionPattern sp "events.UPPER"))

offsetCommitCallback_compiles :: TestTree
offsetCommitCallback_compiles =
  testCase "OffsetCommitCallback type unifies with user callbacks" $ do
    let cb :: SDK.OffsetCommitCallback
        cb _ _ = pure ()
    cb Map.empty Nothing -- shape-only check; success is the unit return
    SDK.noopOffsetCommitCallback Map.empty Nothing

----------------------------------------------------------------------
-- MockProcessorContext
----------------------------------------------------------------------

mock_captures_forwards :: TestTree
mock_captures_forwards =
  testCase "MockProcessorContext: forwardRecord lands in capturedForwards" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    let ctx = M.mockContext mock
    P.forwardRecord ctx (mkRecord (Just "k1") ("hello" :: T.Text) (Timestamp 0))
    P.forwardRecord ctx (mkRecord (Just "k2") ("world" :: T.Text) (Timestamp 1))
    fs <- M.capturedForwards mock
    length fs @?= 2

mock_captures_punctuators :: TestTree
mock_captures_punctuators =
  testCase "MockProcessorContext: schedule registers a punctuator" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    let ctx  = M.mockContext mock
    let pun  = P.Punctuator (\_ -> pure ())
    _ <- P.schedule ctx 1000 P.WallClockTimePunctuation pun
    ps <- M.capturedPunctuators mock
    length ps @?= 1
    map M.cpType ps @?= [P.WallClockTimePunctuation]

mock_commit_request_flag :: TestTree
mock_commit_request_flag =
  testCase "MockProcessorContext: requestCommit toggles the commit flag" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    M.commitRequested mock >>= (@?= False)
    P.requestCommit (M.mockContext mock)
    M.commitRequested mock >>= (@?= True)
    M.readCommitRequested mock >>= (@?= True)
    M.commitRequested mock >>= (@?= False)

----------------------------------------------------------------------
-- Kafka.Common
----------------------------------------------------------------------

common_node_endpoint_smoke :: TestTree
common_node_endpoint_smoke =
  testCase "Kafka.Common: Node/Endpoint/Cluster value types compose" $ do
    let !node = Common.Node 1 "broker-1" 9092 (Just "rack-a")
        !ep   = Common.Endpoint "PLAINTEXT" "broker-1" 9092 "PLAINTEXT"
        !cl   = Common.emptyCluster
                  { Common.clusterId         = Just "cid"
                  , Common.clusterNodes      = [node]
                  , Common.clusterController = Just node
                  }
    Common.nodeId node @?= 1
    Common.endpointPort ep @?= 9092
    Common.clusterId cl @?= Just "cid"
    Common.clusterController cl @?= Just node

common_acl_wildcard :: TestTree
common_acl_wildcard =
  testCase "Kafka.Common.Acl: wildcard filter matches everything by construction" $ do
    let f = Acl.anyAclBindingFilter
    Acl.acefOperation (Acl.aclbfEntryFilter f)      @?= Acl.AclAnyOp
    Acl.acefPermissionType (Acl.aclbfEntryFilter f) @?= Acl.AclAnyPerm
    Resource.rpfResourceType (Acl.aclbfPatternFilter f)
      @?= Resource.ResourceAny

common_quota_helpers :: TestTree
common_quota_helpers =
  testCase "Kafka.Common.Quota: ClientQuotaEntity + filter helpers" $ do
    let e = Quota.clientQuotaEntity
              [ ("user", Just "alice")
              , ("client-id", Nothing)
              ]
    Map.size (Quota.cqeEntries e) @?= 2
    let c = Quota.exactMatch "user" "alice"
    Quota.cqfcMatchType c @?= Quota.MatchExact "alice"
    let d = Quota.defaultEntity "user"
    Quota.cqfcMatchType d @?= Quota.MatchDefault

timeextractor_use_partition_time :: TestTree
timeextractor_use_partition_time =
  testCase
    "Kafka.Streams.Time.usePartitionTimeOnInvalidTimestamp: fall back to stream time on -1"
    $ do
        let ex :: TE T.Text T.Text
            ex = usePartitionTimeOnInvalidTimestamp
        -- Valid embedded timestamp ⇒ returned unchanged.
        r1 <- runTimestampExtractor ex
                (Just "k")
                ("x" :: T.Text)
                (Timestamp 100)
                initialStreamTime
        r1 @?= Timestamp 100
        -- Sentinel -1 + known stream time ⇒ stream time.
        r2 <- runTimestampExtractor ex
                (Just "k")
                ("x" :: T.Text)
                (Timestamp (-1))
                (StreamTime (Timestamp 42))
        r2 @?= Timestamp 42

-- Local type-alias so the @ScopedTypeVariables@-flavoured signature
-- in 'timeextractor_use_partition_time' reads cleanly.
type TE k v = Kafka.Streams.Time.TimestampExtractor k v
