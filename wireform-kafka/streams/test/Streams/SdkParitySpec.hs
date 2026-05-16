{-# LANGUAGE OverloadedStrings #-}

-- | Smoke tests for the JVM-parity shims added in the SDK_PARITY
-- audit pass: the JVM-equivalence surface in 'Kafka.Client.Consumer'
-- (ConsumerRecords, OffsetAndMetadata, ConsumerGroupMetadata,
-- OffsetCommitCallback, SubscriptionPattern) and
-- 'Kafka.Streams.Processor.Mock' (the mock processor context for
-- unit-testing user processors).
module Streams.SdkParitySpec (tests) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Consumer as C
-- The JVM-equivalence shims (ConsumerRecords, OffsetAndMetadata,
-- ConsumerGroupMetadata, OffsetCommitCallback, SubscriptionPattern,
-- the consumer-overload tail) live in 'Kafka.Client.Consumer'
-- itself now; the 'SDK' alias is kept for the tests so the
-- "JVM name" call-sites read clearly.
import qualified Kafka.Client.Consumer as SDK
import qualified Data.UUID as UUID
import qualified Kafka.Client.AdminClient as Adm
import qualified Kafka.Common as Common
import qualified Kafka.Common.Acl as Acl
import qualified Kafka.Common.Quota as Quota
import qualified Kafka.Common.Resource as Resource
import qualified Kafka.Serde as Serde
import qualified Kafka.Streams.Errors as Errors
import qualified Kafka.Streams.Processor.Assignment as TA
import qualified Kafka.Streams.Window as W
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
  [ testGroup "Kafka.Client.Consumer JVM-equivalence shims"
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
  , testGroup "Kafka.Client.AdminClient long-tail RPCs (v3 audit additions)"
      [ admin_extras_value_smoke
      ]
  , testGroup "v3+: leftover gaps from v2 honest-list"
      [ unlimited_windows_smoke
      , list_serde_roundtrip
      , task_assignor_default
      , per_error_streams_exceptions
      ]
  ]

----------------------------------------------------------------------
-- Kafka.Client.Consumer JVM-equivalence shims
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

----------------------------------------------------------------------
-- AdminClient long-tail RPCs
----------------------------------------------------------------------

-- Smoke test: the public types of the new admin operations are
-- reachable and constructable. The operations themselves talk
-- to a real broker and are covered by the integration suite
-- under .github/workflows/wireform-kafka-integration.yml.
admin_extras_value_smoke :: TestTree
admin_extras_value_smoke =
  testCase "Kafka.Client.AdminClient long-tail: public value types compose" $ do
    let np = Adm.NewPartitions
          { Adm.npTopicName      = "events"
          , Adm.npTotalCount     = 6
          , Adm.npNewAssignments = Just [[0, 1, 2], [1, 2, 0], [2, 0, 1]]
          }
    Adm.npTotalCount np @?= 6
    let gl = Adm.GroupListing
          { Adm.glGroupId      = "g1"
          , Adm.glProtocolType = "consumer"
          , Adm.glState        = Just Common.GroupStable
          , Adm.glType         = Just Common.ConsumerGroup
          }
    Adm.glState gl @?= Just Common.GroupStable
    let acrOk = Adm.AclCreationResult
          { Adm.acrBinding =
              Acl.AclBinding
                (Resource.ResourcePattern Resource.ResourceTopic "t" Resource.PatternLiteral)
                (Acl.AccessControlEntry "User:alice" "*" Acl.AclRead Acl.AclAllow)
          , Adm.acrError = Nothing
          }
    Adm.acrError acrOk @?= Nothing
    let adr = Adm.AclDeletionResult { Adm.adrDeletedCount = 3, Adm.adrError = Nothing }
    Adm.adrDeletedCount adr @?= 3
    -- v3b: partition reassignment + transaction admin value types.
    let prs = Adm.PartitionReassignmentSpec "in" 0 (Just [1, 2, 3])
    Adm.prsTopic prs @?= "in"
    let opr = Adm.OngoingPartitionReassignment "in" 0 [1, 2, 3] [4] []
    Adm.oprAddingReplicas opr @?= [4]
    let tl = Adm.TransactionListing
          { Adm.tlTransactionalId = "txn-1"
          , Adm.tlProducerId      = 100
          , Adm.tlState           = "Ongoing"
          }
    Adm.tlState tl @?= "Ongoing"
    let td = Adm.TransactionDescription
          { Adm.tdTransactionalId = "txn-1"
          , Adm.tdProducerId      = 100
          , Adm.tdProducerEpoch   = 7
          , Adm.tdTimeoutMs       = 60_000
          , Adm.tdStartTimeMs     = 0
          , Adm.tdState           = "Ongoing"
          , Adm.tdTopicPartitions =
              [Adm.TransactionTopicPartitions "out" [0, 1, 2]]
          }
    map Adm.ttpTopic (Adm.tdTopicPartitions td) @?= ["out"]
    let cqe = Adm.ClientQuotaEntry
          { Adm.cqeEntity =
              Quota.clientQuotaEntity [("user", Just "alice")]
          , Adm.cqeValues = Map.fromList [("producer_byte_rate", 1024)]
          }
    Adm.cqeValues cqe @?= Map.fromList [("producer_byte_rate", 1024)]
    -- v3c: SCRAM credentials + producer / log-dir / delegation tokens.
    let sci = Adm.ScramCredentialInfo Adm.ScramSha256 4096
    Adm.sciIterations sci @?= 4096
    let scu = Adm.ScramCredentialUpsertion "alice" Adm.ScramSha512 8192 "salt" "hashed"
    Adm.scuMechanism scu @?= Adm.ScramSha512
    let ps = Adm.ProducerState 5001 1 42 0 0 (-1)
    Adm.psProducerId ps @?= 5001
    let ldd = Adm.LogDirDescription "/var/log/kafka-data" 0 (1024 * 1024) 512000 []
    Adm.lddPath ldd @?= "/var/log/kafka-data"
    let dt = Adm.DelegationToken
          { Adm.dtTokenId         = "tid"
          , Adm.dtHmac            = "secret"
          , Adm.dtOwner           = ("User", "alice")
          , Adm.dtTokenRequester  = ("User", "alice")
          , Adm.dtIssueTimestamp  = 0
          , Adm.dtExpiryTimestamp = 60000
          , Adm.dtMaxTimestamp    = 86400000
          }
    Adm.dtTokenId dt @?= "tid"

----------------------------------------------------------------------
-- v3+: leftover gaps from v2 honest-list
----------------------------------------------------------------------

unlimited_windows_smoke :: TestTree
unlimited_windows_smoke =
  testCase "unlimitedWindows assigns a single never-ending window per record" $ do
    let ws = W.unlimitedWindows
        ws_assigned = W.windowsAssign ws (Timestamp 1_000_000_000)
    length ws_assigned @?= 1

list_serde_roundtrip :: TestTree
list_serde_roundtrip =
  testCase "listSerde encodes + decodes a [Text]" $ do
    let s   = Serde.listSerde Serde.textSerde
        xs  = ["alpha", "bravo", "charlie"] :: [T.Text]
        bs  = Serde.serialize s xs
    Serde.deserialize s bs @?= Right xs
    -- empty list edge case
    Serde.deserialize s (Serde.serialize s ([] :: [T.Text])) @?= Right []

task_assignor_default :: TestTree
task_assignor_default =
  testCase "defaultTaskAssignor hands every task to the first client" $ do
    let pidA = TA.ProcessId (UUID.fromWords 0 0 0 1)
        pidB = TA.ProcessId (UUID.fromWords 0 0 0 2)
        states = Map.fromList
          [ ( pidA
            , TA.KafkaStreamsState pidA 1 Map.empty Set.empty Set.empty Nothing
            )
          , ( pidB
            , TA.KafkaStreamsState pidB 1 Map.empty Set.empty Set.empty Nothing
            )
          ]
        tasks = Map.empty
        app = TA.ApplicationState tasks states TA.defaultAssignmentConfigs
    out <- TA.taAssign TA.defaultTaskAssignor app
    Map.keys (TA.taAssignments out) @?= [pidA]

per_error_streams_exceptions :: TestTree
per_error_streams_exceptions =
  testCase "streams errors: per-Java-class discriminated constructors" $ do
    -- Catchable by their specific type now; smoke test the
    -- record / newtype shape.
    let _ = Errors.BrokerNotFoundException "no broker"
        _ = Errors.MissingSourceTopicException "events" "deleted"
        _ = Errors.TaskAssignmentException "boom"
        _ = Errors.TaskCorruptedException "0_0" "wal corrupt"
        _ = Errors.UnknownStateStoreException "counts"
        _ = Errors.LockException "/tmp/state" "held by other"
        _ = Errors.InvalidConfigurationException "bad knob"
        _ = Errors.InvalidPartitionsException "negative count"
    pure ()

-- Set is imported via the imports already, make sure to use it.
_useSet :: Set.Set ()
_useSet = Set.empty
