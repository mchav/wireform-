{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Client.AdminClientSpec (tests) where

import qualified Data.Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Data.Int (Int8, Int16)
import qualified Data.Vector as V
import qualified Kafka.Client.AdminClient as Admin
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeConfigsResponse as DCResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Primitives as P

-- | Test that default admin client config has reasonable values
unit_defaultConfig :: Spec
unit_defaultConfig = it "Default admin client config" $ do
  let config = Admin.defaultAdminClientConfig
  Admin.adminClientId config `shouldBe` "kafka-native-admin"
  Admin.adminRequestTimeoutMs config `shouldBe` 30000

-- | Test NewTopic creation with various configurations
prop_newTopicCreation :: Property
prop_newTopicCreation = property $ do
  name <- forAll $ Gen.text (Range.linear 1 100) Gen.alphaNum
  numPartitions <- forAll $ Gen.int32 (Range.linear 1 100)
  replFactor <- forAll $ Gen.int16 (Range.linear 1 5)
  
  let topic = Admin.NewTopic
        { Admin.ntName = name
        , Admin.ntNumPartitions = numPartitions
        , Admin.ntReplicationFactor = replFactor
        , Admin.ntConfigs = []
        }
  
  annotate $ "Topic name: " ++ show name
  annotate $ "Partitions: " ++ show numPartitions
  annotate $ "Replication factor: " ++ show replFactor
  
  Admin.ntName topic === name
  Admin.ntNumPartitions topic === numPartitions
  Admin.ntReplicationFactor topic === replFactor

-- | Test NewTopic with custom configurations
prop_newTopicWithConfigs :: Property
prop_newTopicWithConfigs = property $ do
  name <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
  retentionMs <- forAll $ Gen.int (Range.linear 1000 1000000)
  
  let configs = [("retention.ms", Data.Text.pack $ show retentionMs)]
      topic = Admin.NewTopic
        { Admin.ntName = name
        , Admin.ntNumPartitions = 3
        , Admin.ntReplicationFactor = 1
        , Admin.ntConfigs = configs
        }
  
  length (Admin.ntConfigs topic) === 1
  fst (head (Admin.ntConfigs topic)) === "retention.ms"

-- | Test TopicDescription structure
unit_topicDescription :: Spec
unit_topicDescription = it "TopicDescription structure" $ do
  let partInfo = Admin.PartitionInfo
        { Admin.piPartitionId = 0
        , Admin.piLeader = 1
        , Admin.piReplicas = [1, 2, 3]
        , Admin.piIsr = [1, 2]
        }
      
      topicDesc = Admin.TopicDescription
        { Admin.tdName = "test-topic"
        , Admin.tdInternal = False
        , Admin.tdPartitions = [partInfo]
        }
  
  Admin.tdName topicDesc `shouldBe` "test-topic"
  Admin.tdInternal topicDesc `shouldBe` False
  length (Admin.tdPartitions topicDesc) `shouldBe` 1
  
  let part = head (Admin.tdPartitions topicDesc)
  Admin.piPartitionId part `shouldBe` 0
  Admin.piLeader part `shouldBe` 1
  length (Admin.piReplicas part) `shouldBe` 3
  length (Admin.piIsr part) `shouldBe` 2

-- | Test ConsumerGroupListing structure
prop_consumerGroupListing :: Property
prop_consumerGroupListing = property $ do
  groupId <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
  isSimple <- forAll Gen.bool
  
  let listing = Admin.ConsumerGroupListing
        { Admin.cglGroupId = groupId
        , Admin.cglIsSimpleGroup = isSimple
        }
  
  Admin.cglGroupId listing === groupId
  Admin.cglIsSimpleGroup listing === isSimple

-- | Test ConsumerGroupDescription structure
unit_consumerGroupDescription :: Spec
unit_consumerGroupDescription = it "ConsumerGroupDescription structure" $ do
  let member1 = Admin.MemberDescription
        { Admin.mdMemberId = "member-1"
        , Admin.mdClientId = "client-1"
        , Admin.mdHost = "host1.example.com"
        }
      
      member2 = Admin.MemberDescription
        { Admin.mdMemberId = "member-2"
        , Admin.mdClientId = "client-2"
        , Admin.mdHost = "host2.example.com"
        }
      
      groupDesc = Admin.ConsumerGroupDescription
        { Admin.cgdGroupId = "test-group"
        , Admin.cgdState = "Stable"
        , Admin.cgdMembers = [member1, member2]
        }
  
  Admin.cgdGroupId groupDesc `shouldBe` "test-group"
  Admin.cgdState groupDesc `shouldBe` "Stable"
  length (Admin.cgdMembers groupDesc) `shouldBe` 2
  
  Admin.mdMemberId member1 `shouldBe` "member-1"
  Admin.mdClientId member2 `shouldBe` "client-2"

-- | Test ConfigResource types
unit_configResourceTypes :: Spec
unit_configResourceTypes = it "ConfigResource types" $ do
  let topicResource = Admin.ConfigResource
        { Admin.crType = Admin.ConfigResourceTopic
        , Admin.crName = "my-topic"
        }
      
      brokerResource = Admin.ConfigResource
        { Admin.crType = Admin.ConfigResourceBroker
        , Admin.crName = "1"
        }
  
  Admin.crType topicResource `shouldBe` Admin.ConfigResourceTopic
  Admin.crName topicResource `shouldBe` "my-topic"
  
  Admin.crType brokerResource `shouldBe` Admin.ConfigResourceBroker
  Admin.crName brokerResource `shouldBe` "1"

-- | Test ConfigEntry structure
prop_configEntry :: Property
prop_configEntry = property $ do
  name <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
  value <- forAll $ Gen.maybe (Gen.text (Range.linear 0 100) Gen.alphaNum)
  readOnly <- forAll Gen.bool
  isDefault <- forAll Gen.bool
  sensitive <- forAll Gen.bool
  
  let entry = Admin.ConfigEntry
        { Admin.ceName = name
        , Admin.ceValue = value
        , Admin.ceReadOnly = readOnly
        , Admin.ceIsDefault = isDefault
        , Admin.ceSensitive = sensitive
        }
  
  Admin.ceName entry === name
  Admin.ceValue entry === value
  Admin.ceReadOnly entry === readOnly
  Admin.ceIsDefault entry === isDefault
  Admin.ceSensitive entry === sensitive

-- | Test that partition count must be positive (validation test)
prop_partitionCountPositive :: Property
prop_partitionCountPositive = property $ do
  partitions <- forAll $ Gen.int32 (Range.linear 1 1000)
  
  let topic = Admin.NewTopic
        { Admin.ntName = "test"
        , Admin.ntNumPartitions = partitions
        , Admin.ntReplicationFactor = 1
        , Admin.ntConfigs = []
        }
  
  assert $ Admin.ntNumPartitions topic > 0

-- | Test that replication factor must be positive
prop_replicationFactorPositive :: Property
prop_replicationFactorPositive = property $ do
  replFactor <- forAll $ Gen.int16 (Range.linear 1 10)
  
  let topic = Admin.NewTopic
        { Admin.ntName = "test"
        , Admin.ntNumPartitions = 1
        , Admin.ntReplicationFactor = replFactor
        , Admin.ntConfigs = []
        }
  
  assert $ Admin.ntReplicationFactor topic > 0

-- | Test partition info with various ISR configurations
prop_partitionInfoISR :: Property
prop_partitionInfoISR = property $ do
  leader <- forAll $ Gen.int32 (Range.linear 0 10)
  replicaCount <- forAll $ Gen.int (Range.linear 1 5)
  
  replicas <- forAll $ Gen.list (Range.singleton replicaCount) (Gen.int32 (Range.linear 0 10))
  
  -- ISR should be a subset of replicas
  isrSize <- forAll $ Gen.int (Range.linear 1 replicaCount)
  let isr = take isrSize replicas
  
  let partInfo = Admin.PartitionInfo
        { Admin.piPartitionId = 0
        , Admin.piLeader = leader
        , Admin.piReplicas = replicas
        , Admin.piIsr = isr
        }
  
  annotate $ "Replicas: " ++ show replicas
  annotate $ "ISR: " ++ show isr
  
  -- ISR size should be <= replica count
  assert $ length (Admin.piIsr partInfo) <= length (Admin.piReplicas partInfo)

-- | Test empty consumer group (no members)
unit_emptyConsumerGroup :: Spec
unit_emptyConsumerGroup = it "Empty consumer group" $ do
  let groupDesc = Admin.ConsumerGroupDescription
        { Admin.cgdGroupId = "empty-group"
        , Admin.cgdState = "Empty"
        , Admin.cgdMembers = []
        }
  
  Admin.cgdGroupId groupDesc `shouldBe` "empty-group"
  Admin.cgdState groupDesc `shouldBe` "Empty"
  null (Admin.cgdMembers groupDesc) `shouldBe` True

----------------------------------------------------------------------
-- DescribeConfigs response decoding
--
-- We exercise 'unpackResourceResult' / 'unpackConfigEntry' /
-- 'decodeResourceTypeCode' directly with synthetic
-- 'DescribeConfigsResult' values, so the per-resource and
-- per-entry mapping logic is tested without spinning up a broker.
----------------------------------------------------------------------

mkResult
  :: Int8                                       -- ^ resourceType
  -> Data.Text.Text                             -- ^ resourceName
  -> Int16                                      -- ^ errorCode
  -> Data.Text.Text                             -- ^ errorMessage
  -> [DCResp.DescribeConfigsResourceResult]     -- ^ entries
  -> DCResp.DescribeConfigsResult
mkResult ty nm ec msg entries = DCResp.DescribeConfigsResult
  { DCResp.describeConfigsResultErrorCode    = ec
  , DCResp.describeConfigsResultErrorMessage = P.mkKafkaString msg
  , DCResp.describeConfigsResultResourceType = ty
  , DCResp.describeConfigsResultResourceName = P.mkKafkaString nm
  , DCResp.describeConfigsResultConfigs      = P.mkKafkaArray (V.fromList entries)
  }

mkEntry'
  :: Data.Text.Text                             -- ^ name
  -> Maybe Data.Text.Text                       -- ^ value (Nothing = null)
  -> Bool                                       -- ^ readOnly
  -> Bool                                       -- ^ isSensitive
  -> Int8                                       -- ^ KIP-226 ConfigSource
  -> DCResp.DescribeConfigsResourceResult
mkEntry' nm mval ro sen src = DCResp.DescribeConfigsResourceResult
  { DCResp.describeConfigsResourceResultName       = P.mkKafkaString nm
  , DCResp.describeConfigsResourceResultValue      = case mval of
      Nothing -> P.KafkaString P.Null
      Just t  -> P.mkKafkaString t
  , DCResp.describeConfigsResourceResultReadOnly      = ro
  , DCResp.describeConfigsResourceResultConfigSource  = src
  , DCResp.describeConfigsResourceResultIsSensitive   = sen
  , DCResp.describeConfigsResourceResultSynonyms      = P.mkKafkaArray V.empty
  , DCResp.describeConfigsResourceResultConfigType    = 0
  , DCResp.describeConfigsResourceResultDocumentation = P.mkKafkaString ""
  }

unit_decodeResourceTypeCode_known :: Spec
unit_decodeResourceTypeCode_known =
  it "decodeResourceTypeCode: 2/4/8 round-trip to Topic/Broker/BrokerLogger" $ do
    Admin.decodeResourceTypeCode 2 `shouldBe` Admin.ConfigResourceTopic
    Admin.decodeResourceTypeCode 4 `shouldBe` Admin.ConfigResourceBroker
    Admin.decodeResourceTypeCode 8 `shouldBe` Admin.ConfigResourceBrokerLogger

unit_decodeResourceTypeCode_unknown_falls_back :: Spec
unit_decodeResourceTypeCode_unknown_falls_back =
  it "decodeResourceTypeCode: unknown code falls back to Topic" $ do
    Admin.decodeResourceTypeCode (-1) `shouldBe` Admin.ConfigResourceTopic
    Admin.decodeResourceTypeCode 99   `shouldBe` Admin.ConfigResourceTopic

unit_unpackConfigEntry_default_value :: Spec
unit_unpackConfigEntry_default_value =
  it "unpackConfigEntry: ConfigSource=5 (DEFAULT_CONFIG) sets ceIsDefault=True" $ do
    let e = Admin.unpackConfigEntry $
              mkEntry' "retention.ms" (Just "604800000") False False 5
    Admin.ceName e      `shouldBe` "retention.ms"
    Admin.ceValue e     `shouldBe` Just "604800000"
    Admin.ceReadOnly e  `shouldBe` False
    Admin.ceIsDefault e `shouldBe` True
    Admin.ceSensitive e `shouldBe` False

unit_unpackConfigEntry_topic_override :: Spec
unit_unpackConfigEntry_topic_override =
  it "unpackConfigEntry: ConfigSource=1 (TOPIC_CONFIG) sets ceIsDefault=False" $ do
    let e = Admin.unpackConfigEntry $
              mkEntry' "cleanup.policy" (Just "compact") False False 1
    Admin.ceIsDefault e `shouldBe` False

unit_unpackConfigEntry_null_value :: Spec
unit_unpackConfigEntry_null_value =
  it "unpackConfigEntry: null KafkaString value yields ceValue=Nothing" $ do
    let e = Admin.unpackConfigEntry $
              mkEntry' "leader.replication.throttled.replicas" Nothing True False 5
    Admin.ceValue e    `shouldBe` Nothing
    Admin.ceReadOnly e `shouldBe` True

unit_unpackConfigEntry_sensitive :: Spec
unit_unpackConfigEntry_sensitive =
  it "unpackConfigEntry: sensitive flag is propagated" $ do
    let e = Admin.unpackConfigEntry $
              mkEntry' "ssl.keystore.password" (Just "redacted") False True 4
    Admin.ceSensitive e `shouldBe` True

unit_unpackResourceResult_success :: Spec
unit_unpackResourceResult_success =
  it "unpackResourceResult: errorCode=0 -> crrError=Nothing" $ do
    let r = Admin.unpackResourceResult $
              mkResult 2 "my-topic" 0 ""
                [ mkEntry' "retention.ms" (Just "604800000") False False 5
                , mkEntry' "cleanup.policy" (Just "compact")  False False 1
                ]
    Admin.crType (Admin.crrResource r) `shouldBe` Admin.ConfigResourceTopic
    Admin.crName (Admin.crrResource r) `shouldBe` "my-topic"
    Admin.crrError r                   `shouldBe` Nothing
    length (Admin.crrEntries r)        `shouldBe` 2

unit_unpackResourceResult_error_with_message :: Spec
unit_unpackResourceResult_error_with_message =
  it "unpackResourceResult: non-zero errorCode + message surface in crrError" $ do
    let r = Admin.unpackResourceResult $
              mkResult 4 "1" 41 "broker not authorized" []
    Admin.crrError r `shouldBe` Just "broker not authorized"
    Admin.crrEntries r `shouldBe` []

unit_unpackResourceResult_error_blank_message :: Spec
unit_unpackResourceResult_error_blank_message =
  it "unpackResourceResult: non-zero errorCode + blank message synthesises 'Error code N'" $ do
    let r = Admin.unpackResourceResult $
              mkResult 2 "topic-x" 3 ""    -- 3 = UNKNOWN_TOPIC_OR_PARTITION
                []
    Admin.crrError r `shouldBe` Just "Error code 3"

unit_unpackResourceResult_preserves_entry_order :: Spec
unit_unpackResourceResult_preserves_entry_order =
  it "unpackResourceResult preserves the order of entries returned by the broker" $ do
    let r = Admin.unpackResourceResult $
              mkResult 2 "ordered" 0 ""
                [ mkEntry' "a" (Just "1") False False 5
                , mkEntry' "b" (Just "2") False False 5
                , mkEntry' "c" (Just "3") False False 5
                ]
    map Admin.ceName (Admin.crrEntries r) `shouldBe` ["a", "b", "c"]

tests :: Spec
tests = describe "AdminClient (KIP-117)" $ sequence_
  [ describe "Properties" $ sequence_
      [ it "NewTopic creation" prop_newTopicCreation
      , it "NewTopic with configs" prop_newTopicWithConfigs
      , it "ConsumerGroupListing" prop_consumerGroupListing
      , it "ConfigEntry" prop_configEntry
      , it "Partition count positive" prop_partitionCountPositive
      , it "Replication factor positive" prop_replicationFactorPositive
      , it "Partition ISR subset of replicas" prop_partitionInfoISR
      ]
  , describe "Unit Tests" $ sequence_
      [ unit_defaultConfig
      , unit_topicDescription
      , unit_consumerGroupDescription
      , unit_configResourceTypes
      , unit_emptyConsumerGroup
      ]
  , describe "describeConfigs unwrap" $ sequence_
      [ unit_decodeResourceTypeCode_known
      , unit_decodeResourceTypeCode_unknown_falls_back
      , unit_unpackConfigEntry_default_value
      , unit_unpackConfigEntry_topic_override
      , unit_unpackConfigEntry_null_value
      , unit_unpackConfigEntry_sensitive
      , unit_unpackResourceResult_success
      , unit_unpackResourceResult_error_with_message
      , unit_unpackResourceResult_error_blank_message
      , unit_unpackResourceResult_preserves_entry_order
      ]
  ]

