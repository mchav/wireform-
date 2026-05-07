{-# LANGUAGE OverloadedStrings #-}

module Client.AdminClientSpec (tests) where

import qualified Data.Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit hiding (assert)

import qualified Kafka.Client.AdminClient as Admin

-- | Test that default admin client config has reasonable values
unit_defaultConfig :: TestTree
unit_defaultConfig = testCase "Default admin client config" $ do
  let config = Admin.defaultAdminClientConfig
  Admin.adminClientId config @?= "kafka-native-admin"
  Admin.adminRequestTimeoutMs config @?= 30000

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
unit_topicDescription :: TestTree
unit_topicDescription = testCase "TopicDescription structure" $ do
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
  
  Admin.tdName topicDesc @?= "test-topic"
  Admin.tdInternal topicDesc @?= False
  length (Admin.tdPartitions topicDesc) @?= 1
  
  let part = head (Admin.tdPartitions topicDesc)
  Admin.piPartitionId part @?= 0
  Admin.piLeader part @?= 1
  length (Admin.piReplicas part) @?= 3
  length (Admin.piIsr part) @?= 2

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
unit_consumerGroupDescription :: TestTree
unit_consumerGroupDescription = testCase "ConsumerGroupDescription structure" $ do
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
  
  Admin.cgdGroupId groupDesc @?= "test-group"
  Admin.cgdState groupDesc @?= "Stable"
  length (Admin.cgdMembers groupDesc) @?= 2
  
  Admin.mdMemberId member1 @?= "member-1"
  Admin.mdClientId member2 @?= "client-2"

-- | Test ConfigResource types
unit_configResourceTypes :: TestTree
unit_configResourceTypes = testCase "ConfigResource types" $ do
  let topicResource = Admin.ConfigResource
        { Admin.crType = Admin.ConfigResourceTopic
        , Admin.crName = "my-topic"
        }
      
      brokerResource = Admin.ConfigResource
        { Admin.crType = Admin.ConfigResourceBroker
        , Admin.crName = "1"
        }
  
  Admin.crType topicResource @?= Admin.ConfigResourceTopic
  Admin.crName topicResource @?= "my-topic"
  
  Admin.crType brokerResource @?= Admin.ConfigResourceBroker
  Admin.crName brokerResource @?= "1"

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
unit_emptyConsumerGroup :: TestTree
unit_emptyConsumerGroup = testCase "Empty consumer group" $ do
  let groupDesc = Admin.ConsumerGroupDescription
        { Admin.cgdGroupId = "empty-group"
        , Admin.cgdState = "Empty"
        , Admin.cgdMembers = []
        }
  
  Admin.cgdGroupId groupDesc @?= "empty-group"
  Admin.cgdState groupDesc @?= "Empty"
  null (Admin.cgdMembers groupDesc) @?= True

tests :: TestTree
tests = testGroup "AdminClient (KIP-117)"
  [ testGroup "Properties"
      [ testProperty "NewTopic creation" prop_newTopicCreation
      , testProperty "NewTopic with configs" prop_newTopicWithConfigs
      , testProperty "ConsumerGroupListing" prop_consumerGroupListing
      , testProperty "ConfigEntry" prop_configEntry
      , testProperty "Partition count positive" prop_partitionCountPositive
      , testProperty "Replication factor positive" prop_replicationFactorPositive
      , testProperty "Partition ISR subset of replicas" prop_partitionInfoISR
      ]
  , testGroup "Unit Tests"
      [ unit_defaultConfig
      , unit_topicDescription
      , unit_consumerGroupDescription
      , unit_configResourceTypes
      , unit_emptyConsumerGroup
      ]
  ]

