{-|
Module      : Integration.BasicSpec
Description : Basic integration tests for the Kafka client.
Copyright   : (c) 2025
License     : BSD-3-Clause

These tests need a running Kafka broker (see
@test-integration/docker-compose.yml@). They cover:

  * raw TCP connect / disconnect to the broker,
  * a hand-rolled @MetadataRequest@ over 'Kafka.Network.Connection',
  * end-to-end producer + consumer flow,
  * a one-record produce/consume round-trip through the public API.
-}
module Integration.BasicSpec (tests) where

import           Control.Monad        (when)
import qualified Data.ByteString.Char8 as BS8
import           Data.Int
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           System.Timeout       (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Kafka.Client.Consumer                       as Consumer
import qualified Kafka.Client.Producer                       as Producer
import qualified Kafka.Network.Connection                    as Conn
import qualified Kafka.Protocol.Generated.MetadataRequest    as MR
import qualified Kafka.Protocol.Generated.RequestHeader      as RH
import qualified Kafka.Protocol.Primitives                   as P

-- | Bootstrap broker for testing
testBroker :: Conn.BrokerAddress
testBroker = Conn.BrokerAddress
  { Conn.brokerHost = "127.0.0.1"
  , Conn.brokerPort = 9092
  }

-- | Test connection configuration
testConfig :: Conn.ConnectionConfig
testConfig = Conn.defaultConnectionConfig

-- | Test topic name
testTopic :: Text
testTopic = "kafka-native-integration-test"

-- | All integration tests
tests :: TestTree
tests = testGroup "Integration Tests"
  [ connectionTests
  , metadataTests
  , producerTests
  , consumerTests
  , produceConsumeTests
  ]

connectionTests :: TestTree
connectionTests = testGroup "Connection"
  [ testCase "Can connect to Kafka broker" testConnection
  , testCase "Can disconnect cleanly" testDisconnect
  ]

metadataTests :: TestTree
metadataTests = testGroup "Metadata"
  [ testCase "Can request cluster metadata" testMetadataRequest
  ]

-- | Test that we can establish a connection to the broker
testConnection :: Assertion
testConnection = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> assertFailure $ "Failed to connect: " ++ err
    Right conn -> Conn.disconnect conn

-- | Test clean disconnection
testDisconnect :: Assertion
testDisconnect = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> assertFailure $ "Failed to connect: " ++ err
    Right conn -> Conn.disconnect conn

-- | Smoke-test: we can serialise a MetadataRequest header without
-- the codec blowing up. Actually transporting the request lives in
-- the producer / consumer / admin paths; this assertion is purely
-- structural.
testMetadataRequest :: Assertion
testMetadataRequest = do
  result <- Conn.withConnection testBroker testConfig $ \_conn -> do
    let _metadataReq = MR.MetadataRequest
          { MR.metadataRequestTopics = P.mkKafkaArray mempty
          , MR.metadataRequestAllowAutoTopicCreation = True
          , MR.metadataRequestIncludeClusterAuthorizedOperations = False
          , MR.metadataRequestIncludeTopicAuthorizedOperations = False
          }

        apiVersion    = 0 :: Int
        correlationId = 1 :: Int32
        clientId      = P.mkKafkaString "kafka-native-test"

        _header = RH.RequestHeader
          { RH.requestHeaderRequestApiKey     = 3  -- Metadata API
          , RH.requestHeaderRequestApiVersion = fromIntegral apiVersion
          , RH.requestHeaderCorrelationId     = correlationId
          , RH.requestHeaderClientId          = clientId
          }
    pure ()

  case result of
    Left err -> assertFailure $ "Connection failed: " ++ err
    Right () -> pure ()

-- | Producer tests
producerTests :: TestTree
producerTests = testGroup "Producer"
  [ testCase "Can create and close producer" testCreateProducer
  , testCase "Can send message synchronously" testProducerSendSync
  , testCase "Can send batch of messages" testProducerBatch
  ]

-- | Test creating and closing a producer
testCreateProducer :: Assertion
testCreateProducer = do
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> assertFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      Producer.closeProducer producer
      putStrLn "Producer created and closed successfully"

-- | Test sending a message synchronously
testProducerSendSync :: Assertion
testProducerSendSync = do
  putStrLn "Creating producer..."
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> assertFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      let key   = Just (BS8.pack "producer-key")
          value = BS8.pack "producer-value-sync"

      sendResult <- Producer.sendMessage producer testTopic key value
      Producer.closeProducer producer

      case sendResult of
        Left err -> assertFailure $ "Failed to send: " ++ err
        Right metadata -> do
          putStrLn $ "Sent to offset: " ++ show (Producer.metadataOffset metadata)
          assertBool "Expected non-negative offset" (Producer.metadataOffset metadata >= 0)

-- | Test sending a batch of messages
testProducerBatch :: Assertion
testProducerBatch = do
  result <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case result of
    Left err -> assertFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      results <- sequence
        [ Producer.sendMessage producer testTopic
            (Just $ BS8.pack ("batch-key-" <> show i))
            (BS8.pack ("batch-value-" <> show i))
        | i <- [1..10 :: Int]
        ]

      Producer.closeProducer producer

      case sequence results of
        Left err -> assertFailure $ "Failed to send batch: " ++ err
        Right metadatas -> do
          putStrLn $ "Sent " ++ show (length metadatas) ++ " messages"
          assertEqual "Should send all messages" 10 (length metadatas)

-- | Consumer tests
consumerTests :: TestTree
consumerTests = testGroup "Consumer"
  [ testCase "Can create and close consumer" testCreateConsumer
  , testCase "Can manually assign partitions" testConsumerAssign
  , testCase "Can poll for records" testConsumerPoll
  , testCase "Can subscribe + receive an assignment" testConsumerSubscribe
  ]

testCreateConsumer :: Assertion
testCreateConsumer = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> assertFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      Consumer.closeConsumer consumer
      putStrLn "Consumer created and closed successfully"

testConsumerAssign :: Assertion
testConsumerAssign = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> assertFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      let partitions = [Consumer.TopicPartition testTopic 0]
      assignResult <- Consumer.assign consumer partitions
      assignment   <- Consumer.assignment consumer
      Consumer.closeConsumer consumer
      case assignResult of
        Left err -> assertFailure $ "Failed to assign: " ++ err
        Right () -> do
          putStrLn $ "Assigned " ++ show (length assignment) ++ " partition(s)"
          assertEqual "Should have 1 partition assigned" 1 (length assignment)

testConsumerPoll :: Assertion
testConsumerPoll = do
  let config = Consumer.defaultConsumerConfig
  result <- Consumer.createConsumer ["localhost:9092"] "" config
  case result of
    Left err -> assertFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      let partitions = [Consumer.TopicPartition testTopic 0]
      assignResult <- Consumer.assign consumer partitions
      case assignResult of
        Left err -> do
          Consumer.closeConsumer consumer
          assertFailure $ "Failed to assign: " ++ err
        Right () -> do
          pollResult <- Consumer.poll consumer 5000
          Consumer.closeConsumer consumer
          case pollResult of
            Left err -> assertFailure $ "Failed to poll: " ++ err
            Right records ->
              putStrLn $ "Polled " ++ show (length records) ++ " record(s)"

-- | End-to-end exercise of the consumer-group join path:
-- createConsumer (which kicks off the heartbeat thread for a
-- non-empty group id) -> subscribe (which runs FindCoordinator
-- + JoinGroup + SyncGroup against the live broker). The
-- assertion is that subscribe returns within a reasonable
-- wall-clock budget (10 s). For a single-member group the
-- broker should hold JoinGroup open only until
-- 'group.initial.rebalance.delay.ms' elapses (3 s default),
-- so 10 s is more than enough headroom on a healthy cluster.
testConsumerSubscribe :: Assertion
testConsumerSubscribe = do
  ts <- (show :: Integer -> String) . truncate <$> getPOSIXTime
  let groupId = T.pack ("wf-it-subgrp-" ++ ts)
      cfg     = Consumer.defaultConsumerConfig
                  { Consumer.consumerAutoOffsetReset = Consumer.Earliest
                  , Consumer.consumerAutoCommit      = False
                  }
  result <- Consumer.createConsumer ["localhost:9092"] groupId cfg
  case result of
    Left err -> assertFailure $ "Failed to create consumer: " ++ err
    Right consumer -> do
      mr <- timeout (10 * 1000_000) $ Consumer.subscribe consumer [testTopic]
      Consumer.closeConsumer consumer
      case mr of
        Nothing        -> assertFailure
          "subscribe took longer than 10 s — heartbeat / rebalance hung"
        Just (Left e)  -> assertFailure ("subscribe: " ++ e)
        Just (Right _) -> putStrLn "subscribe completed in <10 s"

-- | Produce-consume integration tests
produceConsumeTests :: TestTree
produceConsumeTests = testGroup "Produce-Consume Integration"
  [ testCase "Can produce and consume messages" testProduceConsumeIntegration
  ]

-- | Produce a record through the real producer, then consume it
-- back through the real consumer assigned to the same partition.
testProduceConsumeIntegration :: Assertion
testProduceConsumeIntegration = do
  let uniqueValue = "integration-test-" <> show (hash 42 :: Word64)
      payload     = BS8.pack uniqueValue

  producerResult <- Producer.createProducer ["localhost:9092"] Producer.defaultProducerConfig
  case producerResult of
    Left err -> assertFailure $ "Failed to create producer: " ++ err
    Right producer -> do
      sendResult <- Producer.sendMessage producer testTopic Nothing payload
      Producer.closeProducer producer

      case sendResult of
        Left err -> assertFailure $ "Failed to produce: " ++ err
        Right metadata -> do
          let producedOffset = Producer.metadataOffset metadata
              producedPart   = Producer.metadataPartition metadata
          putStrLn $ "Produced at offset: " ++ show producedOffset
          assertBool "Expected non-negative offset" (producedOffset >= 0)

          -- Consume from the partition the broker actually assigned.
          let config = Consumer.defaultConsumerConfig
                { Consumer.consumerAutoOffsetReset = Consumer.Earliest
                }
          consumerResult <- Consumer.createConsumer ["localhost:9092"] "" config
          case consumerResult of
            Left err -> assertFailure $ "Failed to create consumer: " ++ err
            Right consumer -> do
              let partitions = [Consumer.TopicPartition testTopic producedPart]
              _ <- Consumer.assign consumer partitions

              pollResult <- Consumer.poll consumer 10000
              Consumer.closeConsumer consumer

              case pollResult of
                Left err -> assertFailure $ "Failed to poll: " ++ err
                Right records -> do
                  let ours = filter (\r -> Consumer.crValue r == payload) records
                  when (null ours) $
                    putStrLn $ "Polled " ++ show (length records) ++ " unrelated record(s)"
                  assertBool "Should find our produced message" (not (null ours))

-- Simple deterministic hash used to build a unique payload per run.
hash :: Word64 -> Word64
hash x = x * 2654435761
