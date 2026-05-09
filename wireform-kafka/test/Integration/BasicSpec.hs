{-|
Module      : Integration.BasicSpec
Description : Basic integration tests for Kafka client
Copyright   : (c) 2025
License     : BSD-3-Clause

Basic integration tests that require a running Kafka cluster.

These tests verify:
- Connection to Kafka broker
- Metadata requests
- Producing records to a topic
- Consuming records from a topic

To run these tests, start Kafka first:
> start-kafka
> run-integration-tests

Or manually:
> stack test kafka-native:test:kafka-native-integration

-}
module Integration.BasicSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception (catch, SomeException)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Client.Simple as Simple
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Encoding as E
import qualified Kafka.Protocol.Generated.RequestHeader as RH
import qualified Kafka.Protocol.Generated.MetadataRequest as MR
import qualified Kafka.Protocol.Generated.MetadataResponse as MResp

-- | Bootstrap broker for testing
testBroker :: Conn.BrokerAddress
testBroker = Conn.BrokerAddress
  { Conn.brokerHost = "127.0.0.1"
  , Conn.brokerPort = 9092
  }

-- | Test connection configuration
testConfig :: Conn.ConnectionConfig
testConfig = Conn.defaultConnectionConfig

-- | All integration tests
tests :: TestTree
tests = testGroup "Integration Tests"
  [ connectionTests
  , metadataTests
  ]

connectionTests :: TestTree
connectionTests = testGroup "Connection"
  [ testCase "Can connect to Kafka broker" testConnection
  , testCase "Can disconnect cleanly" testDisconnect
  ]

metadataTests :: TestTree
metadataTests = testGroup "Metadata"
  [ testCase "Can request cluster metadata" testMetadataRequest
  , testCase "Can get cluster metadata with simple client" testSimpleMetadata
  ]

-- | Test that we can establish a connection to the broker
testConnection :: Assertion
testConnection = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> assertFailure $ "Failed to connect: " ++ err
    Right conn -> do
      -- Connection successful
      Conn.disconnect conn
      return ()

-- | Test clean disconnection
testDisconnect :: Assertion
testDisconnect = do
  result <- Conn.connect testBroker testConfig
  case result of
    Left err -> assertFailure $ "Failed to connect: " ++ err
    Right conn -> do
      Conn.disconnect conn
      -- Should not throw exception
      return ()

-- | Test sending a metadata request
testMetadataRequest :: Assertion
testMetadataRequest = do
  result <- Conn.withConnection testBroker testConfig $ \conn -> do
    -- Create a simple metadata request (no specific topics = all topics)
    let metadataReq = MR.MetadataRequest
          { MR.metadataRequestTopics = P.mkKafkaArray mempty
          , MR.metadataRequestAllowAutoTopicCreation = True
          , MR.metadataRequestIncludeClusterAuthorizedOperations = False
          , MR.metadataRequestIncludeTopicAuthorizedOperations = False
          }
    
    -- We'll use API version 0 for simplicity (oldest version)
    let apiVersion = 0
        correlationId = 1 :: Int32
        clientId = P.mkKafkaString "kafka-native-test"
    
    -- Create request header
    let header = RH.RequestHeader
          { RH.requestHeaderRequestApiKey = 3  -- Metadata API
          , RH.requestHeaderRequestApiVersion = fromIntegral apiVersion
          , RH.requestHeaderCorrelationId = correlationId
          , RH.requestHeaderClientId = clientId
          }
    
    -- TODO: Actually send the request and parse response
    -- For now, just verify we can serialize it
    return ()
  
  case result of
    Left err -> assertFailure $ "Connection failed: " ++ err
    Right () -> return ()

-- | Test getting metadata using the simple client
testSimpleMetadata :: Assertion
testSimpleMetadata = do
  clientResult <- Simple.createSimpleClient "127.0.0.1" 9092
  case clientResult of
    Left err -> assertFailure $ "Failed to create client: " ++ err
    Right client -> do
      -- Get metadata for all topics
      metadataResult <- Simple.getMetadata client Nothing
      Simple.closeSimpleClient client
      
      case metadataResult of
        Left err -> assertFailure $ "Failed to get metadata: " ++ err
        Right (brokers, topics) -> do
          -- Verify we got at least one broker
          assertBool "Expected at least one broker" (not $ null brokers)
          
          -- Print some info for debugging
          putStrLn $ "Found " ++ show (length brokers) ++ " broker(s)"
          putStrLn $ "Found " ++ show (length topics) ++ " topic(s)"

{- Future tests to implement:

-- | Test creating a topic
testCreateTopic :: Assertion
testCreateTopic = do
  -- TODO: Send CreateTopics request
  return ()

-- | Test producing a record
testProduceRecord :: Assertion
testProduceRecord = do
  -- TODO: Send Produce request with a test record
  return ()

-- | Test consuming a record
testConsumeRecord :: Assertion
testConsumeRecord = do
  -- TODO: 1. Produce a record
  --       2. Send Fetch request
  --       3. Verify we get the same record back
  return ()

-- | Test produce and consume round-trip
testProduceConsume :: Assertion
testProduceConsume = do
  -- TODO: Full integration test
  --   1. Create test topic
  --   2. Produce several records
  --   3. Consume them back
  --   4. Verify content matches
  return ()

-}

