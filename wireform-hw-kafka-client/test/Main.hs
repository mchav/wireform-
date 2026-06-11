module Main (main) where

import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Kafka.Consumer
import Kafka.Consumer qualified as Consumer
import Kafka.Dump
import Kafka.Metadata
import Kafka.Producer
import Kafka.Producer qualified as Producer
import Kafka.Transaction
import System.IO (Handle)
import Test.Syd


main :: IO ()
main = sydTest tests


tests :: Spec
tests =
  describe "wireform-hw-kafka-client" $
    sequence_
      [ it "producer properties are right-biased" $ do
          let props =
                Producer.brokersList ["old:9092"]
                  <> Producer.brokersList ["new:9092"]
                  <> Producer.logLevel KafkaLogInfo
                  <> Producer.logLevel KafkaLogDebug
          Producer.ppKafkaProps props `shouldBe` Map.singleton "bootstrap.servers" "new:9092"
          Producer.ppLogLevel props `shouldBe` Just KafkaLogDebug
      , it "consumer properties are right-biased" $ do
          let props =
                Consumer.groupId "old-group"
                  <> Consumer.groupId "new-group"
                  <> Consumer.noAutoCommit
          Consumer.cpProps props
            `shouldBe` Map.fromList
              [ ("enable.auto.commit", "false")
              , ("group.id", "new-group")
              ]
      , it "subscription combines topics and offset reset" $ do
          let Subscription topicSet props =
                Consumer.topics ["orders"] <> Consumer.offsetReset Earliest
          topicSet `shouldBe` Map.keysSet (Map.singleton (TopicName "orders") ())
          props `shouldBe` Map.singleton "auto.offset.reset" "earliest"
      , it "consumer record bifunctor maps key and value" $ do
          let record =
                ConsumerRecord
                  { crTopic = "orders"
                  , crPartition = PartitionId 0
                  , crOffset = Offset 12
                  , crTimestamp = NoTimestamp
                  , crHeaders = mempty
                  , crKey = Just (1 :: Int)
                  , crValue = Just (2 :: Int)
                  }
          bimap (fmap (+ 1)) (fmap (* 10)) record
            `shouldBe` record {crKey = Just 2, crValue = Just 20}
      , it "producer record keeps hw-kafka field shape" $ do
          let record =
                ProducerRecord
                  { prTopic = "events"
                  , prPartition = SpecifiedPartition 1
                  , prKey = Just "k"
                  , prValue = Just "v"
                  , prHeaders = headersFromList [("h", "x")]
                  }
          headersToList (prHeaders record) `shouldBe` [(BS.pack [104], BS.pack [120])]
      , it "producer error callback fires on create failure" $ do
          seen <- newIORef []
          result <-
            Producer.newProducer
              ( Producer.brokersList ["not-a-host-port"]
                  <> Producer.setCallback
                    (Producer.errorCallback (\err msg -> modifyIORef' seen ((err, msg) :)))
              )
          case result of
            Left _ -> pure ()
            Right producer -> do
              Producer.closeProducer producer
              expectationFailure "newProducer unexpectedly succeeded"
          callbacks <- readIORef seen
          case callbacks of
            [(KafkaError errText, msg)] -> do
              T.isInfixOf "Failed to parse broker addresses" errText `shouldBe` True
              T.isInfixOf "Failed to parse broker addresses" (T.pack msg) `shouldBe` True
            other -> expectationFailure ("unexpected callbacks: " <> show other)
      , it "metadata and transaction compatibility modules return typed errors" $ do
          txResult <- commitTransaction undefined (Timeout 0)
          case fmap getKafkaError txResult of
            Just (KafkaBadSpecification _) -> pure ()
            other -> expectationFailure ("unexpected transaction result: " <> show other)
          let _metadataType :: Either KafkaError KafkaMetadata
              _metadataType = Left (KafkaBadSpecification "compile")
          pure () :: IO ()
      , it "dump module imports and pure helpers typecheck" $ do
          let _printSupported :: Handle -> IO ()
              _printSupported = hPrintSupportedKafkaConf
          pure () :: IO ()
      ]
