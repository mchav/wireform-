module Main (main) where

import Data.Bifunctor (bimap)
import Kafka.Consumer
import Kafka.Dump
import Kafka.Metadata
import Kafka.Producer
import Kafka.Transaction
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Kafka.Consumer as Consumer
import qualified Kafka.Producer as Producer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "wireform-hw-kafka-client"
  [ testCase "producer properties are right-biased" $ do
      let props =
            Producer.brokersList ["old:9092"]
              <> Producer.brokersList ["new:9092"]
              <> Producer.logLevel KafkaLogInfo
              <> Producer.logLevel KafkaLogDebug
      Producer.ppKafkaProps props @?= Map.singleton "bootstrap.servers" "new:9092"
      Producer.ppLogLevel props @?= Just KafkaLogDebug
  , testCase "consumer properties are right-biased" $ do
      let props =
            Consumer.groupId "old-group"
              <> Consumer.groupId "new-group"
              <> Consumer.noAutoCommit
      Consumer.cpProps props @?= Map.fromList
        [ ("enable.auto.commit", "false")
        , ("group.id", "new-group")
        ]
  , testCase "subscription combines topics and offset reset" $ do
      let Subscription topicSet props =
            Consumer.topics ["orders"] <> Consumer.offsetReset Earliest
      topicSet @?= Map.keysSet (Map.singleton (TopicName "orders") ())
      props @?= Map.singleton "auto.offset.reset" "earliest"
  , testCase "consumer record bifunctor maps key and value" $ do
      let record = ConsumerRecord
            { crTopic = "orders"
            , crPartition = PartitionId 0
            , crOffset = Offset 12
            , crTimestamp = NoTimestamp
            , crHeaders = mempty
            , crKey = Just (1 :: Int)
            , crValue = Just (2 :: Int)
            }
      bimap (fmap (+ 1)) (fmap (* 10)) record @?=
        record { crKey = Just 2, crValue = Just 20 }
  , testCase "producer record keeps hw-kafka field shape" $ do
      let record = ProducerRecord
            { prTopic = "events"
            , prPartition = SpecifiedPartition 1
            , prKey = Just "k"
            , prValue = Just "v"
            , prHeaders = headersFromList [("h", "x")]
            }
      headersToList (prHeaders record) @?= [(BS.pack [104], BS.pack [120])]
  , testCase "metadata and transaction compatibility modules return typed errors" $ do
      let txErr = getKafkaError <$> commitTransaction undefined (Timeout 0)
      txErr >>= \case
        Just (KafkaBadSpecification _) -> pure ()
        other -> fail ("unexpected transaction result: " <> show other)
      let _metadataType :: Either KafkaError KafkaMetadata
          _metadataType = Left (KafkaBadSpecification "compile")
      pure ()
  , testCase "dump module imports and pure helpers typecheck" $ do
      let _dumpKafkaConf = dumpKafkaConf
          _dumpTopicConf = dumpTopicConf
      pure ()
  ]
