{-|
Module      : Main
Description : Test suite entry point for kafka-native
Copyright   : (c) 2025
License     : BSD-3-Clause

Main test suite for the kafka-native library.

Tests are organized into groups:

* Protocol tests - Round-trip serialization tests for primitives
* Compression tests - Compression/decompression verification
* Property tests - Hedgehog property-based tests

To run tests:

> stack test

-}
module Main (main) where

import Test.Tasty
import qualified Protocol.CRC32CSpec
import qualified Protocol.PrimitivesSpec
import qualified Protocol.RoundTripSpec
import qualified Protocol.RecordBatchSpec
import qualified Protocol.CompressionSpec
import qualified Protocol.Generated.SimpleRoundTripSpec
import qualified Protocol.Generated.KnownGoodSpec
import qualified Protocol.Generated.ComprehensiveSpec
import qualified Protocol.VersionHandlingSpec
import qualified Protocol.ApiVersionsSpec
import qualified Client.BatchAccumulatorSpec
import qualified Client.ConsumerConfigSpec
import qualified Client.GroupSpec
import qualified Client.MetadataSpec
import qualified Client.PartitionerSpec
import qualified Client.ProducerTimeoutSpec
import qualified Client.ProducerConsumerLifecycleSpec
import qualified Client.TransactionSpec
import qualified Client.TransactionCoordinatorSpec
import qualified Client.AdminClientSpec
import qualified Client.MockBrokerSpec
import qualified Client.MockBrokerFailureModesSpec
import qualified Client.MockBrokerAdvancedSpec
import qualified Client.MockBrokerExtSpec
import qualified Client.MockBrokerIdempotentSpec
import qualified Client.MockBrokerAdminSpec
import qualified Client.MockBrokerCoopSpec
import qualified Client.MockBrokerNetSpec
import qualified Client.MockBrokerStoreSpec
import qualified Client.MockBrokerProtoSpec
import qualified Network.AuthSpec
import qualified Network.ConnectionRetrySpec

main :: IO ()
main = do
  knownGoodTests <- Protocol.Generated.KnownGoodSpec.tests
  comprehensiveTests <- Protocol.Generated.ComprehensiveSpec.tests
  defaultMain $ tests knownGoodTests comprehensiveTests

tests :: TestTree -> TestTree -> TestTree
tests knownGoodTests comprehensiveTests = testGroup "kafka-native"
  [ protocolTests
  , generatedTests knownGoodTests comprehensiveTests
  , versionTests
  , compressionTests
  , clientTests
  , networkTests
  ]

protocolTests :: TestTree
protocolTests = testGroup "Protocol"
  [ Protocol.CRC32CSpec.spec
  , Protocol.PrimitivesSpec.tests
  , Protocol.RoundTripSpec.tests
  , Protocol.RecordBatchSpec.tests
  ]

generatedTests :: TestTree -> TestTree -> TestTree
generatedTests knownGoodTests comprehensiveTests = testGroup "Generated Messages"
  [ Protocol.Generated.SimpleRoundTripSpec.tests
  , knownGoodTests
  , comprehensiveTests
  ]

versionTests :: TestTree
versionTests = Protocol.VersionHandlingSpec.tests

compressionTests :: TestTree
compressionTests = Protocol.CompressionSpec.compressionTests

clientTests :: TestTree
clientTests = testGroup "Client"
  [ Protocol.ApiVersionsSpec.tests
  , Client.BatchAccumulatorSpec.tests
  , Client.ConsumerConfigSpec.consumerConfigSpec
  , Client.MetadataSpec.tests
  , Client.PartitionerSpec.partitionerSpec
  , Client.ProducerTimeoutSpec.tests
  , Client.ProducerConsumerLifecycleSpec.lifecycleSpec
  , Client.TransactionSpec.transactionSpec
  , Client.TransactionCoordinatorSpec.transactionCoordinatorSpec
  , Client.AdminClientSpec.tests
  , Client.GroupSpec.groupSpec
  , Client.MockBrokerSpec.tests
  , Client.MockBrokerFailureModesSpec.tests
  , Client.MockBrokerAdvancedSpec.tests
  , Client.MockBrokerExtSpec.tests
  , Client.MockBrokerIdempotentSpec.tests
  , Client.MockBrokerAdminSpec.tests
  , Client.MockBrokerCoopSpec.tests
  , Client.MockBrokerNetSpec.tests
  , Client.MockBrokerStoreSpec.tests
  , Client.MockBrokerProtoSpec.tests
  ]

networkTests :: TestTree
networkTests = testGroup "Network"
  [ Network.ConnectionRetrySpec.tests
  , Network.AuthSpec.authSpec
  ]
