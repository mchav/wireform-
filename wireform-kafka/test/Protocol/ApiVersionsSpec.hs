{-# LANGUAGE OverloadedStrings #-}

module Protocol.ApiVersionsSpec (tests) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Protocol.ApiVersions qualified as AV
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Generate a broker address
genBrokerAddress :: Gen BrokerAddress
genBrokerAddress = do
  host <- Gen.string (Range.linear 5 20) Gen.alphaNum
  port <- Gen.integral (Range.linear 9000 9999)
  return $ BrokerAddress host port


-- | Generate an API version range
genApiVersionRange :: Gen AV.ApiVersionRange
genApiVersionRange = do
  minVer <- Gen.int16 (Range.linear 0 5)
  maxVer <- Gen.int16 (Range.linear minVer 20)
  return $ AV.ApiVersionRange minVer maxVer


-- | Test creating an empty version cache
unit_createVersionCache :: IO ()
unit_createVersionCache = do
  cache <- AV.createVersionCache

  -- Query for non-existent broker should return Nothing
  result <- atomically $ AV.queryApiVersion cache (BrokerAddress "localhost" 9092) 0
  result `shouldBe` Nothing


-- | Test querying a non-existent API key
prop_queryNonExistentApiKey :: Property
prop_queryNonExistentApiKey = property $ do
  cache <- evalIO AV.createVersionCache
  broker <- forAll genBrokerAddress
  apiKey <- forAll $ Gen.int16 (Range.linear 0 100)

  -- Query should return Nothing
  result <- evalIO $ atomically $ AV.queryApiVersion cache broker apiKey
  result === Nothing


-- | Test version selection
prop_selectVersion :: Property
prop_selectVersion = property $ do
  clientMax <- forAll $ Gen.int16 (Range.linear 0 20)
  brokerRange <- forAll genApiVersionRange

  let result = AV.selectVersion clientMax brokerRange

  case result of
    Nothing -> do
      -- Client version is less than broker minimum
      assert $ clientMax < AV.rangeMinVersion brokerRange
    Just selected -> do
      -- Selected version should be minimum of client and broker max
      selected === min clientMax (AV.rangeMaxVersion brokerRange)
      -- Selected version should be within broker's range
      assert $ selected >= AV.rangeMinVersion brokerRange
      assert $ selected <= AV.rangeMaxVersion brokerRange


-- | Test that selectVersion returns Nothing when client is too old
prop_selectVersionClientTooOld :: Property
prop_selectVersionClientTooOld = property $ do
  brokerMin <- forAll $ Gen.int16 (Range.linear 5 10)
  brokerMax <- forAll $ Gen.int16 (Range.linear brokerMin 20)
  clientMax <- forAll $ Gen.int16 (Range.linear 0 (brokerMin - 1))

  let range = AV.ApiVersionRange brokerMin brokerMax
      result = AV.selectVersion clientMax range

  result === Nothing


-- | Test that selectVersion prefers broker's max when client supports higher
prop_selectVersionPrefersLower :: Property
prop_selectVersionPrefersLower = property $ do
  brokerMin <- forAll $ Gen.int16 (Range.linear 0 5)
  brokerMax <- forAll $ Gen.int16 (Range.linear brokerMin 10)
  clientMax <- forAll $ Gen.int16 (Range.linear (brokerMax + 1) 20)

  let range = AV.ApiVersionRange brokerMin brokerMax
      result = AV.selectVersion clientMax range

  result === Just brokerMax


-- | Test isVersionSupported
prop_isVersionSupported :: Property
prop_isVersionSupported = property $ do
  range@(AV.ApiVersionRange minVer maxVer) <- forAll genApiVersionRange
  version <- forAll $ Gen.int16 (Range.linear 0 25)

  let supported = AV.isVersionSupported version range

  if version >= minVer && version <= maxVer
    then assert supported
    else assert $ not supported


-- | Test that versions within range are supported
prop_versionsInRangeSupported :: Property
prop_versionsInRangeSupported = property $ do
  range@(AV.ApiVersionRange minVer maxVer) <- forAll genApiVersionRange
  version <- forAll $ Gen.int16 (Range.linear minVer maxVer)

  let supported = AV.isVersionSupported version range
  assert supported


-- | Test that versions below range are not supported
prop_versionsBelowRangeNotSupported :: Property
prop_versionsBelowRangeNotSupported = property $ do
  range@(AV.ApiVersionRange minVer maxVer) <- forAll genApiVersionRange

  when (minVer > 0) $ do
    version <- forAll $ Gen.int16 (Range.linear 0 (minVer - 1))
    let supported = AV.isVersionSupported version range
    assert $ not supported


-- | Test that versions above range are not supported
prop_versionsAboveRangeNotSupported :: Property
prop_versionsAboveRangeNotSupported = property $ do
  range@(AV.ApiVersionRange minVer maxVer) <- forAll genApiVersionRange
  version <- forAll $ Gen.int16 (Range.linear (maxVer + 1) (maxVer + 10))

  let supported = AV.isVersionSupported version range
  assert $ not supported


-- | All tests for API version negotiation
tests :: Spec
tests =
  describe "ApiVersions" $
    sequence_
      [ describe "Version Cache" $
          sequence_
            [ it "Create empty cache" unit_createVersionCache
            , it "Query non-existent API key returns Nothing" prop_queryNonExistentApiKey
            ]
      , describe "Version Selection" $
          sequence_
            [ it "Select appropriate version" prop_selectVersion
            , it "Returns Nothing when client too old" prop_selectVersionClientTooOld
            , it "Prefers broker max when client higher" prop_selectVersionPrefersLower
            ]
      , describe "Version Support" $
          sequence_
            [ it "Check if version supported" prop_isVersionSupported
            , it "Versions in range are supported" prop_versionsInRangeSupported
            , it "Versions below range not supported" prop_versionsBelowRangeNotSupported
            , it "Versions above range not supported" prop_versionsAboveRangeNotSupported
            ]
      ]
