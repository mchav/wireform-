{-# LANGUAGE OverloadedStrings #-}
module Test.YAML.Roundtrip (tests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog (Property, forAll, property, tripping, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import YAML.Decode (decode)
import YAML.Encode (encode)
import YAML.Value

tests :: TestTree
tests = testGroup "roundtrip properties"
  [ testProperty "scalar roundtrip" prop_scalarRoundtrip
  , testProperty "value roundtrip"  prop_valueRoundtrip
  ]

genScalar :: H.Gen Value
genScalar = Gen.choice
  [ pure YNull
  , YBool  <$> Gen.bool
  , YInt   <$> Gen.integral (Range.linearFrom 0 (-1_000_000) 1_000_000)
  , YString <$> genSafeString
  ]

genSafeString :: H.Gen T.Text
genSafeString = Gen.text (Range.linear 1 32) (Gen.filter ok Gen.unicode)
  where
    ok c = c /= '\NUL' && c /= '\r' && c /= '\n' && c /= '\t'

genValue :: Int -> H.Gen Value
genValue 0 = genScalar
genValue n = Gen.choice
  [ genScalar
  , YSeq . V.fromList <$> Gen.list (Range.linear 0 5) (genValue (n - 1))
  , YMap . V.fromList <$> Gen.list (Range.linear 0 5) genKV
  ]
  where
    genKV = (,) <$> (YString <$> genSafeKey) <*> genValue (n - 1)
    genSafeKey = Gen.text (Range.linear 1 8) (Gen.element ['a'..'z'])

prop_scalarRoundtrip :: Property
prop_scalarRoundtrip = property $ do
  v <- forAll genScalar
  tripping v encode decode

prop_valueRoundtrip :: Property
prop_valueRoundtrip = property $ do
  v <- forAll (genValue 3)
  tripping v encode decode
