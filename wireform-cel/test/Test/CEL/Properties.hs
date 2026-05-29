{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests (Hedgehog) for invariants that should hold across a
-- wide range of inputs: that integer arithmetic agrees with exact 'Integer'
-- arithmetic (overflowing exactly when the mathematical result leaves the
-- 64-bit range), that numeric ordering agrees with 'compare', and that string
-- @size@ counts code points.
module Test.CEL.Properties (tests) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import CEL

i64 :: Gen Int64
i64 = Gen.integral (Range.constantFrom 0 minBound maxBound)

lit :: Int64 -> Text
lit = T.pack . show

inI64 :: Integer -> Bool
inI64 n = n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Int64)

tests :: TestTree
tests =
  testGroup
    "properties"
    [ testProperty "int addition agrees with Integer / overflows exactly" prop_add
    , testProperty "int subtraction agrees with Integer / overflows exactly" prop_sub
    , testProperty "int ordering agrees with compare" prop_lt
    , testProperty "int equality is reflexive" prop_eq_refl
    , testProperty "string size counts code points" prop_strsize
    ]

prop_add :: Property
prop_add = property $ do
  a <- forAll i64
  b <- forAll i64
  let expected = toInteger a + toInteger b
      result = run emptyEnv (lit a <> " + " <> lit b)
  if inI64 expected
    then result === Right (VInt (fromInteger expected))
    else assert (isLeft result)

prop_sub :: Property
prop_sub = property $ do
  a <- forAll i64
  b <- forAll i64
  let expected = toInteger a - toInteger b
      result = run emptyEnv (lit a <> " - " <> lit b)
  if inI64 expected
    then result === Right (VInt (fromInteger expected))
    else assert (isLeft result)

prop_lt :: Property
prop_lt = property $ do
  a <- forAll i64
  b <- forAll i64
  run emptyEnv (lit a <> " < " <> lit b) === Right (VBool (a < b))

prop_eq_refl :: Property
prop_eq_refl = property $ do
  a <- forAll i64
  run emptyEnv (lit a <> " == " <> lit a) === Right (VBool True)

prop_strsize :: Property
prop_strsize = property $ do
  s <- forAll (Gen.text (Range.linear 0 40) Gen.alphaNum)
  run emptyEnv ("'" <> s <> "'.size()") === Right (VInt (fromIntegral (T.length s)))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
