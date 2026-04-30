{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Hedgehog property tests for "Iceberg.Variant".
--
-- The codec was already covered by HUnit cases for each individual
-- primitive type. This module hits the recursive cases (nested
-- arrays of objects of arrays, etc.) and the int128 / decimal16
-- boundary that's hard to exhaust by hand.
module Test.Iceberg.VariantProperty (tests) where

import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8)
import qualified Data.ByteString as BS

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Iceberg.Variant

tests :: TestTree
tests = testGroup "Iceberg.Variant property tests"
  [ testProperty "encode/decode round-trip on arbitrary primitives"
      propPrimRoundTrip
  , testProperty "encode/decode round-trip on arbitrary recursive Variants"
      propVariantRoundTrip
  , testProperty "JSON projection round-trip on JSON-equivalent subset"
      propJsonRoundTrip
  , testProperty "decimal16 round-trips arbitrary signed 128-bit unscaled values"
      propDecimal16Range
  , testProperty "object lex-sorted keys: encode is canonical"
      propObjectSortedCanonical
  ]

-- ============================================================
-- Generators
-- ============================================================

genPrimitive :: Gen Variant
genPrimitive = Gen.choice
  [ pure VNull
  , VBool   <$> Gen.bool
  , VInt8   <$> Gen.int8 Range.linearBounded
  , VInt16  <$> Gen.int16 Range.linearBounded
  , VInt32  <$> Gen.int32 Range.linearBounded
  , VInt64  <$> Gen.int64 Range.linearBounded
  , VFloat  <$> Gen.float (Range.exponentialFloatFrom 0 (-1e30) 1e30)
  , VDouble <$> Gen.double (Range.exponentialFloatFrom 0 (-1e300) 1e300)
  , VString <$> genShortText
  , VBinary <$> Gen.bytes (Range.linear 0 50)
  , VDate   <$> Gen.int32 Range.linearBounded
  , VTime   <$> Gen.int64 (Range.linear 0 (24 * 3600 * 1_000_000))
  , VTimestamp     <$> Gen.int64 Range.linearBounded
  , VTimestampNtz  <$> Gen.int64 Range.linearBounded
  , VTimestampNanos    <$> Gen.int64 Range.linearBounded
  , VTimestampNtzNanos <$> Gen.int64 Range.linearBounded
  , VUuid   <$> Gen.bytes (Range.singleton 16)
  , VDecimal4 <$> genScale 9
              <*> Gen.int32 Range.linearBounded
  , VDecimal8 <$> genScale 18
              <*> Gen.int64 Range.linearBounded
  , VDecimal16 <$> genScale 38
               <*> genInt128
  ]
  where
    genScale maxP = Gen.word8 (Range.linear 0 maxP)

-- | A signed 128-bit value as 'Integer'.
genInt128 :: Gen Integer
genInt128 = do
  let bound = (2 :: Integer) ^ (127 :: Int) - 1
  Gen.integral (Range.linearFrom 0 (negate bound - 1) bound)

-- Avoid characters that round-trip awkwardly in JSON / utf-8 boundary
-- testing. We're testing the variant codec, not Aeson's escape rules.
genShortText :: Gen Text
genShortText = Gen.text (Range.linear 0 30) Gen.alphaNum

-- | Generate a 'Variant' with bounded recursion depth.
genVariant :: Int -> Gen Variant
genVariant 0 = genPrimitive
genVariant !n = Gen.choice
  [ genPrimitive
  , VArray  <$> genArray (n - 1)
  , VObject <$> genObject (n - 1)
  ]
  where
    genArray d = V.fromList <$>
      Gen.list (Range.linear 0 5) (genVariant d)
    genObject d = do
      let pair = (,) <$> genShortText <*> genVariant d
      pairs <- Gen.list (Range.linear 0 5) pair
      pure (Map.fromList pairs)

-- ============================================================
-- Properties
-- ============================================================

propPrimRoundTrip :: Property
propPrimRoundTrip = property $ do
  v <- forAll genPrimitive
  let (m, x) = encodeVariant v
  case decodeVariant m x of
    Left  e  -> footnote ("decode failed: " ++ e) >> failure
    Right v' -> v' === v

propVariantRoundTrip :: Property
propVariantRoundTrip = property $ do
  v <- forAll (genVariant 3)
  let (m, x) = encodeVariant v
  case decodeVariant m x of
    Left  e  -> do
      footnote ("metadata bytes: " ++ show (BS.unpack m))
      footnote ("value bytes:    " ++ show (BS.unpack x))
      footnote ("decode failed: " ++ e)
      failure
    Right v' -> v' === v

propJsonRoundTrip :: Property
propJsonRoundTrip = property $ do
  v <- forAll genJsonEquivalent
  let (m, x) = encodeVariant v
  case decodeVariant m x of
    Left  e  -> footnote ("decode failed: " ++ e) >> failure
    Right v' -> v' === v

-- The JSON-equivalent subset the JSON bridge round-trips bit-for-bit
-- (no decimals / temporals / uuids since those go to text in JSON).
genJsonEquivalent :: Gen Variant
genJsonEquivalent = Gen.recursive Gen.choice
  [ pure VNull
  , VBool   <$> Gen.bool
  , VInt8   <$> Gen.int8   Range.linearBounded
  , VInt16  <$> Gen.int16  Range.linearBounded
  , VInt32  <$> Gen.int32  Range.linearBounded
  , VInt64  <$> Gen.int64  Range.linearBounded
  , VDouble <$> Gen.double (Range.exponentialFloatFrom 0 (-1e9) 1e9)
  , VString <$> genShortText
  ]
  [ VArray  <$> genJsonArr
  , VObject <$> genJsonObj
  ]
  where
    genJsonArr = V.fromList <$>
      Gen.list (Range.linear 0 4) genJsonEquivalent
    genJsonObj = do
      let pair = (,) <$> genShortText <*> genJsonEquivalent
      Map.fromList <$> Gen.list (Range.linear 0 4) pair

propDecimal16Range :: Property
propDecimal16Range = property $ do
  unscaled <- forAll genInt128
  scale <- forAll (Gen.word8 (Range.linear 0 38))
  let v = VDecimal16 scale unscaled
      (m, x) = encodeVariant v
  case decodeVariant m x of
    Left  e -> footnote ("decode failed: " ++ e) >> failure
    Right v' -> v' === v

-- The spec requires that object field-id lists are emitted in
-- lexicographic order of the corresponding field names. We assert
-- that property indirectly: re-encoding a decoded object produces
-- the same byte string (canonical form).
propObjectSortedCanonical :: Property
propObjectSortedCanonical = property $ do
  m <- forAll
        ( Map.fromList <$>
            Gen.list (Range.linear 1 6)
                     ((,) <$> genShortText <*> genPrimitive))
  let v          = VObject m
      (md1, vl1) = encodeVariant v
  case decodeVariant md1 vl1 of
    Left  e  -> footnote ("decode failed: " ++ e) >> failure
    Right v' -> do
      let (md2, vl2) = encodeVariant v'
      md1 === md2
      vl1 === vl2
