{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Property tests for the low-level Fory encoding primitives.
module Test.Fory.Encoding (tests) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Fory.Encoding as E

roundTripVaruint32 :: Word32 -> Either String Word32
roundTripVaruint32 w = do
  let bs = E.runBuilder (E.varuint32 w)
  (v, off) <- E.readVaruint32 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

roundTripVaruint64 :: Word64 -> Either String Word64
roundTripVaruint64 w = do
  let bs = E.runBuilder (E.varuint64 w)
  (v, off) <- E.readVaruint64 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

roundTripVarint32 :: Int32 -> Either String Int32
roundTripVarint32 n = do
  let bs = E.runBuilder (E.varint32 n)
  (v, off) <- E.readVarint32 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

roundTripVarint64 :: Int64 -> Either String Int64
roundTripVarint64 n = do
  let bs = E.runBuilder (E.varint64 n)
  (v, off) <- E.readVarint64 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

roundTripTaggedInt64 :: Int64 -> Either String Int64
roundTripTaggedInt64 n = do
  let bs = E.runBuilder (E.taggedInt64 n)
  (v, off) <- E.readTaggedInt64 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

roundTripTaggedUint64 :: Word64 -> Either String Word64
roundTripTaggedUint64 n = do
  let bs = E.runBuilder (E.taggedUint64 n)
  (v, off) <- E.readTaggedUint64 bs 0
  if off == BS.length bs
    then Right v
    else Left "trailing bytes"

tests :: TestTree
tests = testGroup "Fory.Encoding"
  [ testProperty "varuint32 round-trip" $ H.property $ do
      w <- H.forAll (Gen.word32 Range.linearBounded)
      roundTripVaruint32 w H.=== Right w
  , testProperty "varuint64 round-trip" $ H.property $ do
      w <- H.forAll (Gen.word64 Range.linearBounded)
      roundTripVaruint64 w H.=== Right w
  , testProperty "varint32 round-trip" $ H.property $ do
      n <- H.forAll (Gen.int32 Range.linearBounded)
      roundTripVarint32 n H.=== Right n
  , testProperty "varint64 round-trip" $ H.property $ do
      n <- H.forAll (Gen.int64 Range.linearBounded)
      roundTripVarint64 n H.=== Right n
  , testProperty "tagged int64 round-trip" $ H.property $ do
      n <- H.forAll (Gen.int64 Range.linearBounded)
      roundTripTaggedInt64 n H.=== Right n
  , testProperty "tagged uint64 round-trip" $ H.property $ do
      n <- H.forAll (Gen.word64 Range.linearBounded)
      roundTripTaggedUint64 n H.=== Right n
  ]
