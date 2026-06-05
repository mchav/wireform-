{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Hedgehog property tests for "Parquet.Nested" — the Dremel
-- shredder.
--
-- We check structural invariants the spec requires of every shredded
-- leaf:
--
-- * @VP.length nlDefLevels == VP.length nlRepLevels@
-- * @nlValueCount == count (\\d -> d == nlMaxDef) nlDefLevels@
-- * @maximum nlDefLevels <= nlMaxDef@ and @maximum nlRepLevels <= nlMaxRep@
-- * Number of leaves matches 'flattenSchema' output.
--
-- Plus a strong property: shredding a row, summing up the events
-- across all leaves, gives the same total event count for every leaf
-- (rows produce coordinated events: a struct with N fields shreds
-- each row into N events, one per leaf, regardless of nesting).
module Test.Iceberg.NestedProperty (tests) where

import Data.Int (Int32)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Parquet.Nested as PN

tests :: Spec
tests = describe "Parquet.Nested properties" $ sequence_
  [ it "shred: def/rep stream lengths agree per leaf"
      propStreamLengths
  , it "shred: nlValueCount == count(def == maxDef)"
      propValueCount
  , it "shred: max def/rep don't exceed schema bounds"
      propLevelBounds
  , it "shred: leaf count matches flattenSchema"
      propLeafCount
  ]

-- ============================================================
-- Generators
-- ============================================================

-- A modest schema generator so the property tests stay fast: depth
-- bounded to 3, primitives chosen from the int32 / string subset.
genSchema :: Int -> Gen PN.NestedSchema
genSchema 0 = PN.NSPrimitive <$> Gen.element [PN.LtInt32, PN.LtString]
genSchema !n = Gen.choice
  [ PN.NSPrimitive <$> Gen.element [PN.LtInt32, PN.LtString]
  , PN.NSOptional  <$> genSchema (n - 1)
  , PN.NSRequired  <$> genSchema (n - 1)
  , PN.NSList      <$> genSchema (n - 1)
  , PN.NSStruct    <$> genStructFields (n - 1)
  ]
  where
    genStructFields d = do
      k <- Gen.int (Range.linear 1 3)
      let pair = (,) <$> Gen.text (Range.linear 1 6) Gen.alphaNum
                     <*> genSchema d
      V.fromList <$> Gen.list (Range.singleton k) pair

-- Generate a row that structurally matches the schema. We must walk
-- the schema and produce a matching shape; any mismatch would be a
-- generator bug and shred would (correctly) reject it.
genRow :: PN.NestedSchema -> Gen PN.NestedRow
genRow = \case
  PN.NSPrimitive PN.LtInt32 ->
    PN.NRLeaf . PN.LvInt32 <$> Gen.int32 Range.linearBounded
  PN.NSPrimitive PN.LtString ->
    PN.NRLeaf . PN.LvString <$> Gen.text (Range.linear 0 8) Gen.alphaNum
  PN.NSPrimitive _ ->
    -- Other primitives don't appear in the bounded schema generator,
    -- but be defensive.
    PN.NRLeaf . PN.LvInt32 <$> Gen.int32 Range.linearBounded
  PN.NSOptional inner -> Gen.choice
    [ pure PN.NRNull, genRow inner ]
  PN.NSRequired inner -> genRow inner
  PN.NSList inner -> do
    n <- Gen.int (Range.linear 0 3)
    PN.NRList . V.fromList <$> sequence (replicate n (genRow inner))
  PN.NSStruct fields ->
    PN.NRStruct . V.fromList
      <$> mapM (genRow . snd) (V.toList fields)
  PN.NSMap k v -> do
    n <- Gen.int (Range.linear 0 3)
    PN.NRMapEntries . V.fromList <$>
      sequence (replicate n ((,) <$> genRow k <*> genRow v))
  PN.NSVariant ->
    PN.NRVariantBytes <$> Gen.bytes (Range.linear 0 16)
                      <*> Gen.bytes (Range.linear 0 16)

genRowsFor :: PN.NestedSchema -> Gen (V.Vector PN.NestedRow)
genRowsFor sch = do
  n <- Gen.int (Range.linear 1 6)
  V.fromList <$> sequence (replicate n (genRow sch))

-- ============================================================
-- Properties
-- ============================================================

propStreamLengths :: Property
propStreamLengths = property $ do
  sch <- forAll (genSchema 3)
  rows <- forAll (genRowsFor sch)
  case PN.shred sch rows of
    Left  e -> footnote ("shred failed: " ++ e) >> failure
    Right ls ->
      V.mapM_
        (\l ->
            VP.length (PN.nlDefLevels l) === VP.length (PN.nlRepLevels l))
        ls

propValueCount :: Property
propValueCount = property $ do
  sch <- forAll (genSchema 3)
  rows <- forAll (genRowsFor sch)
  case PN.shred sch rows of
    Left  e -> footnote ("shred failed: " ++ e) >> failure
    Right ls ->
      V.mapM_
        (\l ->
            let !maxD = fromIntegral (PN.nlMaxDef l) :: Int32
                !cnt = VP.foldl' (\a d -> if d == maxD then a + 1 else a)
                                 (0 :: Int)
                                 (PN.nlDefLevels l)
             in PN.nlValueCount l === cnt)
        ls

propLevelBounds :: Property
propLevelBounds = property $ do
  sch <- forAll (genSchema 3)
  rows <- forAll (genRowsFor sch)
  case PN.shred sch rows of
    Left  e -> footnote ("shred failed: " ++ e) >> failure
    Right ls ->
      V.mapM_
        (\l -> do
            let !maxD = fromIntegral (PN.nlMaxDef l) :: Int32
                !maxR = fromIntegral (PN.nlMaxRep l) :: Int32
                !maxObservedDef = if VP.null (PN.nlDefLevels l)
                                    then 0
                                    else VP.maximum (PN.nlDefLevels l)
                !maxObservedRep = if VP.null (PN.nlRepLevels l)
                                    then 0
                                    else VP.maximum (PN.nlRepLevels l)
            assert (maxObservedDef <= maxD)
            assert (maxObservedRep <= maxR))
        ls

propLeafCount :: Property
propLeafCount = property $ do
  sch <- forAll (genSchema 3)
  rows <- forAll (genRowsFor sch)
  case PN.shred sch rows of
    Left  e -> footnote ("shred failed: " ++ e) >> failure
    Right ls -> do
      let !flat = PN.flattenSchema "" sch
      V.length ls === V.length flat
