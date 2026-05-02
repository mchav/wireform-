{-# LANGUAGE OverloadedStrings #-}

module Test.ASN1.Derive (tests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified ASN1.Value as AV
import ASN1.Derive

import Test.ASN1.Derive.Instances ()
import Test.ASN1.Derive.Types

tests :: TestTree
tests = testGroup "ASN1.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  , wireRoundTripTests
  ]

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record (Person)"
  [ testCase "structure is SEQUENCE of 3 with one Tagged" $ do
      case toASN1 (Person 7 "Alice" True) of
        AV.Sequence vs -> do
          V.length vs @?= 3
          assertIsInteger 7 (vs V.! 0)
          assertIsUTF8 "Alice" (vs V.! 1)
          case vs V.! 2 of
            AV.Tagged AV.ContextSpecific 0 (AV.Boolean True) -> pure ()
            other -> assertFailure
              ("expected Tagged ContextSpecific 0 (Boolean True), got "
               ++ show other)
        v -> assertFailure ("expected Sequence, got " ++ show v)

  , testCase "round-trip via ASN.1 Value" $ do
      let p = Person 42 "Bob" False
      fromASN1 (toASN1 p) @?= Right p

  , testCase "round-trip Person { admin = True }" $ do
      let p = Person 1 "Carol" True
      fromASN1 (toASN1 p) @?= Right p
  ]

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype (Wrapper)"
  [ testCase "encodes as inner Int" $
      toASN1 (Wrapper 99) @?= AV.Integer 99
  , testCase "round-trip" $
      fromASN1 (toASN1 (Wrapper 17)) @?= Right (Wrapper 17)
  ]

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum (Color)"
  [ testCase "Red = INTEGER 0"   $ toASN1 Red   @?= AV.Integer 0
  , testCase "Green = INTEGER 1" $ toASN1 Green @?= AV.Integer 1
  , testCase "Blue = INTEGER 2"  $ toASN1 Blue  @?= AV.Integer 2
  , testCase "round-trip"        $ mapM_ rt [Red, Green, Blue]
  ]
  where
    rt :: Color -> IO ()
    rt c = fromASN1 (toASN1 c) @?= Right c

-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: TestTree
sumTests = testGroup "sum (Shape)"
  [ testCase "Origin = [CONTEXT 0] NULL" $
      toASN1 Origin @?= AV.Tagged AV.ContextSpecific 0 AV.Null

  , testCase "Square 5 = [CONTEXT 1] INTEGER 5" $
      toASN1 (Square 5) @?= AV.Tagged AV.ContextSpecific 1 (AV.Integer 5)

  , testCase "Rect 3 4 = [CONTEXT 2] SEQUENCE { 3, 4 }" $
      toASN1 (Rect 3 4)
        @?= AV.Tagged AV.ContextSpecific 2
              (AV.Sequence (V.fromList [AV.Integer 3, AV.Integer 4]))

  , testCase "round-trip" $ do
      fromASN1 (toASN1 Origin)        @?= Right Origin
      fromASN1 (toASN1 (Square 11))   @?= Right (Square 11)
      fromASN1 (toASN1 (Rect 12 34))  @?= Right (Rect 12 34)
  ]

-- ---------------------------------------------------------------------------
-- Round trip through the actual BER bytes
-- ---------------------------------------------------------------------------

wireRoundTripTests :: TestTree
wireRoundTripTests = testGroup "wire round-trip"
  [ testCase "Person via encodeASN1 / decodeASN1" $ do
      let p  = Person 1234 "Dora" True
          bs = encodeASN1 p
      assertBool "non-empty bytes" (not (null (show bs)))
      decodeASN1 bs @?= Right p

  , testCase "Wrapper via encodeASN1 / decodeASN1" $ do
      let w  = Wrapper 7
          bs = encodeASN1 w
      decodeASN1 bs @?= Right w

  , testCase "Color via encodeASN1 / decodeASN1" $
      decodeASN1 (encodeASN1 Green) @?= Right Green

  , testCase "Shape via encodeASN1 / decodeASN1" $
      decodeASN1 (encodeASN1 (Square 99)) @?= Right (Square 99)
  ]

-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

assertIsInteger :: Integer -> AV.Value -> IO ()
assertIsInteger expected = \case
  AV.Integer n | n == expected -> pure ()
  v -> assertFailure ("expected INTEGER " ++ show expected ++ ", got " ++ show v)

assertIsUTF8 :: T.Text -> AV.Value -> IO ()
assertIsUTF8 expected = \case
  AV.UTF8String t | t == expected -> pure ()
  v -> assertFailure ("expected UTF8String " ++ show expected ++ ", got " ++ show v)
