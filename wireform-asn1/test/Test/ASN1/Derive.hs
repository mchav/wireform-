{-# LANGUAGE OverloadedStrings #-}

module Test.ASN1.Derive (tests) where

import ASN1.Derive
import ASN1.Value qualified as AV
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.ASN1.Derive.Instances ()
import Test.ASN1.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "ASN1.Derive" $
    sequence_
      [ recordTests
      , newtypeTests
      , enumTests
      , sumTests
      , wireRoundTripTests
      ]


-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests =
  describe "record (Person)" $
    sequence_
      [ it "structure is SEQUENCE of 3 with one Tagged" $ do
          case toASN1 (Person 7 "Alice" True) of
            AV.Sequence vs -> do
              V.length vs `shouldBe` 3
              assertIsInteger 7 (vs V.! 0)
              assertIsUTF8 "Alice" (vs V.! 1)
              case vs V.! 2 of
                AV.Tagged AV.ContextSpecific 0 (AV.Boolean True) -> pure ()
                other ->
                  expectationFailure
                    ( "expected Tagged ContextSpecific 0 (Boolean True), got "
                        ++ show other
                    )
            v -> expectationFailure ("expected Sequence, got " ++ show v)
      , it "round-trip via ASN.1 Value" $ do
          let p = Person 42 "Bob" False
          fromASN1 (toASN1 p) `shouldBe` Right p
      , it "round-trip Person { admin = True }" $ do
          let p = Person 1 "Carol" True
          fromASN1 (toASN1 p) `shouldBe` Right p
      ]


-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests =
  describe "newtype (Wrapper)" $
    sequence_
      [ it "encodes as inner Int" $
          toASN1 (Wrapper 99) `shouldBe` AV.Integer 99
      , it "round-trip" $
          fromASN1 (toASN1 (Wrapper 17)) `shouldBe` Right (Wrapper 17)
      ]


-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests =
  describe "enum (Color)" $
    sequence_
      [ it "Red = INTEGER 0" $ toASN1 Red `shouldBe` AV.Integer 0
      , it "Green = INTEGER 1" $ toASN1 Green `shouldBe` AV.Integer 1
      , it "Blue = INTEGER 2" $ toASN1 Blue `shouldBe` AV.Integer 2
      , it "round-trip" $ mapM_ rt [Red, Green, Blue]
      ]
  where
    rt :: Color -> IO ()
    rt c = fromASN1 (toASN1 c) `shouldBe` Right c


-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: Spec
sumTests =
  describe "sum (Shape)" $
    sequence_
      [ it "Origin = [CONTEXT 0] NULL" $
          toASN1 Origin `shouldBe` AV.Tagged AV.ContextSpecific 0 AV.Null
      , it "Square 5 = [CONTEXT 1] INTEGER 5" $
          toASN1 (Square 5) `shouldBe` AV.Tagged AV.ContextSpecific 1 (AV.Integer 5)
      , it "Rect 3 4 = [CONTEXT 2] SEQUENCE { 3, 4 }" $
          toASN1 (Rect 3 4)
            `shouldBe` AV.Tagged
              AV.ContextSpecific
              2
              (AV.Sequence (V.fromList [AV.Integer 3, AV.Integer 4]))
      , it "round-trip" $ do
          fromASN1 (toASN1 Origin) `shouldBe` Right Origin
          fromASN1 (toASN1 (Square 11)) `shouldBe` Right (Square 11)
          fromASN1 (toASN1 (Rect 12 34)) `shouldBe` Right (Rect 12 34)
      ]


-- ---------------------------------------------------------------------------
-- Round trip through the actual BER bytes
-- ---------------------------------------------------------------------------

wireRoundTripTests :: Spec
wireRoundTripTests =
  describe "wire round-trip" $
    sequence_
      [ it "Person via encodeASN1 / decodeASN1" $ do
          let p = Person 1234 "Dora" True
              bs = encodeASN1 p
          (not (null (show bs))) `shouldBe` True
          decodeASN1 bs `shouldBe` Right p
      , it "Wrapper via encodeASN1 / decodeASN1" $ do
          let w = Wrapper 7
              bs = encodeASN1 w
          decodeASN1 bs `shouldBe` Right w
      , it "Color via encodeASN1 / decodeASN1" $
          decodeASN1 (encodeASN1 Green) `shouldBe` Right Green
      , it "Shape via encodeASN1 / decodeASN1" $
          decodeASN1 (encodeASN1 (Square 99)) `shouldBe` Right (Square 99)
      ]


-- ---------------------------------------------------------------------------
-- Local helpers
-- ---------------------------------------------------------------------------

assertIsInteger :: Integer -> AV.Value -> IO ()
assertIsInteger expected = \case
  AV.Integer n | n == expected -> pure ()
  v -> expectationFailure ("expected INTEGER " ++ show expected ++ ", got " ++ show v)


assertIsUTF8 :: T.Text -> AV.Value -> IO ()
assertIsUTF8 expected = \case
  AV.UTF8String t | t == expected -> pure ()
  v -> expectationFailure ("expected UTF8String " ++ show expected ++ ", got " ++ show v)
