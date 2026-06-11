{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Aeson (tests) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Test.Derive.Aeson.Instances ()
import Test.Derive.Aeson.Types
import Test.Syd


tests :: Spec
tests =
  describe "Aeson deriver" $
    sequence_
      [ recordTests
      , newtypeTests
      , enumTests
      , sumTests
      ]


-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "encode applies rename / renameStyle" $ do
          let a = Address "1 Main" "Springfield" "12345" "secret"
          let v = A.toJSON a
          case v of
            A.Object o -> do
              KM.lookup (Key.fromText "street") o `shouldBe` Just (A.String "1 Main")
              KM.lookup (Key.fromText "addr_city") o `shouldBe` Just (A.String "Springfield")
              KM.lookup (Key.fromText "zip") o `shouldBe` Just (A.String "12345")
              (not (KM.member (Key.fromText "addrInternal") o)) `shouldBe` True
            _ -> expectationFailure "expected JSON object"
      , it "decode round-trips (skipped field filled by defaults)" $ do
          let a = Address "1 Main" "Springfield" "12345" "secret"
          case A.fromJSON (A.toJSON a) of
            A.Success a' -> do
              addrStreet a' `shouldBe` addrStreet a
              addrCity a' `shouldBe` addrCity a
              addrZip a' `shouldBe` addrZip a
              addrInternal a' `shouldBe` defaultAddrInternal
            A.Error e -> expectationFailure ("decode failed: " ++ e)
      ]


-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "encode passes through" $
          A.toJSON (UserId 42) `shouldBe` A.Number 42
      , it "round-trip" $ do
          case A.fromJSON (A.toJSON (UserId 7)) of
            A.Success (UserId n) -> n `shouldBe` 7
            A.Error e -> expectationFailure e
      ]


-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it
          "encode literal-renamed constructor"
          (A.toJSON Red `shouldBe` A.String "red")
      , it
          "encode style-renamed constructor (DarkBlue -> dark-blue)"
          (A.toJSON DarkBlue `shouldBe` A.String "dark-blue")
      , it "round-trip Red" $
          A.fromJSON (A.String "red") `shouldBe` A.Success Red
      , it "round-trip Green" $
          A.fromJSON (A.String "green") `shouldBe` A.Success Green
      , it "round-trip DarkBlue" $
          A.fromJSON (A.String "dark-blue") `shouldBe` A.Success DarkBlue
      , it "unknown value fails" $ do
          case A.fromJSON (A.String "purple") :: A.Result Color of
            A.Error _ -> pure ()
            A.Success c -> expectationFailure ("unexpected " ++ show c)
      ]


-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Point  -> tag/contents (null payload)" $
          A.toJSON Point
            `shouldBe` A.object
              [ Key.fromText "tag" A..= A.String "point"
              , Key.fromText "contents" A..= A.Null
              ]
      , it "Circle -> tag/contents (single payload)" $
          A.toJSON (Circle 1.5)
            `shouldBe` A.object
              [ Key.fromText "tag" A..= A.String "circle"
              , Key.fromText "contents" A..= A.Number 1.5
              ]
      , it "Rect (renameStyle SnakeCase) -> tag = \"rect\", array contents" $ do
          let v = A.toJSON (Rect 2 3)
          case v of
            A.Object o -> do
              KM.lookup (Key.fromText "tag") o
                `shouldBe` Just (A.String "rect")
              KM.lookup (Key.fromText "contents") o
                `shouldBe` Just (A.toJSON ([A.Number 2, A.Number 3] :: [A.Value]))
            _ -> expectationFailure "expected JSON object"
      , it "round-trip Point" $ rt Point
      , it "round-trip Circle" $ rt (Circle 2.5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                A.object
                  [ Key.fromText "tag" A..= A.String "triangle"
                  , Key.fromText "contents" A..= A.Null
                  ]
          case A.fromJSON bad :: A.Result Shape of
            A.Error _ -> pure ()
            A.Success s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = case A.fromJSON (A.toJSON s) of
      A.Success s' -> s' `shouldBe` s
      A.Error e -> expectationFailure ("round-trip failed: " ++ e)
