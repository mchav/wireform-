{-# LANGUAGE OverloadedStrings #-}

module Test.HTML.Derive (tests) where

import qualified Data.Primitive.SmallArray as SA
import Test.Syd

import qualified HTML.Class as H
import qualified HTML.Value as HV

import Test.HTML.Derive.Instances ()
import Test.HTML.Derive.Types

tests :: Spec
tests = describe "HTML.Derive" $ sequence_
  [ recordTests
  , enumTests
  , sumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "userId emitted as attribute, others as child elements" $ do
      case H.toHTML (User 7 "Alice" "a@x") of
        HV.HTMLElement tag attrs cs -> do
          tag `shouldBe` "user"
          (anyAttr (\(HV.HTMLAttribute k v) -> k == "id" && v == "7") attrs) `shouldBe` True
          (anyChild (\case
              HV.HTMLElement t _ _ -> t == "user-name"
              _                    -> False) cs) `shouldBe` True
          (anyChild (\case
              HV.HTMLElement t _ _ -> t == "email"
              _                    -> False) cs) `shouldBe` True
        v -> expectationFailure ("expected HTMLElement, got " ++ show v)

  , it "round-trip User" $ do
      let u = User 9 "Bob" "b@x"
      H.fromHTML (H.toHTML u) `shouldBe` Right u
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "Red"   $ do
      case H.toHTML Red of
        HV.HTMLElement tag _ _ -> tag `shouldBe` "red"
        v                      -> expectationFailure (show v)
  , it "round-trip" $ do
      H.fromHTML (H.toHTML Red)   `shouldBe` Right Red
      H.fromHTML (H.toHTML Green) `shouldBe` Right Green
      H.fromHTML (H.toHTML Blue)  `shouldBe` Right Blue
  ]

sumTests :: Spec
sumTests = describe "sum" $ sequence_
  [ it "Origin (nullary)" $
      case H.toHTML Origin of
        HV.HTMLElement tag _ cs -> do
          tag `shouldBe` "origin"
          SA.sizeofSmallArray cs `shouldBe` 0
        v -> expectationFailure (show v)
  , it "Square 5" $
      case H.toHTML (Square 5) of
        HV.HTMLElement tag _ cs -> do
          tag `shouldBe` "square"
          SA.sizeofSmallArray cs `shouldBe` 1
        v -> expectationFailure (show v)
  , it "round-trip" $ do
      H.fromHTML (H.toHTML Origin)      `shouldBe` Right Origin
      H.fromHTML (H.toHTML (Square 42)) `shouldBe` Right (Square 42)
  ]

anyAttr :: (HV.HTMLAttribute -> Bool) -> SA.SmallArray HV.HTMLAttribute -> Bool
anyAttr p arr = go 0
  where
    !n = SA.sizeofSmallArray arr
    go !i | i >= n    = False
          | p (SA.indexSmallArray arr i) = True
          | otherwise = go (i + 1)

anyChild :: (HV.HTMLNode -> Bool) -> SA.SmallArray HV.HTMLNode -> Bool
anyChild p arr = go 0
  where
    !n = SA.sizeofSmallArray arr
    go !i | i >= n    = False
          | p (SA.indexSmallArray arr i) = True
          | otherwise = go (i + 1)
