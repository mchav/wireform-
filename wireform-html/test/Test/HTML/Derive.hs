{-# LANGUAGE OverloadedStrings #-}

module Test.HTML.Derive (tests) where

import qualified Data.Primitive.SmallArray as SA
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified HTML.Class as H
import qualified HTML.Value as HV

import Test.HTML.Derive.Instances ()
import Test.HTML.Derive.Types

tests :: TestTree
tests = testGroup "HTML.Derive"
  [ recordTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "userId emitted as attribute, others as child elements" $ do
      case H.toHTML (User 7 "Alice" "a@x") of
        HV.HTMLElement tag attrs cs -> do
          tag @?= "user"
          assertBool "id attribute present"
            (anyAttr (\(HV.HTMLAttribute k v) -> k == "id" && v == "7") attrs)
          assertBool "user-name child present"
            (anyChild (\case
              HV.HTMLElement t _ _ -> t == "user-name"
              _                    -> False) cs)
          assertBool "email child present"
            (anyChild (\case
              HV.HTMLElement t _ _ -> t == "email"
              _                    -> False) cs)
        v -> fail ("expected HTMLElement, got " ++ show v)

  , testCase "round-trip User" $ do
      let u = User 9 "Bob" "b@x"
      H.fromHTML (H.toHTML u) @?= Right u
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"   $ do
      case H.toHTML Red of
        HV.HTMLElement tag _ _ -> tag @?= "red"
        v                      -> fail (show v)
  , testCase "round-trip" $ do
      H.fromHTML (H.toHTML Red)   @?= Right Red
      H.fromHTML (H.toHTML Green) @?= Right Green
      H.fromHTML (H.toHTML Blue)  @?= Right Blue
  ]

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary)" $
      case H.toHTML Origin of
        HV.HTMLElement tag _ cs -> do
          tag @?= "origin"
          SA.sizeofSmallArray cs @?= 0
        v -> fail (show v)
  , testCase "Square 5" $
      case H.toHTML (Square 5) of
        HV.HTMLElement tag _ cs -> do
          tag @?= "square"
          SA.sizeofSmallArray cs @?= 1
        v -> fail (show v)
  , testCase "round-trip" $ do
      H.fromHTML (H.toHTML Origin)      @?= Right Origin
      H.fromHTML (H.toHTML (Square 42)) @?= Right (Square 42)
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
