{-# LANGUAGE OverloadedStrings #-}

module Test.XML.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified XML.Class as X
import qualified XML.Value as XV

import Test.XML.Derive.Instances ()
import Test.XML.Derive.Types

tests :: TestTree
tests = testGroup "XML.Derive"
  [ recordTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "userId emitted as attribute, others as child elements" $ do
      case X.toXML (User 7 "Alice" "a@x") of
        XV.Element nm attrs cs -> do
          XV.nameLocal nm @?= "User"
          assertBool "id attribute present"
            (V.any (attrIs "id" "7") attrs)
          assertBool "user-name child present"
            (V.any (childIs "user-name") cs)
          assertBool "email child present (StripPrefix + kebab)"
            (V.any (childIs "email") cs)
          -- userId must NOT appear as a child
          assertBool "userId NOT a child element"
            (not (V.any (childIs "id") cs))
        v -> fail ("expected Element, got " ++ show v)

  , testCase "round-trip User" $ do
      let u = User 9 "Bob" "b@x"
      X.fromXML (X.toXML u) @?= Right u

  , testCase "round-trip Status (all-element record)" $ do
      let s = Status 200 "ok"
      X.fromXML (X.toXML s) @?= Right s
  ]
  where
    attrIs name val (XV.Attribute (XV.Name local _ _) v) =
      local == name && v == val
    childIs name (XV.Element (XV.Name local _ _) _ _) = local == name
    childIs _ _ = False

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"   $ X.toXML Red @?=
      XV.Element (XV.simpleName "red") V.empty V.empty
  , testCase "Green" $ X.toXML Green @?=
      XV.Element (XV.simpleName "green") V.empty V.empty
  , testCase "round-trip" $ mapM_ rt [Red, Green, Blue]
  ]
  where
    rt :: Color -> IO ()
    rt c = X.fromXML (X.toXML c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary)" $
      X.toXML Origin @?=
        XV.Element (XV.simpleName "origin") V.empty V.empty
  , testCase "Square 5 (1 child)" $
      X.toXML (Square 5) @?=
        XV.Element (XV.simpleName "square")
                   V.empty
                   (V.singleton (XV.Text "5"))
  , testCase "round-trip" $ do
      X.fromXML (X.toXML Origin)      @?= Right Origin
      X.fromXML (X.toXML (Square 42)) @?= Right (Square 42)
  ]
