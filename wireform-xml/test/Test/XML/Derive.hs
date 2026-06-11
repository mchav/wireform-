{-# LANGUAGE OverloadedStrings #-}

module Test.XML.Derive (tests) where

import Data.Vector qualified as V
import Test.Syd
import Test.XML.Derive.Instances ()
import Test.XML.Derive.Types
import XML.Class qualified as X
import XML.Value qualified as XV


tests :: Spec
tests =
  describe "XML.Derive" $
    sequence_
      [ recordTests
      , enumTests
      , sumTests
      ]


recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "userId emitted as attribute, others as child elements" $ do
          case X.toXML (User 7 "Alice" "a@x") of
            XV.Element nm attrs cs -> do
              XV.nameLocal nm `shouldBe` "User"
              (V.any (attrIs "id" "7") attrs) `shouldBe` True
              (V.any (childIs "user-name") cs) `shouldBe` True
              (V.any (childIs "email") cs) `shouldBe` True
              -- userId must NOT appear as a child
              (not (V.any (childIs "id") cs)) `shouldBe` True
            v -> expectationFailure ("expected Element, got " ++ show v)
      , it "round-trip User" $ do
          let u = User 9 "Bob" "b@x"
          X.fromXML (X.toXML u) `shouldBe` Right u
      , it "round-trip Status (all-element record)" $ do
          let s = Status 200 "ok"
          X.fromXML (X.toXML s) `shouldBe` Right s
      ]
  where
    attrIs name val (XV.Attribute (XV.Name local _ _) v) =
      local == name && v == val
    childIs name (XV.Element (XV.Name local _ _) _ _) = local == name
    childIs _ _ = False


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $
          X.toXML Red
            `shouldBe` XV.Element (XV.simpleName "red") V.empty V.empty
      , it "Green" $
          X.toXML Green
            `shouldBe` XV.Element (XV.simpleName "green") V.empty V.empty
      , it "round-trip" $ mapM_ rt [Red, Green, Blue]
      ]
  where
    rt :: Color -> IO ()
    rt c = X.fromXML (X.toXML c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary)" $
          X.toXML Origin
            `shouldBe` XV.Element (XV.simpleName "origin") V.empty V.empty
      , it "Square 5 (1 child)" $
          X.toXML (Square 5)
            `shouldBe` XV.Element
              (XV.simpleName "square")
              V.empty
              (V.singleton (XV.Text "5"))
      , it "round-trip" $ do
          X.fromXML (X.toXML Origin) `shouldBe` Right Origin
          X.fromXML (X.toXML (Square 42)) `shouldBe` Right (Square 42)
      ]
