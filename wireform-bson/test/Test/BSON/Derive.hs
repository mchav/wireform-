{-# LANGUAGE OverloadedStrings #-}

module Test.BSON.Derive (tests) where

import BSON.Class qualified as B
import BSON.Value qualified as BV
import Data.Vector qualified as V
import Test.BSON.Derive.Instances ()
import Test.BSON.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "BSON.Derive" $
    sequence_
      [ recordTests
      , newtypeTests
      , enumTests
      , sumTests
      ]


recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "encode applies rename + renameStyle, drops skipped" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case B.toBSON p of
            BV.Document kvs -> do
              (V.elem ("name", BV.String "Alice") kvs) `shouldBe` True
              (V.any (keyIs "profile_age") kvs) `shouldBe` True
              (V.any (keyIs "email") kvs) `shouldBe` True
              (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
            v -> expectationFailure ("expected Document, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case B.fromBSON (B.toBSON p) of
            Right p' -> do
              profileName p' `shouldBe` profileName p
              profileAge p' `shouldBe` profileAge p
              profileEmail p' `shouldBe` profileEmail p
              profilePrivate p' `shouldBe` defaultPrivate
            Left e -> expectationFailure e
      ]
  where
    keyIs t (k, _) = k == t


newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "pass-through" $
          B.toBSON (Tag 42) `shouldBe` BV.Int32 42
      , it "round-trip" $
          B.fromBSON (B.toBSON (Tag 7)) `shouldBe` Right (Tag 7)
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $ B.toBSON Red `shouldBe` BV.String "red"
      , it "DarkBlue" $ B.toBSON DarkBlue `shouldBe` BV.String "dark-blue"
      , it "round-trip" $ do
          mapM_ rt [Red, Green, DarkBlue]
      , it "unknown fails" $
          case B.fromBSON (BV.String "purple") :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = B.fromBSON (B.toBSON c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary) -> tag/contents=Null" $
          B.toBSON Origin
            `shouldBe` BV.Document
              ( V.fromList
                  [ ("tag", BV.String "origin")
                  , ("contents", BV.Null)
                  ]
              )
      , it "Circle (unary)   -> contents = inner value" $
          B.toBSON (Circle 1.5)
            `shouldBe` BV.Document
              ( V.fromList
                  [ ("tag", BV.String "circle")
                  , ("contents", BV.Double 1.5)
                  ]
              )
      , it "Rect   (n-ary)   -> contents = Array" $
          B.toBSON (Rect 2 3)
            `shouldBe` BV.Document
              ( V.fromList
                  [ ("tag", BV.String "rect")
                  ,
                    ( "contents"
                    , BV.Array (V.fromList [BV.Double 2, BV.Double 3])
                    )
                  ]
              )
      , it "round-trip Origin" $ rt Origin
      , it "round-trip Circle" $ rt (Circle 2.5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                BV.Document
                  ( V.fromList
                      [ ("tag", BV.String "ellipse")
                      , ("contents", BV.Null)
                      ]
                  )
          case B.fromBSON bad :: Either String Shape of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = B.fromBSON (B.toBSON s) `shouldBe` Right s
