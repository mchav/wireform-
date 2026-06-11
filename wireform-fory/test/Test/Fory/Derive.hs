{-# LANGUAGE OverloadedStrings #-}

module Test.Fory.Derive (tests) where

import Data.Vector qualified as V
import Fory.Class qualified as F
import Fory.Value qualified as VV
import Test.Fory.Derive.Instances ()
import Test.Fory.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "Fory.Derive" $
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
          case F.toFory p of
            VV.StructVal _ _ kvs -> do
              (V.elem ("name", VV.StringVal "Alice") kvs) `shouldBe` True
              (V.any (keyIs "profile_age") kvs) `shouldBe` True
              (V.any (keyIs "email") kvs) `shouldBe` True
              (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
            v -> expectationFailure ("expected StructVal, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case F.fromFory (F.toFory p) of
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
          F.toFory (Tag 42) `shouldBe` VV.VarInt64Val 42
      , it "round-trip" $
          F.fromFory (F.toFory (Tag 7)) `shouldBe` Right (Tag 7)
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $ F.toFory Red `shouldBe` VV.StringVal "red"
      , it "DarkBlue" $ F.toFory DarkBlue `shouldBe` VV.StringVal "dark-blue"
      , it "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
      , it "unknown fails" $
          case F.fromFory (VV.StringVal "purple") :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = F.fromFory (F.toFory c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary) -> tag/contents=None" $
          case F.toFory Origin of
            VV.StructVal _ _ kvs -> do
              V.elem ("tag", VV.StringVal "origin") kvs `shouldBe` True
              V.elem ("contents", VV.NoneVal) kvs `shouldBe` True
            v -> expectationFailure ("expected StructVal, got " ++ show v)
      , it "Circle (unary) -> contents = inner value" $
          case F.toFory (Circle 1.5) of
            VV.StructVal _ _ kvs -> do
              V.elem ("tag", VV.StringVal "circle") kvs `shouldBe` True
              V.elem ("contents", VV.Float64Val 1.5) kvs `shouldBe` True
            v -> expectationFailure ("expected StructVal, got " ++ show v)
      , it "Rect (n-ary) -> contents = ListVal" $
          case F.toFory (Rect 2 3) of
            VV.StructVal _ _ kvs -> do
              V.elem ("tag", VV.StringVal "rect") kvs `shouldBe` True
              ( V.any
                  ( \(k, v) ->
                      k == "contents"
                        && v == VV.ListVal (V.fromList [VV.Float64Val 2, VV.Float64Val 3])
                  )
                  kvs
                )
                `shouldBe` True
            v -> expectationFailure ("expected StructVal, got " ++ show v)
      , it "round-trip Origin" $ rt Origin
      , it "round-trip Circle" $ rt (Circle 5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                VV.StructVal
                  "x"
                  "Shape"
                  (V.fromList [("tag", VV.StringVal "ellipse")])
          case F.fromFory bad :: Either String Shape of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = F.fromFory (F.toFory s) `shouldBe` Right s
