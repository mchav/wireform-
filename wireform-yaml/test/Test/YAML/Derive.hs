{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Derive (tests) where

import Data.Vector qualified as V
import Test.Syd
import Test.YAML.Derive.Instances ()
import Test.YAML.Derive.Types
import YAML.Class qualified as Y
import YAML.Value qualified as YV


tests :: Spec
tests =
  describe "YAML.Derive" $
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
          case Y.toYAML p of
            YV.YMap kvs -> do
              (V.elem (YV.YString "name", YV.YString "Alice") kvs) `shouldBe` True
              (V.any (keyIs "profile_age") kvs) `shouldBe` True
              (V.any (keyIs "email") kvs) `shouldBe` True
              (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
            v -> expectationFailure ("expected YMap, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case Y.fromYAML (Y.toYAML p) of
            Right p' -> do
              profileName p' `shouldBe` profileName p
              profileAge p' `shouldBe` profileAge p
              profileEmail p' `shouldBe` profileEmail p
              profilePrivate p' `shouldBe` defaultPrivate
            Left e -> expectationFailure e
      ]
  where
    keyIs t (YV.YString k, _) = k == t
    keyIs _ _ = False


newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "pass-through" $
          Y.toYAML (Tag 42) `shouldBe` YV.YInt 42
      , it "round-trip" $
          Y.fromYAML (Y.toYAML (Tag 7)) `shouldBe` Right (Tag 7)
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $ Y.toYAML Red `shouldBe` YV.YString "red"
      , it "DarkBlue" $ Y.toYAML DarkBlue `shouldBe` YV.YString "dark-blue"
      , it "round-trip" $
          mapM_ rt [Red, Green, DarkBlue]
      , it "unknown fails" $
          case Y.fromYAML (YV.YString "purple") :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = Y.fromYAML (Y.toYAML c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary) -> tag only, no contents" $
          Y.toYAML Origin
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "origin")
                  ]
              )
      , it "Circle (unary)   -> contents = inner value" $
          Y.toYAML (Circle 1.5)
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "circle")
                  , (YV.YString "contents", YV.YFloat 1.5)
                  ]
              )
      , it "Rect   (n-ary)   -> contents = YSeq" $
          Y.toYAML (Rect 2 3)
            `shouldBe` YV.YMap
              ( V.fromList
                  [ (YV.YString "tag", YV.YString "rect")
                  ,
                    ( YV.YString "contents"
                    , YV.YSeq (V.fromList [YV.YFloat 2, YV.YFloat 3])
                    )
                  ]
              )
      , it "round-trip Origin" $ rt Origin
      , it "round-trip Circle" $ rt (Circle 2.5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                YV.YMap
                  ( V.fromList
                      [ (YV.YString "tag", YV.YString "ellipse")
                      ]
                  )
          case Y.fromYAML bad :: Either String Shape of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = Y.fromYAML (Y.toYAML s) `shouldBe` Right s
