{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Derive (tests) where

import Data.Vector qualified as V
import TOML.Class qualified as T
import TOML.Value qualified as TV
import Test.Syd
import Test.TOML.Derive.Instances ()
import Test.TOML.Derive.Types


tests :: Spec
tests =
  describe "TOML.Derive" $
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
          case T.toTOML p of
            TV.TTable kvs -> do
              (V.elem ("name", TV.TString "Alice") kvs) `shouldBe` True
              (V.any (keyIs "profile_age") kvs) `shouldBe` True
              (V.any (keyIs "email") kvs) `shouldBe` True
              (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
            v -> expectationFailure ("expected TTable, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let p = Profile "Alice" 30 "a@x" "secret"
          case T.fromTOML (T.toTOML p) of
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
          T.toTOML (Tag 42) `shouldBe` TV.TInteger 42
      , it "round-trip" $
          T.fromTOML (T.toTOML (Tag 7)) `shouldBe` Right (Tag 7)
      ]


enumTests :: Spec
enumTests =
  describe "enum" $
    sequence_
      [ it "Red" $ T.toTOML Red `shouldBe` TV.TString "red"
      , it "DarkBlue" $ T.toTOML DarkBlue `shouldBe` TV.TString "dark-blue"
      , it "round-trip" $
          mapM_ rt [Red, Green, DarkBlue]
      , it "unknown fails" $
          case T.fromTOML (TV.TString "purple") :: Either String Color of
            Left _ -> pure ()
            Right c -> expectationFailure ("unexpected " ++ show c)
      ]
  where
    rt :: Color -> IO ()
    rt c = T.fromTOML (T.toTOML c) `shouldBe` Right c


sumTests :: Spec
sumTests =
  describe "sum" $
    sequence_
      [ it "Origin (nullary) -> tag only, no contents" $
          T.toTOML Origin
            `shouldBe` TV.TTable
              ( V.fromList
                  [ ("tag", TV.TString "origin")
                  ]
              )
      , it "Circle (unary)   -> contents = inner value" $
          T.toTOML (Circle 1.5)
            `shouldBe` TV.TTable
              ( V.fromList
                  [ ("tag", TV.TString "circle")
                  , ("contents", TV.TFloat 1.5)
                  ]
              )
      , it "Rect   (n-ary)   -> contents = TArray" $
          T.toTOML (Rect 2 3)
            `shouldBe` TV.TTable
              ( V.fromList
                  [ ("tag", TV.TString "rect")
                  ,
                    ( "contents"
                    , TV.TArray (V.fromList [TV.TFloat 2, TV.TFloat 3])
                    )
                  ]
              )
      , it "round-trip Origin" $ rt Origin
      , it "round-trip Circle" $ rt (Circle 2.5)
      , it "round-trip Rect" $ rt (Rect 4 5)
      , it "unknown tag fails" $ do
          let bad =
                TV.TTable
                  ( V.fromList
                      [ ("tag", TV.TString "ellipse")
                      ]
                  )
          case T.fromTOML bad :: Either String Shape of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Shape -> IO ()
    rt s = T.fromTOML (T.toTOML s) `shouldBe` Right s
