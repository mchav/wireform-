{-# LANGUAGE OverloadedStrings #-}

module Test.TOML.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified TOML.Class as T
import qualified TOML.Value as TV

import Test.TOML.Derive.Instances ()
import Test.TOML.Derive.Types

tests :: TestTree
tests = testGroup "TOML.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case T.toTOML p of
        TV.TTable kvs -> do
          assertBool "name key present"
            (V.elem ("name", TV.TString "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected TTable, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case T.fromTOML (T.toTOML p) of
        Right p' -> do
          profileName  p' @?= profileName p
          profileAge   p' @?= profileAge p
          profileEmail p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (k, _) = k == t

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      T.toTOML (Tag 42) @?= TV.TInteger 42
  , testCase "round-trip" $
      T.fromTOML (T.toTOML (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ T.toTOML Red      @?= TV.TString "red"
  , testCase "DarkBlue" $ T.toTOML DarkBlue @?= TV.TString "dark-blue"
  , testCase "round-trip" $
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case T.fromTOML (TV.TString "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = T.fromTOML (T.toTOML c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag only, no contents" $
      T.toTOML Origin @?=
        TV.TTable (V.fromList
          [ ("tag", TV.TString "origin")
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      T.toTOML (Circle 1.5) @?=
        TV.TTable (V.fromList
          [ ("tag",      TV.TString "circle")
          , ("contents", TV.TFloat 1.5)
          ])

  , testCase "Rect   (n-ary)   -> contents = TArray" $
      T.toTOML (Rect 2 3) @?=
        TV.TTable (V.fromList
          [ ("tag",      TV.TString "rect")
          , ("contents",
              TV.TArray (V.fromList [TV.TFloat 2, TV.TFloat 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = TV.TTable (V.fromList
            [ ("tag", TV.TString "ellipse")
            ])
      case T.fromTOML bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = T.fromTOML (T.toTOML s) @?= Right s
