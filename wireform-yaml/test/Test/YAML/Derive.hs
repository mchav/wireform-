{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified YAML.Class as Y
import qualified YAML.Value as YV

import Test.YAML.Derive.Instances ()
import Test.YAML.Derive.Types

tests :: TestTree
tests = testGroup "YAML.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case Y.toYAML p of
        YV.YMap kvs -> do
          assertBool "name key present"
            (V.elem (YV.YString "name", YV.YString "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected YMap, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case Y.fromYAML (Y.toYAML p) of
        Right p' -> do
          profileName  p' @?= profileName p
          profileAge   p' @?= profileAge p
          profileEmail p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (YV.YString k, _) = k == t
    keyIs _ _                 = False

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      Y.toYAML (Tag 42) @?= YV.YInt 42
  , testCase "round-trip" $
      Y.fromYAML (Y.toYAML (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ Y.toYAML Red      @?= YV.YString "red"
  , testCase "DarkBlue" $ Y.toYAML DarkBlue @?= YV.YString "dark-blue"
  , testCase "round-trip" $
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case Y.fromYAML (YV.YString "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = Y.fromYAML (Y.toYAML c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag only, no contents" $
      Y.toYAML Origin @?=
        YV.YMap (V.fromList
          [ (YV.YString "tag", YV.YString "origin")
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      Y.toYAML (Circle 1.5) @?=
        YV.YMap (V.fromList
          [ (YV.YString "tag",      YV.YString "circle")
          , (YV.YString "contents", YV.YFloat 1.5)
          ])

  , testCase "Rect   (n-ary)   -> contents = YSeq" $
      Y.toYAML (Rect 2 3) @?=
        YV.YMap (V.fromList
          [ (YV.YString "tag",      YV.YString "rect")
          , (YV.YString "contents",
              YV.YSeq (V.fromList [YV.YFloat 2, YV.YFloat 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = YV.YMap (V.fromList
            [ (YV.YString "tag", YV.YString "ellipse")
            ])
      case Y.fromYAML bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = Y.fromYAML (Y.toYAML s) @?= Right s
