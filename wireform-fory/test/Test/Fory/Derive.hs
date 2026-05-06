{-# LANGUAGE OverloadedStrings #-}

module Test.Fury.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Fury.Class as F
import qualified Fury.Value as VV

import Test.Fury.Derive.Instances ()
import Test.Fury.Derive.Types

tests :: TestTree
tests = testGroup "Fury.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case F.toFury p of
        VV.StructVal _ _ kvs -> do
          assertBool "name key present"
            (V.elem ("name", VV.StringVal "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected StructVal, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case F.fromFury (F.toFury p) of
        Right p' -> do
          profileName    p' @?= profileName p
          profileAge     p' @?= profileAge p
          profileEmail   p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (k, _) = k == t

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      F.toFury (Tag 42) @?= VV.VarInt64Val 42
  , testCase "round-trip" $
      F.fromFury (F.toFury (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ F.toFury Red      @?= VV.StringVal "red"
  , testCase "DarkBlue" $ F.toFury DarkBlue @?= VV.StringVal "dark-blue"
  , testCase "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case F.fromFury (VV.StringVal "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = F.fromFury (F.toFury c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag/contents=None" $
      case F.toFury Origin of
        VV.StructVal _ _ kvs -> do
          V.elem ("tag", VV.StringVal "origin") kvs @?= True
          V.elem ("contents", VV.NoneVal)         kvs @?= True
        v -> fail ("expected StructVal, got " ++ show v)

  , testCase "Circle (unary) -> contents = inner value" $
      case F.toFury (Circle 1.5) of
        VV.StructVal _ _ kvs -> do
          V.elem ("tag", VV.StringVal "circle")     kvs @?= True
          V.elem ("contents", VV.Float64Val 1.5)    kvs @?= True
        v -> fail ("expected StructVal, got " ++ show v)

  , testCase "Rect (n-ary) -> contents = ListVal" $
      case F.toFury (Rect 2 3) of
        VV.StructVal _ _ kvs -> do
          V.elem ("tag", VV.StringVal "rect") kvs @?= True
          assertBool "contents is ListVal of two doubles"
            (V.any (\(k,v) -> k == "contents" &&
                v == VV.ListVal (V.fromList [VV.Float64Val 2, VV.Float64Val 3]))
                kvs)
        v -> fail ("expected StructVal, got " ++ show v)

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = VV.StructVal "x" "Shape"
                  (V.fromList [("tag", VV.StringVal "ellipse")])
      case F.fromFury bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = F.fromFury (F.toFury s) @?= Right s
