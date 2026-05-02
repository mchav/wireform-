{-# LANGUAGE OverloadedStrings #-}

module Test.Ion.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Ion.Class as I
import qualified Ion.Value as IV

import Test.Ion.Derive.Instances ()
import Test.Ion.Derive.Types

tests :: TestTree
tests = testGroup "Ion.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case I.toIon p of
        IV.Struct kvs -> do
          assertBool "name key present"
            (V.elem ("name", IV.String "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case I.fromIon (I.toIon p) of
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
      I.toIon (Tag 42) @?= IV.Int 42
  , testCase "round-trip" $
      I.fromIon (I.toIon (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ I.toIon Red      @?= IV.String "red"
  , testCase "DarkBlue" $ I.toIon DarkBlue @?= IV.String "dark-blue"
  , testCase "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case I.fromIon (IV.String "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = I.fromIon (I.toIon c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag/contents=Null" $
      I.toIon Origin @?=
        IV.Struct (V.fromList
          [ ("tag",      IV.String "origin")
          , ("contents", IV.Null)
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      I.toIon (Circle 1.5) @?=
        IV.Struct (V.fromList
          [ ("tag",      IV.String "circle")
          , ("contents", IV.Float 1.5)
          ])

  , testCase "Rect   (n-ary)   -> contents = List" $
      I.toIon (Rect 2 3) @?=
        IV.Struct (V.fromList
          [ ("tag",      IV.String "rect")
          , ("contents",
              IV.List (V.fromList [IV.Float 2, IV.Float 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = IV.Struct (V.fromList
            [ ("tag",      IV.String "ellipse")
            , ("contents", IV.Null)
            ])
      case I.fromIon bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = I.fromIon (I.toIon s) @?= Right s
