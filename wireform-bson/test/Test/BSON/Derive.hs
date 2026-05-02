{-# LANGUAGE OverloadedStrings #-}

module Test.BSON.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified BSON.Class as B
import qualified BSON.Value as BV

import Test.BSON.Derive.Instances ()
import Test.BSON.Derive.Types

tests :: TestTree
tests = testGroup "BSON.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case B.toBSON p of
        BV.Document kvs -> do
          assertBool "name key present"
            (V.elem ("name", BV.String "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected Document, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case B.fromBSON (B.toBSON p) of
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
      B.toBSON (Tag 42) @?= BV.Int32 42
  , testCase "round-trip" $
      B.fromBSON (B.toBSON (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ B.toBSON Red      @?= BV.String "red"
  , testCase "DarkBlue" $ B.toBSON DarkBlue @?= BV.String "dark-blue"
  , testCase "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case B.fromBSON (BV.String "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = B.fromBSON (B.toBSON c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag/contents=Null" $
      B.toBSON Origin @?=
        BV.Document (V.fromList
          [ ("tag",      BV.String "origin")
          , ("contents", BV.Null)
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      B.toBSON (Circle 1.5) @?=
        BV.Document (V.fromList
          [ ("tag",      BV.String "circle")
          , ("contents", BV.Double 1.5)
          ])

  , testCase "Rect   (n-ary)   -> contents = Array" $
      B.toBSON (Rect 2 3) @?=
        BV.Document (V.fromList
          [ ("tag",      BV.String "rect")
          , ("contents",
              BV.Array (V.fromList [BV.Double 2, BV.Double 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = BV.Document (V.fromList
            [ ("tag",      BV.String "ellipse")
            , ("contents", BV.Null)
            ])
      case B.fromBSON bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = B.fromBSON (B.toBSON s) @?= Right s
