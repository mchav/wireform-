{-# LANGUAGE OverloadedStrings #-}

module Test.Bencode.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Bencode.Class as B
import qualified Bencode.Value as BV

import Test.Bencode.Derive.Instances ()
import Test.Bencode.Derive.Types

tests :: TestTree
tests = testGroup "Bencode.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case B.toBencode p of
        BV.BDict kvs -> do
          assertBool "name key present"
            (V.elem ("name", BV.BString "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected BDict, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case B.fromBencode (B.toBencode p) of
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
      B.toBencode (Tag 42) @?= BV.BInteger 42
  , testCase "round-trip" $
      B.fromBencode (B.toBencode (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ B.toBencode Red      @?= BV.BString "red"
  , testCase "DarkBlue" $ B.toBencode DarkBlue @?= BV.BString "dark-blue"
  , testCase "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case B.fromBencode (BV.BString "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = B.fromBencode (B.toBencode c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> single-key dict (no contents)" $
      B.toBencode Origin @?=
        BV.BDict (V.fromList
          [ ("tag", BV.BString "origin")
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      B.toBencode (Circle 1) @?=
        BV.BDict (V.fromList
          [ ("tag",      BV.BString "circle")
          , ("contents", BV.BInteger 1)
          ])

  , testCase "Rect   (n-ary)   -> contents = BList" $
      B.toBencode (Rect 2 3) @?=
        BV.BDict (V.fromList
          [ ("tag",      BV.BString "rect")
          , ("contents",
              BV.BList (V.fromList [BV.BInteger 2, BV.BInteger 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = BV.BDict (V.fromList
            [ ("tag", BV.BString "ellipse")
            ])
      case B.fromBencode bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = B.fromBencode (B.toBencode s) @?= Right s
