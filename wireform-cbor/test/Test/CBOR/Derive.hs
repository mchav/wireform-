{-# LANGUAGE OverloadedStrings #-}

module Test.CBOR.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import qualified CBOR.Class as C
import qualified CBOR.Value as CV

import Test.CBOR.Derive.Instances ()
import Test.CBOR.Derive.Types

tests :: TestTree
tests = testGroup "CBOR.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case C.toCBOR p of
        CV.Map kvs -> do
          assertBool "name key present"
            (V.elem (CV.TextString "name", CV.TextString "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected Map, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case C.fromCBOR (C.toCBOR p) of
        Right p' -> do
          profileName  p' @?= profileName p
          profileAge   p' @?= profileAge p
          profileEmail p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (CV.TextString k, _) = k == t
    keyIs _ _                    = False

-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      C.toCBOR (Tag 42) @?= CV.UInt 42
  , testCase "round-trip" $
      C.fromCBOR (C.toCBOR (Tag 7)) @?= Right (Tag 7)
  ]

-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ C.toCBOR Red      @?= CV.TextString "red"
  , testCase "DarkBlue" $ C.toCBOR DarkBlue @?= CV.TextString "dark-blue"
  , testCase "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case C.fromCBOR (CV.TextString "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = C.fromCBOR (C.toCBOR c) @?= Right c

-- ---------------------------------------------------------------------------

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag/contents=Null" $
      C.toCBOR Origin @?=
        CV.Map (V.fromList
          [ (CV.TextString "tag",      CV.TextString "origin")
          , (CV.TextString "contents", CV.Null)
          ])

  , testCase "Circle (unary)   -> contents = inner value" $
      C.toCBOR (Circle 1.5) @?=
        CV.Map (V.fromList
          [ (CV.TextString "tag",      CV.TextString "circle")
          , (CV.TextString "contents", CV.Float64 1.5)
          ])

  , testCase "Rect   (n-ary)   -> contents = Array" $
      C.toCBOR (Rect 2 3) @?=
        CV.Map (V.fromList
          [ (CV.TextString "tag",      CV.TextString "rect")
          , (CV.TextString "contents",
              CV.Array (V.fromList [CV.Float64 2, CV.Float64 3]))
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
      let bad = CV.Map (V.fromList
            [ (CV.TextString "tag",      CV.TextString "ellipse")
            , (CV.TextString "contents", CV.Null)
            ])
      case C.fromCBOR bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = C.fromCBOR (C.toCBOR s) @?= Right s
