{-# LANGUAGE OverloadedStrings #-}

module Test.FlatBuffers.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified FlatBuffers.Value as FB
import FlatBuffers.Derive

import Test.FlatBuffers.Derive.Instances ()
import Test.FlatBuffers.Derive.Types

tests :: TestTree
tests = testGroup "FlatBuffers.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encodes VTable with positional slots" $ do
      let p = Position "origin" 3 7 (Just "home") "ignored"
      case toFlatBuffers p of
        FB.VTable slots -> do
          V.length slots @?= 5
          slots V.! 0 @?= Just (FB.VString "origin")
          slots V.! 1 @?= Just (FB.VInt32 3)
          slots V.! 2 @?= Just (FB.VInt32 7)
          slots V.! 3 @?= Just (FB.VString "home")
          -- posLabel is skipped for backendFlatBuffers
          slots V.! 4 @?= Nothing
        v -> fail ("expected VTable, got " ++ show v)

  , testCase "Nothing Maybe-field renders as empty slot" $ do
      let p = Position "origin" 0 0 Nothing "ignored"
      case toFlatBuffers p of
        FB.VTable slots -> assertBool "slot 3 is Nothing" (slots V.! 3 == Nothing)
        v -> fail ("expected VTable, got " ++ show v)

  , testCase "round-trip reinstates skipped label from defaults" $ do
      let p = Position "origin" 3 7 (Just "home") "whatever"
      case fromFlatBuffers (toFlatBuffers p) of
        Right p' -> do
          posName  p' @?= posName p
          posX     p' @?= posX p
          posY     p' @?= posY p
          posNote  p' @?= posNote p
          posLabel p' @?= defaultLabel
        Left e  -> fail e

  , testCase "round-trip with Nothing note" $ do
      let p = Position "none" 1 2 Nothing "whatever"
      case fromFlatBuffers (toFlatBuffers p) of
        Right p' -> do
          posNote  p' @?= Nothing
          posLabel p' @?= defaultLabel
        Left e  -> fail e
  ]

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through encodes to VInt32" $
      toFlatBuffers (Tag 42) @?= FB.VInt32 42

  , testCase "round-trip" $
      fromFlatBuffers (toFlatBuffers (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red -> VInt32 0"    $ toFlatBuffers Red      @?= FB.VInt32 0
  , testCase "Green -> VInt32 1"  $ toFlatBuffers Green    @?= FB.VInt32 1
  , testCase "DarkBlue tag 42"    $ toFlatBuffers DarkBlue @?= FB.VInt32 42
  , testCase "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown ordinal fails" $
      case fromFlatBuffers (FB.VInt32 99) :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = fromFlatBuffers (toFlatBuffers c) @?= Right c
