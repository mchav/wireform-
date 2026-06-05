{-# LANGUAGE OverloadedStrings #-}

module Test.FlatBuffers.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified FlatBuffers.Value as FB
import FlatBuffers.Derive

import Test.FlatBuffers.Derive.Instances ()
import Test.FlatBuffers.Derive.Types

tests :: Spec
tests = describe "FlatBuffers.Derive" $ sequence_
  [ recordTests
  , newtypeTests
  , enumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "encodes VTable with positional slots" $ do
      let p = Position "origin" 3 7 (Just "home") "ignored"
      case toFlatBuffers p of
        FB.VTable slots -> do
          V.length slots `shouldBe` 5
          slots V.! 0 `shouldBe` Just (FB.VString "origin")
          slots V.! 1 `shouldBe` Just (FB.VInt32 3)
          slots V.! 2 `shouldBe` Just (FB.VInt32 7)
          slots V.! 3 `shouldBe` Just (FB.VString "home")
          -- posLabel is skipped for backendFlatBuffers
          slots V.! 4 `shouldBe` Nothing
        v -> expectationFailure ("expected VTable, got " ++ show v)

  , it "Nothing Maybe-field renders as empty slot" $ do
      let p = Position "origin" 0 0 Nothing "ignored"
      case toFlatBuffers p of
        FB.VTable slots -> (slots V.! 3 == Nothing) `shouldBe` True
        v -> expectationFailure ("expected VTable, got " ++ show v)

  , it "round-trip reinstates skipped label from defaults" $ do
      let p = Position "origin" 3 7 (Just "home") "whatever"
      case fromFlatBuffers (toFlatBuffers p) of
        Right p' -> do
          posName  p' `shouldBe` posName p
          posX     p' `shouldBe` posX p
          posY     p' `shouldBe` posY p
          posNote  p' `shouldBe` posNote p
          posLabel p' `shouldBe` defaultLabel
        Left e  -> expectationFailure e

  , it "round-trip with Nothing note" $ do
      let p = Position "none" 1 2 Nothing "whatever"
      case fromFlatBuffers (toFlatBuffers p) of
        Right p' -> do
          posNote  p' `shouldBe` Nothing
          posLabel p' `shouldBe` defaultLabel
        Left e  -> expectationFailure e
  ]

newtypeTests :: Spec
newtypeTests = describe "newtype" $ sequence_
  [ it "pass-through encodes to VInt32" $
      toFlatBuffers (Tag 42) `shouldBe` FB.VInt32 42

  , it "round-trip" $
      fromFlatBuffers (toFlatBuffers (Tag 7)) `shouldBe` Right (Tag 7)
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "Red -> VInt32 0"    $ toFlatBuffers Red      `shouldBe` FB.VInt32 0
  , it "Green -> VInt32 1"  $ toFlatBuffers Green    `shouldBe` FB.VInt32 1
  , it "DarkBlue tag 42"    $ toFlatBuffers DarkBlue `shouldBe` FB.VInt32 42
  , it "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , it "unknown ordinal fails" $
      case fromFlatBuffers (FB.VInt32 99) :: Either String Color of
        Left _  -> pure ()
        Right c -> expectationFailure ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = fromFlatBuffers (toFlatBuffers c) `shouldBe` Right c
