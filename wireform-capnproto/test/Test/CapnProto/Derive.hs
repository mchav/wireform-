{-# LANGUAGE OverloadedStrings #-}

module Test.CapnProto.Derive (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified CapnProto.Decode as CPD
import qualified CapnProto.Encode as CPE
import qualified CapnProto.Value as CP
import CapnProto.Derive

import Test.CapnProto.Derive.Instances ()
import Test.CapnProto.Derive.Types

tests :: TestTree
tests = testGroup "CapnProto.Derive"
  [ recordTests
  , blobTests
  , profileTests
  , newtypeTests
  , coercedTests
  , enumTests
  , wireTests
  ]

recordTests :: TestTree
recordTests = testGroup "record (mixed sections)"
  [ testCase "splits scalars into data, Text into pointers" $ do
      let p = Position "origin" 3 7 (Just "home") "ignored"
      case toCapnProto p of
        CP.Struct dat ptrs -> do
          V.length dat  @?= 2
          dat  V.! 0 @?= CP.Int32 3
          dat  V.! 1 @?= CP.Int32 7
          V.length ptrs @?= 2
          ptrs V.! 0 @?= CP.Text "origin"
          ptrs V.! 1 @?= CP.Text "home"
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "Nothing Maybe Text encodes as Void in pointer section" $ do
      let p = Position "none" 0 0 Nothing "ignored"
      case toCapnProto p of
        CP.Struct _ ptrs -> ptrs V.! 1 @?= CP.Void
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip reinstates skipped label from defaults" $ do
      let p = Position "origin" 3 7 (Just "home") "whatever"
      case fromCapnProto (toCapnProto p) of
        Right p' -> do
          posName  p' @?= posName p
          posX     p' @?= posX p
          posY     p' @?= posY p
          posNote  p' @?= posNote p
          posLabel p' @?= defaultLabel
        Left e -> fail e

  , testCase "round-trip with Nothing note" $ do
      let p = Position "none" 1 2 Nothing "whatever"
      case fromCapnProto (toCapnProto p) of
        Right p' -> do
          posNote  p' @?= Nothing
          posLabel p' @?= defaultLabel
        Left e -> fail e
  ]

blobTests :: TestTree
blobTests = testGroup "record (pointers only)"
  [ testCase "Text + ByteString land in pointer section" $ do
      let b = Blob "payload" (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
      case toCapnProto b of
        CP.Struct dat ptrs -> do
          V.length dat @?= 0
          V.length ptrs @?= 2
          ptrs V.! 0 @?= CP.Text "payload"
          ptrs V.! 1 @?= CP.Data (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip" $ do
      let b = Blob "payload" (BS.pack [1, 2, 3])
      fromCapnProto (toCapnProto b) @?= Right b
  ]

profileTests :: TestTree
profileTests = testGroup "record (data only)"
  [ testCase "scalar-only record has empty pointer section" $ do
      let p = Profile 42 True 3.14
      case toCapnProto p of
        CP.Struct dat ptrs -> do
          V.length dat @?= 3
          dat V.! 0 @?= CP.Int32 42
          dat V.! 1 @?= CP.Bool True
          dat V.! 2 @?= CP.Float64 3.14
          V.length ptrs @?= 0
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip" $ do
      let p = Profile 7 False 2.71
      fromCapnProto (toCapnProto p) @?= Right p
  ]

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through encodes to Int32" $
      toCapnProto (Tag 42) @?= CP.Int32 42

  , testCase "round-trip" $
      fromCapnProto (toCapnProto (Tag 7)) @?= Right (Tag 7)
  ]

coercedTests :: TestTree
coercedTests = testGroup "coerced field"
  [ testCase "newtype field encodes via inner Int32 repr" $ do
      let u = User (UserId 7) "alice"
      case toCapnProto u of
        CP.Struct dat ptrs -> do
          V.length dat @?= 1
          dat V.! 0 @?= CP.Int32 7
          V.length ptrs @?= 1
          ptrs V.! 0 @?= CP.Text "alice"
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip via coerce" $ do
      let u = User (UserId 99) "bob"
      fromCapnProto (toCapnProto u) @?= Right u
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red -> Enum 0"   $ toCapnProto Red      @?= CP.Enum 0
  , testCase "Green -> Enum 1" $ toCapnProto Green    @?= CP.Enum 1
  , testCase "DarkBlue tag 42" $ toCapnProto DarkBlue @?= CP.Enum 42
  , testCase "round-trip" $
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown ordinal fails" $
      case fromCapnProto (CP.Enum 99) :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = fromCapnProto (toCapnProto c) @?= Right c

wireTests :: TestTree
wireTests = testGroup "encode . decode"
  [ testCase "Position decodes to Right" $
      case CPD.decode (CPE.encode (toCapnProto (Position "p" 1 2 (Just "n") "ignored"))) of
        Right _ -> pure ()
        Left e  -> fail e

  , testCase "Profile decodes to Right" $
      case CPD.decode (CPE.encode (toCapnProto (Profile 9 True 0.5))) of
        Right _ -> pure ()
        Left e  -> fail e

  , testCase "Blob decodes to Right" $
      case CPD.decode (CPE.encode (toCapnProto (Blob "x" (BS.pack [1, 2, 3])))) of
        Right _ -> pure ()
        Left e  -> fail e
  ]
