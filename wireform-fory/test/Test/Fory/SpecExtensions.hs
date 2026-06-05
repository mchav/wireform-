{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for the four xlang-spec features layered on top of the
-- primitive value layer:
--
-- * Reference tracking ('VV.RefVal' / 'F.Shared').
-- * Meta-string deduplication.
-- * 'NAMED_COMPATIBLE_STRUCT' with a shared 'TypeDef'.
-- * One-dimensional primitive arrays.
module Test.Fory.SpecExtensions (tests) where

import qualified Data.ByteString as BS
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Fory.Class as F
import qualified Fory.Decode as D
import qualified Fory.Encode as E
import qualified Fory.Value as VV

tests :: Spec
tests = describe "Fory.SpecExtensions" $ sequence_
  [ refTrackingTests
  , metaShareTests
  , compatibleStructTests
  , primitiveArrayTests
  ]

-- ---------------------------------------------------------------------------
-- Reference tracking
-- ---------------------------------------------------------------------------

refTrackingTests :: Spec
refTrackingTests = describe "reference tracking" $ sequence_
  [ it "RefVal first occurrence round-trips" $ do
      let v = VV.RefVal 7 (VV.StringVal "hello")
      case D.decode (E.encode v) of
        -- the user's '7' is remapped to the wire ref id (0 here).
        Right (VV.RefVal 0 (VV.StringVal "hello")) -> pure ()
        Right other -> expectationFailure $ "unexpected " ++ show other
        Left e      -> expectationFailure e

  , it "back-reference shares payload bytes" $ do
      let inner = VV.StringVal "lots of repeated content here"
          tagged = VV.RefVal 1 inner
          shared = VV.ListVal (V.fromList [tagged, tagged, tagged])
          unshared =
            VV.ListVal (V.fromList [inner, inner, inner])
          sharedBytes   = BS.length (E.encode shared)
          unsharedBytes = BS.length (E.encode unshared)
      (if (sharedBytes < unsharedBytes) then pure () else expectationFailure ("expected sharing to shrink wire size; shared="
         ++ show sharedBytes ++ " unshared=" ++ show unsharedBytes))

  , it "back-reference round-trips structurally" $ do
      let inner = VV.StringVal "shared"
          tagged = VV.RefVal 1 inner
          v = VV.ListVal (V.fromList [tagged, tagged])
      case D.decode (E.encode v) of
        Right (VV.ListVal xs)
          | V.length xs == 2 -> do
              -- both elements decode to a RefVal wrapping the same
              -- inner string; the wire id is 0 on first occurrence
              -- and 0 on the back-reference (resolved to the same
              -- recorded value).
              case (xs V.! 0, xs V.! 1) of
                (VV.RefVal 0 v0, VV.RefVal 0 v1) -> do
                  v0 `shouldBe` inner
                  v1 `shouldBe` inner
                _ -> expectationFailure $ "unexpected " ++ show xs
        Right other -> expectationFailure $ "unexpected " ++ show other
        Left e      -> expectationFailure e

  , it "Shared newtype convenience round-trips" $ do
      let s :: F.Shared Int
          s = F.Shared 42 9
          encoded = F.encodeFory s
      (F.decodeFory encoded :: Either String (F.Shared Int))
        `shouldBe` Right (F.Shared 0 9)

  , it "RefVal of arbitrary inner round-trips structurally" $
      H.property $ do
        n <- H.forAll (Gen.int32 (Range.linear 0 1000))
        let v = VV.RefVal (fromIntegral n) (VV.Int32Val n)
        case D.decode (E.encode v) of
          Right (VV.RefVal _ inner) -> inner H.=== VV.Int32Val n
          other -> do
            H.annotate (show other)
            H.failure
  ]

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

metaShareTests :: Spec
metaShareTests = describe "meta-string deduplication" $ sequence_
  [ it "two structs of the same type share namespace + type-name" $ do
      let mkS = VV.StructVal "long.namespace.path.example" "MyStruct"
                  (V.fromList [("a_field_with_a_long_name", VV.Int32Val 1)])
          two = VV.ListVal (V.fromList [mkS, mkS])
          one = VV.ListVal (V.fromList [mkS])
          twoBytes = BS.length (E.encode two)
          oneBytes = BS.length (E.encode one)
          -- Naive (no-dedup) cost would be ~2x. With dedup,
          -- additional cost is per-back-reference (1-2 bytes).
          overheadBound = 16
      (if (twoBytes - oneBytes <= overheadBound) then pure () else expectationFailure ("dedup should keep two copies within "
         ++ show overheadBound ++ " bytes of one; one="
         ++ show oneBytes ++ " two=" ++ show twoBytes))

  , it "round-trip preserves struct identity through dedup" $ do
      let s1 = VV.StructVal "Foo.Bar" "Baz"
                 (V.fromList [("x", VV.Int32Val 7)])
          s2 = VV.StructVal "Foo.Bar" "Baz"
                 (V.fromList [("x", VV.Int32Val 9)])
          v  = VV.ListVal (V.fromList [s1, s2, s1])
      D.decode (E.encode v) `shouldBe` Right v
  ]

-- ---------------------------------------------------------------------------
-- NAMED_COMPATIBLE_STRUCT + TypeDef
-- ---------------------------------------------------------------------------

compatibleStructTests :: Spec
compatibleStructTests = describe "NAMED_COMPATIBLE_STRUCT" $ sequence_
  [ it "single CompatibleStructVal round-trips" $ do
      let v = VV.CompatibleStructVal "x.y" "T"
                (V.fromList
                   [ ("a", VV.Int32Val 1)
                   , ("b", VV.StringVal "hi")
                   ])
      D.decode (E.encode v) `shouldBe` Right v

  , it "TypeDef shared across two occurrences" $ do
      let mk a b = VV.CompatibleStructVal "x.y" "T"
                    (V.fromList
                       [ ("a", VV.Int32Val a)
                       , ("b", VV.StringVal b)
                       ])
          v = VV.ListVal (V.fromList [mk 1 "x", mk 2 "y"])
      D.decode (E.encode v) `shouldBe` Right v

  , it "schema sharing shrinks wire size" $ do
      let mk i = VV.CompatibleStructVal "long.namespace" "ManyFields"
                  (V.fromList
                     [ ("first_field",        VV.Int32Val i)
                     , ("second_field_name",  VV.Int64Val (fromIntegral i))
                     , ("third_field_name",   VV.StringVal "x")
                     , ("fourth_field_name",  VV.BoolVal (even i))
                     ])
          two = VV.ListVal (V.fromList [mk 1, mk 2])
          one = VV.ListVal (V.fromList [mk 1])
          twoBytes = BS.length (E.encode two)
          oneBytes = BS.length (E.encode one)
      (if (twoBytes - oneBytes <= 32) then pure () else expectationFailure ("schema sharing should keep second copy small; one="
         ++ show oneBytes ++ " two=" ++ show twoBytes))

  , it "random CompatibleStructVal round-trips" $ H.property $ do
      n      <- H.forAll (Gen.int (Range.linear 0 6))
      fields <- H.forAll $ Gen.list (Range.singleton n) $ do
        nm <- Gen.text (Range.linear 1 12) Gen.alpha
        x  <- Gen.int32 Range.linearBounded
        pure (nm, VV.Int32Val x)
      let v = VV.CompatibleStructVal "ns" "T" (V.fromList fields)
      H.tripping v E.encode D.decode
  ]

-- ---------------------------------------------------------------------------
-- Primitive arrays
-- ---------------------------------------------------------------------------

primitiveArrayTests :: Spec
primitiveArrayTests = describe "primitive 1-D arrays" $ sequence_
  [ it "BoolArray round-trips" $
      rt (VV.BoolArrayVal (VS.fromList [1, 0, 1]))
  , it "Int8Array round-trips" $
      rt (VV.Int8ArrayVal (VS.fromList ([-128, -1, 0, 1, 127] :: [Int8])))
  , it "Int16Array round-trips" $
      rt (VV.Int16ArrayVal (VS.fromList ([-32768, 0, 32767] :: [Int16])))
  , it "Int32Array round-trips" $
      rt (VV.Int32ArrayVal (VS.fromList ([minBound, 0, maxBound] :: [Int32])))
  , it "Int64Array round-trips" $
      rt (VV.Int64ArrayVal (VS.fromList ([minBound, 0, maxBound] :: [Int64])))
  , it "Uint8Array round-trips" $
      rt (VV.Uint8ArrayVal (VS.fromList ([0, 128, 255] :: [Word8])))
  , it "Uint16Array round-trips" $
      rt (VV.Uint16ArrayVal (VS.fromList ([0, 32768, 65535] :: [Word16])))
  , it "Uint32Array round-trips" $
      rt (VV.Uint32ArrayVal (VS.fromList ([0, 1, maxBound] :: [Word32])))
  , it "Uint64Array round-trips" $
      rt (VV.Uint64ArrayVal (VS.fromList ([0, 1, maxBound] :: [Word64])))
  , it "Float32Array round-trips" $
      rt (VV.Float32ArrayVal (VS.fromList [-1.5, 0, 3.14]))
  , it "Float64Array round-trips" $
      rt (VV.Float64ArrayVal (VS.fromList [-1.5e300, 0, 3.14e-200]))

  , it "Int32Array is denser than equivalent ListVal of Int32Val" $ do
      let xs :: [Int32]
          xs = [minBound, -1, 0, 1, maxBound]
          listForm = VV.ListVal (V.fromList (map VV.Int32Val xs))
          arrForm  = VV.Int32ArrayVal (VS.fromList xs)
          listBytes = BS.length (E.encode listForm)
          arrBytes  = BS.length (E.encode arrForm)
      (if (arrBytes < listBytes) then pure () else expectationFailure ("expected array < list; list=" ++ show listBytes
         ++ " arr=" ++ show arrBytes))

  , it "Int32Array typeclass wrapper round-trips" $ do
      let xs = F.Int32Array (VS.fromList [1, 2, 3, 4, 5])
      F.decodeFory (F.encodeFory xs) `shouldBe` Right xs

  , it "Float64Array typeclass wrapper round-trips" $ do
      let xs = F.Float64Array (VS.fromList [1.0, 2.5, -3.75])
      F.decodeFory (F.encodeFory xs) `shouldBe` Right xs

  , it "random Int32Array round-trips" $ H.property $ do
      xs <- H.forAll $ Gen.list (Range.linear 0 32)
                                (Gen.int32 Range.linearBounded)
      let v = VV.Int32ArrayVal (VS.fromList xs)
      H.tripping v E.encode D.decode

  , it "random Float64Array round-trips (excluding NaN)" $
      H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 32)
                                  (Gen.double (Range.linearFracFrom 0 (-1e9) 1e9))
        let v = VV.Float64ArrayVal (VS.fromList xs)
        H.tripping v E.encode D.decode
  ]
  where
    rt :: VV.Value -> IO ()
    rt v = D.decode (E.encode v) `shouldBe` Right v
