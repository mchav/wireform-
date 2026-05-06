{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Round-trip tests for 'Fory.Value' through 'Fory.Encode' /
-- 'Fory.Decode'.
module Test.Fory.Value (tests) where

import qualified Data.Vector as V
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Fory.Decode as D
import qualified Fory.Encode as E
import qualified Fory.Value as VV

genValue :: Int -> H.Gen VV.Value
genValue depth = Gen.choice
  $ [ pure VV.NoneVal
    , VV.BoolVal     <$> Gen.bool
    , VV.Int8Val     <$> Gen.int8     Range.linearBounded
    , VV.Int16Val    <$> Gen.int16    Range.linearBounded
    , VV.Int32Val    <$> Gen.int32    Range.linearBounded
    , VV.Int64Val    <$> Gen.int64    Range.linearBounded
    , VV.Uint8Val    <$> Gen.word8    Range.linearBounded
    , VV.Uint16Val   <$> Gen.word16   Range.linearBounded
    , VV.Uint32Val   <$> Gen.word32   Range.linearBounded
    , VV.Uint64Val   <$> Gen.word64   Range.linearBounded
    , VV.Float32Val  <$> Gen.float    (Range.linearFracFrom 0 (-1e9) 1e9)
    , VV.Float64Val  <$> Gen.double   (Range.linearFracFrom 0 (-1e15) 1e15)
    , VV.StringVal   <$> Gen.text     (Range.linear 0 32) Gen.unicode
    , VV.BinaryVal   <$> Gen.bytes    (Range.linear 0 32)
    ]
 ++ [ VV.ListVal . V.fromList <$>
        Gen.list (Range.linear 0 4) (genValue (depth - 1))
    | depth > 0
    ]
 ++ [ VV.SetVal . V.fromList <$>
        Gen.list (Range.linear 0 4) (genValue (depth - 1))
    | depth > 0
    ]
 ++ [ VV.MapVal . V.fromList <$>
        Gen.list (Range.linear 0 4)
          ((,) <$> genValue (depth - 1) <*> genValue (depth - 1))
    | depth > 0
    ]

roundTrip :: VV.Value -> Either String VV.Value
roundTrip = D.decode . E.encode

tests :: TestTree
tests = testGroup "Fory.Value"
  [ testCase "encode/decode None"    $ roundTrip VV.NoneVal       @?= Right VV.NoneVal
  , testCase "encode/decode Bool"    $ roundTrip (VV.BoolVal True) @?= Right (VV.BoolVal True)
  , testCase "encode/decode String"  $
      roundTrip (VV.StringVal "héllo") @?= Right (VV.StringVal "héllo")
  , testCase "encode/decode struct"  $ do
      let s = VV.StructVal "Foo.Bar" "Baz"
                (V.fromList [("x", VV.Int32Val 7), ("y", VV.StringVal "ok")])
      roundTrip s @?= Right s
  , testProperty "round-trip random Value" $ H.property $ do
      v <- H.forAll (genValue 3)
      H.tripping v E.encode D.decode
  , testCase "decode rejects truncated input" $
      case D.decode "" of
        Left _  -> pure ()
        Right v -> fail ("unexpected " ++ show v)
  ]
