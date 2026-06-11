{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Round-trip tests for 'Fory.Value' through 'Fory.Encode' /
'Fory.Decode'.
-}
module Test.Fory.Value (tests) where

import Data.Vector qualified as V
import Fory.Decode qualified as D
import Fory.Encode qualified as E
import Fory.Value qualified as VV
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


genValue :: Int -> H.Gen VV.Value
genValue depth =
  Gen.choice $
    [ pure VV.NoneVal
    , VV.BoolVal <$> Gen.bool
    , VV.Int8Val <$> Gen.int8 Range.linearBounded
    , VV.Int16Val <$> Gen.int16 Range.linearBounded
    , VV.Int32Val <$> Gen.int32 Range.linearBounded
    , VV.Int64Val <$> Gen.int64 Range.linearBounded
    , VV.Uint8Val <$> Gen.word8 Range.linearBounded
    , VV.Uint16Val <$> Gen.word16 Range.linearBounded
    , VV.Uint32Val <$> Gen.word32 Range.linearBounded
    , VV.Uint64Val <$> Gen.word64 Range.linearBounded
    , VV.Float32Val <$> Gen.float (Range.linearFracFrom 0 (-1e9) 1e9)
    , VV.Float64Val <$> Gen.double (Range.linearFracFrom 0 (-1e15) 1e15)
    , VV.StringVal <$> Gen.text (Range.linear 0 32) Gen.unicode
    , VV.BinaryVal <$> Gen.bytes (Range.linear 0 32)
    ]
      ++ [ VV.ListVal . V.fromList
             <$> Gen.list (Range.linear 0 4) (genValue (depth - 1))
         | depth > 0
         ]
      ++ [ VV.SetVal . V.fromList
             <$> Gen.list (Range.linear 0 4) (genValue (depth - 1))
         | depth > 0
         ]
      ++ [ VV.MapVal . V.fromList
             <$> Gen.list
               (Range.linear 0 4)
               ((,) <$> genValue (depth - 1) <*> genValue (depth - 1))
         | depth > 0
         ]


roundTrip :: VV.Value -> Either String VV.Value
roundTrip = D.decode . E.encode


tests :: Spec
tests =
  describe "Fory.Value" $
    sequence_
      [ it "encode/decode None" $ roundTrip VV.NoneVal `shouldBe` Right VV.NoneVal
      , it "encode/decode Bool" $ roundTrip (VV.BoolVal True) `shouldBe` Right (VV.BoolVal True)
      , it "encode/decode String" $
          roundTrip (VV.StringVal "héllo") `shouldBe` Right (VV.StringVal "héllo")
      , it "encode/decode struct" $ do
          let s =
                VV.StructVal
                  "Foo.Bar"
                  "Baz"
                  (V.fromList [("x", VV.Int32Val 7), ("y", VV.StringVal "ok")])
          roundTrip s `shouldBe` Right s
      , it "round-trip random Value" $ H.property $ do
          v <- H.forAll (genValue 3)
          H.tripping v E.encode D.decode
      , it "decode rejects truncated input" $
          case D.decode "" of
            Left _ -> pure ()
            Right v -> expectationFailure ("unexpected " ++ show v)
      ]
