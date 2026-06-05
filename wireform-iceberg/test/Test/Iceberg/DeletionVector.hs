module Test.Iceberg.DeletionVector (tests) where

import Test.Syd

import Iceberg.DeletionVector
import Iceberg.Puffin (pbData, pbFields)
import qualified Data.Vector as V

tests :: Spec
tests = describe "Iceberg.DeletionVector" $ sequence_
  [ it "encodeDV / decodeDV round-trip empty" $
      case decodeDV (encodeDV emptyDV) of
        Right dv -> deletedPositions dv `shouldBe` []
        Left e   -> expectationFailure e

  , it "encodeDV / decodeDV round-trip a few small ids" $ do
      let dv = addPositions [0, 1, 2, 5, 10, 1000] emptyDV
      case decodeDV (encodeDV dv) of
        Right dv' -> deletedPositions dv' `shouldBe` [0, 1, 2, 5, 10, 1000]
        Left e    -> expectationFailure e

  , it "round-trip across high-32 bucket boundaries" $ do
      let highVals = [4294967296, 4294967300, 8589934592]  -- 2^32, 2^32+4, 2^33
          dv = addPositions (map fromIntegral highVals) emptyDV
      case decodeDV (encodeDV dv) of
        Right dv' -> deletedPositions dv' `shouldBe` map fromIntegral highVals
        Left e    -> expectationFailure e

  , it "toPuffinBlob emits the right blob type and field id" $ do
      let dv = addPositions [3, 7] emptyDV
          blob = toPuffinBlob 99 1 42 dv
      pbFields blob `shouldBe` V.singleton 42
      -- The blob payload is at least the bitmap plus the 4+4 framing bytes.
      (8 <= length (show (pbData blob))) `shouldBe` True
      case fromPuffinBlob blob of
        Right dv' -> deletedPositions dv' `shouldBe` [3, 7]
        Left e    -> expectationFailure e
  ]
