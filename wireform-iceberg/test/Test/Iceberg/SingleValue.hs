module Test.Iceberg.SingleValue (tests) where

import Prelude hiding (encodeFloat, decodeFloat)
import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.SingleValue
import Iceberg.Types

tests :: TestTree
tests = testGroup "Iceberg.SingleValue"
  [ testCase "encode/decode int round-trips" $ do
      decodeInt32 (encodeInt32 1) @?= Right 1
      decodeInt32 (encodeInt32 (-1)) @?= Right (-1)
      decodeInt32 (encodeInt32 maxBound) @?= Right maxBound
      decodeInt32 (encodeInt32 minBound) @?= Right minBound

  , testCase "encode/decode long round-trips" $ do
      decodeInt64 (encodeInt64 0) @?= Right 0
      decodeInt64 (encodeInt64 (-1)) @?= Right (-1)
      decodeInt64 (encodeInt64 maxBound) @?= Right maxBound

  , testCase "encode/decode float round-trips" $ do
      decodeFloat (encodeFloat 1.5) @?= Right 1.5
      decodeFloat (encodeFloat 0.0) @?= Right 0.0
      decodeFloat (encodeFloat (-3.5)) @?= Right (-3.5)

  , testCase "encode/decode double round-trips" $ do
      decodeDouble (encodeDouble 1.5) @?= Right 1.5
      decodeDouble (encodeDouble pi)  @?= Right pi

  , testCase "encode/decode decimal round-trips" $ do
      decodeDecimal (encodeDecimal 0)        @?= Right 0
      decodeDecimal (encodeDecimal 1)        @?= Right 1
      decodeDecimal (encodeDecimal (-1))     @?= Right (-1)
      decodeDecimal (encodeDecimal 123456)   @?= Right 123456
      decodeDecimal (encodeDecimal (-123456)) @?= Right (-123456)
      decodeDecimal (encodeDecimal (10 ^ (30 :: Int))) @?= Right (10 ^ (30 :: Int))

  , testCase "compareSingleValueBy on ints" $ do
      let a = encodeInt32 5
          b = encodeInt32 10
      compareSingleValueBy TInt a b @?= Right LT
      compareSingleValueBy TInt b a @?= Right GT
      compareSingleValueBy TInt a a @?= Right EQ

  , testCase "decodeInt32 rejects wrong size" $
      case decodeInt32 (BS.pack [0, 0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected Left for wrong-sized bytes"
  ]
