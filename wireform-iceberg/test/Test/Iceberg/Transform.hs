module Test.Iceberg.Transform (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Avro.Value as AV
import Iceberg.Transform
import Iceberg.Types

tests :: TestTree
tests = testGroup "Iceberg.Transform"
  [ testCase "Identity returns the value verbatim" $
      applyTransform Identity TInt (AV.Int 7) @?= Right (AV.Int 7)

  , testCase "Bucket on int 34 % 16 = 3" $
      applyTransform (Bucket 16) TInt (AV.Int 34) @?= Right (AV.Int 3)

  , testCase "Bucket on long 34 % 16 = 3" $
      applyTransform (Bucket 16) TLong (AV.Long 34) @?= Right (AV.Int 3)

  , testCase "Truncate(10) on int 23 = 20" $
      applyTransform (Truncate 10) TInt (AV.Int 23) @?= Right (AV.Int 20)

  , testCase "Truncate(10) on int -1 = -10 (floor towards -inf)" $
      applyTransform (Truncate 10) TInt (AV.Int (-1)) @?= Right (AV.Int (-10))

  , testCase "Truncate string \"iceberg\" by 3 = \"ice\"" $
      applyTransform (Truncate 3) TString (AV.String "iceberg") @?= Right (AV.String "ice")

  , testCase "Year of date 0 (1970-01-01) is 0" $
      applyTransform Year TDate (AV.Int 0) @?= Right (AV.Int 0)

  , testCase "Year of date 365 (1971-01-01) is 1" $
      applyTransform Year TDate (AV.Int 365) @?= Right (AV.Int 1)

  , testCase "Month of date 31 (1970-02-01) is 1" $
      applyTransform Month TDate (AV.Int 31) @?= Right (AV.Int 1)

  , testCase "Day of date 1 returns 1 (identity-on-day)" $
      applyTransform Day TDate (AV.Int 1) @?= Right (AV.Int 1)

  , testCase "Hour of timestamp 3600000000 (1 hour) = 1" $
      applyTransform Hour TTimestamp (AV.Long (3600 * 1000000)) @?= Right (AV.Int 1)

  , testCase "Void always yields null" $
      applyTransform Void TInt (AV.Int 99) @?= Right AV.Null

  , testCase "transformResultType picks correct types" $ do
      transformResultType Identity TInt   @?= Just TInt
      transformResultType (Bucket 8) TString @?= Just TInt
      transformResultType Year TDate      @?= Just TInt
      transformResultType Day  TDate      @?= Just TDate
      transformResultType Hour TDate      @?= Nothing
      transformResultType Year TString    @?= Nothing
  ]
