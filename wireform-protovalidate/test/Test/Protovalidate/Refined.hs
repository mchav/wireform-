{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for reifying buf.validate rules as @refined@ refinement types.
module Test.Protovalidate.Refined (tests) where

import Data.Either (isLeft, isRight)
import Refined (RefineException, Refined, refine)
import Test.Tasty
import Test.Tasty.HUnit

import CEL (Value (..))
import Protovalidate
import Protovalidate.Refined (Gt, MinLen)

tests :: TestTree
tests =
  testGroup
    "refined reification"
    [ testCase "string min_len/max_len reify to a Refined type expression" $
        refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
          @?= Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
    , testCase "numeric gt/lte reify to a Refined type expression" $
        refinedFieldType (fieldRules KInt64 [gtV (VInt 0), lteV (VInt 150)])
          @?= Just "Refined (And (Gt 0) (Lte 150)) Int64"
    , testCase "repeated min_items reifies over the element list" $
        refinedFieldType (fieldRules KRepeated [minItems 1])
          @?= Just "Refined (MinLen 1) [a]"
    , testCase "non-reifiable rules yield Nothing" $
        refinedFieldType (fieldRules KString [email]) @?= Nothing
    , -- The reified aliases really are refined predicates: refine enforces them.
      testCase "Refined (Gt 2) Int accepts/rejects at runtime" $ do
        assertBool "5 > 2" (isRight (refine 5 :: Either RefineException (Refined (Gt 2) Int)))
        assertBool "1 not > 2" (isLeft (refine 1 :: Either RefineException (Refined (Gt 2) Int)))
    , testCase "Refined (MinLen 2) [Int] accepts/rejects at runtime" $ do
        assertBool "len 3 >= 2" (isRight (refine [1, 2, 3] :: Either RefineException (Refined (MinLen 2) [Int])))
        assertBool "len 1 < 2" (isLeft (refine [1] :: Either RefineException (Refined (MinLen 2) [Int])))
    ]
