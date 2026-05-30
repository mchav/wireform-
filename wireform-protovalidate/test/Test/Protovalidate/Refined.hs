{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for reifying buf.validate rules (standard /and custom/) as @refined@
-- refinement types.
module Test.Protovalidate.Refined (tests) where

import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Text (Text)
import Refined (RefineException, Refined, refine)
import Test.Tasty
import Test.Tasty.HUnit

import CEL (Value (..))
import CEL.Environment (addFunction)
import Protovalidate
import Protovalidate.Constraint (mkConstraint)
import Protovalidate.Library (libraryEnv)
import Protovalidate.Refined (Cel, CelEnvironment (..), CelWith, Gt, MinLen)

-- A custom environment exposing a user-defined CEL function.
data OddEnv

instance CelEnvironment OddEnv where
  celEnvironment _ = addFunction "isOdd" odd_ libraryEnv
    where
      odd_ [VInt n] = Just (Right (VBool (odd n)))
      odd_ _ = Nothing

customField :: FieldRules
customField =
  (fieldRules KString [minLen 3])
    { frCustom = [either (error . show) id (mkConstraint "f.x" "must start with x" "this.startsWith('x')")]
    }

tests :: TestTree
tests =
  testGroup
    "refined reification"
    [ testCase "Nat rules reify to native refined predicates" $
        refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
          @?= Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
    , testCase "numeric gt/lte reify to native predicates" $
        refinedFieldType (fieldRules KInt64 [gtV (VInt 0), lteV (VInt 150)])
          @?= Just "Refined (And (Gt 0) (Lte 150)) Int64"
    , testCase "well-known format rules reify to a Cel predicate" $
        refinedFieldType (fieldRules KString [email])
          @?= Just "Refined (Cel \"this.isEmail()\") Text"
    , testCase "custom CEL constraints reify to a Cel predicate too" $
        refinedFieldType customField
          @?= Just "Refined (And (MinLen 3) (Cel \"this.startsWith('x')\")) Text"
    , -- The native aliases are real refined predicates.
      testCase "Refined (Gt 2) Int accepts/rejects at runtime" $ do
        assertBool "5 > 2" (isRight (refine 5 :: Either RefineException (Refined (Gt 2) Int)))
        assertBool "1 not > 2" (isLeft (refine 1 :: Either RefineException (Refined (Gt 2) Int)))
    , testCase "Refined (MinLen 2) [Int] accepts/rejects at runtime" $ do
        assertBool "len 3" (isRight (refine [1, 2, 3] :: Either RefineException (Refined (MinLen 2) [Int])))
        assertBool "len 1" (isLeft (refine [1] :: Either RefineException (Refined (MinLen 2) [Int])))
    , -- The Cel predicate runs the CEL expression at runtime.
      testCase "Refined (Cel \"this.isEmail()\") Text enforces the format" $ do
        assertBool "valid email" (isRight (refine "a@b.com" :: Either RefineException (Refined (Cel "this.isEmail()") Text)))
        assertBool "invalid email" (isLeft (refine "nope" :: Either RefineException (Refined (Cel "this.isEmail()") Text)))
    , testCase "CelWith uses a custom-function environment" $ do
        assertBool "3 is odd" (isRight (refine 3 :: Either RefineException (Refined (CelWith OddEnv "isOdd(this)") Int64)))
        assertBool "4 not odd" (isLeft (refine 4 :: Either RefineException (Refined (CelWith OddEnv "isOdd(this)") Int64)))
    ]
