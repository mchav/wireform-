{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for reifying buf.validate rules (standard /and custom/) as @refined@
refinement types.
-}
module Test.Protovalidate.Refined (tests) where

import CEL (Value (..))
import CEL.Environment (addFunction)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Text (Text)
import Protovalidate
import Protovalidate.Constraint (mkConstraint)
import Protovalidate.Library (libraryEnv)
import Protovalidate.Refined (Cel, CelEnvironment (..), CelWith, Gt, MinLen)
import Refined (RefineException, Refined, refine)
import Test.Syd


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


tests :: Spec
tests =
  describe
    "refined reification"
    $ sequence_
      [ it "Nat rules reify to native refined predicates" $
          refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
            `shouldBe` Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
      , it "numeric gt/lte reify to native predicates" $
          refinedFieldType (fieldRules KInt64 [gtV (VInt 0), lteV (VInt 150)])
            `shouldBe` Just "Refined (And (Gt 0) (Lte 150)) Int64"
      , it "well-known format rules reify to a Cel predicate" $
          refinedFieldType (fieldRules KString [email])
            `shouldBe` Just "Refined (Cel \"this.isEmail()\") Text"
      , it "custom CEL constraints reify to a Cel predicate too" $
          refinedFieldType customField
            `shouldBe` Just "Refined (And (MinLen 3) (Cel \"this.startsWith('x')\")) Text"
      , -- The native aliases are real refined predicates.
        it "Refined (Gt 2) Int accepts/rejects at runtime" $ do
          (isRight (refine 5 :: Either RefineException (Refined (Gt 2) Int))) `shouldBe` True
          (isLeft (refine 1 :: Either RefineException (Refined (Gt 2) Int))) `shouldBe` True
      , it "Refined (MinLen 2) [Int] accepts/rejects at runtime" $ do
          (isRight (refine [1, 2, 3] :: Either RefineException (Refined (MinLen 2) [Int]))) `shouldBe` True
          (isLeft (refine [1] :: Either RefineException (Refined (MinLen 2) [Int]))) `shouldBe` True
      , -- The Cel predicate runs the CEL expression at runtime.
        it "Refined (Cel \"this.isEmail()\") Text enforces the format" $ do
          (isRight (refine "a@b.com" :: Either RefineException (Refined (Cel "this.isEmail()") Text))) `shouldBe` True
          (isLeft (refine "nope" :: Either RefineException (Refined (Cel "this.isEmail()") Text))) `shouldBe` True
      , it "CelWith uses a custom-function environment" $ do
          (isRight (refine 3 :: Either RefineException (Refined (CelWith OddEnv "isOdd(this)") Int64))) `shouldBe` True
          (isLeft (refine 4 :: Either RefineException (Refined (CelWith OddEnv "isOdd(this)") Int64))) `shouldBe` True
      ]
