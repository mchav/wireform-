{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reify @buf.validate@ field rules as
-- [@refined@](https://hackage.haskell.org/package/refined) refinement types.
--
-- This is the bridge that lets a protobuf field's validation rules /affect its
-- type/: a @string@ field with @(buf.validate.field).string.min_len = 3@ can be
-- given the type @'R.Refined' (MinLen 3) Text@ so that the constraint is
-- enforced (and documented) by the type system, with @refined@'s 'R.refine'
-- doing the runtime check.
--
-- Two things are provided:
--
--   * type aliases mapping the common rules onto @refined@ predicates
--     ('MinLen', 'MaxLen', 'LenEq', 'Gt', 'Gte', 'Lt', 'Lte', 'ConstEq'); and
--   * 'refinedFieldType', which turns a 'FieldRules' into the Haskell type
--     expression a code generator would emit for that field — i.e. it makes it
--     possible for protobuf codegen to let the rules affect the generated
--     types.
--
-- Only constraints expressible with type-level naturals are reified (length /
-- count bounds and non-negative integer comparisons); other rules continue to
-- be enforced by the value-level validator in "Protovalidate.Eval".
module Protovalidate.Refined
  ( -- * Refinement-type aliases for buf.validate rules
    MinLen
  , MaxLen
  , LenEq
  , Gt
  , Gte
  , Lt
  , Lte
  , ConstEq

    -- * Reifying rules into a generated field type
  , refinedFieldType
  , refinedPredicate

    -- * Re-exports from refined
  , R.Refined
  , R.refine
  , R.unrefine
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Nat)
import qualified Refined as R

import CEL.Value (Value (..))
import Protovalidate.Rules (FieldRules (..), RuleKind (..))

----------------------------------------------------------------------
-- Predicate aliases (rules -> refined predicates)
----------------------------------------------------------------------

-- | Length/size at least @n@ (for @min_len@ / @min_items@ / @min_pairs@).
type MinLen (n :: Nat) = R.Not (R.SizeLessThan n)

-- | Length/size at most @n@ (for @max_len@ / @max_items@ / @max_pairs@).
type MaxLen (n :: Nat) = R.Not (R.SizeGreaterThan n)

-- | Exact length/size @n@ (for @len@).
type LenEq (n :: Nat) = R.SizeEqualTo n

-- | Strictly greater than @n@ (for @gt@).
type Gt (n :: Nat) = R.GreaterThan n

-- | Greater than or equal to @n@ (for @gte@).
type Gte (n :: Nat) = R.From n

-- | Strictly less than @n@ (for @lt@).
type Lt (n :: Nat) = R.LessThan n

-- | Less than or equal to @n@ (for @lte@).
type Lte (n :: Nat) = R.To n

-- | Equal to @n@ (for the numeric @const@).
type ConstEq (n :: Nat) = R.EqualTo n

----------------------------------------------------------------------
-- Reifying a FieldRules into a generated type expression
----------------------------------------------------------------------

-- | The full @'R.Refined' \<predicate\> \<base\>@ type expression for a field,
-- or 'Nothing' if none of its rules are reifiable as refinement types. This is
-- what a code generator would splice in place of the plain field type.
--
-- >>> refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
-- Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
refinedFieldType :: FieldRules -> Maybe Text
refinedFieldType fr = do
  kind <- frKind fr
  base <- baseType kind
  pred_ <- refinedPredicate fr
  pure ("Refined (" <> pred_ <> ") " <> base)

-- | Just the predicate part of 'refinedFieldType' (the @\<predicate\>@), or
-- 'Nothing' if no rule is reifiable.
refinedPredicate :: FieldRules -> Maybe Text
refinedPredicate fr = case frKind fr of
  Nothing -> Nothing
  Just kind -> case concatMap (rulePredicate kind) (frRules fr) of
    [] -> Nothing
    ps -> Just (foldr1 conj ps)
  where
    conj a b = "And (" <> a <> ") (" <> b <> ")"

-- Map a single (ruleName, value) to a predicate alias application, when it is
-- expressible with a type-level natural.
rulePredicate :: RuleKind -> (Text, Value) -> [Text]
rulePredicate kind (name, value) = case (name, natLit value) of
  ("min_len", Just n) -> ["MinLen " <> n]
  ("max_len", Just n) -> ["MaxLen " <> n]
  ("len", Just n) -> ["LenEq " <> n]
  ("min_items", Just n) -> ["MinLen " <> n]
  ("max_items", Just n) -> ["MaxLen " <> n]
  ("min_pairs", Just n) -> ["MinLen " <> n]
  ("max_pairs", Just n) -> ["MaxLen " <> n]
  ("gt", Just n) | numeric kind -> ["Gt " <> n]
  ("gte", Just n) | numeric kind -> ["Gte " <> n]
  ("lt", Just n) | numeric kind -> ["Lt " <> n]
  ("lte", Just n) | numeric kind -> ["Lte " <> n]
  ("const", Just n) | numeric kind -> ["ConstEq " <> n]
  _ -> []

-- A non-negative integer literal usable as a type-level Nat.
natLit :: Value -> Maybe Text
natLit = \case
  VInt n | n >= 0 -> Just (T.pack (show n))
  VUInt n -> Just (T.pack (show n))
  _ -> Nothing

numeric :: RuleKind -> Bool
numeric = \case
  KInt32 -> True
  KInt64 -> True
  KUint32 -> True
  KUint64 -> True
  KSint32 -> True
  KSint64 -> True
  KFixed32 -> True
  KFixed64 -> True
  KSfixed32 -> True
  KSfixed64 -> True
  KEnum -> True
  _ -> False

baseType :: RuleKind -> Maybe Text
baseType = \case
  KString -> Just "Text"
  KBytes -> Just "ByteString"
  KRepeated -> Just "[a]"
  KInt32 -> Just "Int32"
  KInt64 -> Just "Int64"
  KSint32 -> Just "Int32"
  KSint64 -> Just "Int64"
  KSfixed32 -> Just "Int32"
  KSfixed64 -> Just "Int64"
  KEnum -> Just "Int32"
  KUint32 -> Just "Word32"
  KUint64 -> Just "Word64"
  KFixed32 -> Just "Word32"
  KFixed64 -> Just "Word64"
  _ -> Nothing
