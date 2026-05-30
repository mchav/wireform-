{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reify @buf.validate@ field rules — including /custom/ CEL predicates — as
-- [@refined@](https://hackage.haskell.org/package/refined) refinement types.
--
-- This is the bridge that lets a protobuf field's validation rules affect its
-- type. Two flavors of refinement predicate are produced:
--
--   * the common length/count/comparison rules map to native @refined@
--     predicates via type aliases ('MinLen', 'MaxLen', 'Gt', …); and
--   * /every other rule/ — the well-known string formats, regex patterns, and
--     arbitrary @(buf.validate.field).cel@ expressions — maps to the 'Cel'
--     predicate, whose CEL source lives at the type level (a 'Symbol') and is
--     evaluated at runtime against the value bound to @this@.
--
-- 'refinedFieldType' turns a 'FieldRules' into the @'R.Refined' \<p\> \<base\>@
-- type expression a code generator would splice for that field, so __custom
-- predicates reify to refinement types too__.
--
-- @
-- -- A field with a standard length rule and a custom CEL predicate:
-- refinedFieldType (fieldRules KString [minLen 3])  -- Just \"Refined (MinLen 3) Text\"
--
-- -- The Cel predicate is a real refined predicate:
-- refine \"alice\@example.com\" :: Either RefineException (Refined (Cel \"this.isEmail()\") Text)
-- @
module Protovalidate.Refined
  ( -- * Native refinement-type aliases
    MinLen
  , MaxLen
  , LenEq
  , Gt
  , Gte
  , Lt
  , Lte
  , ConstEq

    -- * CEL-backed refinement predicates (custom predicates)
  , Cel
  , CelWith
  , CelEnvironment (..)

    -- * Reifying rules into a generated field type
  , refinedFieldType
  , refinedPredicate

    -- * Re-exports from refined
  , R.Refined
  , R.refine
  , R.unrefine
  , R.RefineException
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep, Typeable, typeRep)
import qualified Data.Vector as V
import GHC.TypeLits (KnownSymbol, Nat, Symbol, symbolVal)
import qualified Refined as R

import CEL (compile, evaluate)
import CEL.Environment (Env, bind)
import CEL.Error (errMsg)
import CEL.Value (Value (..))
import Protovalidate.Class (ToCel (..))
import Protovalidate.Constraint (constraintSource)
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules (FieldRules (..), RuleKind (..))

----------------------------------------------------------------------
-- Native predicate aliases (rules -> refined predicates)
----------------------------------------------------------------------

-- | Length/size at least @n@ (@min_len@ / @min_items@ / @min_pairs@).
type MinLen (n :: Nat) = R.Not (R.SizeLessThan n)

-- | Length/size at most @n@ (@max_len@ / @max_items@ / @max_pairs@).
type MaxLen (n :: Nat) = R.Not (R.SizeGreaterThan n)

-- | Exact length/size @n@ (@len@).
type LenEq (n :: Nat) = R.SizeEqualTo n

-- | Strictly greater than @n@ (@gt@).
type Gt (n :: Nat) = R.GreaterThan n

-- | Greater than or equal to @n@ (@gte@).
type Gte (n :: Nat) = R.From n

-- | Strictly less than @n@ (@lt@).
type Lt (n :: Nat) = R.LessThan n

-- | Less than or equal to @n@ (@lte@).
type Lte (n :: Nat) = R.To n

-- | Equal to @n@ (numeric @const@).
type ConstEq (n :: Nat) = R.EqualTo n

----------------------------------------------------------------------
-- CEL-backed refinement predicates
----------------------------------------------------------------------

-- | A refinement predicate carrying a CEL expression at the type level. The
-- expression is evaluated (in the standard protovalidate CEL environment) with
-- the refined value bound to @this@; it must yield @true@ / @""@ to satisfy the
-- predicate. This is how /any/ custom CEL predicate becomes a refinement type:
--
-- @'R.Refined' ('Cel' \"this.startsWith('x') && this.size() < 10\") Text@
data Cel (expr :: Symbol)

instance (KnownSymbol expr, ToCel x) => R.Predicate (Cel expr) x where
  validate p value = celValidate (typeRep p) libraryEnv (symbolVal (Proxy :: Proxy expr)) value

-- | Like 'Cel', but evaluated in a caller-supplied environment selected by a
-- type-level tag (via 'CelEnvironment'), so custom CEL /functions/ can back the
-- predicate.
data CelWith (tag :: Type) (expr :: Symbol)

-- | Associates a type-level tag with a CEL environment (typically
-- 'Protovalidate.Library.libraryEnv' extended with custom functions via
-- 'CEL.Environment.addFunction').
class CelEnvironment tag where
  celEnvironment :: Proxy tag -> Env

instance
  (KnownSymbol expr, Typeable tag, CelEnvironment tag, ToCel x)
  => R.Predicate (CelWith tag expr) x
  where
  validate p value =
    celValidate (typeRep p) (celEnvironment (Proxy :: Proxy tag)) (symbolVal (Proxy :: Proxy expr)) value

-- Shared evaluation for the CEL-backed predicates.
celValidate :: ToCel x => TypeRep -> Env -> String -> x -> Maybe R.RefineException
celValidate tr env src value =
  case compile (T.pack src) of
    Left e -> fault ("invalid CEL: " <> errMsg e)
    Right expr -> case evaluate (bind "this" (toCel value) env) expr of
      Right (VBool True) -> Nothing
      Right (VString "") -> Nothing
      Right (VBool False) -> fault "CEL predicate does not hold"
      Right (VString msg) -> fault msg
      Right _ -> fault "CEL predicate must evaluate to bool or string"
      Left err -> fault (errMsg err)
  where
    fault = Just . R.RefineOtherException tr

----------------------------------------------------------------------
-- Reifying a FieldRules into a generated type expression
----------------------------------------------------------------------

-- | The full @'R.Refined' \<predicate\> \<base\>@ type expression for a field,
-- or 'Nothing' if the field has no rules (and no base type). This is what a
-- code generator would splice in place of the plain field type.
refinedFieldType :: FieldRules -> Maybe Text
refinedFieldType fr = do
  kind <- frKind fr
  base <- baseType kind
  pred_ <- refinedPredicate fr
  pure ("Refined (" <> pred_ <> ") " <> base)

-- | The predicate part of 'refinedFieldType' (everything inside
-- @Refined (…)@), combining standard rules, well-known formats, and custom CEL
-- constraints. 'Nothing' when the field has no rules at all.
refinedPredicate :: FieldRules -> Maybe Text
refinedPredicate fr =
  let stds = case frKind fr of
        Just kind -> concatMap (rulePred kind) (frRules fr)
        Nothing -> []
      customs = map (celPred . constraintSource) (frCustom fr)
   in case stds ++ customs of
        [] -> Nothing
        ps -> Just (foldr1 conj ps)
  where
    conj a b = "And (" <> a <> ") (" <> b <> ")"

-- A single rule becomes either a native predicate alias (type-level Nat) or a
-- CEL-backed predicate.
rulePred :: RuleKind -> (Text, Value) -> [Text]
rulePred kind rv@(_, _) =
  case natAlias kind rv of
    Just a -> [a]
    Nothing -> case inlineRuleExpr kind rv of
      Just e -> [celPred e]
      Nothing -> []

celPred :: Text -> Text
celPred expr = "Cel " <> T.pack (show (T.unpack expr))

-- Rules expressible as a type-level natural predicate.
natAlias :: RuleKind -> (Text, Value) -> Maybe Text
natAlias kind (name, value) = case (name, natLit value) of
  ("min_len", Just n) -> Just ("MinLen " <> n)
  ("max_len", Just n) -> Just ("MaxLen " <> n)
  ("len", Just n) -> Just ("LenEq " <> n)
  ("min_items", Just n) -> Just ("MinLen " <> n)
  ("max_items", Just n) -> Just ("MaxLen " <> n)
  ("min_pairs", Just n) -> Just ("MinLen " <> n)
  ("max_pairs", Just n) -> Just ("MaxLen " <> n)
  ("gt", Just n) | numeric kind -> Just ("Gt " <> n)
  ("gte", Just n) | numeric kind -> Just ("Gte " <> n)
  ("lt", Just n) | numeric kind -> Just ("Lt " <> n)
  ("lte", Just n) | numeric kind -> Just ("Lte " <> n)
  ("const", Just n) | numeric kind -> Just ("ConstEq " <> n)
  _ -> Nothing

-- A self-contained CEL expression for rules not expressible as a Nat predicate.
inlineRuleExpr :: RuleKind -> (Text, Value) -> Maybe Text
inlineRuleExpr kind (name, value) = case name of
  "email" -> Just "this.isEmail()"
  "hostname" -> Just "this.isHostname()"
  "ip" -> Just "this.isIp()"
  "ipv4" -> Just "this.isIp(4)"
  "ipv6" -> Just "this.isIp(6)"
  "uri" -> Just "this.isUri()"
  "uri_ref" -> Just "this.isUriRef()"
  "address" -> Just "this.isIp() || this.isHostname()"
  "host_and_port" -> Just "this.isHostAndPort(true)"
  "uuid" -> Just "this.matches('^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$')"
  "pattern" -> celMethod "matches"
  "prefix" -> celMethod "startsWith"
  "suffix" -> celMethod "endsWith"
  "contains" -> celMethod "contains"
  "not_contains" -> ("!" <>) <$> celMethod "contains"
  "unique" -> Just "unique(this)"
  "const" -> binOp "=="
  "lt" -> binOp "<"
  "lte" -> binOp "<="
  "gt" -> binOp ">"
  "gte" -> binOp ">="
  "in" -> Just ("this in " <> celLit value)
  "not_in" -> Just ("!(this in " <> celLit value <> ")")
  _ -> Nothing
  where
    _ = kind
    celMethod m = Just ("this." <> m <> "(" <> celLit value <> ")")
    binOp op = Just ("this " <> op <> " " <> celLit value)

-- A non-negative integer literal usable as a type-level Nat.
natLit :: Value -> Maybe Text
natLit = \case
  VInt n | n >= 0 -> Just (T.pack (show n))
  VUInt n -> Just (T.pack (show n))
  _ -> Nothing

-- Render a CEL Value as CEL source.
celLit :: Value -> Text
celLit = \case
  VBool b -> if b then "true" else "false"
  VInt n -> T.pack (show n)
  VUInt n -> T.pack (show n) <> "u"
  VDouble d -> T.pack (show d)
  VString s -> celString s
  VBytes _ -> "b''"
  VList xs -> "[" <> T.intercalate ", " (map celLit (V.toList xs)) <> "]"
  VNull -> "null"
  _ -> "null"

celString :: Text -> Text
celString s = "'" <> T.replace "'" "\\'" (T.replace "\\" "\\\\" s) <> "'"

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
  KFloat -> True
  KDouble -> True
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
  KFloat -> Just "Float"
  KDouble -> Just "Double"
  KBool -> Just "Bool"
  _ -> Nothing
