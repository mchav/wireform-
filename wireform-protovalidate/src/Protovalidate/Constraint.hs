-- | A compiled validation constraint: an identifier, a default failure
-- message, and a CEL expression to evaluate (the equivalent of a
-- @buf.validate.Constraint@ / cel-go @CompiledProgram@).
module Protovalidate.Constraint
  ( Constraint (..)
  , mkConstraint
  , unsafeConstraint
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import CEL (Expr, compile)
import CEL.Error (CelError, errMsg)

-- | A constraint pairs an identifier and a fallback message with a compiled
-- CEL expression. The expression is evaluated with @this@ (and, for standard
-- constraints, @rules@) bound; it must yield a @bool@ (where @false@ is a
-- violation) or a @string@ (where a non-empty result is the violation
-- message).
data Constraint = Constraint
  { constraintId :: !Text
  , constraintMessage :: !Text
  , constraintExpr :: !Expr
  , constraintSource :: !Text
  }

instance Show Constraint where
  show c =
    "Constraint "
      <> show (constraintId c)
      <> " "
      <> show (constraintSource c)

-- | Compile a constraint from its id, fallback message, and CEL source.
mkConstraint :: Text -> Text -> Text -> Either CelError Constraint
mkConstraint cid msg src = case compile src of
  Left err -> Left err
  Right expr -> Right (Constraint cid msg expr src)

-- | Compile a constraint, throwing if the CEL source does not parse. Intended
-- only for statically-known constraint sources (e.g. the standard rule table).
unsafeConstraint :: Text -> Text -> Text -> Constraint
unsafeConstraint cid msg src = case mkConstraint cid msg src of
  Right c -> c
  Left err -> error ("Protovalidate.Constraint: invalid built-in CEL: " <> T.unpack (errMsg err) <> " in " <> T.unpack src)
