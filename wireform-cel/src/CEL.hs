-- | A conformant implementation of Google's
-- [Common Expression Language (CEL)](https://github.com/google/cel-spec).
--
-- CEL is a non-Turing-complete, side-effect-free, strongly- and
-- dynamically-typed expression language. This package implements the lexer,
-- parser, runtime value model, and evaluator (including the standard library
-- of operators, functions, conversions, and comprehension macros) described
-- in the CEL [language definition](https://github.com/google/cel-spec/blob/master/doc/langdef.md).
--
-- == Quick start
--
-- @
-- import CEL
--
-- main :: IO ()
-- main = do
--   -- Evaluate a self-contained expression:
--   print ('run' 'emptyEnv' \"1 + 2 * 3\")            -- Right (VInt 7)
--   print ('run' 'emptyEnv' \"[1, 2, 3].map(x, x * x)\") -- Right (VList [1,4,9])
--
--   -- Bind variables:
--   let env = 'bind' \"name\" ('VString' \"world\") 'emptyEnv'
--   print ('run' env \"'Hello, ' + name + '!'\")
-- @
--
-- == Scope
--
-- The core language and standard library are implemented in full, operating
-- on the dynamic 'Value' model. Protocol-buffer message values are not yet
-- modelled (the well-known abstract types @google.protobuf.Timestamp@ and
-- @google.protobuf.Duration@ /are/ supported via 'VTimestamp' / 'VDuration'),
-- and the date/time accessors support @UTC@ and fixed @±HH:MM@ offsets rather
-- than named IANA/Joda timezones.
module CEL
  ( -- * Values
    Value (..)
  , CelType (..)
  , Timestamp (..)
  , Duration (..)
  , CelMap
  , celMap
  , celMapFromList
  , celMapEntries
  , celMapLookup
  , typeOf
  , typeNameText

    -- * Environment
  , Env
  , Overload
  , emptyEnv
  , bind
  , bindAll
  , withContainer
  , addFunction

    -- * Errors
  , CelError (..)
  , ErrKind (..)

    -- * Syntax
  , Expr

    -- * Compiling and evaluating
  , compile
  , evaluate
  , run
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import CEL.Environment
import CEL.Error
import CEL.Eval (evalIn)
import CEL.Parser (parse)
import CEL.Syntax (Expr)
import CEL.Value

-- | Parse CEL source text into an 'Expr', turning lexer/parser failures into a
-- 'CelError' with 'ErrParse' kind.
compile :: Text -> Either CelError Expr
compile src = case parse src of
  Left e -> Left (parseErr (T.pack e))
  Right ex -> Right ex

-- | Evaluate a previously-'compile'd expression in an environment.
evaluate :: Env -> Expr -> Either CelError Value
evaluate = evalIn

-- | Compile and evaluate CEL source text in one step.
run :: Env -> Text -> Either CelError Value
run env src = compile src >>= evalIn env
