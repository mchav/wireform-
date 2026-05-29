{-# LANGUAGE OverloadedStrings #-}

-- | The validation engine: evaluate a message's standard and custom CEL
-- constraints and collect 'Violation's.
--
-- A message is represented as a CEL 'VMap' from field name to value (use
-- "Protovalidate.Proto" to obtain one from a @wireform-proto@ dynamic
-- message). Each field's value is bound to @this@ and its rule message to
-- @rules@ before the applicable standard constraints (and any custom CEL) are
-- evaluated. A constraint that yields @false@ — or a non-empty @string@ —
-- produces a violation.
module Protovalidate.Eval
  ( validate
  , validateIn
  , evalConstraint
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import CEL (evaluate)
import CEL.Environment (Env, bind)
import CEL.Error (CelError, errMsg, invalidArg)
import CEL.Value
  ( CelMap
  , Value (..)
  , celMapFromList
  , celMapLookup
  , celMapSize
  )
import Protovalidate.Constraint (Constraint (..))
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules
import Protovalidate.Violation (Violation (..))

-- | Validate a message (as a CEL value, normally a 'VMap') against a
-- 'MessageRules' using the standard protovalidate CEL environment (base CEL
-- plus the protovalidate extension library).
validate :: Value -> MessageRules -> [Violation]
validate = validateIn libraryEnv

-- | As 'validate', but with a caller-supplied base environment (e.g. with
-- additional custom functions or variables registered).
validateIn :: Env -> Value -> MessageRules -> [Violation]
validateIn env = validateMessage env ""

-- | Evaluate a single constraint in an environment where @this@/@rules@ are
-- already bound. Returns 'Nothing' when satisfied, or @'Just' message@ for a
-- violation.
evalConstraint :: Env -> Constraint -> Either CelError (Maybe Text)
evalConstraint env con = case evaluate env (constraintExpr con) of
  Left err -> Left err
  Right (VBool True) -> Right Nothing
  Right (VBool False) -> Right (Just (constraintMessage con))
  Right (VString "") -> Right Nothing
  Right (VString s) -> Right (Just s)
  Right _ -> Left (invalidArg "constraint expression must evaluate to bool or string")

validateMessage :: Env -> Text -> Value -> MessageRules -> [Violation]
validateMessage env path msg mr =
  concatMap (validateNamedField env path msg) (mrFields mr)
    ++ runConstraints (bind "this" msg env) path (mrCustom mr)

validateNamedField :: Env -> Text -> Value -> (Text, FieldRules) -> [Violation]
validateNamedField env prefixPath msg (name, fr) =
  case lookupField name msg of
    Nothing
      | frRequired fr -> [Violation fieldPath "required" "value is required"]
      | otherwise -> []
    Just v -> validateFieldValue env fieldPath v fr
  where
    fieldPath = if T.null prefixPath then name else prefixPath <> "." <> name

lookupField :: Text -> Value -> Maybe Value
lookupField name (VMap m) = celMapLookup (VString name) m
lookupField _ _ = Nothing

validateFieldValue :: Env -> Text -> Value -> FieldRules -> [Violation]
validateFieldValue env path v fr
  | frIgnoreEmpty fr && isZeroValue v = []
  | otherwise = stdViols ++ customViols ++ itemViols ++ nestedViols
  where
    rulesMap = VMap (rulesAsMap (frRules fr))
    env' = bind "rules" rulesMap (bind "this" v env)

    applicable =
      [ con
      | (ruleField, con) <- maybe [] standardConstraints (frKind fr)
      , ruleActive ruleField (frRules fr)
      ]
    stdViols = runConstraints env' path applicable
    customViols = runConstraints env' path (frCustom fr)

    itemViols = case (frItems fr, v) of
      (Just ifr, VList xs) ->
        concat (zipWith (\i x -> validateFieldValue env (indexed path i) x ifr) [0 ..] (V.toList xs))
      _ -> []

    nestedViols = case (frMessage fr, v) of
      (Just mr, VMap _) -> validateMessage env path v mr
      (Just mr, VList xs) ->
        concat (zipWith (\i x -> validateMessage env (indexed path i) x mr) [0 ..] (V.toList xs))
      _ -> []

indexed :: Text -> Int -> Text
indexed path i = path <> "[" <> T.pack (show i) <> "]"

rulesAsMap :: [(Text, Value)] -> CelMap
rulesAsMap rs = celMapFromList [(VString k, v) | (k, v) <- rs]

ruleActive :: Text -> [(Text, Value)] -> Bool
ruleActive ruleField rs = case lookup ruleField rs of
  Nothing -> False
  Just (VBool False) -> False
  Just _ -> True

runConstraints :: Env -> Text -> [Constraint] -> [Violation]
runConstraints env path = concatMap go
  where
    go con = case evalConstraint env con of
      Left err -> [Violation path (constraintId con) ("evaluation error: " <> errMsg err)]
      Right Nothing -> []
      Right (Just m) -> [Violation path (constraintId con) m]

isZeroValue :: Value -> Bool
isZeroValue = \case
  VNull -> True
  VBool b -> not b
  VInt 0 -> True
  VUInt 0 -> True
  VDouble d -> d == 0
  VString s -> T.null s
  VBytes b -> BS.null b
  VList xs -> V.null xs
  VMap m -> celMapSize m == 0
  _ -> False
