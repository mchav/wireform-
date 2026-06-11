{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Generate a protobuf message validator __at compile time__, with every
rule's CEL compiled to Haskell.

Where "Protovalidate.Eval" interprets each constraint's CEL on every call,
'compileMessageValidator' reads a @.proto@'s @buf.validate@ rules at compile
time and emits a @'Value' -> ['Violation']@ function in which each rule (the
standard ones, inlined to a self-contained CEL expression over @this@, and
any custom @(buf.validate.field).cel@) is turned into Haskell via
"CEL.TH".'CEL.TH.compileCelFn'. There is no runtime parsing, no AST walk,
and no per-node dispatch — GHC optimizes the predicates like ordinary code.

@
{\-# LANGUAGE TemplateHaskell #-\}
import Protovalidate
import Protovalidate.TH (compileMessageValidator)
import MyProtoSource (userProto)   -- a separate module (stage restriction)

validateUser :: Value -> [Violation]
validateUser = $(compileMessageValidator userProto \"User\")
@

The current generator validates the message's own fields (standard +
custom + message-level CEL); nested-message and repeated-element recursion
is not yet emitted.
-}
module Protovalidate.TH (
  compileMessageValidator,
  messageValidatorE,
  runCompiledValidator,
  CompiledConstraint,
) where

import CEL.Environment (bind)
import CEL.Error (errMsg)
import CEL.Eval (Compiled)
import CEL.TH (compileCelFn)
import CEL.Value (Duration (..), Timestamp (..), Value (..), celMapLookup)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH (Exp, Q, listE)
import Language.Haskell.TH.Syntax (lift)
import Protovalidate.Constraint (Constraint (..))
import Protovalidate.Library (libraryEnv)
import Protovalidate.Rules
import Protovalidate.Schema (parseProtoRules)
import Protovalidate.Violation (Violation (..))


{- | A compiled constraint: its id, fallback message, and the CEL program
compiled to Haskell.
-}
type CompiledConstraint = (Text, Text, Compiled)


----------------------------------------------------------------------
-- Compile-time generation
----------------------------------------------------------------------

{- | Parse a @.proto@ at compile time and emit a compiled validator
(@'Value' -> ['Violation']@) for the named message.
-}
compileMessageValidator :: Text -> Text -> Q Exp
compileMessageValidator protoSrc name = case parseProtoRules protoSrc of
  Left err -> fail ("Protovalidate.TH: " <> T.unpack err)
  Right rs -> case lookup name rs of
    Nothing -> fail ("Protovalidate.TH: no message named " <> T.unpack name)
    Just mr -> messageValidatorE mr


-- | Emit a compiled validator for an already-known 'MessageRules'.
messageValidatorE :: MessageRules -> Q Exp
messageValidatorE mr = do
  fieldExps <- mapM emitField (mrFields mr)
  msgExps <- mapM emitConstraint (map fromCustom (mrCustom mr))
  [|runCompiledValidator $(listE (map pure fieldExps)) $(listE (map pure msgExps))|]
  where
    emitField (fname, fr) = do
      conExps <- mapM emitConstraint (fieldConstraints fr)
      [|($(lift fname), $(listE (map pure conExps)))|]


-- Emit one (id, message, compiledProgram) triple.
emitConstraint :: (Text, Text, Text) -> Q Exp
emitConstraint (cid, msg, src) =
  [|($(lift cid), $(lift msg), $(compileCelFn (T.unpack src)))|]


fromCustom :: Constraint -> (Text, Text, Text)
fromCustom c = (constraintId c, constraintMessage c, constraintSource c)


-- The (id, message, self-contained CEL) triples for a field: applicable
-- standard rules (inlined over @this@) plus custom CEL constraints.
fieldConstraints :: FieldRules -> [(Text, Text, Text)]
fieldConstraints fr =
  standard ++ map fromCustom (frCustom fr)
  where
    standard = case frKind fr of
      Nothing -> []
      Just kind ->
        [ (cid, msg, src)
        | (rf, value) <- frRules fr
        , active value
        , Just src <- [inlineRuleCel rf value]
        , let (cid, msg) = standardMeta kind rf
        ]
    active v = case v of VBool False -> False; _ -> True


standardMeta :: RuleKind -> Text -> (Text, Text)
standardMeta kind rf =
  case lookup rf [(r, (constraintId c, constraintMessage c)) | (r, c) <- standardConstraints kind] of
    Just m -> m
    Nothing -> (rf, "constraint failed")


----------------------------------------------------------------------
-- Runtime support (referenced by the emitted code)
----------------------------------------------------------------------

{- | Run a compiled validator: per-field constraints (with @this@ bound to the
field value) and message-level constraints (with @this@ bound to the whole
message). Used by the code 'compileMessageValidator' emits.
-}
runCompiledValidator
  :: [(Text, [CompiledConstraint])]
  -> [CompiledConstraint]
  -> Value
  -> [Violation]
runCompiledValidator fields msgCons msg =
  concatMap fieldChecks fields ++ runCons "" msg msgCons
  where
    fieldChecks (fname, cons) = case msg of
      VMap m -> case celMapLookup (VString fname) m of
        Just v -> runCons fname v cons
        Nothing -> []
      _ -> []

    runCons path this = concatMap (one path this)
    one path this (cid, m, prog) =
      let env = bind "this" this libraryEnv
      in case prog env of
           Right (VBool True) -> []
           Right (VString "") -> []
           Right (VBool False) -> [Violation path cid m]
           Right (VString s) -> [Violation path cid s]
           Right _ -> [Violation path cid "constraint must evaluate to bool or string"]
           Left e -> [Violation path cid ("evaluation error: " <> errMsg e)]


----------------------------------------------------------------------
-- Inlining standard rules to self-contained CEL (over @this@)
----------------------------------------------------------------------

{- | A self-contained CEL expression (referencing only @this@) for a standard
rule, with the rule value inlined as a literal. 'Nothing' for rules with no
direct CEL form.
-}
inlineRuleCel :: Text -> Value -> Maybe Text
inlineRuleCel rf value = case rf of
  "const" -> Just ("this == " <> celLit value)
  "lt" -> Just ("this < " <> celLit value)
  "lte" -> Just ("this <= " <> celLit value)
  "gt" -> Just ("this > " <> celLit value)
  "gte" -> Just ("this >= " <> celLit value)
  "in" -> Just ("this in " <> celLit value)
  "not_in" -> Just ("!(this in " <> celLit value <> ")")
  "len" -> Just ("uint(size(this)) == " <> celLit value)
  "min_len" -> Just ("uint(size(this)) >= " <> celLit value)
  "max_len" -> Just ("uint(size(this)) <= " <> celLit value)
  "min_bytes" -> Just ("uint(size(bytes(this))) >= " <> celLit value)
  "max_bytes" -> Just ("uint(size(bytes(this))) <= " <> celLit value)
  "len_bytes" -> Just ("uint(size(bytes(this))) == " <> celLit value)
  "min_items" -> Just ("uint(size(this)) >= " <> celLit value)
  "max_items" -> Just ("uint(size(this)) <= " <> celLit value)
  "min_pairs" -> Just ("uint(size(this)) >= " <> celLit value)
  "max_pairs" -> Just ("uint(size(this)) <= " <> celLit value)
  "pattern" -> Just ("this.matches(" <> celLit value <> ")")
  "prefix" -> Just ("this.startsWith(" <> celLit value <> ")")
  "suffix" -> Just ("this.endsWith(" <> celLit value <> ")")
  "contains" -> Just ("this.contains(" <> celLit value <> ")")
  "not_contains" -> Just ("!this.contains(" <> celLit value <> ")")
  "email" -> Just "this.isEmail()"
  "hostname" -> Just "this.isHostname()"
  "ip" -> Just "this.isIp()"
  "ipv4" -> Just "this.isIp(4)"
  "ipv6" -> Just "this.isIp(6)"
  "ip_prefix" -> Just "this.isIpPrefix()"
  "ipv4_prefix" -> Just "this.isIpPrefix(4, true)"
  "ipv6_prefix" -> Just "this.isIpPrefix(6, true)"
  "ip_with_prefixlen" -> Just "this.isIpPrefix()"
  "ipv4_with_prefixlen" -> Just "this.isIpPrefix(4)"
  "ipv6_with_prefixlen" -> Just "this.isIpPrefix(6)"
  "uri" -> Just "this.isUri()"
  "uri_ref" -> Just "this.isUriRef()"
  "address" -> Just "this.isIp() || this.isHostname()"
  "host_and_port" -> Just "this.isHostAndPort(true)"
  "uuid" -> Just "this.matches('^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$')"
  "tuuid" -> Just "this.matches('^[0-9a-fA-F]{32}$')"
  "unique" -> Just "unique(this)"
  "finite" -> Just "!isInf(this) && !isNan(this)"
  _ -> Nothing


celLit :: Value -> Text
celLit = \case
  VBool b -> if b then "true" else "false"
  VInt n -> tshow n
  VUInt n -> tshow n <> "u"
  VDouble d -> tshow d
  VString s -> celString s
  VBytes b -> celBytes b
  VList xs -> "[" <> T.intercalate ", " (map celLit (V.toList xs)) <> "]"
  VTimestamp (Timestamp s _) -> "timestamp(" <> tshow s <> ")"
  VDuration d -> celDuration d
  VNull -> "null"
  _ -> "null"
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show


-- A CEL @duration("…s")@ literal. Seconds plus a 9-digit fractional part for
-- any nanoseconds (trailing zeros trimmed).
celDuration :: Duration -> Text
celDuration (Duration s n)
  | n == 0 = "duration(\"" <> tshow s <> "s\")"
  | otherwise = "duration(\"" <> sign <> tshow (abs s) <> "." <> frac <> "s\")"
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    sign = if s < 0 || n < 0 then "-" else ""
    frac =
      let padded = T.justifyRight 9 '0' (tshow (abs (fromIntegral n :: Integer)))
      in case T.dropWhileEnd (== '0') padded of
           "" -> "0"
           t -> t


celString :: Text -> Text
celString s = "'" <> T.replace "'" "\\'" (T.replace "\\" "\\\\" s) <> "'"


-- A CEL bytes literal: every octet as a @\\xHH@ escape inside @b'…'@.
celBytes :: BS.ByteString -> Text
celBytes b = "b'" <> T.concat (map hexEsc (BS.unpack b)) <> "'"
  where
    hexEsc w = "\\x" <> T.pack [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
    hexDigit n = "0123456789abcdef" !! fromIntegral n
