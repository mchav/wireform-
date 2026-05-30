{-# LANGUAGE OverloadedStrings #-}

-- | Extract protovalidate 'MessageRules' from @buf.validate@ annotations on a
-- parsed @.proto@ file.
--
-- This closes the gap between annotated schemas and the validation engine:
-- rather than constructing 'MessageRules' by hand, parse a @.proto@ that uses
-- @(buf.validate.field)@ / @(buf.validate.message)@ options and read the rules
-- straight out of the @wireform-proto@ IDL AST.
--
-- Supported annotations:
--
--   * scalar / numeric / bool / bytes / enum / duration / timestamp rules,
--     e.g. @(buf.validate.field).string.min_len = 3@,
--     @(buf.validate.field).int32.gt = 0@;
--   * @repeated@ rules (@min_items@, @max_items@, @unique@) and per-element
--     @repeated.items.<type>.<rule>@ rules;
--   * @map@ rules (@min_pairs@, @max_pairs@) plus @map.keys@ / @map.values@
--     sub-rules;
--   * @enum.defined_only@ (resolving the enum's declared values) and
--     @string.well_known_regex@ (resolving the @KnownRegex@ enum + @strict@),
--     both scalar and @repeated.items@;
--   * @timestamp@ / @duration@ bounds whose option value is a
--     @{seconds:.., nanos:..}@ message literal (and @timestamp.within@, a span);
--   * oneof @required@ (@(buf.validate.oneof).required@);
--   * @required@ and @ignore@ / @ignore_empty@;
--   * field-level and message-level custom CEL
--     (@(buf.validate.field).cel@ / @(buf.validate.message).cel@);
--   * nested message validation (resolved against the other messages in the
--     file, with a cycle guard).
--
-- Reading the equivalent options out of a compiled @FileDescriptorSet@
-- (extension #1159 on @FieldOptions@) additionally requires @descriptor.proto@
-- options support in @wireform-proto@; the @.proto@ AST is the in-repo source
-- of truth used here.
module Protovalidate.Schema
  ( parseProtoRules
  , fileMessageRules
  , extractMessageRules
  ) where

import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import CEL.Error (errMsg)
import Proto.IDL.AST
import Proto.IDL.Inspect (fieldOptionsOf, messageFields, messageMapFields, messageOneofs, messageOptions)
import Proto.IDL.Parser (parseProtoFile)
import Protovalidate.Constraint (Constraint, mkConstraint)
import Protovalidate.Rules
import CEL.Value (Duration (..), Timestamp (..), Value (..))
import qualified Data.Vector as V

-- | Parse @.proto@ source text and extract validation rules for every message
-- it defines, keyed by (simple) message name.
parseProtoRules :: Text -> Either Text [(Text, MessageRules)]
parseProtoRules src = case parseProtoFile "<protovalidate>" src of
  Left err -> Left (T.pack (show err))
  Right pf -> fileMessageRules pf

-- | Extract validation rules for every message in a parsed file.
fileMessageRules :: ProtoFile -> Either Text [(Text, MessageRules)]
fileMessageRules pf =
  let msgs = collectMessages pf
      enums = collectEnums pf
   in traverse (\m -> (,) (msgName m) <$> extractMessageRules enums msgs Set.empty m) msgs

-- | Extract the rules for a single message, given the file's enum + message
-- tables (for enum-value and nested-message resolution) and the set of messages
-- currently being expanded (cycle guard).
extractMessageRules :: [EnumDef] -> [MessageDef] -> Set Text -> MessageDef -> Either Text MessageRules
extractMessageRules enums msgs visiting msg = do
  let vis = Set.insert (msgName msg) visiting
  fields <- traverse (extractField enums msgs vis) (messageFields msg)
  mapFields <- traverse extractMapField (messageMapFields msg)
  customs <- traverse constraintFromAggregate (messageCelOptions msg)
  Right (messageRules (fields ++ mapFields) (customs ++ oneofConstraints msg))

-- | A @(buf.validate.oneof).required@ constraint for each oneof so annotated.
oneofConstraints :: MessageDef -> [Constraint]
oneofConstraints msg =
  [ oneofRequired (oneofName od) (map oneofFieldName (oneofFields od))
  | od <- messageOneofs msg
  , any isOneofRequired (oneofOptions od)
  ]
  where
    isOneofRequired od = case optNameParts (optName od) of
      [ExtensionOption "buf.validate.oneof", SimpleOption "required"] -> isTrue (optValue od)
      _ -> False

-- | Extract rules for a @map@ field (which is a distinct AST element), including
-- @map.keys@ / @map.values@ sub-rules.
extractMapField :: MapField -> Either Text (Text, FieldRules)
extractMapField mf = do
  items <- traverse classify (mapMaybe withParts (mapOptions mf))
  let mapRs = mergeRuleValues [(f, v) | IMapRule f v <- items]
      keyTyped = [(k, f, v) | IMapKeyTyped k f v <- items]
      keyCustoms = [con | IMapKeyCustom con <- items]
      valTyped = [(k, f, v) | IMapValueTyped k f v <- items]
      valCustoms = [con | IMapValueCustom con <- items]
      customs = [con | ICustom con <- items]
      subRules tps cs
        | null tps && null cs = Nothing
        | otherwise =
            Just
              emptyFieldRules
                { frKind = firstKindOf tps
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- tps]
                , frCustom = cs
                }
  Right
    ( mapFieldName mf
    , emptyFieldRules
        { frKind = Just KMap
        , frRules = mapRs
        , frMapKeys = subRules keyTyped keyCustoms
        , frMapValues = subRules valTyped valCustoms
        , frCustom = customs
        , frRequired = any isReqItem items
        , frIgnoreEmpty = any isIgnItem items
        }
    )

firstKindOf :: [(RuleKind, Text, Value)] -> Maybe RuleKind
firstKindOf ts = case ts of ((k, _, _) : _) -> Just k; _ -> Nothing

isReqItem :: Item -> Bool
isReqItem IRequired = True
isReqItem _ = False

isIgnItem :: Item -> Bool
isIgnItem IIgnoreEmpty = True
isIgnItem _ = False

----------------------------------------------------------------------
-- Message-level CEL
----------------------------------------------------------------------

messageCelOptions :: MessageDef -> [Constant]
messageCelOptions msg =
  [ optValue od
  | od <- messageOptions msg
  , case optNameParts (optName od) of
      [ExtensionOption "buf.validate.message", SimpleOption "cel"] -> True
      _ -> False
  ]

----------------------------------------------------------------------
-- Field-level extraction
----------------------------------------------------------------------

-- An interpreted field option.
data Item
  = ICustom !Constraint
  | IRequired
  | IIgnoreEmpty
  | ITyped !RuleKind !Text !Value
  | IRepeatedRule !Text !Value
  | IItemTyped !RuleKind !Text !Value
  | IItemCustom !Constraint
  | IMapRule !Text !Value
  | IMapKeyTyped !RuleKind !Text !Value
  | IMapKeyCustom !Constraint
  | IMapValueTyped !RuleKind !Text !Value
  | IMapValueCustom !Constraint
  | IDefinedOnly
  | IWellKnownRegex !Int
  | IStrict !Bool
  | IItemDefinedOnly
  | IItemWellKnownRegex !Int
  | IItemStrict !Bool
  | ISkip

extractField :: [EnumDef] -> [MessageDef] -> Set Text -> FieldDef -> Either Text (Text, FieldRules)
extractField enums msgs visiting fd = do
  items <- traverse classify (mapMaybe withParts (fieldOptionsOf fd))
  let typed = [(k, f, v) | ITyped k f v <- items]
      repeatedRs = mergeRuleValues [(f, v) | IRepeatedRule f v <- items]
      itemTyped = [(k, f, v) | IItemTyped k f v <- items]
      mapRs = mergeRuleValues [(f, v) | IMapRule f v <- items]
      customs = [c | ICustom c <- items] ++ contextCustoms
      itemCustoms = [c | IItemCustom c <- items] ++ itemContextCustoms
      isRequired = any isReq items
      isIgnoreEmpty = any isIgn items
      isRepeated = fieldLabel fd == Just Repeated
      namedMsg = resolveMessage enums msgs visiting (fieldType fd)

      -- Constraints needing the field's declared type (enum values for
      -- @defined_only@) or paired options (@well_known_regex@ + @strict@).
      strictFlag = case [b | IStrict b <- items] of (b : _) -> b; [] -> True
      itemStrictFlag = case [b | IItemStrict b <- items] of (b : _) -> b; [] -> True
      contextCustoms =
        [definedOnly nums | IDefinedOnly <- items, Just nums <- [enumValuesFor enums (fieldType fd)]]
          ++ [wellKnownRegex k strictFlag | IWellKnownRegex k <- items]
      itemContextCustoms =
        [definedOnly nums | IItemDefinedOnly <- items, Just nums <- [enumValuesFor enums (fieldType fd)]]
          ++ [wellKnownRegex k itemStrictFlag | IItemWellKnownRegex k <- items]

      scalarKind = firstKind typed
      scalarRules = mergeRuleValues [(f, v) | (_, f, v) <- typed]

      itemsRules
        | isRepeated =
            Just
              emptyFieldRules
                { frKind = firstKind itemTyped
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- itemTyped]
                , frCustom = itemCustoms
                , frMessage = namedMsg
                }
        | not (null itemTyped) || not (null itemCustoms) =
            Just
              emptyFieldRules
                { frKind = firstKind itemTyped
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- itemTyped]
                , frCustom = itemCustoms
                }
        | otherwise = Nothing

      base
        | isRepeated || not (null repeatedRs) =
            emptyFieldRules
              { frKind = Just KRepeated
              , frRules = repeatedRs
              , frItems = itemsRules
              }
        | not (null mapRs) =
            emptyFieldRules {frKind = Just KMap, frRules = mapRs}
        | otherwise =
            emptyFieldRules
              { frKind = scalarKind
              , frRules = scalarRules
              , frMessage = namedMsg
              }

  Right
    ( fieldName fd
    , base
        { frRequired = isRequired
        , frIgnoreEmpty = isIgnoreEmpty
        , frCustom = customs
        }
    )
  where
    isReq IRequired = True
    isReq _ = False
    isIgn IIgnoreEmpty = True
    isIgn _ = False
    firstKind ts = case ts of ((k, _, _) : _) -> Just k; _ -> Nothing

-- Pair an option with its @buf.validate.field@ path (simple-name segments).
withParts :: OptionDef -> Maybe ([Text], Constant)
withParts od = case optNameParts (optName od) of
  (ExtensionOption "buf.validate.field" : rest) -> Just (map partName rest, optValue od)
  _ -> Nothing
  where
    partName (SimpleOption t) = t
    partName (ExtensionOption t) = t

classify :: ([Text], Constant) -> Either Text Item
classify (parts, value) = case parts of
  ["cel"] -> ICustom <$> constraintFromAggregate value
  ["required"] -> Right (if isTrue value then IRequired else ISkip)
  ["ignore_empty"] -> Right (if isTrue value then IIgnoreEmpty else ISkip)
  ["ignore"] -> Right (if ignoresEmpty value then IIgnoreEmpty else ISkip)
  ["repeated", "items", "cel"] -> IItemCustom <$> constraintFromAggregate value
  ["repeated", "items", "enum", "defined_only"] ->
    Right (if isTrue value then IItemDefinedOnly else ISkip)
  ["repeated", "items", "string", "well_known_regex"] ->
    Right (maybe ISkip IItemWellKnownRegex (wellKnownRegexCode value))
  ["repeated", "items", "string", "strict"] ->
    Right (maybe ISkip IItemStrict (asBoolMaybe value))
  ["repeated", "items", k, f]
    | Just kind <- kindFromName k -> Right (maybe ISkip (IItemTyped kind f) (ruleValue kind f value))
  ["repeated", f]
    | f `elem` repeatedRuleFields -> Right (maybe ISkip (IRepeatedRule f) (repeatedRuleValue f value))
  ["map", "keys", "cel"] -> IMapKeyCustom <$> constraintFromAggregate value
  ["map", "keys", k, f]
    | Just kind <- kindFromName k -> Right (maybe ISkip (IMapKeyTyped kind f) (ruleValue kind f value))
  ["map", "values", "cel"] -> IMapValueCustom <$> constraintFromAggregate value
  ["map", "values", k, f]
    | Just kind <- kindFromName k -> Right (maybe ISkip (IMapValueTyped kind f) (ruleValue kind f value))
  ["map", f]
    | f `elem` ["min_pairs", "max_pairs"] -> Right (maybe ISkip (IMapRule f) (asUInt value))
  ["enum", "defined_only"] -> Right (if isTrue value then IDefinedOnly else ISkip)
  ["string", "well_known_regex"] -> Right (maybe ISkip IWellKnownRegex (wellKnownRegexCode value))
  ["string", "strict"] -> Right (maybe ISkip IStrict (asBoolMaybe value))
  [k, f]
    | Just kind <- kindFromName k -> Right (maybe ISkip (ITyped kind f) (ruleValue kind f value))
  _ -> Right ISkip

-- The @buf.validate.KnownRegex@ enum: HTTP header name = 1, header value = 2.
wellKnownRegexCode :: Constant -> Maybe Int
wellKnownRegexCode = \case
  CIdent "KNOWN_REGEX_HTTP_HEADER_NAME" -> Just 1
  CIdent "KNOWN_REGEX_HTTP_HEADER_VALUE" -> Just 2
  CInt 1 -> Just 1
  CInt 2 -> Just 2
  _ -> Nothing

asBoolMaybe :: Constant -> Maybe Bool
asBoolMaybe (CBool b) = Just b
asBoolMaybe _ = Nothing

repeatedRuleFields :: [Text]
repeatedRuleFields = ["min_items", "max_items", "unique"]

repeatedRuleValue :: Text -> Constant -> Maybe Value
repeatedRuleValue "unique" v = asBool v
repeatedRuleValue _ v = asUInt v -- min_items / max_items

----------------------------------------------------------------------
-- Nested message resolution
----------------------------------------------------------------------

resolveMessage :: [EnumDef] -> [MessageDef] -> Set Text -> FieldType -> Maybe MessageRules
resolveMessage enums msgs visiting ft = case ft of
  FTNamed n ->
    let simple = last (T.splitOn "." n)
     in if Set.member simple visiting
          then Nothing -- break recursion on self-referential messages
          else case [m | m <- msgs, msgName m == simple] of
            (m : _) -> either (const Nothing) Just (extractMessageRules enums msgs visiting m)
            [] -> Nothing
  _ -> Nothing

collectMessages :: ProtoFile -> [MessageDef]
collectMessages pf = concatMap fromTop (protoTopLevels pf)
  where
    fromTop (TLMessage m) = m : nested m
    fromTop _ = []
    nested m = concatMap go (msgElements m)
    go (MEMessage m) = m : nested m
    go _ = []

-- All enum definitions in the file, top-level and nested in any message.
collectEnums :: ProtoFile -> [EnumDef]
collectEnums pf = concatMap fromTop (protoTopLevels pf)
  where
    fromTop (TLEnum e) = [e]
    fromTop (TLMessage m) = nested m
    fromTop _ = []
    nested m = concatMap go (msgElements m)
    go (MEEnum e) = [e]
    go (MEMessage m) = nested m
    go _ = []

-- The declared value numbers of the enum a field refers to (if its type names
-- one of the file's enums).
enumValuesFor :: [EnumDef] -> FieldType -> Maybe [Integer]
enumValuesFor enums ft = case ft of
  FTNamed n ->
    let simple = last (T.splitOn "." n)
     in case [e | e <- enums, enumName e == simple] of
          (e : _) -> Just (map (fromIntegral . evNumber) (enumValues e))
          [] -> Nothing
  _ -> Nothing

----------------------------------------------------------------------
-- Value / constant helpers
----------------------------------------------------------------------

constraintFromAggregate :: Constant -> Either Text Constraint
constraintFromAggregate (CAggregate kvs) =
  let str k = case lookup k kvs of Just (CString s) -> s; _ -> ""
      cid = str "id"
      msg = str "message"
      expr = str "expression"
   in if T.null expr
        then Left "buf.validate cel option missing 'expression'"
        else case mkConstraint cid msg expr of
          Left e -> Left ("invalid CEL in constraint '" <> cid <> "': " <> errMsg e)
          Right c -> Right c
constraintFromAggregate _ = Left "buf.validate cel option must be an aggregate {id:..,message:..,expression:..}"

kindFromName :: Text -> Maybe RuleKind
kindFromName = \case
  "float" -> Just KFloat
  "double" -> Just KDouble
  "int32" -> Just KInt32
  "int64" -> Just KInt64
  "uint32" -> Just KUint32
  "uint64" -> Just KUint64
  "sint32" -> Just KSint32
  "sint64" -> Just KSint64
  "fixed32" -> Just KFixed32
  "fixed64" -> Just KFixed64
  "sfixed32" -> Just KSfixed32
  "sfixed64" -> Just KSfixed64
  "bool" -> Just KBool
  "string" -> Just KString
  "bytes" -> Just KBytes
  "enum" -> Just KEnum
  "duration" -> Just KDuration
  "timestamp" -> Just KTimestamp
  _ -> Nothing

countFields :: [Text]
countFields = ["min_len", "max_len", "len", "min_bytes", "max_bytes", "len_bytes"]

formatFlagFields :: [Text]
formatFlagFields =
  [ "email", "hostname", "ip", "ipv4", "ipv6", "ip_prefix"
  , "ipv4_prefix", "ipv6_prefix", "ip_with_prefixlen"
  , "ipv4_with_prefixlen", "ipv6_with_prefixlen"
  , "uri", "uri_ref", "address", "host_and_port", "uuid", "tuuid", "finite"
  , "lt_now", "gt_now"
  ]

-- Interpret a rule value given the rule kind and the rule field name.
ruleValue :: RuleKind -> Text -> Constant -> Maybe Value
ruleValue kind field con
  | field `elem` countFields = asUInt con
  | field `elem` formatFlagFields = asBool con
  | otherwise = case kind of
      KString -> case con of
        CString s -> Just (VString s)
        CBool b -> Just (VBool b)
        _ -> Nothing
      KBytes -> case field of
        _ -> asBytes con
      KBool -> asBool con
      -- Duration / timestamp bounds are message literals (@{seconds:.., nanos:..}@).
      KDuration -> uncurry mkDuration <$> secondsNanos con
      KTimestamp
        -- @within@ on a timestamp is itself a duration span.
        | field == "within" -> uncurry mkDuration <$> secondsNanos con
        | otherwise -> uncurry mkTimestamp <$> secondsNanos con
      _
        | isUnsigned kind -> asUInt con
        | isFloat kind -> asDouble con
        | otherwise -> asInt con

isUnsigned :: RuleKind -> Bool
isUnsigned k = k `elem` [KUint32, KUint64, KFixed32, KFixed64]

isFloat :: RuleKind -> Bool
isFloat k = k == KFloat || k == KDouble

asUInt :: Constant -> Maybe Value
asUInt (CInt i) | i >= 0 = Just (VUInt (fromInteger i))
asUInt _ = Nothing

asInt :: Constant -> Maybe Value
asInt (CInt i) = Just (VInt (fromInteger i))
asInt _ = Nothing

asDouble :: Constant -> Maybe Value
asDouble (CFloat d) = Just (VDouble d)
asDouble (CInt i) = Just (VDouble (fromInteger i))
asDouble _ = Nothing

asBool :: Constant -> Maybe Value
asBool (CBool b) = Just (VBool b)
asBool _ = Nothing

asBytes :: Constant -> Maybe Value
asBytes (CString s) = Just (VBytes (TE.encodeUtf8 s))
asBytes _ = Nothing

-- Read @{seconds: .., nanos: ..}@ (either component optional, default 0) from a
-- @google.protobuf.Duration@ / @Timestamp@ message literal.
secondsNanos :: Constant -> Maybe (Integer, Integer)
secondsNanos (CAggregate kvs) =
  let intAt k = case lookup k kvs of Just (CInt i) -> Just i; Nothing -> Just 0; _ -> Nothing
   in (,) <$> intAt "seconds" <*> intAt "nanos"
secondsNanos _ = Nothing

mkDuration :: Integer -> Integer -> Value
mkDuration s n = VDuration (Duration (fromInteger s) (fromInteger n))

mkTimestamp :: Integer -> Integer -> Value
mkTimestamp s n = VTimestamp (Timestamp (fromInteger s) (fromInteger n))

isTrue :: Constant -> Bool
isTrue (CBool b) = b
isTrue _ = False

ignoresEmpty :: Constant -> Bool
ignoresEmpty (CIdent i) = i `elem` ["IGNORE_IF_UNPOPULATED", "IGNORE_IF_DEFAULT_VALUE", "IGNORE_ALWAYS"]
ignoresEmpty _ = False

-- Merge raw rule values: @in@ / @not_in@ accumulate into a list, others keep
-- the last value.
mergeRuleValues :: [(Text, Value)] -> [(Text, Value)]
mergeRuleValues entries = foldr step [] keys
  where
    keys = nubOrd (map fst entries)
    nubOrd = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | Set.member x seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs
    step k acc =
      let vs = [v | (k', v) <- entries, k' == k]
       in (k, if k == "in" || k == "not_in" then VList (V.fromList vs) else last vs) : acc
