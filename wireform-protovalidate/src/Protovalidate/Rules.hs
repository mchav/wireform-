{-# LANGUAGE OverloadedStrings #-}

-- | The protovalidate rule model and the table of standard constraints
-- expressed as CEL.
--
-- Like reference protovalidate, each standard rule (e.g. @string.min_len@,
-- @int32.gt@, @repeated.unique@) is just a CEL expression evaluated with the
-- field value bound to @this@ and the rule message bound to @rules@. This
-- module enumerates those expressions per 'RuleKind' and provides a small
-- builder vocabulary for assembling 'FieldRules' / 'MessageRules'.
module Protovalidate.Rules
  ( RuleKind (..)
  , FieldRules (..)
  , MessageRules (..)
  , emptyFieldRules
  , fieldRules
  , messageRules
  , standardConstraints
    -- * Rule-value builders
  , constV
  , ltV
  , lteV
  , gtV
  , gteV
  , inV
  , notInV
  , minLen
  , maxLen
  , lenV
  , prefix
  , suffix
  , contains
  , pattern
  , email
  , hostname
  , ip
  , uri
  , uuid
  , minItems
  , maxItems
  , unique
  ) where

import Data.Text (Text)

import CEL.Value (Value (..))
import qualified Data.Vector as V
import Protovalidate.Constraint (Constraint, unsafeConstraint)

-- | The protobuf type a set of field rules applies to. Determines which
-- standard constraints are available.
data RuleKind
  = KFloat
  | KDouble
  | KInt32
  | KInt64
  | KUint32
  | KUint64
  | KSint32
  | KSint64
  | KFixed32
  | KFixed64
  | KSfixed32
  | KSfixed64
  | KBool
  | KString
  | KBytes
  | KEnum
  | KRepeated
  | KMap
  | KDuration
  | KTimestamp
  deriving stock (Eq, Show)

-- | Validation rules for a single field.
data FieldRules = FieldRules
  { frRequired :: !Bool
  -- ^ The field must be present (set / non-default).
  , frIgnoreEmpty :: !Bool
  -- ^ Skip the standard constraints when the value is the type's zero value.
  , frKind :: !(Maybe RuleKind)
  -- ^ Which standard-constraint family applies (if any).
  , frRules :: ![(Text, Value)]
  -- ^ Set rule values, keyed by rule field name (e.g. @("min_len", VUInt 3)@).
  , frCustom :: ![Constraint]
  -- ^ Field-level custom CEL constraints (@(buf.validate.field).cel@).
  , frItems :: !(Maybe FieldRules)
  -- ^ Rules applied to each element of a repeated field.
  , frMessage :: !(Maybe MessageRules)
  -- ^ Rules applied to a (possibly repeated) message-typed field, recursively.
  }
  deriving stock (Show)

-- | Validation rules for a whole message: per-field rules plus message-level
-- custom CEL (@(buf.validate.message).cel@).
data MessageRules = MessageRules
  { mrFields :: ![(Text, FieldRules)]
  , mrCustom :: ![Constraint]
  }
  deriving stock (Show)

-- | Field rules with everything empty/disabled.
emptyFieldRules :: FieldRules
emptyFieldRules =
  FieldRules
    { frRequired = False
    , frIgnoreEmpty = False
    , frKind = Nothing
    , frRules = []
    , frCustom = []
    , frItems = Nothing
    , frMessage = Nothing
    }

-- | Build field rules of a given kind from a list of set rule values.
fieldRules :: RuleKind -> [(Text, Value)] -> FieldRules
fieldRules k rs = emptyFieldRules {frKind = Just k, frRules = rs}

-- | Build message rules.
messageRules :: [(Text, FieldRules)] -> [Constraint] -> MessageRules
messageRules = MessageRules

----------------------------------------------------------------------
-- Rule-value builders
----------------------------------------------------------------------

constV, ltV, lteV, gtV, gteV :: Value -> (Text, Value)
constV v = ("const", v)
ltV v = ("lt", v)
lteV v = ("lte", v)
gtV v = ("gt", v)
gteV v = ("gte", v)

inV, notInV :: [Value] -> (Text, Value)
inV vs = ("in", VList (V.fromList vs))
notInV vs = ("not_in", VList (V.fromList vs))

minLen, maxLen, lenV :: Word -> (Text, Value)
minLen n = ("min_len", VUInt (fromIntegral n))
maxLen n = ("max_len", VUInt (fromIntegral n))
lenV n = ("len", VUInt (fromIntegral n))

minItems, maxItems :: Word -> (Text, Value)
minItems n = ("min_items", VUInt (fromIntegral n))
maxItems n = ("max_items", VUInt (fromIntegral n))

prefix, suffix, contains, pattern :: Text -> (Text, Value)
prefix s = ("prefix", VString s)
suffix s = ("suffix", VString s)
contains s = ("contains", VString s)
pattern s = ("pattern", VString s)

email, hostname, uri, uuid, unique :: (Text, Value)
email = ("email", VBool True)
hostname = ("hostname", VBool True)
uri = ("uri", VBool True)
uuid = ("uuid", VBool True)
unique = ("unique", VBool True)

-- | @ip@ / @ipv4@ / @ipv6@ format selector. Use @ip@ for any version.
ip :: (Text, Value)
ip = ("ip", VBool True)

----------------------------------------------------------------------
-- Standard constraint table
----------------------------------------------------------------------

-- | The standard constraints available for a 'RuleKind', as
-- @(ruleFieldName, constraint)@ pairs. A constraint applies to a field when
-- the corresponding rule value is set (and, for boolean flag rules, is
-- @true@).
standardConstraints :: RuleKind -> [(Text, Constraint)]
standardConstraints = \case
  KString -> stringConstraints
  KBytes -> bytesConstraints
  KBool -> [("const", c "bool.const" "value must equal the configured constant" "this == rules.const")]
  KRepeated -> repeatedConstraints
  KMap -> mapConstraints
  KEnum -> numericConstraints "enum"
  KFloat -> numericConstraints "float"
  KDouble -> numericConstraints "double"
  KInt32 -> numericConstraints "int32"
  KInt64 -> numericConstraints "int64"
  KUint32 -> numericConstraints "uint32"
  KUint64 -> numericConstraints "uint64"
  KSint32 -> numericConstraints "sint32"
  KSint64 -> numericConstraints "sint64"
  KFixed32 -> numericConstraints "fixed32"
  KFixed64 -> numericConstraints "fixed64"
  KSfixed32 -> numericConstraints "sfixed32"
  KSfixed64 -> numericConstraints "sfixed64"
  KDuration -> numericConstraints "duration"
  KTimestamp -> numericConstraints "timestamp"

-- A constraint with the given id / message / CEL source.
c :: Text -> Text -> Text -> Constraint
c = unsafeConstraint

numericConstraints :: Text -> [(Text, Constraint)]
numericConstraints kind =
  [ ("const", c (kind <> ".const") "value must equal the configured constant" "this == rules.const")
  , ("lt", c (kind <> ".lt") "value must be less than the configured bound" "this < rules.lt")
  , ("lte", c (kind <> ".lte") "value must be less than or equal to the configured bound" "this <= rules.lte")
  , ("gt", c (kind <> ".gt") "value must be greater than the configured bound" "this > rules.gt")
  , ("gte", c (kind <> ".gte") "value must be greater than or equal to the configured bound" "this >= rules.gte")
  , ("in", c (kind <> ".in") "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c (kind <> ".not_in") "value must not be in the forbidden set" "!(this in rules.not_in)")
  ]

stringConstraints :: [(Text, Constraint)]
stringConstraints =
  [ ("const", c "string.const" "value must equal the configured constant" "this == rules.const")
  , ("len", c "string.len" "value must be the configured number of characters" "uint(size(this)) == rules.len")
  , ("min_len", c "string.min_len" "value is too short" "uint(size(this)) >= rules.min_len")
  , ("max_len", c "string.max_len" "value is too long" "uint(size(this)) <= rules.max_len")
  , ("min_bytes", c "string.min_bytes" "value has too few bytes" "uint(size(bytes(this))) >= rules.min_bytes")
  , ("max_bytes", c "string.max_bytes" "value has too many bytes" "uint(size(bytes(this))) <= rules.max_bytes")
  , ("pattern", c "string.pattern" "value does not match the required pattern" "this.matches(rules.pattern)")
  , ("prefix", c "string.prefix" "value does not have the required prefix" "this.startsWith(rules.prefix)")
  , ("suffix", c "string.suffix" "value does not have the required suffix" "this.endsWith(rules.suffix)")
  , ("contains", c "string.contains" "value does not contain the required substring" "this.contains(rules.contains)")
  , ("not_contains", c "string.not_contains" "value contains a forbidden substring" "!this.contains(rules.not_contains)")
  , ("in", c "string.in" "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c "string.not_in" "value must not be in the forbidden set" "!(this in rules.not_in)")
  , ("email", c "string.email" "value must be a valid email address" "this.isEmail()")
  , ("hostname", c "string.hostname" "value must be a valid hostname" "this.isHostname()")
  , ("ip", c "string.ip" "value must be a valid IP address" "this.isIp()")
  , ("ipv4", c "string.ipv4" "value must be a valid IPv4 address" "this.isIp(4)")
  , ("ipv6", c "string.ipv6" "value must be a valid IPv6 address" "this.isIp(6)")
  , ("ip_prefix", c "string.ip_prefix" "value must be a valid IP prefix" "this.isIpPrefix()")
  , ("uri", c "string.uri" "value must be a valid URI" "this.isUri()")
  , ("uri_ref", c "string.uri_ref" "value must be a valid URI reference" "this.isUriRef()")
  , ("address", c "string.address" "value must be a valid hostname or IP address" "this.isIp() || this.isHostname()")
  , ("host_and_port", c "string.host_and_port" "value must be a valid host and port" "this.isHostAndPort(true)")
  , ("uuid", c "string.uuid" "value must be a valid UUID" "this == '' || this.matches('^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$')")
  ]

bytesConstraints :: [(Text, Constraint)]
bytesConstraints =
  [ ("const", c "bytes.const" "value must equal the configured constant" "this == rules.const")
  , ("len", c "bytes.len" "value must be the configured number of bytes" "uint(size(this)) == rules.len")
  , ("min_len", c "bytes.min_len" "value has too few bytes" "uint(size(this)) >= rules.min_len")
  , ("max_len", c "bytes.max_len" "value has too many bytes" "uint(size(this)) <= rules.max_len")
  , ("in", c "bytes.in" "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c "bytes.not_in" "value must not be in the forbidden set" "!(this in rules.not_in)")
  ]

repeatedConstraints :: [(Text, Constraint)]
repeatedConstraints =
  [ ("min_items", c "repeated.min_items" "value has too few items" "uint(size(this)) >= rules.min_items")
  , ("max_items", c "repeated.max_items" "value has too many items" "uint(size(this)) <= rules.max_items")
  , ("unique", c "repeated.unique" "value must contain unique items" "unique(this)")
  ]

mapConstraints :: [(Text, Constraint)]
mapConstraints =
  [ ("min_pairs", c "map.min_pairs" "value has too few pairs" "uint(size(this)) >= rules.min_pairs")
  , ("max_pairs", c "map.max_pairs" "value has too many pairs" "uint(size(this)) <= rules.max_pairs")
  ]
