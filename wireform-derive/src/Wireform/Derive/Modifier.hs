{- | Annotation vocabulary for customising wireform-driven instance
generation.

A 'Modifier' is a single customisation directive (\"rename this
field\", \"flatten this nested record\", \"this field is optional in
Thrift\", ...). Multiple modifiers compose as a 'Modifiers' value.

Modifiers are attached to types and fields using GHC @ANN@ pragmas:

@
data Person = Person
  { personName :: !Text   -- ^ wire key: \"name\"
  , personAge  :: !Int    -- ^ wire key: \"age\"
  } deriving stock Generic

{\-\# ANN type Person (rename "person") \#-\}
{\-\# ANN personName  (rename "name") \#-\}
{\-\# ANN personAge   (renameStyle SnakeCase) \#-\}
@

Per-format derivers (@deriveProto@, @deriveCBOR@, @deriveAeson@, ...)
read these annotations via 'Wireform.Derive.ModifierInfo.reifyModifierInfoFor'
and shape the generated code accordingly.
-}
module Wireform.Derive.Modifier (
  -- * Modifiers
  Modifier (..),
  Modifiers (..),
  noModifiers,

  -- * Renames
  Rename (..),
  rename,
  renameStyle,
  renameWith,
  renameIdiomatic,

  -- * Wire-format overrides
  WireOverride (..),

  -- * Map keys (proto-flavoured)
  MapKeyScalar (..),

  -- * Smart constructors (general)
  coerced,
  flatten,
  skip,
  defaults,
  tag,
  required,
  optional,
  wireOverride,
  customModifier,
  mapKey,
  oneof,

  -- * Per-backend targeting
  forBackend,
  forBackends,
  disableFor,
  onlyFor,

  -- * Inspection
  modifierBackends,
  modifierIsBackendScoped,
) where

import Data.Data (Data)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Name)
import Wireform.Derive.Backend
import Wireform.Derive.NameStyle


-- ---------------------------------------------------------------------------
-- Rename
-- ---------------------------------------------------------------------------

-- | How to compute a wire key from a Haskell field's selector base name.
data Rename
  = -- | Use this exact 'Text' as the wire key.
    RenameTo !Text
  | {- | Apply a 'NameStyle' transformation. Evaluated entirely at
    splice time, so the result is baked into the generated code as
    a literal 'Text'.
    -}
    RenameStyle !NameStyle
  | {- | Apply the named @Text -> Text@ function at runtime. Use only
    when 'NameStyle' cannot express the desired transformation.
    -}
    RenameFn !Name
  deriving stock (Eq, Ord, Show, Data, Generic)


-- ---------------------------------------------------------------------------
-- WireOverride
-- ---------------------------------------------------------------------------

-- | Force a non-default wire encoding for a numeric or string field.
data WireOverride
  = {- | Use ZigZag encoding for signed integers (proto's @sint32@ /
    @sint64@).
    -}
    WireZigZag
  | {- | Use a fixed-width little-endian encoding (proto's @fixed32@
    / @fixed64@ / @sfixed32@ / @sfixed64@).
    -}
    WireFixed
  | -- | Pack a repeated scalar field (proto3 default for scalars).
    WirePacked
  | {- | Encode this field as a UTF-8 'Text' regardless of the inferred
    representation.
    -}
    WireString
  | {- | Encode this field as raw bytes regardless of the inferred
    representation.
    -}
    WireBytes
  deriving stock (Eq, Ord, Show, Data, Generic)


-- ---------------------------------------------------------------------------
-- MapKeyScalar
-- ---------------------------------------------------------------------------

{- | Scalar types that protobuf permits as map keys (proto2 §maps,
proto3 §maps). Excludes @float@, @double@, @bytes@, message and
enum types.

Lives in @wireform-derive@ rather than @wireform-proto@ so that
annotations can reference proto map-key types without forcing
non-proto packages to depend on proto. The proto deriver projects
this onto its own @Proto.IDL.AST.ScalarType@.
-}
data MapKeyScalar
  = MapKeyInt32
  | MapKeyInt64
  | MapKeyUInt32
  | MapKeyUInt64
  | MapKeySInt32
  | MapKeySInt64
  | MapKeyFixed32
  | MapKeyFixed64
  | MapKeySFixed32
  | MapKeySFixed64
  | MapKeyBool
  | MapKeyString
  deriving stock (Eq, Ord, Show, Data, Generic, Enum, Bounded)


-- ---------------------------------------------------------------------------
-- Modifier
-- ---------------------------------------------------------------------------

{- | A single annotation directive. Each format deriver consults
modifiers it understands and silently ignores the rest, so a single
annotation can simultaneously inform several backends.
-}
data Modifier
  = -- | Rename a field on the wire.
    ModRename !Rename
  | {- | Encode / decode the field via the named @newtype@-style
    coercion target.
    -}
    ModCoerce !Name
  | {- | Inline the contents of this nested record into its parent's
    field set, rather than nesting it as a sub-message.
    -}
    ModFlatten
  | {- | Omit this field from the wire encoding entirely (decoders
    supply 'mempty' / @def@).
    -}
    ModSkip
  | {- | Use the named function as the field's default when decoding
    a missing value.
    -}
    ModDefaults !Name
  | {- | Manually fix this field's numeric tag / id (proto field
    number, Thrift field id).
    -}
    ModTag !Int
  | -- | Mark a field required (Thrift / proto2 semantics).
    ModRequired
  | -- | Mark a field optional.
    ModOptional
  | -- | Override the field's wire encoding (see 'WireOverride').
    ModWireOverride !WireOverride
  | -- | Apply the inner modifiers /only/ for the listed backends.
    ModForBackends ![Backend] ![Modifier]
  | {- | Apply this single modifier /only/ for the listed backends.
    Equivalent to @ModForBackends bs [m]@; provided for cheaper
    pretty-printing and tighter @ANN@ payloads in common cases.
    -}
    ModBackendOnly ![Backend] !Modifier
  | {- | Skip this name entirely for the listed backends. Equivalent to
    @ModForBackends bs [ModSkip]@; provided as its own constructor
    so per-format derivers can short-circuit cheaply.
    -}
    ModBackendDisable ![Backend]
  | {- | Backend-specific opaque payload. Tagged by an arbitrary
    'Text' identifier so backends can recognise their own; the
    payload itself is a 'String' rather than a 'ByteString'
    because GHC's 'ANN' machinery serialises the modifier via
    @Data.Data@, which works for any algebraic type but fails on
    the sealed 'ByteString' instance.
    -}
    ModCustom !Text !String
  | {- | This @HashMap@ / @Map@ field is encoded as a proto3 @map@.
    The 'MapKeyScalar' picks the wire encoding for the key half.
    Value type is inferred from the field's Haskell type.

    Only consulted by the proto deriver; other backends ignore it.
    -}
    ModMapKey !MapKeyScalar
  | {- | Group this constructor (in a sum) or this field (in a
    record) into the named proto @oneof@. All fields sharing the
    same group name encode under one @oneof@ block.

    Only consulted by the proto deriver; other backends ignore it.
    -}
    ModOneof !Text
  deriving stock (Eq, Ord, Show, Data, Generic)


-- ---------------------------------------------------------------------------
-- Modifiers (composition)
-- ---------------------------------------------------------------------------

{- | A composed bundle of modifiers. Newtype around @[Modifier]@ with a
right-biased 'Semigroup' (later writes shadow earlier ones, matching
the order in which @ANN@ pragmas are reified).
-}
newtype Modifiers = Modifiers {getModifiers :: [Modifier]}
  deriving stock (Eq, Ord, Show, Data, Generic)


instance Semigroup Modifiers where
  Modifiers xs <> Modifiers ys = Modifiers (xs <> ys)


instance Monoid Modifiers where
  mempty = Modifiers []


-- | Empty modifier bundle.
noModifiers :: Modifiers
noModifiers = mempty


-- ---------------------------------------------------------------------------
-- Smart constructors: renames
-- ---------------------------------------------------------------------------

-- | Rename a field to a fixed string.
rename :: Text -> Modifier
rename = ModRename . RenameTo


{- | Apply a 'NameStyle' transformation. The deriver evaluates this at
splice time.
-}
renameStyle :: NameStyle -> Modifier
renameStyle = ModRename . RenameStyle


{- | Apply the named @Text -> Text@ function at runtime. Pass the
function's name with TH's @\'@ syntax, e.g. @renameWith \'myRenamer@.
-}
renameWith :: Name -> Modifier
renameWith = ModRename . RenameFn


{- | Use the active backend's idiomatic naming convention. Equivalent to
@renameStyle Idiomatic@; provided as a smart constructor for
discoverability.
-}
renameIdiomatic :: Modifier
renameIdiomatic = renameStyle Idiomatic


-- ---------------------------------------------------------------------------
-- Smart constructors: general
-- ---------------------------------------------------------------------------

-- | See 'ModCoerce'.
coerced :: Name -> Modifier
coerced = ModCoerce


-- | See 'ModFlatten'.
flatten :: Modifier
flatten = ModFlatten


-- | See 'ModSkip'.
skip :: Modifier
skip = ModSkip


-- | See 'ModDefaults'.
defaults :: Name -> Modifier
defaults = ModDefaults


-- | See 'ModTag'.
tag :: Int -> Modifier
tag = ModTag


-- | See 'ModRequired'.
required :: Modifier
required = ModRequired


-- | See 'ModOptional'.
optional :: Modifier
optional = ModOptional


-- | See 'ModWireOverride'.
wireOverride :: WireOverride -> Modifier
wireOverride = ModWireOverride


{- | Embed an opaque backend-specific payload. The 'Text' tag should be
a fully-qualified, namespaced identifier (e.g.
@"wireform-proto.json-name"@) so different backends do not clash on
the same tag.

The payload is typed as 'String' rather than 'ByteString' because
GHC's 'ANN' machinery serialises modifiers via @Data.Data@, and
'ByteString' has a sealed 'Data' instance that fails the round
trip. For typed payloads, prefer
'Wireform.Derive.Extension.extension'.
-}
customModifier :: Text -> String -> Modifier
customModifier = ModCustom


{- | Mark a @HashMap@ / @Map@ field as a proto @map<K,V>@ with the
supplied key encoding. See 'ModMapKey'.
-}
mapKey :: MapKeyScalar -> Modifier
mapKey = ModMapKey


-- | Group this name into the named proto @oneof@. See 'ModOneof'.
oneof :: Text -> Modifier
oneof = ModOneof


-- ---------------------------------------------------------------------------
-- Smart constructors: per-backend targeting
-- ---------------------------------------------------------------------------

{- | Restrict a single modifier to one backend.

@forBackend backendProto (rename "name")@
-}
forBackend :: Backend -> Modifier -> Modifier
forBackend b = ModBackendOnly [b]


{- | Restrict a list of modifiers to a list of backends.

@forBackends [backendProto, backendJSON] [rename "name", tag 7]@
-}
forBackends :: [Backend] -> [Modifier] -> Modifier
forBackends = ModForBackends


-- | Restrict a single modifier to several backends.
onlyFor :: [Backend] -> Modifier -> Modifier
onlyFor = ModBackendOnly


-- | Skip this name entirely for the listed backends.
disableFor :: [Backend] -> Modifier
disableFor = ModBackendDisable


-- ---------------------------------------------------------------------------
-- Inspection
-- ---------------------------------------------------------------------------

{- | The set of backends a modifier targets, or 'Nothing' for a global
modifier (one that applies to every backend).
-}
modifierBackends :: Modifier -> Maybe [Backend]
modifierBackends = \case
  ModForBackends bs _ -> Just bs
  ModBackendOnly bs _ -> Just bs
  ModBackendDisable bs -> Just bs
  _ -> Nothing


-- | True iff the modifier is restricted to a specific set of backends.
modifierIsBackendScoped :: Modifier -> Bool
modifierIsBackendScoped m = case modifierBackends m of
  Just _ -> True
  Nothing -> False
