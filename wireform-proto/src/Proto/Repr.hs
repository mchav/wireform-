{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Configurable field representations (the adapter system).

By default, proto @string@ fields map to strict 'Data.Text.Text',
@bytes@ to strict 'Data.ByteString.ByteString', @repeated@ to
'Data.Vector.Vector', and @map@ to 'Data.Map.Strict.Map'. This module
lets you override those choices per-field, per-message, or globally.

== Adapter records

Each proto field category has an adapter record that bundles the
Template Haskell splices needed by the code generator:

* 'StringAdapter' -- for @string@ fields
* 'BytesAdapter'  -- for @bytes@ fields
* 'RepeatedAdapter' -- for @repeated@ fields
* 'MapAdapter' -- for @map@ fields

== Built-in adapters

__Strings:__ 'strictTextAdapter' (default), 'lazyTextAdapter',
'shortTextAdapter', 'hsStringAdapter'.

__Bytes:__ 'strictBytesAdapter' (default), 'lazyBytesAdapter',
'shortBytesAdapter'.

__Repeated:__ 'vectorAdapter' (default), 'unboxedVectorAdapter',
'listAdapter', 'seqAdapter'.

__Maps:__ 'ordMapAdapter' (default), 'hashMapAdapter'.

== Overriding from Haskell

Use 'RepConfig' fields in 'Proto.TH.LoadOpts' to override per-field
or per-message:

@
\$(loadProtoWith (defaultLoadOpts { loRepConfig = defaultRepConfig
    { configFieldOverrides = Map.fromList
        [ (("Person","name"), defaultFieldRep { fieldString = shortTextAdapter })
        , (("Blob","data"), defaultFieldRep { fieldBytes = lazyBytesAdapter })
        , (("Config","tags"), defaultFieldRep { fieldRepeated = listAdapter })
        ]
    }
}) "path/to/file.proto")
@

== Overriding from .proto annotations

Annotate fields in your @.proto@ file using wireform extension options,
and they will be resolved through the 'AdapterRegistry':

@
message Blob {
  bytes data = 1 [(wireform.haskell_bytes) = \"lazy\"];
  string name = 2 [(wireform.haskell_string) = \"short\"];
  repeated int32 ids = 3 [(wireform.haskell_repeated) = \"list\"];
  map\<string, string\> tags = 4 [(wireform.haskell_map) = \"hash\"];
}
@

The 'defaultAdapterRegistry' maps the built-in short names
(@\"strict\"@, @\"lazy\"@, @\"short\"@, @\"string\"@, @\"vector\"@,
@\"list\"@, @\"seq\"@, @\"unboxed\"@, @\"ord\"@, @\"hash\"@) to
their corresponding adapters. Register custom adapters by extending
the registry.

== Defining custom adapters

Start from an existing adapter and override the fields you need:

@
newtype Url = Url { unUrl :: Text }

urlAdapter :: StringAdapter
urlAdapter = strictTextAdapter
  { stringType    = [t| Url |]
  , stringEncode  = [| \\tag (Url t) -> encodeStrictTextFN tag t |]
  , stringDecode  = [| Url |]
  , stringEmpty   = [| Url T.empty |]
  , stringIsEmpty = [| \\(Url t) -> T.null t |]
  }
@

Then register it in an 'AdapterRegistry' or use it directly in
'configFieldOverrides'.
-}
module Proto.Repr (
  -- * Adapter records
  StringAdapter (..),
  BytesAdapter (..),
  RepeatedAdapter (..),
  MapAdapter (..),

  -- * Built-in string adapters
  strictTextAdapter,
  lazyTextAdapter,
  shortTextAdapter,
  hsStringAdapter,

  -- * Built-in bytes adapters
  strictBytesAdapter,
  lazyBytesAdapter,
  shortBytesAdapter,

  -- * Built-in repeated adapters
  vectorAdapter,
  unboxedVectorAdapter,
  listAdapter,
  seqAdapter,

  -- * Built-in map adapters
  ordMapAdapter,
  hashMapAdapter,

  -- * Per-field configuration
  FieldRep (..),
  defaultFieldRep,

  -- * Configuration table
  RepConfig (..),
  defaultRepConfig,
  lookupFieldRep,

  -- * Adapter registry (for .proto annotation support)
  AdapterRegistry (..),
  defaultAdapterRegistry,
  wireformFieldOverrides,

  -- * Legacy enum types (convenience aliases)
  StringRep (..),
  BytesRep (..),
  RepeatedRep (..),
  MapRep (..),
  stringRepAdapter,
  bytesRepAdapter,
  repeatedRepAdapter,
  mapRepAdapter,

  -- * Runtime encode adapters (used by TH-generated code)
  encodeStrictText,
  encodeLazyText,
  encodeShortByteString,
  encodeHsString,
  encodeStrictBytes,
  encodeLazyBytes,
  encodeShortBytes,

  -- * Runtime encode adapters (field-number variants, used by adapters)
  encodeStrictTextFN,
  encodeLazyTextFN,
  encodeShortTextFN,
  encodeHsStringFN,
  encodeStrictBytesFN,
  encodeLazyBytesFN,
  encodeShortBytesFN,

  -- * Runtime size helpers
  sizeStrictText,
  sizeLazyText,
  sizeShortText,
  sizeHsString,
  sizeStrictBytes,
  sizeLazyBytes,
  sizeShortBytes,

  -- * Fold adapters
  foldVector,
  foldList,
  foldSeq,

  -- * Decode adapters
  decodeToStrictText,
  decodeToLazyText,
  decodeToShortText,
  decodeToHsString,
  decodeToLazyBytes,
  decodeToShortBytes,

  -- * Container operations
  emptyVector,
  emptyList,
  emptySeq,
  snocVector,
  snocList,
  snocSeq,
  nullVector,
  nullList,
  nullSeq,

  -- * Map operations
  emptyOrdMap,
  emptyHashMap,
  insertOrdMap,
  insertHashMap,
  nullOrdMap,
  nullHashMap,
  foldOrdMap,
  foldHashMap,
  sizeOrdMap,
  sizeHashMap,

  -- * Default values
  emptyStrictText,
  emptyLazyText,
  emptyShortBytes,
  emptyHsString,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word8)
import Language.Haskell.TH (Exp, Q, Type)
import Proto.IDL.AST (Constant (..), OptionDef, OptionName (..), OptionNamePart (..), optName, optValue)
import Proto.Internal.Wire (WireType (..))
import Proto.Internal.Wire.Encode (putLengthDelimited, putTag, putText, putVarint, varintSize)
import Wireform.Builder qualified as B


-- =========================================================================
-- Adapter records
-- =========================================================================

{- | A complete recipe for mapping a proto @string@ field to a Haskell type.
Each field is a TH expression that the codegen splices directly.

To define a custom adapter for a newtype, start from an existing adapter
and override the fields you need:

@
newtype Url = Url { unUrl :: Text }

urlAdapter :: StringAdapter
urlAdapter = strictTextAdapter
  { stringType    = [t| Url |]
  , stringEncode  = [| \\tag (Url t) -> $(stringEncode strictTextAdapter) tag t |]
  , stringDecode  = [| \\d -> Url \<$\> $(stringDecode strictTextAdapter) d |]
  , stringEmpty   = [| Url T.empty |]
  , stringIsEmpty = [| \\(Url t) -> T.null t |]
  }
@
-}
data StringAdapter = StringAdapter
  { stringType :: Q Type
  -- ^ The Haskell type (e.g. @[t| Text |]@, @[t| Url |]@).
  , stringEncode :: Q Exp
  -- ^ @Int -> a -> Builder@. The 'Int' is the proto field number.
  -- The adapter is responsible for emitting the tag + payload.
  , stringSize :: Q Exp
  -- ^ @a -> Int@. Wire size of the field /including/ the tag byte.
  , stringDecode :: Q Exp
  -- ^ @Text -> a@. Post-processing function applied after the wire
  -- decoder produces a strict 'Text'. For the default (strict text)
  -- this is 'id'.
  , stringEmpty :: Q Exp
  -- ^ @a@. The proto default / zero value.
  , stringIsEmpty :: Q Exp
  -- ^ @a -> Bool@. For the proto3 default-skip rule.
  , stringBaseRep :: !StringRep
  -- ^ The underlying base representation, used by the JSON/metadata
  -- codegen to select the right serialisation path. Custom adapters
  -- wrapping a base type should set this to the corresponding enum
  -- (e.g. a newtype over 'Text' should use 'StrictTextRep').
  }


-- | A complete recipe for mapping a proto @bytes@ field to a Haskell type.
data BytesAdapter = BytesAdapter
  { bytesType :: Q Type
  , bytesEncode :: Q Exp
  -- ^ @Int -> a -> Builder@. The 'Int' is the proto field number.
  , bytesSize :: Q Exp
  -- ^ @a -> Int@. Wire size including tag byte.
  , bytesDecode :: Q Exp
  -- ^ @ByteString -> a@. Post-processing function applied after the
  -- wire decoder produces a strict 'ByteString'. For the default
  -- (strict bytes) this is 'id'.
  , bytesEmpty :: Q Exp
  -- ^ @a@.
  , bytesIsEmpty :: Q Exp
  -- ^ @a -> Bool@.
  , bytesBaseRep :: !BytesRep
  -- ^ The underlying base representation, used by JSON/metadata
  -- codegen to select the right serialisation path.
  }


-- | A recipe for the container backing a proto @repeated@ field.
data RepeatedAdapter = RepeatedAdapter
  { repeatedType :: Q Type -> Q Type
  -- ^ Given an element type, produce the container type.
  -- E.g. @\\t -> [t| V.Vector $t |]@.
  , repeatedEmpty :: Q Exp
  -- ^ @f a@. Empty container.
  , repeatedSnoc :: Q Exp
  -- ^ @f a -> a -> f a@. Append an element.
  , repeatedFoldl :: Q Exp
  -- ^ @(b -> a -> b) -> b -> f a -> b@. Strict left fold.
  , repeatedIsEmpty :: Q Exp
  -- ^ @f a -> Bool@.
  , repeatedBaseRep :: !RepeatedRep
  -- ^ The underlying base representation, used by JSON/metadata
  -- codegen to select the right serialisation path.
  }


-- | A recipe for the container backing a proto @map@ field.
data MapAdapter = MapAdapter
  { mapType :: Q Type -> Q Type -> Q Type
  -- ^ Given key and value types, produce the map type.
  , mapEmpty :: Q Exp
  -- ^ @m k v@. Empty map.
  , mapInsert :: Q Exp
  -- ^ @k -> v -> m k v -> m k v@.
  , mapFoldl :: Q Exp
  -- ^ @(b -> k -> v -> b) -> b -> m k v -> b@.
  , mapIsEmpty :: Q Exp
  -- ^ @m k v -> Bool@.
  , mapSize :: Q Exp
  -- ^ @m k v -> Int@.
  }


-- =========================================================================
-- Built-in string adapters
-- =========================================================================

-- | Strict 'Text' (default). Zero-copy decode, archetype-encoded.
strictTextAdapter :: StringAdapter
strictTextAdapter =
  StringAdapter
    { stringType = [t|Text|]
    , stringEncode = [|encodeStrictTextFN|]
    , stringSize = [|sizeStrictText|]
    , stringDecode = [|id :: Text -> Text|]
    , stringEmpty = [|T.empty|]
    , stringIsEmpty = [|T.null|]
    , stringBaseRep = StrictTextRep
    }


-- | Lazy 'TL.Text'.
lazyTextAdapter :: StringAdapter
lazyTextAdapter =
  StringAdapter
    { stringType = [t|TL.Text|]
    , stringEncode = [|encodeLazyTextFN|]
    , stringSize = [|sizeLazyText|]
    , stringDecode = [|TL.fromStrict|]
    , stringEmpty = [|TL.empty|]
    , stringIsEmpty = [|TL.null|]
    , stringBaseRep = LazyTextRep
    }


-- | 'SBS.ShortByteString' (UTF-8 stored as short bytestring, compact).
shortTextAdapter :: StringAdapter
shortTextAdapter =
  StringAdapter
    { stringType = [t|SBS.ShortByteString|]
    , stringEncode = [|encodeShortTextFN|]
    , stringSize = [|sizeShortText|]
    , stringDecode = [|SBS.toShort . TE.encodeUtf8|]
    , stringEmpty = [|SBS.empty|]
    , stringIsEmpty = [|SBS.null|]
    , stringBaseRep = ShortTextRep
    }


-- | Haskell 'String' (@[Char]@). Convenient but slow.
hsStringAdapter :: StringAdapter
hsStringAdapter =
  StringAdapter
    { stringType = [t|String|]
    , stringEncode = [|encodeHsStringFN|]
    , stringSize = [|sizeHsString|]
    , stringDecode = [|T.unpack|]
    , stringEmpty = [|"" :: String|]
    , stringIsEmpty = [|null|]
    , stringBaseRep = HsStringRep
    }


-- =========================================================================
-- Built-in bytes adapters
-- =========================================================================

-- | Strict 'ByteString' (default). Zero-copy decode.
strictBytesAdapter :: BytesAdapter
strictBytesAdapter =
  BytesAdapter
    { bytesType = [t|ByteString|]
    , bytesEncode = [|encodeStrictBytesFN|]
    , bytesSize = [|sizeStrictBytes|]
    , bytesDecode = [|id :: ByteString -> ByteString|]
    , bytesEmpty = [|BS.empty|]
    , bytesIsEmpty = [|BS.null|]
    , bytesBaseRep = StrictBytesRep
    }


-- | Lazy 'BL.ByteString'.
lazyBytesAdapter :: BytesAdapter
lazyBytesAdapter =
  BytesAdapter
    { bytesType = [t|BL.ByteString|]
    , bytesEncode = [|encodeLazyBytesFN|]
    , bytesSize = [|sizeLazyBytes|]
    , bytesDecode = [|BL.fromStrict|]
    , bytesEmpty = [|BL.empty|]
    , bytesIsEmpty = [|BL.null|]
    , bytesBaseRep = LazyBytesRep
    }


-- | 'SBS.ShortByteString' (unpinned, GC-friendly).
shortBytesAdapter :: BytesAdapter
shortBytesAdapter =
  BytesAdapter
    { bytesType = [t|SBS.ShortByteString|]
    , bytesEncode = [|encodeShortBytesFN|]
    , bytesSize = [|sizeShortBytes|]
    , bytesDecode = [|SBS.toShort|]
    , bytesEmpty = [|SBS.empty|]
    , bytesIsEmpty = [|SBS.null|]
    , bytesBaseRep = ShortBytesRep
    }


-- =========================================================================
-- Built-in repeated adapters
-- =========================================================================

-- | 'V.Vector' (default). O(1) index, good general-purpose.
vectorAdapter :: RepeatedAdapter
vectorAdapter =
  RepeatedAdapter
    { repeatedType = \t -> [t|V.Vector $t|]
    , repeatedEmpty = [|V.empty|]
    , repeatedSnoc = [|V.snoc|]
    , repeatedFoldl = [|V.foldl'|]
    , repeatedIsEmpty = [|V.null|]
    , repeatedBaseRep = VectorRep
    }


{- | Unboxed vector @VU.Vector@. No per-element heap objects for
primitive types (Int32, Int64, Word32, Word64, Float, Double, Bool).
Requires @Unbox@ constraint on the element type.
-}
unboxedVectorAdapter :: RepeatedAdapter
unboxedVectorAdapter =
  RepeatedAdapter
    { repeatedType = \t -> [t|VU.Vector $t|]
    , repeatedEmpty = [|VU.empty|]
    , repeatedSnoc = [|VU.snoc|]
    , repeatedFoldl = [|VU.foldl'|]
    , repeatedIsEmpty = [|VU.null|]
    , repeatedBaseRep = VectorRep -- reuses VectorRep for encode/decode dispatch
    }


-- | Plain list @[]@. Good fusion, convenient.
listAdapter :: RepeatedAdapter
listAdapter =
  RepeatedAdapter
    { repeatedType = \t -> [t|[$t]|]
    , repeatedEmpty = [|[]|]
    , repeatedSnoc = [|\xs x -> xs <> [x]|]
    , repeatedFoldl = [|foldl'|]
    , repeatedIsEmpty = [|null|]
    , repeatedBaseRep = ListRep
    }


-- | 'Seq'. O(log n) snoc, good for building.
seqAdapter :: RepeatedAdapter
seqAdapter =
  RepeatedAdapter
    { repeatedType = \t -> [t|Seq $t|]
    , repeatedEmpty = [|Seq.empty|]
    , repeatedSnoc = [|(Seq.|>)|]
    , repeatedFoldl = [|foldl'|]
    , repeatedIsEmpty = [|Seq.null|]
    , repeatedBaseRep = SeqRep
    }


-- =========================================================================
-- Built-in map adapters
-- =========================================================================

-- | 'Map' (default, ordered, O(log n) lookup).
ordMapAdapter :: MapAdapter
ordMapAdapter =
  MapAdapter
    { mapType = \k v -> [t|Map $k $v|]
    , mapEmpty = [|Map.empty|]
    , mapInsert = [|Map.insert|]
    , mapFoldl = [|Map.foldlWithKey'|]
    , mapIsEmpty = [|Map.null|]
    , mapSize = [|Map.size|]
    }


-- | 'HM.HashMap' (unordered, O(1) avg lookup).
hashMapAdapter :: MapAdapter
hashMapAdapter =
  MapAdapter
    { mapType = \k v -> [t|HM.HashMap $k $v|]
    , mapEmpty = [|HM.empty|]
    , mapInsert = [|HM.insert|]
    , mapFoldl = [|HM.foldlWithKey'|]
    , mapIsEmpty = [|HM.null|]
    , mapSize = [|HM.size|]
    }


-- =========================================================================
-- FieldRep + RepConfig
-- =========================================================================

{- | Representation choices for a single field.

Proto3 @optional@ scalars are always materialised as @'Maybe' a@; that
is the only representation the wire codecs support, so it is not a
knob on 'FieldRep'.
-}
data FieldRep = FieldRep
  { fieldString :: !StringAdapter
  , fieldBytes :: !BytesAdapter
  , fieldRepeated :: !RepeatedAdapter
  , fieldMap :: !MapAdapter
  }


-- | Sensible defaults: strict Text, strict ByteString, Vector, ordered Map.
defaultFieldRep :: FieldRep
defaultFieldRep =
  FieldRep
    { fieldString = strictTextAdapter
    , fieldBytes = strictBytesAdapter
    , fieldRepeated = vectorAdapter
    , fieldMap = ordMapAdapter
    }


-- | Configuration table mapping (message, field) pairs to representation choices.
data RepConfig = RepConfig
  { configDefault :: !FieldRep
  -- ^ Default representation for all fields.
  , configUnboxedRepeated :: !Bool
  -- ^ When 'True', repeated fields whose element type is 'Unbox'-able
  -- (all numeric scalars and Bool) default to @Data.Vector.Unboxed@
  -- instead of @Data.Vector@. Zero per-element heap overhead for
  -- packed repeated fields. Default: 'False' (for backwards compat).
  , configMessageOverrides :: !(Map Text FieldRep)
  -- ^ Per-message override (applies to all fields in the message).
  , configFieldOverrides :: !(Map (Text, Text) FieldRep)
  -- ^ Per-field override. Key is (messageName, fieldName).
  -- Haskell-side overrides take precedence over .proto annotations.
  , configAdapterRegistry :: !AdapterRegistry
  -- ^ Maps string names (from .proto annotations like
  -- @[(wireform.haskell_bytes) = \"lazy\"]@) to adapters.
  }


{- | Sensible defaults: strict Text, strict ByteString, boxed Vector,
ordered Map, no per-field or per-message overrides, and the built-in
adapter registry.
-}
defaultRepConfig :: RepConfig
defaultRepConfig =
  RepConfig
    { configDefault = defaultFieldRep
    , configUnboxedRepeated = False
    , configMessageOverrides = Map.empty
    , configFieldOverrides = Map.empty
    , configAdapterRegistry = defaultAdapterRegistry
    }


{- | Look up the representation for a specific field, falling back through
message-level then default config.
-}
lookupFieldRep :: Text -> Text -> RepConfig -> FieldRep
lookupFieldRep msgName fldName cfg =
  case Map.lookup (msgName, fldName) (configFieldOverrides cfg) of
    Just rep -> rep
    Nothing -> case Map.lookup msgName (configMessageOverrides cfg) of
      Just rep -> rep
      Nothing -> configDefault cfg


-- =========================================================================
-- =========================================================================
-- Adapter registry (for .proto annotation support)
-- =========================================================================

{- | Maps short string names to adapters. Used to resolve
@wireform.haskell_string@, @wireform.haskell_bytes@, etc.
options from @.proto@ files.

Built-in names: @\"strict\"@, @\"lazy\"@, @\"short\"@, @\"string\"@
(for strings); @\"strict\"@, @\"lazy\"@, @\"short\"@ (for bytes);
@\"vector\"@, @\"list\"@, @\"seq\"@ (for repeated); @\"ord\"@,
@\"hash\"@ (for maps).

Register your own adapters to use custom names:

@
myRegistry = defaultAdapterRegistry
  { arStringAdapters = Map.insert \"url\" myUrlAdapter
      (arStringAdapters defaultAdapterRegistry)
  }
@
-}
data AdapterRegistry = AdapterRegistry
  { arStringAdapters :: !(Map Text StringAdapter)
  , arBytesAdapters :: !(Map Text BytesAdapter)
  , arRepeatedAdapters :: !(Map Text RepeatedAdapter)
  , arMapAdapters :: !(Map Text MapAdapter)
  }


-- | Registry with all built-in adapters.
defaultAdapterRegistry :: AdapterRegistry
defaultAdapterRegistry =
  AdapterRegistry
    { arStringAdapters =
        Map.fromList
          [ ("strict", strictTextAdapter)
          , ("lazy", lazyTextAdapter)
          , ("short", shortTextAdapter)
          , ("string", hsStringAdapter)
          ]
    , arBytesAdapters =
        Map.fromList
          [ ("strict", strictBytesAdapter)
          , ("lazy", lazyBytesAdapter)
          , ("short", shortBytesAdapter)
          ]
    , arRepeatedAdapters =
        Map.fromList
          [ ("vector", vectorAdapter)
          , ("unboxed", unboxedVectorAdapter)
          , ("list", listAdapter)
          , ("seq", seqAdapter)
          ]
    , arMapAdapters =
        Map.fromList
          [ ("ord", ordMapAdapter)
          , ("hash", hashMapAdapter)
          ]
    }


{- | Read wireform-specific field options from a list of proto
@OptionDef@s and produce per-category overrides. Returns a
function that patches a 'FieldRep' with any overrides found.

Reads these extension options:

  * @(wireform.haskell_string)@ = @\"strict\"@ | @\"lazy\"@ | @\"short\"@ | @\"string\"@
  * @(wireform.haskell_bytes)@  = @\"strict\"@ | @\"lazy\"@ | @\"short\"@
  * @(wireform.haskell_repeated)@ = @\"vector\"@ | @\"list\"@ | @\"seq\"@
  * @(wireform.haskell_map)@    = @\"ord\"@ | @\"hash\"@
-}
wireformFieldOverrides :: AdapterRegistry -> [OptionDef] -> FieldRep -> FieldRep
wireformFieldOverrides reg opts base =
  let applyStr = case lookupWfOption "wireform.haskell_string" opts of
        Just name -> case Map.lookup name (arStringAdapters reg) of
          Just a -> \r -> r {fieldString = a}
          Nothing -> id
        Nothing -> id
      applyBytes = case lookupWfOption "wireform.haskell_bytes" opts of
        Just name -> case Map.lookup name (arBytesAdapters reg) of
          Just a -> \r -> r {fieldBytes = a}
          Nothing -> id
        Nothing -> id
      applyRepeated = case lookupWfOption "wireform.haskell_repeated" opts of
        Just name -> case Map.lookup name (arRepeatedAdapters reg) of
          Just a -> \r -> r {fieldRepeated = a}
          Nothing -> id
        Nothing -> id
      applyMap = case lookupWfOption "wireform.haskell_map" opts of
        Just name -> case Map.lookup name (arMapAdapters reg) of
          Just a -> \r -> r {fieldMap = a}
          Nothing -> id
        Nothing -> id
  in applyMap (applyRepeated (applyBytes (applyStr base)))


-- | Look up a wireform extension option and extract its string value.
lookupWfOption :: Text -> [OptionDef] -> Maybe Text
lookupWfOption name opts = case filter matchExt opts of
  (o : _) -> constToText (optValue o)
  [] -> Nothing
  where
    matchExt o = case optNameParts (optName o) of
      [ExtensionOption n] -> n == name
      _ -> False
    constToText (CString t) = Just t
    constToText (CIdent t) = Just t
    constToText _ = Nothing


-- Legacy enum types (for convenience / backwards compat)
-- =========================================================================

-- | How to represent proto @string@ fields (legacy convenience enum).
data StringRep
  = -- | Data.Text.Text (default, zero-copy decode)
    StrictTextRep
  | -- | Data.Text.Lazy.Text
    LazyTextRep
  | -- | Data.Text.Short.ShortText (via ShortByteString, compact)
    ShortTextRep
  | -- | [Char] (convenient but slow)
    HsStringRep
  deriving stock (Show, Eq, Ord)


-- | How to represent proto @bytes@ fields (legacy convenience enum).
data BytesRep
  = -- | Data.ByteString.ByteString (default, zero-copy decode)
    StrictBytesRep
  | -- | Data.ByteString.Lazy.ByteString
    LazyBytesRep
  | -- | Data.ByteString.Short.ShortByteString (unpinned, GC-friendly)
    ShortBytesRep
  deriving stock (Show, Eq, Ord)


-- | How to represent proto @repeated@ fields (legacy convenience enum).
data RepeatedRep
  = -- | Data.Vector.Vector (default, O(1) index)
    VectorRep
  | -- | [] (convenient, good fusion)
    ListRep
  | -- | Data.Sequence.Seq (O(log n) snoc, good for building)
    SeqRep
  deriving stock (Show, Eq, Ord)


-- | How to represent proto @map@ fields (legacy convenience enum).
data MapRep
  = -- | Data.Map.Strict.Map (default, ordered, O(log n) lookup)
    OrdMapRep
  | -- | Data.HashMap.Strict.HashMap (unordered, O(1) avg lookup)
    HashMapRep
  deriving stock (Show, Eq, Ord)


-- | Convert a legacy 'StringRep' enum to a 'StringAdapter'.
stringRepAdapter :: StringRep -> StringAdapter
stringRepAdapter = \case
  StrictTextRep -> strictTextAdapter
  LazyTextRep -> lazyTextAdapter
  ShortTextRep -> shortTextAdapter
  HsStringRep -> hsStringAdapter


-- | Convert a legacy 'BytesRep' enum to a 'BytesAdapter'.
bytesRepAdapter :: BytesRep -> BytesAdapter
bytesRepAdapter = \case
  StrictBytesRep -> strictBytesAdapter
  LazyBytesRep -> lazyBytesAdapter
  ShortBytesRep -> shortBytesAdapter


-- | Convert a legacy 'RepeatedRep' enum to a 'RepeatedAdapter'.
repeatedRepAdapter :: RepeatedRep -> RepeatedAdapter
repeatedRepAdapter = \case
  VectorRep -> vectorAdapter
  ListRep -> listAdapter
  SeqRep -> seqAdapter


-- | Convert a legacy 'MapRep' enum to a 'MapAdapter'.
mapRepAdapter :: MapRep -> MapAdapter
mapRepAdapter = \case
  OrdMapRep -> ordMapAdapter
  HashMapRep -> hashMapAdapter


-- =========================================================================
-- Runtime encode/decode/size helpers (referenced by adapter Q Exp values
-- and by TH-generated code at runtime)
-- =========================================================================

-- Field-number encode helpers (Int -> a -> Builder).
-- These are referenced by the built-in adapters.
-- They emit the full tag + payload for a length-delimited field.

encodeStrictTextFN :: Int -> Text -> B.Builder
encodeStrictTextFN !fn !val =
  putTag fn WireLengthDelimited <> putText val
{-# INLINE encodeStrictTextFN #-}


encodeLazyTextFN :: Int -> TL.Text -> B.Builder
encodeLazyTextFN !fn !val =
  let !bs = BL.toStrict (TLE.encodeUtf8 val)
  in putTag fn WireLengthDelimited <> putVarint (fromIntegral (BS.length bs)) <> B.byteStringCopy bs
{-# INLINE encodeLazyTextFN #-}


encodeShortTextFN :: Int -> SBS.ShortByteString -> B.Builder
encodeShortTextFN !fn !val =
  let !bs = SBS.fromShort val
  in putTag fn WireLengthDelimited <> putVarint (fromIntegral (BS.length bs)) <> B.byteStringCopy bs
{-# INLINE encodeShortTextFN #-}


encodeHsStringFN :: Int -> String -> B.Builder
encodeHsStringFN !fn !val = encodeStrictTextFN fn (T.pack val)
{-# INLINE encodeHsStringFN #-}


encodeStrictBytesFN :: Int -> ByteString -> B.Builder
encodeStrictBytesFN !fn !val =
  putTag fn WireLengthDelimited <> putLengthDelimited val
{-# INLINE encodeStrictBytesFN #-}


encodeLazyBytesFN :: Int -> BL.ByteString -> B.Builder
encodeLazyBytesFN !fn !val = encodeStrictBytesFN fn (BL.toStrict val)
{-# INLINE encodeLazyBytesFN #-}


encodeShortBytesFN :: Int -> SBS.ShortByteString -> B.Builder
encodeShortBytesFN !fn !val = encodeStrictBytesFN fn (SBS.fromShort val)
{-# INLINE encodeShortBytesFN #-}


-- Size helpers (a -> Int, including tag byte).

sizeStrictText :: Text -> Int
sizeStrictText !val =
  let !bs = TE.encodeUtf8 val
      !len = BS.length bs
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE sizeStrictText #-}


sizeLazyText :: TL.Text -> Int
sizeLazyText !val =
  let !bs = BL.toStrict (TLE.encodeUtf8 val)
      !len = BS.length bs
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE sizeLazyText #-}


sizeShortText :: SBS.ShortByteString -> Int
sizeShortText !val =
  let !len = SBS.length val
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE sizeShortText #-}


sizeHsString :: String -> Int
sizeHsString !val = sizeStrictText (T.pack val)
{-# INLINE sizeHsString #-}


sizeStrictBytes :: ByteString -> Int
sizeStrictBytes !val =
  let !len = BS.length val
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE sizeStrictBytes #-}


sizeLazyBytes :: BL.ByteString -> Int
sizeLazyBytes !val = sizeStrictBytes (BL.toStrict val)
{-# INLINE sizeLazyBytes #-}


sizeShortBytes :: SBS.ShortByteString -> Int
sizeShortBytes !val =
  let !len = SBS.length val
  in 1 + varintSize (fromIntegral len) + len
{-# INLINE sizeShortBytes #-}


-- Decoder wrappers for adapters (produce the decoded Haskell value
-- from strict ByteString wire bytes). These are referenced by
-- stringDecode / bytesDecode. Note: the actual Decoder integration is done
-- by the codegen which wraps these in the appropriate Decoder
-- combinator. For now these are just conversion functions used by
-- the TH splices.

decodeToLazyTextD :: Text -> TL.Text
decodeToLazyTextD = TL.fromStrict
{-# INLINE decodeToLazyTextD #-}


decodeToShortTextD :: Text -> SBS.ShortByteString
decodeToShortTextD = SBS.toShort . TE.encodeUtf8
{-# INLINE decodeToShortTextD #-}


decodeToHsStringD :: Text -> String
decodeToHsStringD = T.unpack
{-# INLINE decodeToHsStringD #-}


decodeToStrictBytesD :: ByteString -> ByteString
decodeToStrictBytesD = id
{-# INLINE decodeToStrictBytesD #-}


decodeToLazyBytesD :: ByteString -> BL.ByteString
decodeToLazyBytesD = BL.fromStrict
{-# INLINE decodeToLazyBytesD #-}


decodeToShortBytesD :: ByteString -> SBS.ShortByteString
decodeToShortBytesD = SBS.toShort
{-# INLINE decodeToShortBytesD #-}


-- Legacy field-number encode helpers (Int -> a -> Builder).
-- Kept for existing code that references them.

-- | Encode strict Text (the default — no conversion needed).
encodeStrictText :: Int -> Text -> B.Builder
encodeStrictText fn t =
  putTag fn WireLengthDelimited
    <> let bs = TE.encodeUtf8 t in putVarint (fromIntegral (BS.length bs)) <> B.byteStringCopy bs
{-# INLINE encodeStrictText #-}


-- | Encode lazy Text.
encodeLazyText :: Int -> TL.Text -> B.Builder
encodeLazyText fn t =
  let bs = BL.toStrict (TLE.encodeUtf8 t)
  in putTag fn WireLengthDelimited <> putVarint (fromIntegral (BS.length bs)) <> B.byteStringCopy bs
{-# INLINE encodeLazyText #-}


-- | Encode ShortByteString (used for ShortText rep — stored as UTF-8 SBS).
encodeShortByteString :: Int -> SBS.ShortByteString -> B.Builder
encodeShortByteString fn sbs =
  let bs = SBS.fromShort sbs
  in putTag fn WireLengthDelimited <> putLengthDelimited bs
{-# INLINE encodeShortByteString #-}


-- | Encode String.
encodeHsString :: Int -> String -> B.Builder
encodeHsString fn s = encodeStrictText fn (T.pack s)
{-# INLINE encodeHsString #-}


-- | Encode strict ByteString (no conversion).
encodeStrictBytes :: Int -> ByteString -> B.Builder
encodeStrictBytes fn bs =
  putTag fn WireLengthDelimited <> putLengthDelimited bs
{-# INLINE encodeStrictBytes #-}


-- | Encode lazy ByteString.
encodeLazyBytes :: Int -> BL.ByteString -> B.Builder
encodeLazyBytes fn lbs = encodeStrictBytes fn (BL.toStrict lbs)
{-# INLINE encodeLazyBytes #-}


-- | Encode short ByteString.
encodeShortBytes :: Int -> SBS.ShortByteString -> B.Builder
encodeShortBytes = encodeShortByteString
{-# INLINE encodeShortBytes #-}


-- Decode adapters (legacy)

-- | Decode to strict Text (zero-copy from wire).
decodeToStrictText :: ByteString -> Either String Text
decodeToStrictText bs = case TE.decodeUtf8' bs of
  Left _ -> Left "Invalid UTF-8"
  Right t -> Right t


-- | Decode to lazy Text.
decodeToLazyText :: ByteString -> Either String TL.Text
decodeToLazyText bs = case TE.decodeUtf8' bs of
  Left _ -> Left "Invalid UTF-8"
  Right t -> Right (TL.fromStrict t)


-- | Decode to ShortByteString (UTF-8 stored as SBS).
decodeToShortText :: ByteString -> SBS.ShortByteString
decodeToShortText = SBS.toShort


-- | Decode to Haskell String.
decodeToHsString :: ByteString -> Either String String
decodeToHsString bs = case TE.decodeUtf8' bs of
  Left _ -> Left "Invalid UTF-8"
  Right t -> Right (T.unpack t)


-- | Decode to lazy ByteString.
decodeToLazyBytes :: ByteString -> BL.ByteString
decodeToLazyBytes = BL.fromStrict


-- | Decode to ShortByteString.
decodeToShortBytes :: ByteString -> SBS.ShortByteString
decodeToShortBytes = SBS.toShort


-- Repeated field adapters

-- | Fold over a Vector to encode each element.
foldVector :: (a -> B.Builder) -> V.Vector a -> B.Builder
foldVector f = V.foldl' (\acc v -> acc <> f v) mempty
{-# INLINE foldVector #-}


-- | Fold over a list to encode each element.
foldList :: (a -> B.Builder) -> [a] -> B.Builder
foldList f = go mempty
  where
    go !acc [] = acc
    go !acc (x : xs) = go (acc <> f x) xs
{-# INLINE foldList #-}


-- | Fold over a Seq to encode each element.
foldSeq :: (a -> B.Builder) -> Seq a -> B.Builder
foldSeq f = foldl' (\acc v -> acc <> f v) mempty
{-# INLINE foldSeq #-}


-- Decode: empty and snoc for each container type.

emptyVector :: V.Vector a
emptyVector = V.empty


emptyList :: [a]
emptyList = []


emptySeq :: Seq a
emptySeq = Seq.empty


snocVector :: V.Vector a -> a -> V.Vector a
snocVector = V.snoc
{-# INLINE snocVector #-}


snocList :: [a] -> a -> [a]
snocList xs x = xs ++ [x]
{-# INLINE snocList #-}


snocSeq :: Seq a -> a -> Seq a
snocSeq = (Seq.|>)
{-# INLINE snocSeq #-}


-- Functions for checking emptiness of each container type.

nullVector :: V.Vector a -> Bool
nullVector = V.null


nullList :: [a] -> Bool
nullList [] = True
nullList _ = False


nullSeq :: Seq a -> Bool
nullSeq = Seq.null


-- String emptiness checks for each representation.

emptyStrictText :: Text
emptyStrictText = T.empty


emptyLazyText :: TL.Text
emptyLazyText = TL.empty


emptyShortBytes :: SBS.ShortByteString
emptyShortBytes = SBS.empty


emptyHsString :: String
emptyHsString = ""


-- Map operations for each map representation.

emptyOrdMap :: Map k v
emptyOrdMap = Map.empty


emptyHashMap :: HM.HashMap k v
emptyHashMap = HM.empty


insertOrdMap :: Ord k => k -> v -> Map k v -> Map k v
insertOrdMap = Map.insert
{-# INLINE insertOrdMap #-}


insertHashMap :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
insertHashMap = HM.insert
{-# INLINE insertHashMap #-}


nullOrdMap :: Map k v -> Bool
nullOrdMap = Map.null


nullHashMap :: HM.HashMap k v -> Bool
nullHashMap = HM.null


foldOrdMap :: (a -> k -> v -> a) -> a -> Map k v -> a
foldOrdMap = Map.foldlWithKey'
{-# INLINE foldOrdMap #-}


foldHashMap :: (a -> k -> v -> a) -> a -> HM.HashMap k v -> a
foldHashMap = HM.foldlWithKey'
{-# INLINE foldHashMap #-}


sizeOrdMap :: Map k v -> Int
sizeOrdMap = Map.size


sizeHashMap :: HM.HashMap k v -> Int
sizeHashMap = HM.size
