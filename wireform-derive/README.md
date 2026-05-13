# wireform-derive

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Annotation-driven Template Haskell deriver core for the
[`wireform`][wireform] family.  One `{-# ANN ... #-}` vocabulary
drives instance generation for every supported wire format -- JSON,
protobuf, CBOR, MessagePack, Thrift, BSON, Ion, EDN, TOML, Bencode,
NDJSON, CSV, XML, HTML, ASN.1, FlatBuffers, Cap'n Proto, Avro, Bond,
Arrow, Parquet, ORC, Iceberg.

[wireform]: https://github.com/iand675/wireform-

## How it fits together

* **This package** ships the cross-cutting vocabulary
  (`Wireform.Derive.Modifier`) and TH reflection helpers
  (`Wireform.Derive.TypeInfo` / `Wireform.Derive.NameStyle` /
  `Wireform.Derive.Backend` / `Wireform.Derive.Extension`).
* **Per-format packages** (`wireform-proto`, `wireform-cbor`,
  `wireform-msgpack`, …) each ship a `<Format>.Derive` module that
  consumes the same vocabulary.  Adding a new format mostly means
  cloning the nearest existing `<Format>.Derive` and adapting the
  value-mapping calls.
* `Wireform.Derive.Aeson` (in this package) is the canonical
  worked-example deriver and the only one that lives outside a
  format-specific package.

## Vocabulary at a glance

```haskell
data Person = Person
  { personFullName :: !Text
  , personAge      :: !Word32
  , personBalance  :: !Int64
  , personSecret   :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Person ("Person" :: String) #-}

-- Wire tags / field numbers, used by tag-based formats (proto,
-- Thrift, Bond, Iceberg).
{-# ANN personFullName (tag 1) #-}
{-# ANN personAge      (tag 2) #-}
{-# ANN personBalance  (tag 3) #-}
{-# ANN personSecret   (tag 4) #-}

-- snake_case by default everywhere.
{-# ANN personFullName (renameStyle SnakeCase) #-}
{-# ANN personAge      (renameStyle SnakeCase) #-}
{-# ANN personBalance  (renameStyle SnakeCase) #-}
{-# ANN personSecret   (renameStyle SnakeCase) #-}

-- JSON-only override: `fullName` instead of `full_name`.
{-# ANN personFullName (forBackend backendJSON (rename "fullName")) #-}

-- JSON-only `skip`: never serialise `personSecret` to JSON.
{-# ANN personSecret   (forBackend backendJSON skip) #-}
```

Each per-format deriver then runs from a separate TH splice:

```haskell
import qualified Proto.Derive    as DProto
import qualified CBOR.Derive     as DCBOR
import qualified MsgPack.Derive  as DMP
import qualified Wireform.Derive.Aeson as DAeson

DProto.deriveProto ''Person
DCBOR.deriveCBOR   ''Person
DMP.deriveMsgPack  ''Person
DAeson.deriveJSON  ''Person
```

Result: `personFullName` becomes `full_name` on every binary wire
but `fullName` in JSON, and `personSecret` is omitted from JSON.

## `BackendModifier` extensions

Backends that need their own typed payloads (e.g. XML attribute vs.
element, ASN.1 explicit / implicit tagging, HTML attr vs. child)
opt in via the `BackendModifier` typeclass:

```haskell
class (Eq a, Show a, Read a, Typeable a) => BackendModifier a where
  backendModifierTag :: Proxy a -> Text
```

Each backend declares its own ADT under a unique tag namespace
(e.g. `wireform-xml.field-opt`, `wireform-asn1.field-opt`).
Annotations attach via `extension`, and per-backend deriver code
reads them via `lookupExtension` / `lookupExtensions` /
`hasExtension`.

## Inspirations

* riz0id's [`serde-th`](https://github.com/riz0id/serde-th)
  for the per-field annotation idea.
* `aeson-th` for the proven TH-driven JSON pattern.

## License

BSD-3-Clause.
