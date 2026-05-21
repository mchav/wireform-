---
title: wireform-derive
description: "Annotation-driven TH deriving: one set of annotations, every wire format."
sidebar:
  order: 2
---

`wireform-derive` is the shared annotation vocabulary that powers Template
Haskell deriving across every wireform format. You annotate your Haskell types
once with `ANN` pragmas, call the format's TH deriver, and those annotations
control how fields are named, tagged, skipped, or restructured for MessagePack,
CBOR, YAML, XML, Protocol Buffers, Avro, and every other supported backend.

## Two ways to get instances

Every format's `Class` module (e.g. `MsgPack.Class`, `CBOR.Class`) provides
two paths to derive instances:

### Path 1: Generic defaults (no annotations)

The typeclasses have `DefaultSignatures` that delegate to `GHC.Generics`.
You can write empty instances and get working codecs with **plain field names
and no customization**:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)
import MsgPack.Class (ToMsgPack, FromMsgPack)

data User = User
  { userName :: !Text
  , userAge  :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToMsgPack User
instance FromMsgPack User
```

This works, but field names go to the wire verbatim (`userName`, `userAge`).
There is no way to rename, skip, tag, or otherwise customize the encoding.

### Path 2: Template Haskell deriver (with annotations)

Each format ships a `Derive` module that reads `ANN` pragmas through the
`wireform-derive` vocabulary and splices customized instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Wireform.Derive (renameStyle, SnakeCase)
import MsgPack.Derive (deriveMsgPack)

data User = User
  { userName :: !Text
  , userAge  :: !Int
  }

{-# ANN type User (renameStyle SnakeCase) #-}

deriveMsgPack ''User
```

Now the wire keys are `user_name` and `user_age`. This is the only path that
supports the annotation vocabulary described below. It also supports multi-format
in a single file:

```haskell
{-# ANN type User (renameStyle SnakeCase) #-}
deriveMsgPack ''User
deriveCBOR ''User
deriveYAML ''User
```

All three formats will see `user_name` and `user_age` on the wire, driven by
the same annotation.

## When to use which

| | Generic defaults | TH deriver |
|---|---|---|
| Annotations (rename, tag, skip, etc.) | No | Yes |
| Requires `TemplateHaskell` | No | Yes |
| Field names | Verbatim from Haskell | Controlled by annotations |
| Multi-format from one annotation set | No | Yes |
| Backend-specific overrides | No | Yes |

If your types already have the wire names you want and you don't need any
customization, Generic defaults are fine. For everything else, use the TH
deriver.

## Annotations

All annotations come from `Wireform.Derive.Modifier`. Import `Wireform.Derive`
for the full vocabulary.

### Renaming

| Annotation | Effect |
|------------|--------|
| `rename "wire_name"` | Use an exact wire key |
| `renameStyle SnakeCase` | Apply a naming convention to all fields |
| `renameWith 'myFunction` | Apply a `Text -> Text` function at splice time |
| `renameIdiomatic` | Use the backend's default convention |

Apply to a type (affects all fields) or to individual fields.

### Naming conventions

The `NameStyle` type supports:

| Style | Example output |
|-------|---------------|
| `SnakeCase` | `user_name` |
| `CamelCase` | `userName` |
| `PascalCase` | `UserName` |
| `KebabCase` | `user-name` |
| `UpperSnake` | `USER_NAME` |
| `UpperKebab` | `USER-NAME` |
| `LowerCase` / `UpperCase` | `username` / `USERNAME` |

Styles compose with `andThen`: `StripPrefix "cfg" \`andThen\` SnakeCase`
strips the prefix first, then applies snake_case.

`Idiomatic` resolves to whatever convention the target backend prefers. For
example, JSON and Proto default to `CamelCase`, YAML and HTML to `KebabCase`,
TOML and Avro to `SnakeCase`, and XML to `PascalCase`.

### Field control

| Annotation | Effect |
|------------|--------|
| `tag 3` | Explicit numeric tag (proto field number, Thrift field ID) |
| `skip` | Omit from the wire entirely |
| `flatten` | Inline nested record fields into the parent |
| `defaults 'myDefault` | Supply a default when the field is missing during decode |
| `required` / `optional` | Override the format's default nullability |
| `coerced 'MyNewtype` | Encode/decode through a newtype coercion |

### Wire encoding overrides

`wireOverride` forces a non-default encoding for numeric fields:

| Override | Meaning |
|----------|---------|
| `WireZigZag` | ZigZag-encoded varint (proto `sint32`/`sint64`) |
| `WireFixed` | Fixed-width encoding (proto `fixed32`/`fixed64`) |
| `WirePacked` | Packed repeated field |
| `WireString` | Encode as string |
| `WireBytes` | Encode as bytes |

### Proto-specific

| Annotation | Effect |
|------------|--------|
| `mapKey MapKeyString` | Proto3 map key type |
| `oneof "choice"` | Group fields into a proto oneof |

### Backend targeting

Not every annotation makes sense for every format. These let you scope
annotations to specific backends:

```haskell
{-# ANN userName (forBackend "proto" (tag 1)) #-}
{-# ANN userName (forBackend "json" (rename "name")) #-}
{-# ANN userName (disableFor ["csv"]) #-}
```

| Annotation | Effect |
|------------|--------|
| `forBackend backend mods` | Apply only to one backend |
| `forBackends [backends] mods` | Apply to several backends |
| `disableFor [backends]` | Skip this field for listed backends |

Later annotations shadow earlier ones, so you can set a global rename and
override it for a specific format.

## How the deriver pipeline works

Each per-format `Derive` module (e.g. `MsgPack.Derive`, `CBOR.Derive`,
`Proto.TH.Derive`) follows the same steps:

1. **Reify** the type with `reifyTypeInfo` to get constructor and field
   information.
2. **Resolve** annotations with `reifyModifierInfoFor` to get the wire keys,
   tags, and flags for each field, scoped to that backend.
3. **Splice** instance declarations that use the resolved keys and flags.

If you're adding a new format to wireform, the path of least resistance is to
clone the nearest existing deriver and adapt the value-mapping calls.

## Extension mechanism

For backend-specific payloads that don't belong in the core `Modifier` type,
use the `BackendModifier` typeclass:

```haskell
data XmlFieldOpt = XmlAttr | XmlText | XmlCData
  deriving (Eq, Show, Read)

instance BackendModifier XmlFieldOpt where
  backendModifierTag _ = "wireform-xml.field-opt"
```

Then annotate with `extension XmlAttr` and retrieve it in the deriver with
`lookupExtension @XmlFieldOpt modInfo`. This keeps the core vocabulary clean
while letting each format define its own concepts.

## Aeson as a worked example

`Wireform.Derive.Aeson` is a complete Aeson deriver built on the shared core.
It serves as the reference implementation for how to write a format deriver:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Wireform.Derive (renameStyle, SnakeCase)
import Wireform.Derive.Aeson (deriveJSON)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  }

{-# ANN type Person (renameStyle SnakeCase) #-}

deriveJSON ''Person
-- splices ToJSON and FromJSON instances
-- wire keys: "person_name", "person_age"
```

The deriver handles records, newtypes, enums, and sum types. Sum types use
Aeson's `TaggedObject` shape by default.

## Quick reference

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Wireform.Derive

-- Type-level: applies to all fields
{-# ANN type MyRecord (renameStyle SnakeCase) #-}

-- Field-level: specific overrides
{-# ANN myField (rename "id") #-}
{-# ANN myField (tag 1) #-}
{-# ANN myField (forBackend "xml" (extension XmlAttr)) #-}

-- Combine multiple
{-# ANN myField (forBackends ["json", "yaml"] (rename "name")) #-}

-- Splice the instances
deriveMsgPack ''MyRecord
deriveCBOR ''MyRecord
deriveYAML ''MyRecord
```
