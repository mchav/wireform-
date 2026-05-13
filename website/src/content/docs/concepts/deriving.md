---
title: Generic deriving
description: How to use wireform-derive annotations to control serialization across formats.
sidebar:
  order: 1
---

:::caution[Not yet on Hackage]
wireform is in heavy development and has not been published to Hackage yet.
The annotation vocabulary described here may change.
:::

wireform uses a single annotation vocabulary — defined in `wireform-derive`
— that works across every format with Generic support. Annotate once,
and the same rules apply whether you're serializing to MessagePack, CBOR,
YAML, XML, or any other supported backend.

## Basic usage

Derive the format's typeclasses with `DeriveAnyClass`:

```haskell
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}

data User = User
  { userName :: !Text
  , userAge  :: !Int
  , userRole :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToMsgPack, FromMsgPack, ToCBOR, FromCBOR)
```

By default, field names are used as-is. The annotations below let you
customize how fields are mapped to the wire format.

## Annotations

All annotations are applied via `wireform-derive`'s modifier system.

### `rename`

Override the wire name of a specific field:

```haskell
data Config = Config
  { configHost :: !Text  -- becomes "host" on the wire
  , configPort :: !Int   -- becomes "port" on the wire
  }
```

### `renameStyle`

Apply a naming convention to all fields. Supported styles include
`camelCase`, `snake_case`, `PascalCase`, `kebab-case`, and
`SCREAMING_SNAKE_CASE`.

### `tag`

Set an explicit numeric tag for a field (used in tagged binary formats
like Protocol Buffers and Thrift).

### `skip`

Exclude a field from serialization entirely.

### `defaults`

Provide default values for fields that may be missing during decode.

### `optional` / `required`

Mark fields as optional (decoded to `Maybe`) or required (decode fails
if missing).

### `flatten`

Inline a nested record's fields into the parent, rather than nesting
them under a key.

### `oneof`

Model a sum type as a protobuf-style `oneof` field.

### `forBackend` / `forBackends` / `disableFor`

Apply annotations only to specific backends. Useful when different
formats need different field names or strategies.

## Format-specific modules

Each format package re-exports everything you need:

| Format | To-class | From-class | Module |
|--------|----------|------------|--------|
| MessagePack | `ToMsgPack` | `FromMsgPack` | `MsgPack.Class` |
| CBOR | `ToCBOR` | `FromCBOR` | `CBOR.Class` |
| YAML | `ToYAML` | `FromYAML` | `YAML.Class` |
| TOML | `ToTOML` | `FromTOML` | `TOML.Class` |
| XML | `ToXML` | `FromXML` | `XML.Class` |
| HTML | `ToHTML` | `FromHTML` | `HTML.Class` |
| BSON | `ToBSON` | `FromBSON` | `BSON.Class` |
| Ion | `ToIon` | `FromIon` | `Ion.Class` |
| EDN | `ToEDN` | `FromEDN` | `EDN.Class` |
| Bencode | `ToBencode` | `FromBencode` | `Bencode.Class` |
| CSV | `ToCSV` | `FromCSV` | `CSV.Class` |
| NDJSON | `ToNDJSON` | `FromNDJSON` | `NDJSON.Class` |
