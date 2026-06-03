---
title: Deriving instances
description: "How to derive codec instances across wireform formats using Template Haskell or Generic defaults."
sidebar:
  order: 1
---

:::caution[Not yet on Hackage]
wireform is in heavy development and has not been published to Hackage yet.
The annotation vocabulary described here may change.
:::

wireform provides two ways to derive encode/decode instances for your types.

## Template Haskell deriver (recommended)

Each format ships a `Derive` module with a TH splice that reads `ANN` pragmas
from the shared `wireform-derive` annotation vocabulary. This is the only path
that supports field renaming, tagging, skipping, and backend-specific overrides:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Wireform.Derive (renameStyle, SnakeCase)
import MsgPack.Derive (deriveMsgPack)
import CBOR.Derive (deriveCBOR)

data User = User
  { userName :: !Text
  , userAge  :: !Int
  , userRole :: !Text
  }

{-# ANN type User (renameStyle SnakeCase) #-}

deriveMsgPack ''User
deriveCBOR ''User
```

Both formats see `user_name`, `user_age`, and `user_role` on the wire,
driven by the same annotation.

## Generic defaults (no annotations)

Every format's typeclass has `DefaultSignatures` that delegate to
`GHC.Generics`. If you don't need any customization, write empty instance
declarations:

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

Field names go to the wire verbatim. The `wireform-derive` annotation
vocabulary has no effect on this path.

## Full annotation reference

See [wireform-derive](../../packages/derive/) for the complete annotation
vocabulary: `rename`, `renameStyle`, `tag`, `skip`, `flatten`, `defaults`,
`optional`, `required`, `coerced`, `wireOverride`, `forBackend`, and
`extension`.

## Format-specific derivers

| Format | TH deriver | Module |
|--------|------------|--------|
| MessagePack | `deriveMsgPack` | `MsgPack.Derive` |
| CBOR | `deriveCBOR` | `CBOR.Derive` |
| YAML | `deriveYAML` | `YAML.Derive` |
| TOML | `deriveTOML` | `TOML.Derive` |
| XML | `deriveXML` | `XML.Derive` |
| HTML | `deriveHTML` | `HTML.Derive` |
| BSON | `deriveBSON` | `BSON.Derive` |
| Ion | `deriveIon` | `Ion.Derive` |
| EDN | `deriveEDN` | `EDN.Derive` |
| Bencode | `deriveBencode` | `Bencode.Derive` |
| CSV | `deriveCSV` | `CSV.Derive` |
| Fory | `deriveFory` | `Fory.Derive` |
| Thrift | `deriveThrift` | `Thrift.Derive` |
| Avro | `deriveAvro` | `Avro.Derive` |
| Bond | `deriveBond` | `Bond.Derive` |
| FlatBuffers | `deriveFlatBuffers` | `FlatBuffers.Derive` |
| Cap'n Proto | `deriveCapnProto` | `CapnProto.Derive` |
| ASN.1 | `deriveASN1` | `ASN1.Derive` |
| Aeson (JSON) | `deriveJSON` | `Wireform.Derive.Aeson` |
