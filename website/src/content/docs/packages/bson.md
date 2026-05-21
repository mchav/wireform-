---
title: wireform-bson
description: "MongoDB BSON encoding and decoding with TH deriving, wireform-derive annotations, and full MongoDB element types."
sidebar:
  order: 12
---

`wireform-bson` implements BSON, the binary document format used by MongoDB
on the wire and in storage. BSON extends JSON-like documents with typed
fields, binary subtypes, and MongoDB-specific types such as `ObjectId` and
`Decimal128`. Use this package when you talk to MongoDB drivers, parse
change streams, or exchange documents with services that speak BSON rather
than JSON.

## Key features

- **Template Haskell deriving** via `deriveBSON` for Haskell record types,
  with `wireform-derive` annotations; Generic defaults (empty instances) work
  for simple cases
- **Full MongoDB element set** including `ObjectId`, `Decimal128`,
  JavaScript code, regex, timestamps, and MinKey/MaxKey
- **Binary subtypes** for UUID, user-defined payloads, and other BSON binary
  conventions
- **Dynamic values** via the untyped `Value` ADT for schema-less documents
- **Direct encoding** for pre-sized buffer writes on hot paths

## Basic usage

Map a Haskell record to a BSON document with the Template Haskell deriver:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module UserDoc where

import BSON.Class (ToBSON, FromBSON, encodeBSON, decodeBSON)
import BSON.Derive (deriveBSON)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Text (Text)

data User = User
  { userName :: !Text
  , userAge  :: !Int
  , userId   :: !ByteString
  }
  deriving stock (Show, Eq, Generic)

$(deriveBSON ''User)

insertBytes :: User -> ByteString
insertBytes user = encodeBSON user

readUser :: ByteString -> Either String User
readUser bs = decodeBSON bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToBSON User` and
`instance FromBSON User` declarations.

When you need MongoDB-specific field types, model them with the `Value`
constructors and use the dynamic ADT, or wrap the wire shapes in newtypes
with custom instances:

```haskell
import BSON.Value qualified as B
import Data.Vector qualified as V

paymentDoc :: B.Value
paymentDoc =
  B.Document $
    V.fromList
      [ ("amount", B.Decimal128 amountBytes)
      , ("note", B.JavaScript "function() { return true; }")
      , ("tags", B.Regex "paid" "i")
      ]
```

For documents whose shape is only known at runtime, work with the dynamic ADT:

```haskell
import BSON.Value qualified as B
import BSON.Encode (encode)
import BSON.Decode (decode)
import Data.Vector qualified as V

lookupName :: B.Value -> Maybe Text
lookupName doc =
  case doc of
    B.Document fields ->
      case V.find ((== "name") . fst) (V.toList fields) of
        Just (_, B.String t) -> Just t
        _                    -> Nothing
    _ -> Nothing
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `BSON.Class` | `ToBSON` / `FromBSON`, `encodeBSON`, `decodeBSON` |
| `BSON.Encode` / `BSON.Decode` | Low-level wire encode and decode |
| `BSON.Value` | Dynamic `Value` ADT and MongoDB-specific types (`ObjectId`, `Decimal128`, `Regex`, ...) |
| `BSON.Derive` | Template Haskell deriver with `wireform-derive` annotations |
