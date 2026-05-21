---
title: wireform-msgpack
description: "MessagePack encoding and decoding with TH deriving, streaming decode, msgpack-RPC, and a JSON bridge."
sidebar:
  order: 11
---

`wireform-msgpack` implements the MessagePack binary serialization format.
MessagePack is widely used for RPC, caching, and inter-service communication
because it is compact, fast to parse, and supported by libraries in most
languages. Use this package when you want a lightweight alternative to JSON
with similar flexibility but smaller payloads.

## Key features

- **Template Haskell deriving** via `deriveMsgPack` for records, enums, and sum
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Streaming decode** for concatenated or length-prefixed MessagePack frames
- **msgpack-RPC** message encoding for request/response/notification patterns
- **JSON bridge** for converting between MessagePack and Aeson `Value`
- **Dynamic values** via the untyped `Value` ADT when schemas are unknown at
  compile time

## Basic usage

Define a type and derive codec instances with Template Haskell:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Person where

import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)
import MsgPack.Derive (deriveMsgPack)
import GHC.Generics (Generic)
import Data.Text (Text)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveMsgPack ''Person)

roundTrip :: Person -> Either String Person
roundTrip p =
  case decodeMsgPack (encodeMsgPack p) of
    Left err  -> Left err
    Right val -> Right val
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToMsgPack Person` and
`instance FromMsgPack Person` declarations.

For RPC-style messaging, use the msgpack-RPC envelope helpers:

```haskell
import MsgPack.RPC (RPCMessage(..), encodeRPC, decodeRPC)
import Data.Vector (Vector)
import qualified Data.Vector as V
import MsgPack.Value qualified as MV

call :: Text -> Vector MV.Value -> ByteString
call method params =
  encodeRPC (RPCRequest 1 method params)

handle :: ByteString -> Either String RPCMessage
handle = decodeRPC
```

When processing a buffer that may contain multiple MessagePack values, decode
one at a time and advance the cursor:

```haskell
import MsgPack.Stream (decodeOneWithLeftover)

takeNext :: ByteString -> Either String (MV.Value, ByteString)
takeNext = decodeOneWithLeftover
```

To inspect or transform values without generated types, round-trip through
the dynamic ADT:

```haskell
import MsgPack.Value qualified as MV
import MsgPack.Encode (encode)
import MsgPack.Decode (decode)

dynamicRoundTrip :: MV.Value -> Either String MV.Value
dynamicRoundTrip val =
  case decode (encode val) of
    Left err  -> Left err
    Right out -> Right out
```

## Performance

wireform-msgpack is ~4x faster than the Hackage `msgpack` package on both
encode and decode for a typical record payload.

### wireform-msgpack vs Hackage `msgpack`

| Operation | wireform-msgpack | msgpack | Speedup |
|-----------|-----------------|---------|---------|
| encode | 292 ns | 1191 ns | 4.1x |
| decode | 391 ns | 1702 ns | 4.3x |

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-msgpack/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `MsgPack.Class` | `ToMsgPack` / `FromMsgPack`, `encodeMsgPack`, `decodeMsgPack` |
| `MsgPack.Encode` / `MsgPack.Decode` | Low-level wire encode and decode |
| `MsgPack.Value` | Dynamic untyped `Value` ADT |
| `MsgPack.Stream` | Incremental decode for framed input |
| `MsgPack.RPC` | msgpack-RPC request, response, and notification messages |
| `MsgPack.JSON` | MessagePack ↔ JSON conversion |
| `MsgPack.Derive` | Template Haskell deriver with `wireform-derive` annotations |
