---
title: wireform-msgpack
description: "MessagePack encoding and decoding with Generic deriving, streaming decode, msgpack-RPC, and a JSON bridge."
sidebar:
  order: 11
---

`wireform-msgpack` implements the MessagePack binary serialization format.
MessagePack is widely used for RPC, caching, and inter-service communication
because it is compact, fast to parse, and supported by libraries in most
languages. Use this package when you want a lightweight alternative to JSON
with similar flexibility but smaller payloads.

## Key features

- **Generic deriving** via `ToMsgPack` and `FromMsgPack` for records, enums,
  and sum types
- **Streaming decode** for concatenated or length-prefixed MessagePack frames
- **msgpack-RPC** message encoding for request/response/notification patterns
- **JSON bridge** for converting between MessagePack and Aeson `Value`
- **Dynamic values** via the untyped `Value` ADT when schemas are unknown at
  compile time

## Basic usage

Define a type, derive `Generic`, and attach the codec instances:

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Person where

import MsgPack.Class (ToMsgPack, FromMsgPack, encodeMsgPack, decodeMsgPack)
import GHC.Generics (Generic)
import Data.Text (Text)

data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToMsgPack, FromMsgPack)

roundTrip :: Person -> Either String Person
roundTrip p =
  case decodeMsgPack (encodeMsgPack p) of
    Left err  -> Left err
    Right val -> Right val
```

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
