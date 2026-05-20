---
title: wireform-thrift
description: "Apache Thrift binary and compact wire protocols with IDL codegen, RPC message framing, and Generic deriving."
sidebar:
  order: 31
---

`wireform-thrift` implements [Apache Thrift](https://thrift.apache.org/), the
IDL-driven serialization framework used by Cassandra, Parquet footers, and many
high-throughput services. Thrift structs carry numbered field IDs for forward and
backward compatibility, and the package supports both the legacy binary protocol
and the smaller compact protocol. Use this package when you need Thrift wire
compatibility, RPC message framing, or schema codegen from `.thrift` IDL files.

## Key features

- **Typeclass API** via `ToThrift` and `FromThrift` with GHC `Generic` auto field IDs
- **Binary and Compact wire protocols** with matching encode/decode entry points
- **Thrift IDL parser and codegen** from `.thrift` schema files
- **Service definitions** for RPC method signatures
- **Message framing** for request/response envelopes (`Thrift.Message`)
- **JSON bridge** for self-describing text rendering
- **QuasiQuoter** for inline `[thrift| ... |]` schemas
- **Runtime registry** for dynamic struct lookup

## Basic usage

Derive instances with `Generic`, then pick a wire protocol. Compact is the
recommended choice for new code because it produces smaller payloads:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Text (Text)
import GHC.Generics (Generic)
import Thrift.Class
  ( ToThrift, FromThrift
  , encodeThriftBinary, decodeThriftBinary
  , encodeThriftCompact, decodeThriftCompact
  )

data LogEntry = LogEntry
  { level   :: !Text
  , message :: !Text
  , code    :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToThrift, FromThrift)

entry :: LogEntry
entry = LogEntry "ERROR" "disk full" 507

-- Binary protocol (tag-prefixed fields, larger on the wire)
binaryBytes :: ByteString
binaryBytes = encodeThriftBinary entry

decodeBinary :: Either String LogEntry
decodeBinary = decodeThriftBinary binaryBytes

-- Compact protocol (variable-length encoding, recommended)
compactBytes :: ByteString
compactBytes = encodeThriftCompact entry

decodeCompact :: Either String LogEntry
decodeCompact = decodeThriftCompact compactBytes
```

For RPC-style communication, wrap payloads in a message envelope:

```haskell
import Thrift.Class (toThrift)
import Thrift.Message
  ( ThriftMessage (..), ThriftMessageType (..)
  , encodeMessageCompact, decodeMessageCompact
  )

sendRequest :: Text -> LogEntry -> ByteString
sendRequest methodName entry =
  encodeMessageCompact $
    ThriftMessage methodName TMsgCall 1 (toThrift entry)

receiveResponse :: ByteString -> Either String LogEntry
receiveResponse framed = do
  ThriftMessage _ TMsgReply _ payload <- decodeMessageCompact framed
  fromThrift payload
```

Generate types from IDL with the quasiquoter or CLI:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Thrift.QQ (thrift)

[thrift|
  struct Person {
    1: string name,
    2: i32 age,
  }
|]
```

```bash
wireform-gen thrift -i service.thrift -o src/Gen/
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Thrift.Class` | `ToThrift` / `FromThrift` plus `encodeThriftBinary` / `encodeThriftCompact` |
| `Thrift.Encode` / `Thrift.Decode` | Low-level binary and compact wire primitives |
| `Thrift.Wire` | Type tags, field IDs, and TType constants |
| `Thrift.Value` | Dynamic untyped `Value` ADT |
| `Thrift.Schema` / `Thrift.Parser` | IDL AST and `.thrift` parser |
| `Thrift.CodeGen` / `Thrift.QQ` | Haskell codegen and quasiquoter |
| `Thrift.Message` | RPC message envelope and framing |
| `Thrift.Transport` | Length-prefixed transport helpers |
| `Thrift.Registry` | Runtime struct schema registry |
| `Thrift.JSON` | Thrift to JSON bridge |
| `Thrift.Derive` | Template Haskell deriver with annotation modifiers |
