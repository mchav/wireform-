---
title: wireform-ndjson
description: "Newline-delimited JSON framing on aeson with streaming decode, concurrent processing, and SIMD newline scanning."
sidebar:
  order: 25
---

`wireform-ndjson` adds newline-delimited JSON framing on top of aeson. Log
aggregators, analytics pipelines, and many HTTP streaming APIs emit one JSON
value per line; this package splits those lines efficiently, parses each with
aeson, and exposes both batch and streaming APIs so memory stays flat on large
inputs. Use it when you already model records with `ToJSON`/`FromJSON` and only
need the NDJSON container format.

## Key features

| Capability | Why it matters |
|------------|----------------|
| NDJSON framing on aeson | Reuse existing JSON instances without a second schema |
| Streaming decode | `decodeStream` calls back per line with bounded memory |
| Concurrent producer/consumer | `decodeConcurrent` parses and dispatches across a `TBQueue` |
| SIMD newline scanning | `Wireform.FFI.findByteBS` finds `\n` in 16-byte chunks |
| Typed batch helpers | `decodeRecords` and `encodeRecords` for `Vector` workflows |

## Basic usage

### Encode a batch of records

Each value becomes one JSON object followed by a newline. The encoder uses
`Wireform.Builder` to avoid unnecessary intermediate buffers.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import NDJSON.Encode (encodeRecords)

data Event = Event
  { eventId   :: !Int
  , eventName :: !Text
  } deriving stock (Generic, ToJSON)

writeLog :: Vector Event -> ByteString
writeLog = encodeRecords
```

### Stream decode with a callback

When lines arrive from a socket or a file read loop, `decodeStream` parses one
line at a time and invokes your handler. Empty lines are skipped.

```haskell
import Data.ByteString (ByteString)
import NDJSON.Decode (decodeStream)
import qualified Data.Aeson as Aeson

processLines :: ByteString -> IO (Either String ())
processLines bs =
  decodeStream bs $ \val -> do
    case Aeson.fromJSON val of
      Aeson.Success ev -> handleEvent (ev :: Event)
      Aeson.Error err  -> print err
```

### Batch decode into typed records

When the entire blob fits in memory, `decodeRecords` returns a `Vector` of
parsed rows in one pass.

```haskell
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import NDJSON.Decode (decodeRecords)

loadEvents :: ByteString -> Either String (Vector Event)
loadEvents = decodeRecords
```

### Concurrent parsing

`decodeConcurrent` runs a producer thread that scans newlines and enqueues
parsed `Aeson.Value` values into a `TBQueue`, while the same call processes
each value through your callback on the consumer side. Pass the queue depth to
control how many parsed lines can buffer between producer and consumer.

```haskell
import Data.ByteString (ByteString)
import NDJSON.Decode (decodeConcurrent)
import qualified Data.Aeson as Aeson

processConcurrent :: ByteString -> Int -> IO (Either String ())
processConcurrent bs queueDepth =
  decodeConcurrent bs queueDepth $ \val -> do
    case Aeson.fromJSON val of
      Aeson.Success ev -> handleEvent (ev :: Event)
      Aeson.Error err  -> print err
```

For untyped pipelines, `NDJSON.Decode.decode` returns `Vector Aeson.Value`, and
`NDJSON.Encode.encode` accepts the same.

## Notable modules

| Module | Role |
|--------|------|
| `NDJSON.Decode` | `decode`, `decodeStream`, `decodeRecords`, `decodeConcurrent` |
| `NDJSON.Encode` | `encode`, `encodeRecords` |
| `NDJSON.Derive` | Helpers aligned with the wireform deriver ecosystem |
