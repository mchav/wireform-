# wireform-ndjson

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

[Newline-delimited JSON](https://github.com/ndjson/ndjson-spec) (NDJSON,
also commonly called JSON Lines) for Haskell. Encode and decode
streams of `aeson` values with a SIMD-accelerated newline scanner, an
optional concurrent multi-line decoder for batch ingestion, and a
Template Haskell deriver that plumbs a typed Haskell record straight
through to NDJSON without going through an intermediate dynamic value.

NDJSON is the format you reach for when "stream of JSON values" is
the right abstraction. One JSON value per line, no enclosing array,
trivial to append to, trivial to grep, trivial to feed into another
process line by line. It's the wire format for the Elasticsearch
bulk API, the storage format for most "structured logs" pipelines,
and the dump format for tools that need a forward-streamable
alternative to a single giant JSON object.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-ndjson,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-ndjson` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import qualified NDJSON.Encode as NJE
import qualified NDJSON.Decode as NJD

data LogEntry = LogEntry
  { ts      :: !Text
  , level   :: !Text
  , message :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (A.ToJSON, A.FromJSON)

main :: IO ()
main = do
  let rows = V.fromList
        [ LogEntry "2026-05-13T08:00:00Z" "INFO"  "starting"
        , LogEntry "2026-05-13T08:00:01Z" "WARN"  "slow query"
        , LogEntry "2026-05-13T08:00:02Z" "INFO"  "ready"
        ]
      bytes = NJE.encodeRecords rows
  BS8.putStrLn bytes
  case NJD.decodeRecords bytes of
    Right (decoded :: V.Vector LogEntry) -> mapM_ print decoded
    Left  err                            -> putStrLn err
```

`encodeRecords rows` renders to:

```ndjson
{"ts":"2026-05-13T08:00:00Z","level":"INFO","message":"starting"}
{"ts":"2026-05-13T08:00:01Z","level":"WARN","message":"slow query"}
{"ts":"2026-05-13T08:00:02Z","level":"INFO","message":"ready"}
```

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `NDJSON.Encode`  | `encode :: Vector Aeson.Value -> ByteString` and `encodeRecords :: Aeson.ToJSON a => Vector a -> ByteString` |
| `NDJSON.Decode`  | `decode`, `decodeStream` (one value per callback), `decodeConcurrent` (multi-line in parallel), `decodeRecords` (typed); SIMD newline-scanning via `Wireform.FFI.findByteBS` |
| `NDJSON.Derive`  | `deriveNDJSON` / `deriveToNDJSON` / `deriveFromNDJSON` Template Haskell entry points |

## Encode and decode

NDJSON values themselves are just `aeson` `Value`s, so encode and
decode lean on `aeson`'s instances directly. The package adds the
line framing (and the SIMD scanner that keeps the framing cheap):

```haskell
encode        ::                       Vector Aeson.Value -> ByteString
encodeRecords :: Aeson.ToJSON   a   => Vector a           -> ByteString

decode        ::                       ByteString         -> Either String (Vector Aeson.Value)
decodeRecords :: Aeson.FromJSON a   => ByteString         -> Either String (Vector a)

decodeStream     :: ByteString -> (Aeson.Value -> IO ()) -> IO (Either String ())
decodeConcurrent :: ByteString -> Int -> (Aeson.Value -> IO ()) -> IO (Either String ())
```

`decodeStream` is the right entry point for inputs that don't fit in
memory: it walks the buffer one line at a time and calls back with
each parsed `Aeson.Value`, holding only the current line's allocation.

`decodeConcurrent n` splits the input into `n` worker chunks at line
boundaries and parses them in parallel, then calls back with each
parsed value. Useful for the warm-cache batch-import case where the
input is fully resident and you want to spend the cores you have.

## Annotation-driven deriving

`NDJSON.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md), so
the same annotated record can produce NDJSON, regular JSON, and any
other backend's instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified NDJSON.Derive        as DNDJ
import qualified Wireform.Derive.Aeson as DAeson

data LogEntry = LogEntry
  { ts      :: !Text
  , level   :: !Text
  , message :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type LogEntry ("LogEntry" :: String) #-}

DNDJ.deriveNDJSON   ''LogEntry
DAeson.deriveJSON   ''LogEntry
```

## Performance

The decoder uses `Wireform.FFI.findByteBS` from `wireform-core` to
locate `\n` boundaries in 16-byte SIMD chunks (SSE2, AVX2, or NEON
depending on the host). The aeson-based per-line parse is the
remaining hot path; for ingest pipelines that bottleneck on it,
`decodeConcurrent` parallelises across line boundaries.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-ndjson:wireform-ndjson-derive-test
```

It covers the typeclass instances, the deriver, line-framing edge
cases (trailing newline, missing newline, empty lines, CRLF), and
both the synchronous and concurrent decoder paths.

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Haskell: line-splitting [`aeson`](https://hackage.haskell.org/package/aeson)
  by hand (the obvious baseline), plus
  [`json-stream`](https://hackage.haskell.org/package/json-stream).
- Rust: [`serde_jsonlines`](https://crates.io/crates/serde_jsonlines).
- C: [simdjson](https://github.com/simdjson/simdjson)'s ndjson
  iterator.
- Python: [`orjson`](https://pypi.org/project/orjson/) (the
  fastest-in-class Python JSON parser, used line by line).

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [NDJSON specification](https://github.com/ndjson/ndjson-spec)
- [JSON Lines](https://jsonlines.org/) (the same format under a
  different name)
