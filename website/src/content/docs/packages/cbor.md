---
title: wireform-cbor
description: "RFC 8949 CBOR encoding and decoding with TH deriving, CDDL codegen, diagnostic notation, and deterministic encoding."
sidebar:
  order: 10
---

`wireform-cbor` implements Concise Binary Object Representation (CBOR) per
RFC 8949. CBOR is a compact, self-describing binary format used in IoT
protocols, COSE/JOSE, WebAuthn, and many other standards. Use this package
when you need a schema-flexible binary codec with strong tooling for
debugging, schema definition, and cross-language interoperability.

## Key features

- **Template Haskell deriving** via `deriveCBOR` for records, enums, and sum
  types, with `wireform-derive` annotations; Generic defaults (empty instances)
  work for simple cases
- **Streaming decode** for framed or concatenated CBOR values without loading
  the entire input into memory
- **CDDL schema language** (RFC 8610) with a parser and Haskell code generator
- **Diagnostic notation** for human-readable debug output (RFC 8949 Section 8)
- **JSON bridge** for converting between CBOR and Aeson `Value`
- **Deterministic encoding** per RFC 8949 Section 4.2 for canonical byte
  sequences suitable for hashing and signing
- **Tag registry** for application-specific CBOR tags

## Basic usage

Derive instances with the Template Haskell deriver, then encode and decode in
one call:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import CBOR.Class (ToCBOR, FromCBOR, encodeCBOR, decodeCBOR)
import CBOR.Derive (deriveCBOR)
import GHC.Generics (Generic)
import Data.Text (Text)

data Config = Config
  { cfgHost :: !Text
  , cfgPort :: !Int
  }
  deriving stock (Show, Eq, Generic)

$(deriveCBOR ''Config)

save :: Config -> IO ()
save cfg = do
  let bytes = encodeCBOR cfg
  writeFileBinary "config.cbor" bytes

load :: IO (Either String Config)
load = do
  bytes <- readFileBinary "config.cbor"
  pure (decodeCBOR bytes)
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToCBOR Config` and
`instance FromCBOR Config` declarations.

For signed payloads or content-addressed storage, use deterministic encoding
so the same value always produces the same bytes:

```haskell
import CBOR.Encode (encodeDeterministic)
import CBOR.Value qualified as CV
import CBOR.Class (toCBOR)

canonicalBytes :: Config -> ByteString
canonicalBytes cfg = encodeDeterministic (toCBOR cfg)
```

When debugging wire format issues, render values as diagnostic notation:

```haskell
import CBOR.Diagnostic (toDiagnostic)
import CBOR.Class (toCBOR)

debugConfig :: Config -> Text
debugConfig cfg = toDiagnostic (toCBOR cfg)
```

For streams of CBOR items (logs, multiplexed channels), decode one value at
a time and keep the leftover bytes:

```haskell
import CBOR.Stream (decodeOneWithLeftover)

decodeStream :: ByteString -> [(Either String CV.Value, ByteString)]
decodeStream bs = go bs
  where
    go rest
      | BS.null rest = []
      | otherwise =
          case decodeOneWithLeftover rest of
            Left err -> [(Left err, BS.empty)]
            Right (val, leftover) -> (Right val, leftover) : go leftover
```

## Performance

### wireform-cbor vs cborg

| Operation | wireform-cbor | cborg | Winner |
|-----------|--------------|-------|--------|
| encode | 305 ns | 275 ns | cborg (1.1x) |
| decode | 460 ns | 1214 ns | wireform (2.6x) |

Encode performance is roughly even with cborg (the established Haskell CBOR library). Decode is 2.6x faster due to wireform's unboxed-sum decoder architecture.

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-cbor/bench-results/` for raw data.

## Notable modules

| Module | Purpose |
|--------|---------|
| `CBOR.Class` | `ToCBOR` / `FromCBOR` typeclasses, `encodeCBOR`, `decodeCBOR` |
| `CBOR.Encode` / `CBOR.Decode` | Low-level wire primitives and `encodeDeterministic` |
| `CBOR.Value` | Dynamic untyped `Value` ADT for schema-less processing |
| `CBOR.Diagnostic` | Diagnostic notation rendering and parsing |
| `CBOR.CDDL` / `CBOR.CDDLCodeGen` | CDDL parser and Haskell stub generator |
| `CBOR.JSON` | CBOR ↔ JSON conversion |
| `CBOR.Stream` | Incremental decode for framed input |
| `CBOR.TagRegistry` | Application tag registration and lookup |
| `CBOR.Derive` | Template Haskell deriver with `wireform-derive` annotations |

## Conformance

Deterministic encoding follows RFC 8949 Section 4.2: shortest integer forms,
definite-length containers, and canonical map key ordering. Use
`encodeDeterministic` when byte-for-byte reproducibility matters for signatures
or content hashes.
