---
title: wireform-yaml
description: "YAML 1.2 encoding and decoding with TH deriving, anchors, tags, and multi-document streams."
sidebar:
  order: 22
---

`wireform-yaml` implements YAML 1.2 read and write paths for Haskell
applications. Configuration files, CI manifests, and Kubernetes-style multi-doc
streams all map cleanly onto typed records via `Generic`, while the value layer
still exposes anchors, aliases, tags, and both block and flow styles when you
need full YAML expressiveness. The decoder and emitter are validated against the
upstream [yaml-test-suite](https://github.com/yaml/yaml-test-suite) with 100%
conformance.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveYAML` Template Haskell deriver | Derive config types with `wireform-derive` annotations; Generic defaults work for simple cases |
| Block and flow styles | Human-readable block output; compact flow for inline structures |
| Anchors and aliases | Preserve shared references and cyclic graphs in the value layer |
| Tags | Explicit scalar typing (`!!int`, application-specific tags) |
| Literal and folded scalars | Round-trip multiline strings (`\|` and `>`) |
| Multi-document streams | `---` separated documents for kubectl-style files |
| YAML 1.2 core schema | Plain scalars that look like bools or numbers stay quoted on encode |

## Basic usage

### Typed records

Derive codecs with the Template Haskell deriver and use `encodeYAML` /
`decodeYAML` for the common case.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import YAML.Class (ToYAML, FromYAML, encodeYAML, decodeYAML)
import YAML.Derive (deriveYAML)

data Server = Server
  { host :: !Text
  , port :: !Int
  , tls  :: !Bool
  } deriving stock (Generic)

$(deriveYAML ''Server)

loadConfig :: Text -> Either String Server
loadConfig = decodeYAML
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToYAML Server` and
`instance FromYAML Server` declarations.

Field naming follows the same `Wireform.Derive` annotation vocabulary as other
wireform formats (`rename`, `omitEmpty`, and friends via `YAML.Derive`).

### Value-level API for anchors and tags

When the shape is dynamic or you must preserve YAML-specific features, work in
`YAML.Value` and encode with `YAML.Encode`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified YAML.Decode as YD
import qualified YAML.Encode as YE

roundtripAnchors :: Text -> Either String Text
roundtripAnchors doc = do
  val <- YD.decode doc
  pure (YE.encode val)
```

### Multi-document streams

Kubernetes and other tools emit several YAML documents in one file. Decode the
stream as a `YAML.Value.Stream`, or encode many values with `encodeStream`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import YAML.Encode (encodeStream)
import YAML.Value (Document(..), Stream(..), mapping, string)

writeStream :: Stream -> Text
writeStream = encodeStream

exampleStream :: Stream
exampleStream =
  Stream $
    V.fromList
      [ Document True False (mapping [(string "apiVersion", string "v1")])
      , Document True False (mapping [(string "kind", string "ConfigMap")])
      ]
```

The emitter chooses block style by default and falls back to flow for empty
containers. Output is round-trippable: `encode` followed by `decode` recovers
the same `Value`.

## Performance

wireform-yaml is a pure Haskell parser that consistently outperforms the C
libyaml bindings by 3-32x across all input sizes, making it one of the fastest
YAML parsers in any language. Against HsYAML (the other pure Haskell option), it
is 244-408x faster.

### wireform-yaml vs libyaml (C bindings via Hackage `yaml` package)

| Input | wireform-yaml | yaml (libyaml) | Speedup |
|-------|--------------|----------------|---------|
| tiny | 0.25 µs | 8.01 µs | 32x |
| small | 3.80 µs | 40.2 µs | 11x |
| flow | 4.02 µs | 60.5 µs | 15x |
| literal | 3.97 µs | 13.2 µs | 3.3x |
| big | 14.9 µs | 139 µs | 9.4x |

### wireform-yaml vs HsYAML (pure Haskell)

| Input | wireform-yaml | HsYAML | Speedup |
|-------|--------------|--------|---------|
| tiny | 0.25 µs | 102 µs | 408x |
| small | 3.80 µs | 1002 µs | 264x |
| flow | 4.02 µs | 1242 µs | 309x |
| literal | 3.97 µs | 1167 µs | 294x |
| big | 14.9 µs | 3627 µs | 244x |

Criterion, GHC 9.8.4, Apple Silicon. See `wireform-yaml/bench-results/` for raw data.

## Notable modules

| Module | Role |
|--------|------|
| `YAML.Class` | `ToYAML` / `FromYAML`, `encodeYAML`, `decodeYAML` |
| `YAML.Value` | AST for mappings, sequences, scalars, anchors, and streams |
| `YAML.Encode` | Block and flow emitter with YAML 1.2 quoting rules |
| `YAML.Decode` | Parser for documents and multi-doc streams |
| `YAML.Derive` | Template Haskell deriver wired to `Wireform.Derive` annotations |
| `YAML.JSON` | Bridge between YAML values and JSON |
