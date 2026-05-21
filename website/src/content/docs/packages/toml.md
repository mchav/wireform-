---
title: wireform-toml
description: "TOML 1.0 and 1.1 encoding and decoding with TH deriving, section-aware pretty printing, and datetime support."
sidebar:
  order: 23
---

`wireform-toml` reads and writes TOML configuration for Haskell services and
tools. TOML's table sections, inline tables, and array-of-tables map naturally
onto Haskell records when you derive `Generic`, while the encoder places
`[section]` headers and `[parent.child]` paths correctly on output. The parser
is validated against the upstream [toml-test](https://github.com/toml-lang/toml-test)
suite for both TOML 1.0 and 1.1.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveTOML` Template Haskell deriver | Load config files into typed records with `wireform-derive` annotations; Generic defaults work for simple cases |
| Section-aware pretty printing | `[database]` and nested `[database.pool]` headers land in sensible order |
| Datetime support | RFC 3339 offsets and local datetimes as first-class values |
| Inline and standard tables | Compact inline `{ key = "val" }` or full `[table]` blocks |
| Array-of-tables | `[[items]]` repeated sections for list-of-struct configs |
| toml-test conformance | Confidence that edge cases match the spec |

## Basic usage

### Typed configuration

Derive codecs with the Template Haskell deriver and round-trip with
`encodeTOML` / `decodeTOML`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import TOML.Class (ToTOML, FromTOML, encodeTOML, decodeTOML)
import TOML.Derive (deriveTOML)

data Database = Database
  { dbHost :: !Text
  , dbPort :: !Int
  } deriving stock (Generic)

data AppConfig = AppConfig
  { appName  :: !Text
  , database :: !Database
  } deriving stock (Generic)

$(deriveTOML ''Database)
$(deriveTOML ''AppConfig)

loadConfig :: Text -> Either String AppConfig
loadConfig = decodeTOML
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToTOML Database` and
`instance FromTOML Database` declarations (and likewise for `AppConfig`).

Nested records become TOML subtables. The encoder emits a `[database]` section
with keys under it rather than flattening everything at the top level.

### Datetimes

TOML datetimes live in `TOML.Value` as `TLocalDateTime`, `TOffsetDateTime`, and
related constructors. Deriving handles them when fields use the corresponding
Haskell types from the value module.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import TOML.Class (FromTOML, decodeTOML)
import TOML.Derive (deriveTOML)

data Job = Job
  { jobName     :: !Text
  , scheduledAt :: !Text
  } deriving stock (Generic)

$(deriveTOML ''Job)

parseJob :: Text -> Either String Job
parseJob = decodeTOML
```

TOML datetimes decode as `Text` in the value layer (`TDateTime`, `TDate`,
`TTime`). Map them into `time` types in application code when you need calendar
arithmetic.

### Direct encoding for large configs

`encodeTOMLDirect` routes through `toEncoding` when you want the same path the
TH deriver uses, which can avoid an extra conversion for complex nested values.

```haskell
import TOML.Class (ToTOML, encodeTOMLDirect)

writeConfig :: ToTOML a => a -> Text
writeConfig = encodeTOMLDirect
```

Use `TOML.Decode.decode` on raw text when you need the untyped `TOML.Value`
AST before mapping into application types.

## Notable modules

| Module | Role |
|--------|------|
| `TOML.Class` | `ToTOML` / `FromTOML`, `encodeTOML`, `decodeTOML` |
| `TOML.Value` | AST for tables, arrays, datetimes, and inline tables |
| `TOML.Encode` | Section-aware TOML writer |
| `TOML.Decode` | Parser for TOML 1.0 / 1.1 documents |
| `TOML.Encoding` | Intermediate encoding type used by the deriver |
| `TOML.Derive` | Template Haskell deriver with `Wireform.Derive` annotations |
