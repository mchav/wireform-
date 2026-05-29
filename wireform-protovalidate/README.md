# wireform-protovalidate

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[protovalidate](https://protovalidate.com/) for the wireform Protocol Buffers
stack: protobuf message validation driven by CEL. It is the companion to
[`wireform-proto`](../wireform-proto) and is built on
[`wireform-cel`](../wireform-cel).

protovalidate expresses validation rules as CEL — both the standard
annotations and arbitrary custom logic:

```proto
message User {
  string id         = 1 [(buf.validate.field).string.uuid = true];
  uint32 age        = 2 [(buf.validate.field).uint32.lte = 150];
  string email      = 3 [(buf.validate.field).string.email = true];
  string first_name = 4 [(buf.validate.field).string.max_len = 64];

  option (buf.validate.message).cel = {
    id: "first_name_requires_last_name"
    message: "last_name must be present if first_name is present"
    expression: "!has(this.first_name) || has(this.last_name)"
  };
}
```

This package provides the pieces needed to enforce those rules in Haskell:

- **`Protovalidate.Library`** — the protovalidate CEL extension library
  registered onto a `CEL.Env`: `isEmail`, `isHostname`, `isHostAndPort`,
  `isIp`/`isIpPrefix`, `isUri`/`isUriRef`, `isNan`/`isInf`, and `unique`.
- **`Protovalidate.Format`** — the underlying pure `Text -> Bool` predicates
  (RFC-1034 hostnames, RFC-5321 mailboxes, IPv4/IPv6 + CIDR, host:port,
  RFC-3986 URIs).
- **`Protovalidate.Rules`** — the standard rules expressed as CEL (exactly as
  reference protovalidate does: each rule is a CEL expression over `this` and
  `rules`), plus a builder vocabulary.
- **`Protovalidate.Eval`** — the engine: bind each field value to `this` and
  its rule message to `rules`, evaluate the applicable standard + custom
  constraints, and collect `Violation`s (including nested-message and
  repeated-element paths).
- **`Protovalidate.Proto`** — a bridge that turns a `wireform-proto`
  `DynamicMessage` into the CEL value the engine consumes, given a small
  field schema.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Protovalidate
import CEL (Value (..), celMapFromList)

userRules :: MessageRules
userRules = messageRules
  [ ("id",         fieldRules KString [uuid])
  , ("age",        fieldRules KUint32 [lteV (VUInt 150)])
  , ("email",      fieldRules KString [email])
  , ("first_name", fieldRules KString [maxLen 64])
  ]
  [ either (error . show) id $
      mkConstraint "first_name_requires_last_name"
                   "last_name must be present if first_name is present"
                   "!has(this.first_name) || has(this.last_name)"
  ]

main :: IO ()
main = do
  let user = VMap (celMapFromList
        [ (VString "id",    VString "not-a-uuid")
        , (VString "age",   VUInt 200)
        , (VString "email", VString "alice@example.com")
        ])
  mapM_ print (validate user userRules)
  -- Violation {fieldPath = "id",  constraintId = "string.uuid", ...}
  -- Violation {fieldPath = "age", constraintId = "uint32.lte",  ...}
```

A message is represented as a CEL map from field name to value. `validate`
uses the standard protovalidate CEL environment (base CEL plus the extension
library); `validateIn` lets you supply your own base environment.

## Scope

This package implements the CEL-driven core of protovalidate: the extension
function library, the standard rules as CEL, and the violation-collecting
engine, with custom field- and message-level CEL fully supported.

Not yet implemented:

- Reading `buf.validate` options directly from compiled protobuf descriptors
  (the `FieldOptions`/`MessageOptions` extension #1159). `wireform-proto`'s
  descriptor subset drops options today; until that lands, construct
  `MessageRules` programmatically (or from the parsed `.proto` AST) and bridge
  message values with `Protovalidate.Proto`.
- A handful of less-common standard rules (e.g. bytes `prefix`/`suffix`,
  `well_known_regex`) and the full set of `ignore` modes; the common rules
  across string / numeric / bool / bytes / repeated / map / duration /
  timestamp are covered.

## Building and testing

```
cabal build wireform-protovalidate
cabal test  wireform-protovalidate
```

`Test.Protovalidate.Format` exercises the format predicates;
`Test.Protovalidate.Validation` runs end-to-end validation (standard rules,
custom CEL, message-level CEL, nested messages, repeated fields).
