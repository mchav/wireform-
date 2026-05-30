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

## Reading rules from annotations

Instead of writing `MessageRules` by hand, read them straight from a
`buf.validate`-annotated `.proto` (via `wireform-proto`'s IDL parser):

```haskell
case parseProtoRules protoSource of
  Right rulesByMessage ->
    let Just userRules = lookup "User" rulesByMessage
     in validate userMsg userRules
  Left err -> error (show err)
```

`parseProtoRules` understands the scalar/numeric/bool/bytes/enum/duration/
timestamp rules, `repeated` (incl. `repeated.items.*`) and `map` rules,
`required` / `ignore`, field- and message-level `cel`, and nested-message
validation. (`fileMessageRules` / `extractMessageRules` work on an
already-parsed `ProtoFile` / `MessageDef`.)

### …or from a compiled descriptor

protovalidate stores its rules as option *extensions* — extension #1159 on
`google.protobuf.FieldOptions` / `MessageOptions`. Because `wireform-proto`'s
`descriptor.proto` now preserves unknown fields, those extension bytes survive
decoding, so rules can be read straight from a `FileDescriptorProto` (e.g. a
`protoc`-produced `FileDescriptorSet`):

```haskell
case messageRulesFromDescriptor fileDescriptorProto "acme.user.v1.User" of
  Right userRules -> validate userMsg userRules
  Left err        -> error (show err)
```

`fileRulesFromDescriptor` returns rules for every message in the file. Custom
`cel`, `required`, and `ignore` are always read; the standard rule sets are
mapped for the common kinds (string / numeric / bool / bytes / repeated / map)
using the buf.validate v1 field numbers.

## Validating typed messages (no dynamic round trip)

`compileValidator` compiles a `MessageRules` once — the CEL expressions and
base environment are captured up front — and the resulting `Validator` can be
reused across many messages. `ToCel` converts a typed Haskell record directly
into CEL, so validation never decodes to a schemaless `DynamicMessage`:

```haskell
data User = User { id :: Text, age :: Word32, email :: Text }
  deriving stock (Generic) deriving anyclass (ToCel)

userValidator :: Validator
userValidator = compileValidator userRules

check :: User -> [Violation]
check = validateValue userValidator
```

`Protovalidate.Proto.dynamicMessageToCel` remains available for when you only
have a schemaless `wireform-proto` `DynamicMessage`.

## Refinement types

`Protovalidate.Refined` reifies rules as
[`refined`](https://hackage.haskell.org/package/refined) refinement types, so a
field's constraints can show up in its type. `refinedFieldType` turns a
`FieldRules` into the type expression a code generator would emit:

```haskell
refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
-- Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
```

Two flavors of predicate are produced:

- **Native predicates** for length/count/comparison rules, via the aliases
  `MinLen`, `MaxLen`, `LenEq`, `Gt`, `Gte`, `Lt`, `Lte`, `ConstEq` (type-level
  naturals). These are ordinary `refined` predicates:

  ```haskell
  refine "abc" :: Either RefineException (Refined (MinLen 3) Text)  -- Right
  ```

- **CEL-backed predicates** for everything else — the well-known string
  formats, regex patterns, and arbitrary `(buf.validate.field).cel`
  expressions — via the `Cel` predicate, which carries the CEL source at the
  type level and runs it at validation time. This is how **custom predicates
  also become refinement types**:

  ```haskell
  refinedFieldType (fieldRules KString [email])
  -- Just "Refined (Cel \"this.isEmail()\") Text"

  refine "a@b.com" :: Either RefineException (Refined (Cel "this.isEmail()") Text)  -- Right
  ```

  `CelWith tag expr` (with a `CelEnvironment tag` instance) runs in a
  caller-supplied environment, so custom CEL *functions* can back a refinement
  predicate too.

## Scope

This package implements the CEL-driven core of protovalidate: the extension
function library, the standard rules as CEL, the violation-collecting engine
(with custom field- and message-level CEL), annotation extraction from `.proto`
sources, and a compile-once typed validation path.

Not yet implemented:

- A handful of less-common standard rules (e.g. bytes `prefix`/`suffix`,
  `well_known_regex`, duration/timestamp literal bounds) and the full `ignore`
  matrix; the common rules across string / numeric / bool / bytes / repeated /
  map are covered, from both `.proto` annotations and compiled descriptors.

## Building and testing

```
cabal build wireform-protovalidate
cabal test  wireform-protovalidate
```

`Test.Protovalidate.Format` exercises the format predicates;
`Test.Protovalidate.Validation` runs end-to-end validation (standard rules,
custom CEL, message-level CEL, nested messages, repeated fields).
