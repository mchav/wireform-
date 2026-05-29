# wireform-cel

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

A conformant Haskell implementation of Google's
[Common Expression Language (CEL)](https://github.com/google/cel-spec/blob/master/doc/langdef.md).
CEL is a non-Turing-complete, side-effect-free, strongly- and
dynamically-typed expression language used for policy, validation, and
filtering (IAM conditions, Envoy/xDS, protobuf field validation, …).

This package provides the lexer, recursive-descent parser, the dynamic
runtime [`Value`](src/CEL/Value.hs) model, and an evaluator with the full
standard library of operators, conversions, functions, and comprehension
macros.

Unlike most other packages in the
[wireform](https://github.com/iand675/wireform-) monorepo, `wireform-cel`
depends only on Hackage libraries, so it builds standalone.

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import CEL

main :: IO ()
main = do
  -- Self-contained expressions:
  print (run emptyEnv "1 + 2 * 3")               -- Right (VInt 7)
  print (run emptyEnv "[1, 2, 3].map(x, x * x)") -- Right (VList [VInt 1,VInt 4,VInt 9])
  print (run emptyEnv "'foobar'.matches('o+b')") -- Right (VBool True)

  -- Bind variables into the environment:
  let env = bindAll [("user", VMap (celMapFromList [("age", VInt 30)]))] emptyEnv
  print (run env "user.age >= 18")               -- Right (VBool True)
```

`compile` parses to an `Expr` you can evaluate repeatedly with `evaluate`;
`run` is the parse-and-evaluate convenience. Errors are returned as
`Left CelError`.

## What's supported

- **Syntax**: the complete grammar — ternary `?:`, `||`/`&&`, relations and
  `in`, arithmetic, unary `-`/`!`, member selection / indexing / calls, list
  and map literals, and `Name{...}` message-literal syntax. Full literal
  lexis: decimal/hex integers, `u` unsigned suffix, floats, single/double and
  triple-quoted strings, raw (`r"..."`) strings, bytes (`b"..."`) literals,
  and the entire escape-sequence set with surrogate/range validation.
- **Values**: `int` (64-bit signed), `uint` (64-bit unsigned), `double`,
  `bool`, `string`, `bytes`, `null`, `list`, `map`, `type`, and the abstract
  `google.protobuf.Timestamp` / `google.protobuf.Duration`.
- **Semantics**: the numeric number-line model (`1 == 1u == 1.0`,
  cross-type ordering), `NaN` that is never equal and always unordered,
  heterogeneous equality, overflow-checked arithmetic, and the commutative,
  error-absorbing `&&`/`||` operators.
- **Macros**: `has`, `all`, `exists`, `exists_one`, `map` (3- and 4-argument),
  and `filter`, with comprehension scoping.
- **Standard library**: `size`, `type`, `dyn`, the conversions
  (`int`/`uint`/`double`/`string`/`bool`/`bytes`/`duration`/`timestamp`), the
  string functions (`contains`, `startsWith`, `endsWith`, `matches`), and the
  timestamp/duration accessors (`getFullYear`, `getMonth`, `getDate`,
  `getHours`, `getMinutes`, `getSeconds`, `getMilliseconds`, `getDayOfWeek`,
  `getDayOfYear`, …).

## Not yet supported

- Protocol-buffer **message** values (construction and field access). The
  well-known abstract types `Timestamp` and `Duration` are supported.
- Named IANA/Joda **timezones** in date/time accessors (`UTC` and fixed
  `±HH:MM` offsets work).
- The optional static **type-checking** phase. CEL is dynamically typed;
  evaluation does not depend on it.

## Building and testing

```
cabal build wireform-cel
cabal test  wireform-cel
```

The default test suite (`test/`) has two parts: `Test.CEL.Conformance`
(example-based tests taken directly from the worked examples in the language
definition) and `Test.CEL.Properties` (Hedgehog properties for arithmetic,
ordering, and `size`).

### Upstream conformance suite

A second, opt-in test suite (`wireform-cel-conformance`) runs the official
[`cel-spec`](https://github.com/google/cel-spec) `tests/simple/testdata/*.textproto`
suite (matching the monorepo's `TOML_TEST_SUITE` / `YAML_TEST_SUITE` pattern).
Point `CEL_SPEC_DIR` at a checkout and run it:

```
git clone https://github.com/google/cel-spec
CEL_SPEC_DIR=$PWD/cel-spec cabal test wireform-cel:wireform-cel-conformance
```

It parses the textproto suites, evaluates each case, and compares the result
against the expected `cel.expr.Value` (or expected error). Tests that need
features this library does not implement — protocol-buffer message values and
the CEL extension libraries (`string_ext`, `math_ext`, optionals, …) — are
reported as *skipped*, not failed.

Current result over the core language files (everything except the extension
libraries, the protobuf-enum file, the type-deduction/unknown-tracking files):

```
TOTAL  pass=1124  skip=128  fail=0
```

All 128 skips are protocol-buffer-message cases (construction, field access,
wrapper/`Any`/`Struct` conversions) — the one documented gap below.
