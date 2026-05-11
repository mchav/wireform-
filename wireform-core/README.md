# wireform-core

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Shared SWAR / SIMD-accelerated primitives used by every
[`wireform-*`][wireform] format package.  Format-agnostic; the C
sources live in `cbits/` and the vendored [simde][simde] headers in
`include/simde/`.

[wireform]: https://github.com/iand675/wireform-
[simde]: https://github.com/simd-everywhere/simde

## What's in here

| Module                  | Role                                                              |
|-------------------------|-------------------------------------------------------------------|
| `Wireform.FFI`          | C FFI surface (varints, UTF-8 / ASCII / NUL / JSON scanners, …). |
| `Wireform.Encode.Direct`| Shared direct-write encode buffer.                                |
| `Wireform.Hash`         | SIMD-accelerated hashing helpers.                                 |

This package is intentionally tiny; it exists so the per-format
packages can share their hottest C kernels (e.g. `validateUtf8SWAR`,
`countPackedVarints`, `findByte`) without duplicating the
`__attribute__((target(...)))` / `simde-features.h` plumbing.

## License

BSD-3-Clause.  Vendored `simde` headers carry their own MIT license
under `include/simde/simde/COPYING` and friends.
