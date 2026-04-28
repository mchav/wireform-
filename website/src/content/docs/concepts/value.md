---
title: The Value type
description: A schema-tagged value used as the lingua franca for cross-format conversion.
sidebar:
  order: 1
---

Every schema-aware wireform format exposes a `Value` type. They share the
same shape (`VBool`, `VInt`, `VBytes`, `VText`, `VList`, `VRecord`, plus
format-specific extensions) so converting between formats at the value level
is mostly map-and-rebuild.

```haskell
import qualified Wireform.MsgPack.Value as MP
import qualified Wireform.CBOR.Value as CBOR

mpToCbor :: MP.Value -> CBOR.Value
mpToCbor = \case
  MP.VBool b   -> CBOR.VBool b
  MP.VInt n    -> CBOR.VInt n
  MP.VBytes bs -> CBOR.VBytes bs
  MP.VText t   -> CBOR.VText t
  MP.VList xs  -> CBOR.VList (fmap mpToCbor xs)
  MP.VMap kvs  -> CBOR.VMap (fmap (\(k, v) -> (mpToCbor k, mpToCbor v)) kvs)
```

For schema-backed formats (`Proto`, `Avro`, `Thrift`), `Value` carries enough
information to round-trip without the schema, but the schema is needed to
emit the correct field tags / wire types when re-encoding.
