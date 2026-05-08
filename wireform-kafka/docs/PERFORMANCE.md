# wireform-kafka performance notes

## How to bench

```bash
# Set up a local broker (KRaft mode, single node):
cd /tmp && curl -sL https://dlcdn.apache.org/kafka/3.9.2/kafka_2.13-3.9.2.tgz | tar xz
cd kafka_2.13-3.9.2
KAFKA_CLUSTER_ID=$(bin/kafka-storage.sh random-uuid)
bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties
KAFKA_HEAP_OPTS="-Xmx1G -Xms512M" bin/kafka-server-start.sh config/kraft/server.properties

# Run unit + integration:
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration

# Run microbenches (see bench/Benchmarks/SerLib.hs for a head-to-head
# of the serialization library options):
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix Serialization"
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix SerLib"
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix CRC32C"
```

The library is built at `-O2` (see `cabal.project` `package wireform-kafka`)
so the `INLINE` / `SPECIALISE` pragmas in `Kafka.Protocol.Primitives`
and `Kafka.Protocol.Encoding` actually fire. Without the `-O2` package
opt-in the workspace-wide `optimization: 1` masks the wins.

## CRC32C

`Kafka.Protocol.CRC32C` calls `simde_mm_crc32_u8/u16/u32/u64` (see
`cbits/crc32c.c`). With `-O2 -march=native` the SIMDe layer compiles
straight to native CRC32 instructions on x86_64 (SSE4.2) and
AArch64 (the optional `+crc` extension), and falls back to a portable
C reference everywhere else.

Measured on Sapphire Rapids, 1 ms time limit per bench:

| Input  | Hardware        | Naive        | Speedup |
|--------|-----------------|--------------|---------|
| 16 B   |   71 ns         |  103 ns      |   1.5×  |
| 64 B   |   74 ns         |  410 ns      |   5.6×  |
| 256 B  |   80 ns         |  1.7 µs      |  21×    |
| 1 KB   |  134 ns         |  6.7 µs      |  50×    |
| 16 KB  |  1.6 µs         |  424 µs      | 264×    |
| 64 KB  |  6.3 µs         |  1.78 ms     | 283×    |
| 1 MB   |   99 µs         |  28.9 ms     | 292×    |

That is ~10 GiB/s on a 1 MiB chunk, which puts us at the throughput
ceiling of the hardware CRC32C instruction. There is no headroom to
chase here.

## Serialisation

The protocol message encoders bottom out on a tight loop that
emits a 4-byte length, then per element { Int32, Int32, length-prefixed
ByteString }. The `Serialization/*` benchmarks measure the full
`ProduceRequest` / `FetchRequest` / `MetadataRequest` encoders;
`SerLib/*` is a controlled head-to-head of that *same loop shape* across
four serialisation libraries:

| Stack                         | 1000-element loop | vs current |
|-------------------------------|-------------------|------------|
| `bytes` + `Serial` typeclass  | 95.98 µs          | 1.00× (current production path) |
| `cereal` direct (no typeclass)| 64.50 µs          | 1.49×      |
| `binary` direct               | 63.38 µs          | 1.51×      |
| `Data.ByteString.Builder`     | 43.47 µs          | **2.21×**  |

Two readings:

1. **~33% of current encode time is the `Serial`-typeclass dictionary
   indirection.** The `INLINE` pragmas in `Kafka.Protocol.Primitives`
   help when the call site is monomorphic, but generated code passes
   the `MonadPut m` / `Serial a` constraints around so much that GHC
   often can't specialise. Eliminating the typeclass and calling
   primitives directly recovers that 33% wholesale.

2. **Beyond that, switching the underlying buffer-builder library
   from cereal to `Data.ByteString.Builder` recovers another ~33%
   on top.** `Builder` has had years of focused tuning (chunked output,
   bounded writes) that cereal's `PutM` doesn't — at the cost of a
   slightly less convenient API surface.

Total realistic headroom on encode: **~2.2× (95.98 µs → 43.47 µs)** with
no other architectural change. That is what librdkafka-style raw
buffer writes look like in idiomatic Haskell.

### Plan to land it

The 197 modules under `Kafka.Protocol.Generated.*` are emitted by the
`kafka-codegen` executable from upstream JSON. Switching to a
`Builder`-based encoder is therefore a *codegen* change, not a 197-file
hand-edit:

1. Define a thin `KafkaPut` type in `Kafka.Protocol.Encoding` that is
   either a newtype around `Builder` (encode side) or a CPS unboxed-sum
   `Decoder` (decode side, similar to `Proto.Wire.Decode.Result#`).
2. Add `kafkaPut*` primitives for the fixed-width types and length-
   prefixed strings/bytes/arrays.
3. Update `codegen/Kafka/Protocol/Codegen/Generator.hs` to emit calls
   to the new `kafkaPut*` instead of `serialize`. The shape is the
   same — each `serialize x` becomes `kafkaPutT x` with a known type.
4. Regenerate the 197 modules.
5. Keep the old `Serial` instances on the primitive types so
   non-codegen callers (and the existing test suite) keep working;
   only the codegen output flips.

Property tests (`Protocol.Generated.SimpleRoundTripSpec`,
`RoundTripSpec`, `RecordBatchSpec`) and the broker-gated integration
suite both already cover end-to-end correctness, so the swap is
testable.

## Decode

`SerLib/*` only measures encode. The `Serialization/*` decoder
benchmarks (using `Get` from cereal under the `bytes` facade) show
that decode is consistently ~1.5× slower than encode on the same
message. The shape of the win on the decode side will look similar
once we switch to a hand-tuned `Decoder` (CPS over an unboxed sum
result, the same thing `wireform-proto` uses in
`Proto.Wire.Decode.Result`). I did not chase this in the current
iteration.

## Per-record hot path

The producer's actual hot path is `RecordBatch.encodeRecordBatch`
(per request) plus `encodePartitionProduceData` (per partition in a
ProduceRequest). The latter calls into the per-record varint encoder
for offsetDelta / timestampDelta / keyLen / valueLen, then writes the
header / key / value bytes. Today that path inherits the same `Serial`
overhead the `SerLib` head-to-head measured. Numbers should improve
proportionally once we switch encoders.

The compression layer is FFI to the system `gzip` / `snappy` (fixed
in this branch) / `lz4` / `zstd` libraries; that is already at native
C speeds and is not on the critical path for what we can move with
Haskell-side changes.

## What is *not* worth doing

- **Hand-rolling `unsafeCreate` + raw `Ptr Word8` arithmetic.** Builder
  is within ~10% of that according to the published benchmarks for
  `bytestring`, and the maintenance cost is high.
- **Chasing CRC32C improvements.** We are already at the SSE4.2
  ceiling; folding more bytes per cycle requires AVX-512 + VPCLMULQDQ
  and is a ~2× win at best, vs. the >2× win available from the encoder
  swap.
