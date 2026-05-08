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

| Stack                                   | 10 elem | 100 elem | 1000 elem | vs current |
|-----------------------------------------|--------:|---------:|----------:|-----------:|
| `bytes` + `Serial` typeclass (current)  |  1.39 µs |  10.95 µs |  100.15 µs | 1.00× |
| `cereal` direct (no typeclass)          |  1.03 µs |   6.86 µs |   64.54 µs | 1.55× |
| `binary` direct                         |  1.03 µs |   8.11 µs |   64.86 µs | 1.54× |
| `Data.ByteString.Builder`               |  0.93 µs |   5.72 µs |   43.06 µs | 2.33× |
| **`unsafeCreate` + `pokeByteOff`**      | **0.28 µs** | **2.59 µs** | **27.60 µs** | **3.63×** |

Three readings:

1. **~35% of current encode time is the `Serial`-typeclass dictionary
   indirection.** The `INLINE` pragmas in `Kafka.Protocol.Primitives`
   help when the call site is monomorphic, but generated code passes
   the `MonadPut m` / `Serial a` constraints around so much that GHC
   often can't specialise. Calling primitives directly recovers that
   wholesale.

2. **Switching the underlying buffer-builder library from cereal to
   `Data.ByteString.Builder` recovers another ~33% on top.**
   `Builder` has had years of focused tuning (chunked output, bounded
   writes) that cereal's `PutM` doesn't.

3. **Pre-sizing the output and writing into a `Ptr Word8` with
   `pokeByteOff` + `memcpy` recovers another ~36% on top of Builder.**
   This is what every Kafka request body actually wants — every
   variable-length field is a length-prefix + raw bytes, so the size
   *is* knowable up front, and Builder's chunked-buffer bookkeeping
   becomes pure overhead. With `BSI.unsafeCreate (sizeOf msg) $ \p ->
   ...` we get one allocation, one pass, no thunks. Counts at 1000
   elements drop from ~17 GC bytes/iter (Builder) to ~9 GC bytes/iter
   (poke), which is just the output ByteString itself.

The size-then-poke approach is exactly what the Java client and
librdkafka do internally; it is the realistic ceiling for what the
encoder side can do without dropping into C, and the gap between
"poke" and "Builder" is wider than the gap between "Builder" and
"current". This is where the focus should be.

Total realistic headroom on encode: **~3.6× (100 µs → 28 µs at 1000
elements)** with no other architectural change.

### Plan to land it

The 197 modules under `Kafka.Protocol.Generated.*` are emitted by the
`kafka-codegen` executable from upstream JSON. Switching encoders is
therefore a *codegen* change, not a 197-file hand-edit:

1. Define two new typeclasses in `Kafka.Protocol.Encoding`:

    * `KafkaSize`  — `kafkaSize :: ApiVersion -> a -> Int` so we can
      pre-compute the buffer size for any message tree.
    * `KafkaPoke`  — `kafkaPoke :: ApiVersion -> a -> Ptr Word8 ->
      Int -> IO Int` (returns the new offset). Implementations are
      direct `pokeByteOff` writes, plus a `memcpy` for byte arrays.

2. Add `kafkaPokeInt32BE` / `kafkaPokeKafkaString` / etc. helpers in
   `Kafka.Protocol.Primitives.Poke`.

3. Update `codegen/Kafka/Protocol/Codegen/Generator.hs` to emit
   `KafkaSize` / `KafkaPoke` instances alongside (or instead of) the
   current `Serial`-style functions. The shape is the same — each
   `serialize x` becomes one `pokeByteOff` write at a known offset
   computed from the size pass.

4. Top-level entry point becomes:

   ```haskell
   encodeProduceRequest version msg =
     let !sz = kafkaSize version msg
     in BSI.unsafeCreate sz $ \p -> do
          _ <- kafkaPoke version msg p 0
          pure ()
   ```

5. Regenerate the 197 modules.

6. Keep the old `Serial` instances on the primitive types so
   non-codegen callers (and the existing test suite) keep working;
   only the codegen output flips. After the cutover those instances
   can be dropped if nothing else uses them.

Property tests (`Protocol.Generated.SimpleRoundTripSpec`,
`RoundTripSpec`, `RecordBatchSpec`) and the broker-gated integration
suite both already cover end-to-end correctness, so the swap is
testable.

### Why not `store` / `flat` / `persist`?

I considered but rejected each:

* **`store`** — fastest existing library but assumes fixed-size types
  with a `Storable` instance. Variable-length fields (every Kafka
  string and array) need a custom `Peek` / `Poke` anyway, at which
  point we are already doing what the size-then-poke plan does without
  the type-class machinery overhead.
* **`flat`** — bit-packed; optimised for compact output, not encode
  speed. Wrong tradeoff for a wire protocol where the layout is fixed
  by Kafka.
* **`persist`** — strict `cereal` cousin. Does not give us anything
  over the `cereal` direct row already in the table above.

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

- **Chasing CRC32C improvements.** We are already at the SSE4.2
  ceiling; folding more bytes per cycle requires AVX-512 + VPCLMULQDQ
  and is a ~2× win at best, vs. the >3× win available from the encoder
  swap.
- **Switching to `store` / `flat` / `persist`.** See the table and the
  rationale in the *Plan to land it* section above.
