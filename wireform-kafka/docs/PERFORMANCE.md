# wireform-kafka performance notes

The headline producer / consumer hot-path numbers + reproduction
recipe live in the top-level [`PERFORMANCE.md`](../PERFORMANCE.md).
This file captures two narrower items that don't fit there: a
reproducible CRC32C benchmark and a quick recipe for spinning up a
local broker to run the integration suite against.

## How to run the suite against a local broker

```bash
# Set up a local broker (KRaft mode, single node):
cd /tmp && curl -sL https://dlcdn.apache.org/kafka/4.0.0/kafka_2.13-4.0.0.tgz | tar xz
cd kafka_2.13-4.0.0
KAFKA_CLUSTER_ID=$(bin/kafka-storage.sh random-uuid)
bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties
KAFKA_HEAP_OPTS="-Xmx1G -Xms512M" bin/kafka-server-start.sh config/kraft/server.properties

# Run integration tests against it:
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration

# Run the microbenchmarks (the wire codec + record-batch + CRC32C
# benches all run in-process — no broker required):
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix CRC32C"
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix RecordBatch"
```

The library is built at `-O2` (see `cabal.project` `package
wireform-kafka`) so the `INLINE` / `SPECIALISE` pragmas in
`Kafka.Protocol.Primitives` actually fire. Without the `-O2`
package opt-in the workspace-wide `optimization: 1` masks the
wins.

## CRC32C

`Kafka.Protocol.CRC32C` calls `simde_mm_crc32_u8/u16/u32/u64`
(see `cbits/crc32c.c`). With `-O2 -march=native` the SIMDe layer
compiles straight to native CRC32 instructions on x86_64 (SSE4.2)
and AArch64 (the optional `+crc` extension), and falls back to a
portable C reference everywhere else.

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

That is ~10 GiB/s on a 1 MiB chunk, which puts us at the
throughput ceiling of the hardware CRC32C instruction. There is no
headroom to chase here.

## Where the encoder hot path now lives

The wire encoder + decoder is the single-allocation, direct-poke
`Kafka.Protocol.Wire.Codec` typeclass (see `WireCodec` /
`WireCodecImpl`), populated by codegen-emitted `wirePokeFoo` /
`wirePeekFoo` / `wireMaxSizeFoo` triples from
`Kafka.Protocol.Codegen.WireGenerator`. Every public Kafka request
/ response goes through it — there is no Serial / Builder
fallback in the runtime path. The per-record hot-path numbers
(57 ns / record encode, 45 ns / record decode at 100-record batches)
are in [`PERFORMANCE.md`](../PERFORMANCE.md).
