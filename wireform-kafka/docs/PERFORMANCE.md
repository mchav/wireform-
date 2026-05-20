# Additional Performance Notes

Supplements main [PERFORMANCE.md](../PERFORMANCE.md).

## Local broker setup

```bash
cd /tmp && curl -sL https://dlcdn.apache.org/kafka/4.0.0/kafka_2.13-4.0.0.tgz | tar xz
cd kafka_2.13-4.0.0

KAFKA_CLUSTER_ID=$(bin/kafka-storage.sh random-uuid)
bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties
KAFKA_HEAP_OPTS="-Xmx1G -Xms512M" bin/kafka-server-start.sh config/kraft/server.properties
```

Run tests:
```bash
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration
```

## Microbenchmarks

```bash
# CRC32C
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix CRC32C"

# Record batch encoding
cabal bench wireform-kafka --benchmark-options="--time-limit=1 -m prefix RecordBatch"
```

## CRC32C

Hardware-accelerated via SIMDe. Compiles to native instructions on x86_64 (SSE4.2) and AArch64 (CRC extension).

Performance (Sapphire Rapids):

| Size | Hardware CRC32C | Naive | Speedup |
|---|---|---|---|
| 16 B | 71 ns | 103 ns | 1.5x |
| 256 B | 80 ns | 1.7 us | 21x |
| 1 KB | 134 ns | 6.7 us | 50x |
| 1 MB | 99 us | 28.9 ms | 292x |

~10 GiB/s throughput at 1 MiB chunks.

## Build

Built with `-O2` so `INLINE` and `SPECIALISE` pragmas fire. Don't measure at `optimization: 1`.

## Wire codec

Typeclass-based (`Kafka.Protocol.Wire.Codec`) with codegen-emitted implementations. All requests/responses go through this path.
