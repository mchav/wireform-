# Streams runtime benchmark results

Numbers captured from `wireform-kafka:wireform-kafka-streams-bench`
running against the in-process `TopologyTestDriver`. The driver
takes the broker entirely out of the picture so the timings
isolate the runtime's per-record CPU envelope.

## How to reproduce

```bash
cabal bench wireform-kafka:wireform-kafka-streams-bench \
  --benchmark-options="--time-limit 1.5 \
                       --csv=streams/bench/results/streams.csv"
```

## Current results (GHC 9.6.4, -O1)

Captured 2026-05-11 on the cloud-agent runner (single core,
default RTS). All means are wall-clock; standard deviations stay
under 7% of the mean across every workload.

| Workload                 | Mean      | Records/sec |
|--------------------------|-----------|-------------|
| passthrough / 1k records | 780 μs    | ~1.28 M/s   |
| passthrough / 10k records| 10.36 ms  | ~965 K/s    |
| filter + map / 1k        | 943 μs    | ~1.06 M/s   |
| groupByKey + count / 1k  | 872 μs    | ~1.15 M/s   |
| windowed count / 1k      | 1.29 ms   | ~776 K/s    |

The 10k-record passthrough rate is ~25% lower than the 1k
rate — that's the cost of allocating 10× more `ConsumerRecord`
values and feeding them through the driver's record collector.
Once the working set fits in L2 the per-record cost is
~780 ns; under cache pressure it climbs to ~1.0 μs.

The windowed-count path is the slowest because every record
opens (or extends) a window-store entry in addition to the
KV-store update; that's the per-record cost of windowed
aggregation.

## Cross-validation against librdkafka

For end-to-end producer comparison against the `hw-kafka-client`
(librdkafka) bindings, run the parent package's benchmark suite
against a live broker:

```bash
docker compose -f wireform-kafka/test-integration/docker-compose.yml up -d
WIREFORM_KAFKA_BROKER=localhost:9092 cabal bench \
  wireform-kafka:wireform-kafka-bench \
  --benchmark-options="--time-limit 5"
```

That suite (`Benchmarks.HwKafkaComparison`) runs the same workload
through both the wireform-kafka producer and `hw-kafka-client`,
which lets you compare records/sec directly. The streams
benchmark above doesn't talk to a broker, so its numbers measure
the runtime in isolation rather than end-to-end throughput.
