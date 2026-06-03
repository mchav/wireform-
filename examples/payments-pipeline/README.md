# payments-pipeline

An end-to-end example that stitches together **gRPC**, **protobuf semantic
events**, and **Kafka Streams** into a small event-sourcing pipeline:

```
                    ┌──────────────────────────────────────────────────────────┐
  gRPC client       │  PaymentService (wireform-grpc)                            │
  CreatePayment ───►│    • mint transaction id + timestamp                       │
                    │    • PaymentRequest ──► TransactionEvent (protobuf)        │
                    │    • append event to Kafka topic  "payments.transactions"  │
                    └───────────────────────────┬──────────────────────────────┘
                                                 │  (event log / source of truth)
                                                 ▼
                    ┌──────────────────────────────────────────────────────────┐
                    │  Kafka Streams topology (wireform-kafka)                   │
                    │    source payments.transactions                            │
                    │            │                                               │
                    │      ┌─────┴─────┐  (fork = free-arrow &&&)                │
                    │      ▼           ▼                                         │
                    │  RiskFeature   BookkeepingEntry                            │
                    │  (risk engine) (bookkeeping product)                       │
                    │      │           │                                         │
                    │      ▼           ▼                                         │
                    │  payments.    payments.                                    │
                    │  risk-features bookkeeping-entries                         │
                    └──────────────────────────────────────────────────────────┘
```

Every contract lives in [`proto/payments.proto`](proto/payments.proto):
the gRPC request/response, the `TransactionEvent` written to the log, and the
two derived projections (`RiskFeature`, `BookkeepingEntry`).

## The idea

The synchronous gRPC call does the bare minimum: it validates and records a
single **semantic event** to an append-only Kafka topic. That event log is the
source of truth. Everything else is a **projection** — a pure function of the
ordered stream of events — computed asynchronously by a Kafka Streams topology:

- **Risk engine view** (`RiskFeature`): a flattened, numeric "feature" keyed by
  the assessed account, carrying a derived `is_high_value` flag and direction.
- **Bookkeeping view** (`BookkeepingEntry`): a domain-specific ledger posting
  with debit/credit accounts, keyed by transaction id.

Because both views are total functions of the event, you can rebuild either
one from scratch by replaying the log — the essence of event sourcing.

## Layout

| Module | Role |
|---|---|
| `Proto.Payments` | `loadProto`-generated message + enum types |
| `Proto.API.Payments` | hand-written gRPC service binding for `PaymentService` |
| `Payments.Domain` | **pure** projections (request→event, event→risk, event→entry) |
| `Payments.Serdes` | `HasSerde` instances (protobuf) + topic names |
| `Payments.Streams` | the Kafka Streams topology (the fork) |
| `Payments.Server` | gRPC server; `CreatePayment` appends to the event log |
| `Payments.Client` | minimal gRPC client |
| `Payments.Demo` | in-process run via the `TopologyTestDriver` (no broker) |

## Running

### In-process demo (no broker)

The fastest way to see the fan-out. It feeds synthetic events straight into the
topology's source topic via the in-process test driver and prints both derived
views:

```bash
cabal run payments-pipeline -- demo
```

### Full path against a real broker

Start a Kafka broker on `localhost:9092`, then in three shells:

```bash
# 1. the streams topology — run the examples runner, or embed paymentsTopology
#    in your own Runtime.newKafkaStreams against the broker.

# 2. the gRPC server (emits events to Kafka)
cabal run payments-pipeline -- server 50051 localhost:9092

# 3. fire a payment
cabal run payments-pipeline -- client localhost 50051
```

The server appends a `TransactionEvent` per accepted payment; the streams
topology turns each into a `RiskFeature` and a `BookkeepingEntry` on their
respective output topics.

## Notes

- The protobuf wire codec comes from `wireform-proto`; the same generated types
  are used by gRPC (`Proto`-wrapped) and by Kafka (`protoSerde`), so there is a
  single source of truth for the schema.
- The streams DSL is the free-arrow topology API
  (`Kafka.Streams.Topology.Free`). The two-way fork is just `(&&&)`; each branch
  is an ordinary `mapValues >>> selectKey >>> sink` chain.
- The schema uses the `google.protobuf` well-known types `Duration` (the
  authorization window) and `Timestamp` (the event's `received_at`).
  `loadProto` resolves these through its built-in WKT registry — and, after the
  accompanying `wireform-proto` fix, the generated code references the WKT
  types by their fully-resolved names, so `Proto.Payments` does **not** need to
  import the `Proto.Google.Protobuf.*` modules itself. Plain `int64`
  epoch-millis are kept alongside for the fields the topology keys / prints on.
