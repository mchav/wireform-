#!/usr/bin/env python3
"""Replace existing ASCII diagrams with Mermaid + add new diagrams.

Each entry is a (file, anchor_old, anchor_new) tuple. The anchor
must appear exactly once in the file. Edits fail loudly.
"""

import sys
from pathlib import Path

ROOT = Path("/workspace/website/src/content/docs/kafka-streams")


# ---------------------------------------------------------------------------
# Replacements: ASCII -> Mermaid for existing diagrams
# ---------------------------------------------------------------------------

REPLACEMENTS: list[tuple[str, str, str]] = [
    # --- get-started/what-is-kafka-streams.md: "service contains topology" tree
    (
        "get-started/what-is-kafka-streams.md",
        """In Kafka Streams, the topology is part of *your* service. You
deploy your service binary like any other:

```
your-service-v2 binary
├── HTTP server
├── Kafka producer / consumer
└── Streams topology     <-- runs here, in the same OS process
```""",
        """In Kafka Streams, the topology is part of *your* service. You
deploy your service binary like any other:

```mermaid
flowchart TB
  subgraph proc["your-service binary (one OS process)"]
    direction TB
    HTTP["HTTP / gRPC handlers"]
    Client["Kafka producer / consumer"]
    Topo["Streams topology"]
    Mem["Local state stores"]
  end
  Topo -. consumes from .-> Brokers[("Kafka brokers")]
  Topo -. produces to .-> Brokers
  Topo -. writes changelog to .-> Brokers
  Client -. reads / writes .-> Brokers
  Topo --- Mem
```""",
    ),
    # --- get-started/joins-and-tables.md: join box diagram
    (
        "get-started/joins-and-tables.md",
        """```
        ┌─────────────────┐
        │  PageViews      │  KStream Text Text
        │  (per-event)    │   key = user, value = page
        └────────┬────────┘
                 │
                 ▼
            ┌─────────────────────┐
            │ joinStreamTable     │
            │ "for each page view │     KStream Text Text
            │  look up the user's │     key = user
            │  current region"    │     value = page,region
            └─────────┬───────────┘
                      ▲
                      │
            ┌─────────┴───────┐
            │  UserProfiles   │  KTable Text Text
            │  (current value │   key = user, value = region
            │   per key)      │
            └─────────────────┘
```""",
        """```mermaid
flowchart TB
  PV["PageViews\\nKStream Text Text\\nkey=user, value=page"]
  UP["UserProfiles\\nKTable Text Text\\nkey=user, value=region"]
  Join{{"joinStreamTable\\n'for each page view\\nlook up the user's\\ncurrent region'"}}
  Out["EnrichedPageViews\\nKStream Text Text\\nkey=user, value=page,region"]
  PV -- per-event --> Join
  UP -- current value per key --> Join
  Join --> Out
```""",
    ),
    # --- guides/enrichment.md: decision tree
    (
        "guides/enrichment.md",
        """```
Is the lookup table small (fits in memory) and slow-changing?
├── Yes  → GlobalKTable
└── No
    │
    Is the lookup table keyed and you control the publisher?
    ├── Yes  → KTable + stream-table join (or foreign-key join)
    └── No
        │
        Can the lookup happen synchronously (latency < 10 ms median)?
        ├── Yes  → mapValuesM into a connection pool / local cache
        └── No   → asyncMapValues / asyncMapKeyValue
```""",
        """```mermaid
flowchart TD
  Q1{"Lookup table small\\n(fits in memory)\\nand slow-changing?"}
  Q2{"Keyed and you\\ncontrol the publisher?"}
  Q3{"Sync call viable?\\n(median p50 under ~10 ms)"}
  A1["GlobalKTable"]
  A2["KTable + stream-table join\\n(or foreign-key join)"]
  A3["mapValuesM into a\\nconnection pool / local cache"]
  A4["asyncMapValues /\\nasyncMapKeyValue"]
  Q1 -- Yes --> A1
  Q1 -- No --> Q2
  Q2 -- Yes --> A2
  Q2 -- No --> Q3
  Q3 -- Yes --> A3
  Q3 -- No --> A4
```""",
    ),
    # --- operating/exactly-once.md: commit cycle arrow chain
    (
        "operating/exactly-once.md",
        """```
beginTxn
  → flush               (run user processors; send records via transactional producer)
  → commitOffsets       (TxnOffsetCommit — consumer offsets land inside the txn)
  → preCommit2PC        (external sinks transition to "prepared")
  → commitTxn           (producer commits; records + offsets become visible)
  → commit2PC           (external sinks finalise; their data becomes visible)
  → storeCommit         (transactional state stores drain to underlying stores)
```""",
        """```mermaid
flowchart LR
  begin["beginTxn"] --> flush["flush\\n(run processors; send via transactional producer)"]
  flush --> off["commitOffsets\\n(TxnOffsetCommit;\\noffsets land inside the txn)"]
  off --> pre["preCommit2PC\\n(external sinks → 'prepared')"]
  pre --> ctxn["commitTxn\\n(producer commits;\\nrecords + offsets visible)"]
  ctxn --> c2pc["commit2PC\\n(external sinks finalise)"]
  c2pc --> sc["storeCommit\\n(transactional stores\\ndrain to underlying stores)"]
```""",
    ),
]


# ---------------------------------------------------------------------------
# Insertions: new diagrams in places that need clarity
# ---------------------------------------------------------------------------

INSERTIONS: list[tuple[str, str, str]] = [
    # --- operating/exactly-once.md: failure-path flowchart right after the failure table
    (
        "operating/exactly-once.md",
        "The two `CommitFatal` cases are the only ones that put the runtime\ninto \"operator must look\" territory. Everything else is\nauto-recoverable on the next cycle.",
        """The two `CommitFatal` cases are the only ones that put the runtime
into \"operator must look\" territory. Everything else is
auto-recoverable on the next cycle.

The same picture as a flow:

```mermaid
flowchart TD
  start([Cycle starts]) --> step1[beginTxn]
  step1 -->|fail| fatal1[CommitFatal\\nsupervisor restart]
  step1 -->|ok| step2[flush]
  step2 -->|fail| abort[Abort path:\\nabortTxn + storeAbort + abort2PC]
  step2 -->|ok| step3[commitOffsets]
  step3 -->|fail| abort
  step3 -->|ok| step4[preCommit2PC]
  step4 -->|fail| abort
  step4 -->|ok| step5[commitTxn]
  step5 -->|fail| abort
  step5 -->|ok| step6[commit2PC]
  step6 -->|fail| fatal2["CommitFatal\\nstranded SinkTxnId;\\ntpsRecover on restart"]
  step6 -->|ok| step7[storeCommit]
  step7 -->|fail| fatal3[CommitFatal\\nmanual investigation]
  step7 -->|ok| done([CommitSucceeded])
  abort --> retry([CommitAborted\\nretry next interval])
```""",
    ),

    # --- operating/scaling.md: KIP-848 reconciliation sequence diagram after "5 loop steps"
    (
        "operating/scaling.md",
        "5. Each transfer is observable via `setRebalanceListener` on\n   `KafkaStreams`. The handler fires on every revoke / assign so\n   you can drain external resources keyed by partition.",
        """5. Each transfer is observable via `setRebalanceListener` on
   `KafkaStreams`. The handler fires on every revoke / assign so
   you can drain external resources keyed by partition.

A single task moving from member A to member B over three
heartbeats:

```mermaid
sequenceDiagram
  participant Coord as Group coordinator (broker)
  participant A as Member A (losing task T)
  participant B as Member B (gaining task T)
  Note over A,B: Steady state: A owns task T
  B->>Coord: Heartbeat (join)
  Coord->>Coord: Recompute TargetAssignment
  Coord-->>A: Reconciliation { rRemove = {T} }
  Coord-->>B: Reconciliation { rAdd = {T} }\\n(blocked: A still owns T)
  A->>A: Drain T; commit cycle on T closes
  A->>Coord: Heartbeat (currentlyOwned no longer includes T)
  Coord->>Coord: Mark T released
  Coord-->>B: Reconciliation { rAdd = {T} }
  B->>B: Fetch standby state / replay tail
  B->>Coord: Heartbeat (currentlyOwned += T)
  Note over A,B: New steady state: B owns task T
```

At no point during the transfer is T owned by both members.""",
    ),

    # --- operating/topology-evolution.md: rolling deploy timeline diagram
    (
        "operating/topology-evolution.md",
        "### Suggested rollout shape\n\nFor a topology that owns meaningful state:",
        """### Suggested rollout shape

For a topology that owns meaningful state, the rolling deploy
unfolds along this timeline. `numStandbyReplicas >= 1` is what
makes the failover step metadata-only:

```mermaid
sequenceDiagram
  participant Op as Operator
  participant v1 as v1 instance (active)
  participant v1s as v1 instance (standby, lag ≈ 0)
  participant v2 as v2 instance (new)
  participant Br as Broker (coordinator)
  Op->>Op: Run topology JSON golden diff\\nRun orphan-topic detector
  Op->>v2: Start v2 binary
  v2->>Br: JoinGroup (subscription + member epoch)
  Br-->>v1s: Reconciliation { rRemove = {T}, ... }
  v1s->>Br: Heartbeat (released T)
  Br-->>v2: Reconciliation { rAdd = {T}, ... }
  v2->>v2: Warm task from standby state\\n+ replay tail
  v2->>Br: Heartbeat (currentlyOwned += T)
  Note over v2: streamsStatus = RUNNING
  Op->>v1: Drain + close
  Op->>Op: Re-run orphan-topic detector
```

Concrete steps:""",
    ),

    # --- operating/exactly-once.md: 2PC sink lifecycle diagram
    (
        "operating/exactly-once.md",
        "Use `withTwoPhaseSinks` to compose a list of sinks onto an\nexisting `EOSCoordinator`. The signature:",
        """Use `withTwoPhaseSinks` to compose a list of sinks onto an
existing `EOSCoordinator`. The signature:

The sink's lifecycle inside one commit cycle:

```mermaid
sequenceDiagram
  participant Eng as Engine (stream thread)
  participant Sink as TwoPhaseSink
  participant Coord as EOSCoordinator
  participant Br as Broker
  Eng->>Sink: tpsStage(record)  ⨯ N
  Coord->>Sink: tpsPrepare(txnId, [extraRows])
  Sink-->>Coord: SinkOK
  Coord->>Br: commitTxn
  Br-->>Coord: ok
  Coord->>Sink: tpsCommit(txnId)
  Sink-->>Coord: SinkOK
  Note over Sink: Rows visible downstream
```

On restart after a crash between `commitTxn` and `tpsCommit`:

```mermaid
sequenceDiagram
  participant Sink as TwoPhaseSink
  participant Run as Runtime (boot)
  Run->>Sink: tpsRecover
  Sink-->>Run: [stranded SinkTxnId1, ...]
  Run->>Run: Cross-reference committed offsets
  Run->>Sink: tpsCommit(SinkTxnId1)\\nor tpsAbort(SinkTxnId1)
  Sink-->>Run: SinkOK
```
""",
    ),

    # --- riffle.md: where Riffle plugs in (three layers)
    (
        "riffle.md",
        "The single `compile :: Topology Void o -> IO (o, Topo.Topology)`\nremains the bridge.",
        """The single `compile :: Topology Void o -> IO (o, Topo.Topology)`
remains the bridge.

```mermaid
flowchart TB
  subgraph dsl["Typed AST — Kafka.Streams.Topology.Free"]
    Prim["Prim GADT\\n(parity + Riffle constructors)"]
    Opt["Optimizer\\n(fusion, repartition, sync→async)"]
  end
  subgraph imp["Imperative graph — Kafka.Streams.Topology"]
    Sources["SourceSpec\\n+ optional WatermarkStrategy"]
    Procs["ProcessorSpec"]
    Stores["AnyStoreBuilder\\n(KV / Window / Session / Snapshot / Tiered / Remote)"]
  end
  subgraph rt["Runtime — Kafka.Streams.Runtime.*"]
    WP["WorkerPool\\n(Partition / Hashed / KeyGroup)"]
    EOS["EOSCoordinator\\n(+ 2PC sink hooks)"]
    Snap["Snapshot manager"]
    Reb["RebalanceProtocol\\n(KIP-848)"]
    Async["AsyncIO processor"]
    WC["WatermarkCoordinator"]
  end
  dsl --> imp
  imp --> rt
```

Riffle features either compile to *existing* `ProcessorSpec`
shapes (async I/O lives inside one task) or to *new* spec
shapes added alongside (snapshot-aware stores get their own
`AnyStoreBuilder` constructor).""",
    ),

    # --- guides/enrichment.md: async I/O processor architecture (replaces text-only description)
    (
        "guides/enrichment.md",
        "Hot path per record:\n\n1. The runtime serialises the key.",
        """Hot path per record:

The async-I/O processor is a small system in its own right:

```mermaid
flowchart LR
  Up["Upstream\\nprocessor"] -->|record| Inbox[("In-flight TBQueue\\n(aioBufferCapacity)")]
  Inbox -->|consumed by| W1["Worker 1"]
  Inbox -->|consumed by| W2["Worker 2"]
  Inbox -->|consumed by| Wn["Worker n\\n(aioWorkers)"]
  W1 -->|user IO| Ext[(External\\nsystem)]
  W2 -->|user IO| Ext
  Wn -->|user IO| Ext
  W1 -->|result| Reorder[("Reorder buffer\\n(if OrderedOutput)")]
  W2 -->|result| Reorder
  Wn -->|result| Reorder
  Reorder -->|drain on stream thread| Down["Downstream\\nprocessor"]
  Coord["EOSCoordinator\\ncommit cycle"] -. preCommitDrain hook .-> Reorder
```

1. The runtime serialises the key.""",
    ),

    # --- get-started/stateful-processing.md: KStream vs KTable visualization
    (
        "get-started/stateful-processing.md",
        "A KTable is **derived state**. The truth lives in the Kafka topic\n(or the changelog topic backing the store); the KTable is a\nmaterialised view.",
        """A KTable is **derived state**. The truth lives in the Kafka topic
(or the changelog topic backing the store); the KTable is a
materialised view.

Same input stream, two interpretations:

```mermaid
flowchart LR
  subgraph in[Input records]
    R1["(alice, 100, t=1)"]
    R2["(bob,   50, t=2)"]
    R3["(alice, 150, t=3)"]
    R4["(alice, 200, t=4)"]
  end
  in --> KS["KStream view\\n4 independent events"]
  in --> KT["KTable view\\nalice = 200\\nbob = 50\\n(newer overwrites older)"]
```

The relationship in your topology:

```mermaid
flowchart LR
  Src["KStream\\nof events"] --> Group["groupBy"]
  Group --> Agg["count / aggregate / reduce"]
  Agg --> Tbl["KTable\\nof running result"]
  Tbl --> ToStr["toStream"]
  ToStr --> Sink["sink to topic\\n(or stream-table join)"]
  Tbl -. backed by .-> Store[("State store\\n+ changelog topic")]
```
""",
    ),

    # --- operating/observability.md: observability surfaces overview
    (
        "operating/observability.md",
        "Plus interactive queries ([interactive queries (IQ)](../../glossary/#interactive-query-iq) (`Kafka.Streams.InteractiveQueries`) as a\ndebugging tool — not strictly observability, but the closest thing\nto a \"look at the current state\" view.",
        """Plus interactive queries ([interactive queries (IQ)](../../glossary/#interactive-query-iq) (`Kafka.Streams.InteractiveQueries`) as a
debugging tool — not strictly observability, but the closest thing
to a \"look at the current state\" view.

```mermaid
flowchart LR
  subgraph rt[Running streams runtime]
    Eng[Engine]
    Stores[(State stores)]
    Standby[(Standby tasks)]
    Mr["MetricsRegistry\\n(counters / gauges / timers)"]
    Topo[Compiled Topology]
  end
  Eng -->|recordCounter| Mr
  Standby -->|warmup lag| LagL[LagListener callback]
  Eng -->|state transitions| StateL[StateListener callback]
  Mr -->|dumpMetrics poll| Push[Push gateway / OTel / Prometheus]
  Topo -->|topologyDescription| GoldenFile[CI golden file]
  Topo -->|liveTopologyDescription| UIOverlay[Web UI overlay]
  Topo -->|detectOrphans + AdminClient.listTopics| Orphan[Orphan-topic log]
  Stores -->|queryEngineStore / queryKVStore| IQ[Interactive Queries\\n(your HTTP handler)]
```
""",
    ),

    # --- operating/visibility.md: commit boundary timeline
    (
        "operating/visibility.md",
        "A 30 s `commitIntervalMs` (default) means downstream\n  `read_committed` consumers see records with up to 30 s of\n  staleness.",
        """A 30 s `commitIntervalMs` (default) means downstream
  `read_committed` consumers see records with up to 30 s of
  staleness.

```mermaid
sequenceDiagram
  participant Up as Upstream producer
  participant Eng as Streams engine
  participant Store as State store
  participant IQ as Interactive query
  participant Down as read_committed consumer
  Up->>Eng: record at t=0
  Eng->>Store: put (buffered in txn store)
  IQ->>Store: get
  Store-->>IQ: pre-commit value (your call:\\nread overlay or underlying?)
  Note over Eng: ...more records...
  Eng->>Eng: commit cycle at t=30s
  Eng->>Down: records become visible
  Eng->>Store: storeCommit drains txn buffer
  IQ->>Store: get
  Store-->>IQ: committed value
```
""",
    ),
]


def apply_one(path: Path, old: str, new: str) -> None:
    body = path.read_text(encoding="utf-8")
    if old not in body:
        sys.exit(f"MISS in {path}: {old[:120]!r}")
    if body.count(old) > 1:
        sys.exit(f"AMBIG in {path}: {old[:120]!r} appears {body.count(old)} times")
    path.write_text(body.replace(old, new, 1), encoding="utf-8")
    print(f"  {path.relative_to(ROOT)}: ok")


def main() -> None:
    print("Replacements:")
    for rel, old, new in REPLACEMENTS:
        apply_one(ROOT / rel, old, new)
    print("\nInsertions:")
    for rel, old, new in INSERTIONS:
        apply_one(ROOT / rel, old, new)
    print("\ndone")


if __name__ == "__main__":
    main()
