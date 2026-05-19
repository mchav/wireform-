#!/usr/bin/env python3
"""Add TL;DR callouts + friendly openings to dense docs pages.

Each edit replaces the section between the frontmatter and the
first '## ' heading. The new opening is:

  * A 1-2 sentence framing.
  * A `:::tip[Unfamiliar terms?]` callout (preserved from prior pass
    where present, re-inserted otherwise).
  * A `:::note[TL;DR]` callout summarising the page in 3-5 bullets.

The existing prose between the callouts and the first H2 is
preserved verbatim — the goal is to *front-load* a skim path, not
to lose any of the depth.
"""

import re
import sys
from pathlib import Path

ROOT = Path("/workspace/website/src/content/docs/kafka-streams")

# rel-path -> (opening sentence override OR None to keep, list of TL;DR bullets)
PAGES: dict[str, tuple[str | None, list[str]]] = {
    "operating/topology-evolution.md": (
        "Your topology is part of the deployment contract. The internal Kafka topics the framework creates for you depend on operator names — and those names depend on the shape of your code. Get the rolling-deploy story wrong and you'll leak topics on the broker, lose state on rebalance, or strand running tasks.\n\nThis page is the operating manual for a binary rollout where the topology might change between versions.",
        [
            "Five kinds of topology diff each have a different operational story; the table below classifies them.",
            "Name every stateful operator explicitly (`Named` + `materializedAs`) — auto-generated names shift when you reshuffle the topology, which renames their changelog topics.",
            "Run the [topology-JSON golden-file diff](../observability/#topology-json) in CI and the [orphan-topic detector](../observability/#orphan-internal-topics) on startup.",
            "Set `numStandbyReplicas` to at least 1 for any non-trivial state, otherwise rebalance means a full changelog replay.",
            "KIP-848 makes the rebalance itself incremental — no double-ownership at any point during a transfer.",
        ],
    ),
    "operating/scaling.md": (
        "There are three axes to scale a streams app on: more **threads** in a process, more **processes** in the consumer group, or more **key-groups** to shard logical work past the partition count. Each one has its own trade-offs.\n\nThis page walks through all three, plus the rebalance protocol and the standby-task machinery that ties them together.",
        [
            "Parity Streams parallelism is capped at `numStreamThreads × instances × partition_count`. The Riffle key-group model decouples it from partition count entirely.",
            "Three dispatch modes — `DispatchPartition` (default, parity), `DispatchHashed`, `DispatchKeyGroup` (Riffle).",
            "`numStandbyReplicas >= 1` is the difference between metadata-only failover and a full changelog replay.",
            "KIP-848 incremental rebalance: tasks are never double-owned during a transfer.",
            "`addStreamThread` / `removeStreamThread` reshape in-process workers without triggering a broker-side rebalance.",
        ],
    ),
    "operating/exactly-once.md": (
        "Exactly-once-semantics on Kafka itself is a known story: transactional producer, `TxnOffsetCommit`, KIP-892 transactional state stores. The library wires all of that for you behind `processingGuarantee = ExactlyOnceP`.\n\nWhat that doesn't cover is any side effect that leaves Kafka — a write to Postgres, S3, Iceberg, or an HTTP endpoint. The Riffle [two-phase commit sink](../../glossary/#two-phase-commit-sink) contract closes that gap. This page covers both halves.",
        [
            "The commit cycle is six ordered steps: `beginTxn → flush → commitOffsets → preCommit2PC → commitTxn → commit2PC → storeCommit`.",
            "A `TwoPhaseSink r` has five operations: `tpsStage`, `tpsPrepare`, `tpsCommit`, `tpsAbort`, `tpsRecover`. Every one must be idempotent.",
            "Failure at `commit2PC` (after the producer txn already committed) is the only `CommitFatal` case; the in-flight `SinkTxnId` is resolved by `tpsRecover` on next boot.",
            "Four reference sinks ship in core (in-memory, filesystem rename, HTTP echo); real adapters for JDBC / Iceberg / S3 live in separate packages.",
            "If you just need EOS for the output stream of an async-I/O operator, that comes for free — the pre-commit drain hook handles it.",
        ],
    ),
    "operating/observability.md": (
        "A Kafka Streams app fails in ways that a stateless HTTP service does not, and many of those failures are invisible to standard request-latency dashboards. This page enumerates every observability surface the library exposes and what each one tells you.",
        [
            "Four surfaces: metrics registry, topology JSON, orphan-topic detector, lag tracking.",
            "The metrics registry is plain in-memory; you wire it to your push gateway via a periodic `dumpMetrics` poll.",
            "Topology JSON (`topologyDescription` / `liveTopologyDescription`) is a versioned document suitable for CI golden-file diffing and live UI overlays.",
            "Always alert on `droppedRecordsTotal` (silent data loss), `commit-cycle-fatal` (operator-required), and per-task warmup lag above `acceptableRecoveryLag`.",
            "Interactive queries (IQ) are a debugging surface, not a replacement for a query layer.",
        ],
    ),
    "operating/visibility.md": (
        "A Postgres `SELECT` after an `INSERT ... COMMIT` always returns the inserted row. A streams app's view of its state is more nuanced — and the differences are not bugs. They follow from using an append-only log as the source of truth and rebuilding derived state asynchronously.\n\nThis page maps each ACID property onto wireform-kafka-streams so you know which guarantees you get for free and which require explicit work.",
        [
            "The commit boundary is `commitIntervalMs` (default 30 s), not a `COMMIT` statement. Read-committed downstream consumers see records with up to that much staleness.",
            "IQ reads see the live in-memory store; they don't necessarily see writes atomically with the EOS commit cycle.",
            "State is partitioned across instances. A query must route to the instance that owns the partition; `StreamsMetadata` + `KeyQueryMetadata` tell you which one.",
            "Event time and processing time are different clocks. Windowed aggregations are event-time; processing-rate metrics are wall-clock.",
            "Side effects in `peek` / `foreach` / `mapValuesM` replay on rewind. Use a two-phase commit sink or an idempotency token for exactly-once external effects.",
        ],
    ),
    "operating/runbooks.md": (
        "Each runbook starts with the **alert** the on-call sees, then walks through diagnosis and resolution. The intent is to keep this page open during an incident and follow the steps; the explanatory pages it links to have the deeper story.",
        [
            "Most-common incidents covered: rebalance storm, `CommitFatal` after `commit2PC`, standby that never promotes, orphan internal topics, producer fenced, async-I/O stall, deserialisation flood, unbounded state-dir growth, slow commit cycle, IQ during rebalance.",
            "Each runbook is structured the same way: alert → diagnosis → resolve → prevent.",
            "Use the quick metric-reference at the bottom when you don't know what to look at first.",
        ],
    ),
    "concepts/topology-optimization.md": (
        "The topology you write is not necessarily the topology that runs. The compiler walks the [`FreeArrow Prim`](../../glossary/#free-arrow--freearrow) AST and applies a set of rewrite passes that reduce node count, eliminate redundant repartitions, hoist pure work out of expensive paths, and (with Riffle async I/O) fuse pure work into async workers.\n\nThis page enumerates every rewrite and tells you how to inspect the result.",
        [
            "Two layers: AST-level fusion (`Kafka.Streams.Topology.Free.Optimize`, default-on) and graph-level KIP-295 rewrites (`Kafka.Streams.Topology.Optimization`, default-off).",
            "Sixteen AST rewrites for compose-associativity, function fusion, repartition elimination, auto-repartition insertion, and sync-into-async fusion. All toggleable via `OptimizeConfig`.",
            "Three KIP-295 graph rewrites change *internal topic layout* — treat them like a topology change between deploys.",
            "Inspect what the compiler did via `optimizationStats`; pin the output for CI via `compileNoOptimize` + a golden file.",
        ],
    ),
    "concepts/dynamic-topology.md": (
        "Kafka Streams builds a [`Topology`](../../glossary/#topology) value once, at compile time, and binds it to consumer-group state, internal topics, and local stores at startup. After that point the topology shape is frozen — but a handful of things around it can still change.\n\nThis page is the map of what's mutable where.",
        [
            "Four tiers: hot (live, no rebalance), warm (live, one rebalance), restart-required, migration-required.",
            "Hot tier: worker count, pause/resume, every `set*Listener`, EOS coordinator swap, `setApplicationServer`.",
            "Warm tier: group membership changes (add/remove an instance), `applicationServer` advertisement.",
            "Restart-required: `processingGuarantee`, `dispatchMode`, most `StreamsConfig` fields.",
            "Migration-required: topology shape, `applicationId`, key-group count, stateful-store serde. Each has a documented procedure.",
            "Truly mutable topology isn't shipped on purpose; `TopicNameExtractor` / `Branched.withConsumer` / `addReadOnlyStateStore` cover the cases people usually want it for.",
        ],
    ),
    "guides/enrichment.md": (
        "The single most-cited reason teams leave Kafka Streams for Flink is \"we needed to enrich records from an external API and couldn't do it cleanly.\" The parity DSL gives you `mapValuesM`, which runs synchronously on the [stream thread](../../glossary/#stream-thread) — throughput collapses when the external call takes 50 ms. The Riffle [async-I/O operator](../../glossary/#async-io-operator) family in `Kafka.Streams.AsyncIO` fixes this.\n\nThis page walks through every enrichment pattern, when to use each, and how to size async I/O for your latency budget.",
        [
            "Decision tree across six patterns: GlobalKTable (small reference data), KTable join, foreign-key join, sync `mapValuesM`, async I/O, idempotency-token state-store dedup.",
            "Async I/O gives you bounded backpressure, EOS-correct offsets, ordered or unordered output, per-request timeout + retry, explicit failure policy.",
            "Capacity sizing: `aioWorkers ≈ throughput × medianLatency` (Little's law); `aioBufferCapacity ≈ 4 × aioWorkers` so brief stalls don't immediately propagate.",
            "EOS-correct via the pre-commit drain hook — offsets only advance once every in-flight request has deposited a result.",
            "For external *writes* with strong consistency, use a [two-phase commit sink](../../operating/exactly-once/) instead.",
        ],
    ),
}


HEADER_BREAK_RE = re.compile(r"^(---\n.*?\n---\n)(.*?)(\n## )", re.DOTALL)


def lighten(path: Path, opening_override: str | None, tldr_bullets: list[str]) -> None:
    body = path.read_text(encoding="utf-8")
    m = HEADER_BREAK_RE.match(body)
    if not m:
        sys.exit(f"could not find frontmatter + first H2 in {path}")

    frontmatter, intro_block, h2_prefix = m.group(1), m.group(2), m.group(3)

    # Preserve any existing :::tip glossary callout from the prior pass.
    tip_match = re.search(
        r":::tip\[Unfamiliar terms\?\][^:]*:::", intro_block, re.DOTALL
    )
    glossary_tip = tip_match.group(0) if tip_match else None

    # Build new opening: friendly intro + (optional) glossary tip + TL;DR.
    tldr = ":::note[TL;DR]\n" + "\n".join(f"- {b}" for b in tldr_bullets) + "\n:::"

    parts = ["\n" + opening_override.strip() + "\n"]
    if glossary_tip:
        parts.append("\n" + glossary_tip + "\n")
    parts.append("\n" + tldr + "\n")

    new_intro = "".join(parts)
    rest = body[m.end(2) :]  # everything from the first \n## onwards
    path.write_text(frontmatter + new_intro + rest, encoding="utf-8")
    print(f"  lightened {path.relative_to(ROOT)}")


def main() -> None:
    for rel, (opening, bullets) in PAGES.items():
        path = ROOT / rel
        if not path.exists():
            sys.exit(f"missing: {path}")
        lighten(path, opening, bullets)
    print("done")


if __name__ == "__main__":
    main()
