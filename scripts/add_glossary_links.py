#!/usr/bin/env python3
"""Add glossary tips + inline links across the Kafka Streams docs.

Each edit is targeted: a unique anchor string and its replacement. Edits
fail loudly so the file isn't silently left half-edited.
"""

import sys
from pathlib import Path

ROOT = Path("/workspace/website/src/content/docs/kafka-streams")

EDITS: dict[str, list[tuple[str, str]]] = {
    "index.md": [
        (
            ":::\n\n`wireform-kafka-streams` is a Haskell port",
            ":::\n\n:::tip[Unfamiliar terms?]\nAny acronym or jargon below is defined in the [Glossary](./glossary/).\nLink-rich entries cross-reference the deeper pages.\n:::\n\n`wireform-kafka-streams` is a Haskell port",
        ),
        (
            "what a partition is, what a consumer group does",
            "what a [partition](./glossary/#partition) is, what a [consumer group](./glossary/#consumer-group) does",
        ),
    ],
    "riffle.md": [
        (
            "Read the design contract in",
            ":::tip[Unfamiliar terms?]\nAcronyms and jargon used below are defined in the [Glossary](./glossary/).\n:::\n\nRead the design contract in",
        ),
    ],
    "operating/topology-evolution.md": [
        (
            "The default mental model for a binary upgrade",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nThe default mental model for a binary upgrade",
        ),
        (
            "encoded in two places the broker keeps for you",
            "encoded in two places the [broker](../glossary/#broker) keeps for you",
        ),
        (
            "1. **Internal topics** the framework auto-creates",
            "1. **[Internal topics](../glossary/#internal-topic)** the framework auto-creates",
        ),
        (
            "2. **Consumer-group state** that pins which assignor protocol",
            "2. **[Consumer-group](../glossary/#consumer-group) state** that pins which [assignor](../glossary/#assignor) protocol",
        ),
        (
            "under the **KIP-848 next-gen protocol** (see\n`Kafka.Streams.Runtime.RebalanceProtocol`)",
            "under the **[KIP-848](../glossary/#kip) next-gen protocol** (see\n`Kafka.Streams.Runtime.RebalanceProtocol`)",
        ),
        (
            "- Standby tasks (`StandbyTask`, `StandbyDriver`) keep",
            "- [Standby tasks](../glossary/#standby-task) (`StandbyTask`, `StandbyDriver`) keep",
        ),
        (
            "3. **Processing-guarantee mismatch.** Switching `processingGuarantee`",
            "3. **[Processing-guarantee](../glossary/#processing-guarantee) mismatch.** Switching `processingGuarantee`",
        ),
    ],
    "operating/scaling.md": [
        (
            "The parity surface of wireform-kafka-streams scales",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nThe parity surface of wireform-kafka-streams scales",
        ),
        (
            "parallelism is bounded by the partition count of the\ninput topics",
            "[parallelism](../glossary/#parallelism) is bounded by the [partition](../glossary/#partition) count of the\ninput topics",
        ),
        (
            "The Riffle key-group model\ndecouples parallelism from partitions",
            "The Riffle [key-group](../glossary/#key-group) model\ndecouples parallelism from partitions",
        ),
        (
            "Each new process is a new\ngroup member; KIP-848 handles the assignment incrementally.",
            "Each new process is a new\ngroup member; [KIP-848](../glossary/#kip) handles the assignment incrementally via [reconciliation](../glossary/#reconciliation).",
        ),
        (
            "`numStandbyReplicas` in `StreamsConfig` is the per-task replication",
            "`numStandbyReplicas` in `StreamsConfig` is the per-[task](../glossary/#task) replication",
        ),
    ],
    "operating/exactly-once.md": [
        (
            "Exactly-once-semantics (EOS) on Kafka itself is well-understood",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\n[Exactly-once-semantics (EOS)](../glossary/#eos--eos-v2--eos-v3) on Kafka itself is well-understood",
        ),
        (
            "KIP-892 transactional state stores",
            "[KIP-892](../glossary/#kip) transactional [state stores](../glossary/#state-store)",
        ),
        (
            "The Riffle two-phase-commit sink contract closes that gap",
            "The Riffle [two-phase-commit sink](../glossary/#two-phase-commit-sink) contract closes that gap",
        ),
        (
            "If the runtime rewinds on a rebalance or a fault",
            "If the runtime rewinds on a [rebalance](../glossary/#rebalance) or a fault",
        ),
        (
            "every operation may be re-invoked. The reference",
            "every operation may be re-invoked ([idempotence](../glossary/#idempotent--idempotency) is mandatory). The reference",
        ),
    ],
    "operating/observability.md": [
        (
            "A Kafka Streams app fails in ways that a stateless HTTP service does",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nA Kafka Streams app fails in ways that a stateless HTTP service does",
        ),
        (
            "Plus interactive queries (`Kafka.Streams.InteractiveQueries`) as a\ndebugging tool",
            "Plus [interactive queries (IQ)](../glossary/#interactive-query-iq) (`Kafka.Streams.InteractiveQueries`) as a\ndebugging tool",
        ),
    ],
    "operating/visibility.md": [
        (
            "A Postgres `SELECT` after an `INSERT … COMMIT` returns",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nA Postgres `SELECT` after an `INSERT … COMMIT` returns",
        ),
        (
            "whether the commit cycle\nhas fired",
            "whether the [commit cycle](../glossary/#commit-cycle)\nhas fired",
        ),
        (
            "rebuilds derived state\nasynchronously.",
            "rebuilds derived state\nasynchronously. (Same shape as [CQRS](../glossary/#cqrs-command-query-responsibility-segregation).)",
        ),
        (
            "Kafka Streams **may replay records on failure**",
            "Kafka Streams **may [replay](../glossary/#replay) records on failure**",
        ),
    ],
    "operating/runbooks.md": [
        (
            "Each runbook below starts with the **alert** the on-call sees",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nEach runbook below starts with the **alert** the on-call sees",
        ),
        (
            "A \"storm\" is rebalances triggered faster than the group can",
            "A \"storm\" is [rebalances](../glossary/#rebalance) triggered faster than the group can",
        ),
        (
            "Under EOS-V2 the broker fences a producer when a newer producer",
            "Under [EOS-V2](../glossary/#eos--eos-v2--eos-v3) the broker [fences a producer](../glossary/#fenced-producer) when a newer producer",
        ),
    ],
    "concepts/topology-optimization.md": [
        (
            "The topology you write is not necessarily the topology that runs.",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nThe [topology](../glossary/#topology) you write is not necessarily the topology that runs.",
        ),
        (
            "The compiler walks the `FreeArrow Prim` AST",
            "The compiler walks the [`FreeArrow Prim`](../glossary/#free-arrow--freearrow) AST",
        ),
    ],
    "concepts/dynamic-topology.md": [
        (
            "Kafka Streams' DSL builds a `Topology` value once, at compile time.",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nKafka Streams' [DSL](../glossary/#dsl) builds a [`Topology`](../glossary/#topology) value once, at compile time.",
        ),
        (
            "binds it to consumer-group state, internal topics, and local stores.",
            "binds it to [consumer-group](../glossary/#consumer-group) state, [internal topics](../glossary/#internal-topic), and local [state stores](../glossary/#state-store).",
        ),
    ],
    "guides/enrichment.md": [
        (
            "The single most-cited reason teams leave Kafka Streams for Flink",
            ":::tip[Unfamiliar terms?]\nKafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).\n:::\n\nThe single most-cited reason teams leave Kafka Streams for Flink",
        ),
        (
            "synchronous on the stream thread — throughput collapses\nwhen the external call takes 50 ms.",
            "synchronous on the [stream thread](../glossary/#stream-thread) — throughput collapses\nwhen the external call takes 50 ms.",
        ),
        (
            "The async-I/O operator family\nin `Kafka.Streams.AsyncIO` fixes this",
            "The [async-I/O operator](../glossary/#async-io-operator) family\nin `Kafka.Streams.AsyncIO` fixes this",
        ),
        (
            "**Small, slow-changing, fits in memory** — GlobalKTable. Every",
            "**Small, slow-changing, fits in memory** — [GlobalKTable](../glossary/#globalktable). Every",
        ),
        (
            "Per Little's law: `workers ≈ desiredThroughput × medianLatency`",
            "Per [Little's law](../glossary/#littles-law): `workers ≈ desiredThroughput × medianLatency`",
        ),
        (
            "Backpressure is automatic — the consumer poll loop is naturally\n  paced",
            "[Backpressure](../glossary/#backpressure) is automatic — the consumer poll loop is naturally\n  paced",
        ),
    ],
}


def apply_edit(path: Path, old: str, new: str) -> bool:
    body = path.read_text(encoding="utf-8")
    if old not in body:
        return False
    if body.count(old) > 1:
        sys.exit(f"AMBIGUOUS in {path}: {old[:60]!r}... appears {body.count(old)} times")
    path.write_text(body.replace(old, new, 1), encoding="utf-8")
    return True


def main() -> None:
    misses: list[tuple[str, str]] = []
    hits = 0
    for rel, edits in EDITS.items():
        path = ROOT / rel
        if not path.exists():
            sys.exit(f"missing file: {path}")
        for old, new in edits:
            if apply_edit(path, old, new):
                hits += 1
            else:
                misses.append((rel, old[:80]))
    print(f"applied {hits} edits")
    if misses:
        print("misses:")
        for rel, snip in misses:
            print(f"  {rel}: {snip!r}")
        sys.exit(1)


if __name__ == "__main__":
    main()
