---
title: Topology optimization
description: Which topology rewrites happen automatically, which you opt into, and how to inspect the compiled graph.
sidebar:
  order: 2
---

:::tip[Unfamiliar terms?]
Kafka, Streams, and Riffle terminology is defined in the [Glossary](../glossary/).
:::

The [topology](../glossary/#topology) you write is not necessarily the topology that runs.
The compiler walks the [`FreeArrow Prim`](../glossary/#free-arrow--freearrow) AST and applies a set of
rewrite passes that reduce node count, eliminate redundant
repartitions, hoist pure work out of expensive paths, and (with
Riffle async I/O) fuse pure work into async workers. Two layers
exist:

1. **AST-level fusion** — `Kafka.Streams.Topology.Free.Optimize`.
   Rewrites the typed `Topology i o` value before it is compiled to
   the imperative graph. Default-on; toggle via `OptimizeConfig`.
2. **Graph-level (KIP-295) rewrites** —
   `Kafka.Streams.Topology.Optimization`. The Java-parity knob
   from `StreamsConfig.topology.optimization`. Default-off; opt in
   per topology.

This page enumerates both, and explains how to inspect the
compiled output to verify what actually ran.

## AST-level rewrites (default-on)

The `defaultOptimizeConfig` enables every safe rewrite. The full
list is in `Kafka.Streams.Topology.Free.OptimizeConfig`; the
abridged version:

| Flag | Rewrite | Win |
| ---- | ------- | --- |
| `optAssociateCompose` | Right-associate `Compose` chains | Surfaces adjacent operators for fusion |
| `optFusePureFunctions` | Fuse adjacent `Arr` applications; push through `First` / `Second` / `Parallel` / `Fanout` / `Fork` | Fewer nodes; better Core inlining |
| `optFuseMaps` | Fuse adjacent `MapValues` / `MapValuesM` / `MapKeyValue` | One pass instead of two |
| `optFuseFilters` | Fuse adjacent `Filter` / `FilterNot` | Combined predicate |
| `optFuseConcatMaps` | Fuse `ConcatMapValues` with adjacent `MapValues` and with itself | Removes intermediate list materialisation |
| `optFuseSelectKeys` | Fuse adjacent `SelectKey` | One key-extraction call |
| `optFusePeeks` | Fuse adjacent `Peek`; collapse `Foreach . Peek` and `Tap (Foreach f) = Peek f` | Fewer side-effect callbacks |
| `optCollapseIdentity` | `Tap Id`, `First Id`, `Parallel Id Id`, etc. → `Id` | Removes wrapping overhead |
| `optFuseSelectKeyIntoGroupBy` | `GroupByKey g . SelectKey f = GroupBy f g` | Matches Apache docs' recommended idiom |
| `optCollapseRepartition` | Remove a second `Repartition` after a first with no key-change between | Eliminates redundant shuffle |
| `optDropPreKeyChangeRepartition` | Drop a `Repartition` that's about to be invalidated by a key-changing op | Saves wasted shuffle |
| `optHoistThroughRepartition` | Move pure stateless ops upstream of an adjacent `Repartition` | Pure work runs on smaller, pre-shuffle records and can fuse with upstream pure ops |
| `optAutoInsertRepartition` | Insert a `Repartition` whenever a stateful op would read mis-partitioned records | Mirrors JVM auto-repartition; prevents silent join misses |
| `optCollapseValues` | `Values . Values = Values` | Idempotent key drop |
| `optFuseTaps` | Combine adjacent `Tap` nodes by `Fanout` | One side-pipeline pass |
| `optFuseSyncIntoAsync` | Lift a pure `MapValues` / `MapKeyValue` into an immediately-following `AsyncMapValues` | Pure work runs on the async worker, not the stream thread |
| `optMaxPasses` | Iteration cap | Belt-and-braces against pathological inputs (12 by default) |

### Important non-rewrites

These are deliberately **not** automated; the compiler leaves them
to you:

- **Async-async fusion.** Two adjacent `AsyncMapValues` carry
  independent `AsyncIOConfig` instances (different buffer budgets,
  retry policies, ordering modes). Collapsing them would silently
  change the backpressure profile. Reach for `mapValues` /
  `mapValuesM` when you want fused sync work; use two async ops
  when you want two independent budgets.
- **Sync-after-async fusion.** A pure op *after* an async op
  executes on the stream thread anyway, so leaving it as a
  distinct sync node keeps the async bookkeeping focused on the
  actually-async work.
- **`MapValuesM` hoisting through repartition.** IO variants are
  not pure key-preserving ops; their observable semantics depend
  on which partition (pre- vs post-shuffle) they run on.
- **Peek hoisting through repartition.** Side observation order
  matters for debugging; the compiler leaves Peek alone.
- **Key-changing op hoisting through repartition.** Would
  invalidate the repartition.

### Inspecting the optimised topology

`Kafka.Streams.Topology.Free.optimizationStats` reports the
before-after node count and a per-rule breakdown:

```haskell
import qualified Kafka.Streams.Topology.Free as F
import qualified Kafka.Streams.Topology.Free.Optimize as Opt

let stats = Opt.optimizationStats topology
print stats
-- → OptimizationStats { osBefore = 18, osAfter = 11
--                     , osRulesFired = fromList [("fuseMaps", 3), ...] }
```

If you want to compile **without** optimisation (e.g. to diff the
naïve graph against the optimised one in CI), use
`Kafka.Streams.Topology.Free.compileNoOptimize` or
`compileWith noOptimization`.

### Tuning the optimiser

Two reasons to override `defaultOptimizeConfig`:

1. **You want a stable golden-file topology for CI** and the
   optimiser's pass-order produces noise in the JSON. Pin to
   `noOptimization`, snapshot the unoptimised JSON, and rely on
   the property suite (`OptimizerEqSpec`) to verify the optimised
   version is equivalent.
2. **You want to disable a specific rule** that interacts badly
   with a custom processor. The flags are all individually
   addressable:

   ```haskell
   import qualified Kafka.Streams.Topology.Free.Optimize as Opt

   myCfg :: Opt.OptimizeConfig
   myCfg = Opt.defaultOptimizeConfig
     { Opt.optFusePeeks = False    -- keep per-peek side effects distinct
     }

   topo <- Opt.optimizeWith myCfg topology >>= compileTopology
   ```

In practice, the defaults are correct for every topology in the
test suite (~390 cases, including 250+ Hedgehog-randomised
executions) and for every example in `examples/`. Override only
when you have a measured reason.

## Graph-level (KIP-295) rewrites

`Kafka.Streams.Topology.Optimization.TopologyOptimizationLevel`
mirrors the Java client's `topology.optimization` config knob:

```haskell
data TopologyOptimizationLevel
  = OptimizeNone
  | OptimizeReuseKtableSourceTopics
  | OptimizeMergeRepartitionTopics
  | OptimizeSingleStoreSelfJoin
  | OptimizeAll
```

| Rewrite | Effect |
| ------- | ------ |
| `REUSE_KTABLE_SOURCE_TOPICS` | A KTable backed by a topic whose key is already correct skips the per-source-table repartition |
| `MERGE_REPARTITION_TOPICS` | Adjacent repartitions sharing a key collapse into one |
| `SINGLE_STORE_SELF_JOIN` | A stream-stream self-join reading from one source uses one window-store instead of two |

These rewrites change the **internal-topic layout** on the broker
— a `REUSE_KTABLE_SOURCE_TOPICS` topology has fewer internal
topics than the same topology without it. That has two
operational consequences:

1. **Switching the level between deploys is a topology change.**
   Treat it as a stateful-op rename (see
   [Topology evolution](../operating/topology-evolution/)). The
   safest path is to drain, deploy with the new level, and accept
   a brief warmup window.
2. **The orphan-topic detector accounts for it.**
   `Kafka.Streams.Observability.OrphanTopics` consults the
   topology's `topoChangelogPlan` (populated by the optimiser) and
   excludes any store whose changelog reuses an external topic
   from the expected set. So the detector won't false-positive
   the topics this rewrite eliminates.

### When to enable

- **`OptimizeAll`** is the right default for a new topology where
  you don't yet have a deployed shape to preserve.
- **`OptimizeNone`** is the right default for an existing
  deployment until you're ready to manage the topology-shape
  diff.
- The individual flags are useful when one rewrite is desired but
  another would change the topology in a way you don't want yet.

## Putting it together

A typical compile pipeline:

```haskell
import qualified Kafka.Streams.Topology.Free as F
import qualified Kafka.Streams.Topology.Free.Optimize as Opt
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Optimization as KipOpt

(o, topo) <- F.compileWith
  Opt.defaultOptimizeConfig             -- AST-level rewrites
  (Topo.fromOptimizationFlags
    (KipOpt.optimizationFlags KipOpt.OptimizeAll))   -- graph-level
  topology
```

The two passes are independent and compose: AST rewrites run
first, producing a smaller `Topology i o`; graph rewrites run
during the imperative-graph build, producing the final
`Topo.Topology` that the runtime executes.

## What never gets optimised away

For mental-model clarity:

- **Sources and sinks** are user-declared; the optimiser never
  drops them.
- **`Through` operators** are explicit re-publish points; the
  optimiser may merge adjacent repartitions but never drops the
  `Through` itself.
- **Named operators** keep their stable names through every
  rewrite. The auto-generated names of fused operators are
  re-derived, but anything you pinned with `Named` stays.
- **State store names** are stable. A store created by `count` /
  `aggregate` / etc. keeps the same name across rewrites; the
  optimiser may share a single store across self-join legs but
  never renames an explicitly-named store.

## Related reading

- [Dynamic topology changes](./dynamic-topology/) — what's
  mutable at runtime versus what requires a recompile.
- [Topology evolution](../operating/topology-evolution/) — how
  the optimiser interacts with internal-topic names across
  deploys.
- [Observability](../operating/observability/) — how to inspect
  the optimised topology in CI and in production.
