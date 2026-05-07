# Architecture: shared predicate pushdown algebra

The columnar formats (Apache Arrow, Apache Parquet, Apache ORC,
Apache Iceberg) each have their own statistics format on disk
but a common structure for /how/ a reader uses them: take a
predicate, ask whether the per-slice statistics prove no row
matches, and skip the slice if so.

`Columnar.Predicate` lifts the predicate algebra out of any one
format so the per-format evaluators all share the same
soundness guarantee.

## Layering

```
+----------------------------------------+
|  Columnar.Predicate (Predicate ADT)    |   wireform-columnar
+--------------------+-------------------+
                     |
       +-------------+-------------+
       |                           |
       v                           v
+----------------+        +----------------+
| Parquet        |        | ORC            |
| .Predicate     |        | .Statistics    |
+--------+-------+        +--------+-------+
         |                         |
+--------v-------+        +--------v-------+
| evalRowGroup   |        | evalStripe     |
| evalPagesByCI  |        | evalRowGroupE. |
| evalBloomChunk |        | (Bloom too)    |
+----------------+        +----------------+
```

`Columnar.Predicate` exports:

- `PValue` — typed scalars covering every Parquet physical
  type that can appear in a column statistic (`PVInt32`,
  `PVInt64`, `PVFloat`, `PVDouble`, `PVBool`, `PVText`,
  `PVBinary`).
- `PColPredicate` — leaf comparisons (`PEq`, `PLt`, `PLtEq`,
  `PGt`, `PGtEq`, `PIn`, `PIsNull`, `PIsNotNull`, `PNeq`).
- `Predicate` — the boolean tree (`PCol name leaf`, `PAnd`,
  `POr`, `PNot`, `PTrue`, `PFalse`).
- `Decision` — `PSkip` | `PMaybeKeep`.
- `combineDecisions` — AND short-circuits on `PSkip`.
- `evalRange` — given an inclusive `[mn, mx]` `PValue` pair,
  evaluate one leaf predicate.

Per-format modules contribute only:

- A `decodeStatistics :: <format-stats-bytes> -> PValue` mapper.
- A walker that traverses the format's slice hierarchy
  (row-group / page / stripe / row-group-within-stripe) and
  feeds `evalRange` one leaf at a time.

The walkers live in `Parquet.Predicate` (rows + pages + bloom
filter) and `ORC.Statistics` (stripes + row groups). Both
produce the same `Decision` shape, so the cross-format
`Wireform.Columnar.decodeFilteredIter` can hand an unmodified
`Predicate` to either.

## Soundness invariant

The contract every evaluator obeys:

> `PSkip` is only ever returned when the evaluator can /prove/
> that no row in the slice satisfies the predicate.

Where the proof requires statistics the writer didn't populate
(a missing `min_value`, an unknown sub-statistics variant, a
cross-type comparison the value algebra hasn't grown yet), the
fallback is always `PMaybeKeep` — the slice gets decoded
normally. Readers therefore can't accidentally drop rows that
should match.

## What this enables

- `Wireform.Columnar.decodeFilteredIter` returns
  `(totalRowGroups, dropped, iter)` so a caller can log skip
  ratios without holding the file.
- `Parquet.Arrow.readParquetColumnWithPagePruning` seeks
  directly to surviving pages via `OffsetIndex.plOffset` —
  pages whose `ColumnIndex` proved no match never enter the
  decoder.
- `Parquet.HighLevel.encodeParquet` auto-populates `Sbbf` for
  every column listed in `writeBloomFilters`, and the read
  side's `evalBloomChunk` checks membership against the same
  hash construction.
- ORC's `streamStripesFilteredIter` + the new
  `ColumnStatistics` codec give stripe-level skipping with the
  same predicate vocabulary Parquet uses.
- Iceberg's `pruneManifestFiles` extends the same
  `[(columnId, lower, upper)]` shape one level higher: drop
  whole manifests at scan-planning time before a single data
  file is opened.

## Adding a new tier

To add a new pushdown tier (say, Parquet `ColumnIndex`
histograms or ORC's per-row-index BloomFilterIndex):

1. Decode the format-specific bytes into the corresponding
   `PValue` range or membership signal.
2. Walk `Predicate` recursively, returning `PSkip` /
   `PMaybeKeep` for each `PCol`.
3. Combine via `combineDecisions` for `PAnd`, take the
   intersection (`PSkip` iff /both/ disjuncts skip) for `POr`.
4. Make sure the unproven case is `PMaybeKeep`.

That's it — the rest of the pipeline (the `Iter`-shaped
decode, the `(total, dropped)` tally, the facade integration)
is generic.
