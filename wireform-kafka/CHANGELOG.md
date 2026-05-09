# Changelog for `kafka-native`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `Kafka.Client.Producer.bindTransaction` lets a
  `Kafka.Client.Transaction` value be attached to a `Producer` so
  that `sendMessage` participates in the broker-side KIP-98 /
  KIP-447 transaction lifecycle: the gate rejects sends outside
  `InTransaction`, the first send to a new (topic, partition)
  issues `AddPartitionsToTxn`, and outgoing record batches are
  stamped with the transactional producer-id / epoch / sequence
  and have their `attrIsTransactional` bit set.
- `Kafka.Client.Producer.producerTxnGate`: pure helper exposing
  the transactional-state gate so unit tests can drive every
  branch without a live broker.
- `Kafka.Client.Internal.BatchAccumulator.appendRecordStamped` +
  `BatchStamp`: explicit per-batch (producer-id, epoch,
  base-sequence, isTransactional) stamping. The legacy
  `appendRecord` / `appendRecordWithCallback` stay as the
  no-stamp shortcut for non-idempotent producers.
- `Kafka.Client.Internal.BatchAccumulator.ProducerBatch` carries
  a new `batchIsTransactional` field. Read by
  `Kafka.Client.Internal.ProducerSender.buildRecordBatch`, which
  is now exported (for tests).

### Changed

- `closeProducer` aborts an open transaction (if any) before
  shutdown, mirroring `KafkaProducer.close` in the JVM client.

## 0.1.0.0 - YYYY-MM-DD
