---
title: Columnar format support
description: "Overview of wireform's columnar and lake-format packages."
sidebar:
  order: 3
---

wireform includes native Haskell implementations of the major columnar and
lake-table formats. Each package page describes its feature set, API surface,
and interoperability status:

- [Parquet](/packages/parquet/) -- Apache Parquet reader and writer
- [Arrow](/packages/arrow/) -- Apache Arrow IPC and typed record batches
- [ORC](/packages/orc/) -- Apache ORC reader and writer
- [Iceberg](/packages/iceberg/) -- Apache Iceberg table format and catalog clients
- [Delta](/packages/delta/) -- Delta Lake transaction log reader
- [Hudi](/packages/hudi/) -- Apache Hudi timeline reader
- [Lance](/packages/lance/) -- Lance file and dataset reader
- [Columnar](/packages/columnar/) -- Shared infrastructure (IO, predicates, SIMD, streaming)
