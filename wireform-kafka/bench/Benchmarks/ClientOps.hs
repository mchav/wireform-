{- |
Module      : Benchmarks.ClientOps
Description : Benchmarks for end-to-end client operations
Copyright   : (c) 2025
License     : BSD-3-Clause

This module contains placeholders for future end-to-end benchmarks of
Kafka client operations. These benchmarks will require a running Kafka
cluster and will measure real-world performance characteristics.

All benchmarks in this module are currently TODOs and return empty
benchmark groups.
-}
module Benchmarks.ClientOps (benchmarks) where

import Criterion (Benchmark, bgroup)


-- -----------------------------------------------------------------------------
-- Client Operation Benchmarks (TODOs)
-- -----------------------------------------------------------------------------

-- | All client operation benchmarks (currently placeholders)
benchmarks :: Benchmark
benchmarks =
  bgroup
    "ClientOps"
    [ producerBenchmarks
    , consumerBenchmarks
    , transactionBenchmarks
    , connectionBenchmarks
    ]


-- -----------------------------------------------------------------------------
-- Producer Benchmarks
-- -----------------------------------------------------------------------------

-- | Producer operation benchmarks
producerBenchmarks :: Benchmark
producerBenchmarks =
  bgroup
    "Producer"
    []


-- TODO: Implement producer benchmarks
-- These will require:
-- - A running Kafka cluster (likely dockerized)
-- - Connection management
-- - Topic creation/cleanup
-- - Proper timing around network I/O

-- Planned benchmarks:
-- 1. Single message publish latency
--    - Measure end-to-end latency for publishing one message
--    - With and without acks (acks=0, acks=1, acks=-1)

-- 2. Batch publishing throughput
--    - Vary batch sizes: 1, 10, 100, 1000 messages
--    - Measure messages/second and MB/s

-- 3. Compression codec comparison
--    - none, gzip, snappy, lz4, zstd
--    - Measure throughput and latency with each
--    - Test with different message sizes

-- 4. Idempotent vs non-idempotent producer
--    - Measure overhead of idempotent producer
--    - Compare throughput and latency

-- 5. Producer buffer management
--    - Test with different buffer sizes
--    - Measure memory usage vs throughput tradeoffs

-- Example implementation sketch:
-- bench "single-message-latency/acks=1" $ nfIO $ do
--   producer <- createProducer defaultProducerConfig
--   publishMessage producer "test-topic" "test-message"
--   closeProducer producer

-- -----------------------------------------------------------------------------
-- Consumer Benchmarks
-- -----------------------------------------------------------------------------

-- | Consumer operation benchmarks
consumerBenchmarks :: Benchmark
consumerBenchmarks =
  bgroup
    "Consumer"
    []


-- TODO: Implement consumer benchmarks
-- These will require:
-- - A running Kafka cluster
-- - Pre-populated topics with test data
-- - Connection management
-- - Proper timing around network I/O and deserialization

-- Planned benchmarks:
-- 1. Single message fetch latency
--    - Measure end-to-end latency for fetching one message
--    - Include deserialization time

-- 2. Batch fetching throughput
--    - Vary fetch sizes: 1, 10, 100, 1000 messages
--    - Measure messages/second and MB/s
--    - Test with different max.partition.fetch.bytes

-- 3. Partition assignment overhead
--    - Measure time to assign/reassign partitions
--    - Test with different numbers of partitions
--    - Compare different assignment strategies

-- 4. Offset commit performance
--    - Auto-commit vs manual commit
--    - Sync vs async commit
--    - Measure impact on throughput

-- 5. Consumer group rebalancing
--    - Measure rebalance time with different group sizes
--    - Test with different numbers of partitions

-- 6. Decompression performance
--    - Measure overhead of different compression codecs
--    - Compare with producer compression benchmarks

-- Example implementation sketch:
-- bench "single-message-fetch" $ nfIO $ do
--   consumer <- createConsumer defaultConsumerConfig
--   subscribe consumer ["test-topic"]
--   msg <- poll consumer 1000
--   closeConsumer consumer
--   return msg

-- -----------------------------------------------------------------------------
-- Transaction Benchmarks
-- -----------------------------------------------------------------------------

-- | Transaction operation benchmarks
transactionBenchmarks :: Benchmark
transactionBenchmarks =
  bgroup
    "Transactions"
    []


-- TODO: Implement transaction benchmarks
-- These will require:
-- - A running Kafka cluster with transactions enabled
-- - Idempotent producer configuration
-- - Transaction coordinator interaction

-- Planned benchmarks:
-- 1. Transaction initialization
--    - Measure time to initProducerId
--    - Measure time to beginTransaction

-- 2. AddPartitionsToTxn overhead
--    - Vary number of partitions added
--    - Measure latency vs number of partitions

-- 3. Commit transaction latency
--    - End-to-end transaction commit time
--    - Vary number of messages/partitions in transaction

-- 4. Abort transaction latency
--    - End-to-end transaction abort time
--    - Compare with commit latency

-- 5. Transaction throughput
--    - Messages/second with transactions enabled
--    - Compare with non-transactional throughput
--    - Vary transaction size (messages per transaction)

-- 6. Consumer offset commit in transaction
--    - Measure overhead of sendOffsetsToTransaction
--    - Test with different numbers of partitions

-- Example implementation sketch:
-- bench "commit-transaction" $ nfIO $ do
--   producer <- createTransactionalProducer config
--   initTransactions producer
--   beginTransaction producer
--   publishMessage producer "test-topic" "test-message"
--   commitTransaction producer
--   closeProducer producer

-- -----------------------------------------------------------------------------
-- Connection Benchmarks
-- -----------------------------------------------------------------------------

-- | Connection and authentication benchmarks
connectionBenchmarks :: Benchmark
connectionBenchmarks =
  bgroup
    "Connection"
    []

-- TODO: Implement connection benchmarks
-- These will require:
-- - A running Kafka cluster
-- - Various auth configurations (PLAIN, SCRAM, TLS)
-- - Ability to measure connection setup time separately from operations

-- Planned benchmarks:
-- 1. Connection establishment time
--    - Plain TCP connection
--    - Measure time to establish connection to broker

-- 2. TLS handshake time
--    - Compare plain vs TLS connections
--    - Measure SSL/TLS overhead

-- 3. Authentication overhead
--    - PLAIN authentication
--    - SCRAM-SHA-256 authentication
--    - SCRAM-SHA-512 authentication
--    - Compare authentication method latencies

-- 4. API version negotiation
--    - Measure time for ApiVersions request/response
--    - Test with different numbers of supported APIs

-- 5. Metadata refresh
--    - Measure time to fetch cluster metadata
--    - Vary number of topics/partitions
--    - Test with different metadata.max.age.ms settings

-- 6. Connection pool performance
--    - Measure overhead of connection pooling
--    - Test with different pool sizes
--    - Measure connection reuse vs creation cost

-- Example implementation sketch:
-- bench "connect-plain" $ nfIO $ do
--   startTime <- getCurrentTime
--   conn <- connect brokerAddress
--   endTime <- getCurrentTime
--   close conn
--   return (diffUTCTime endTime startTime)

-- bench "connect-tls" $ nfIO $ do
--   startTime <- getCurrentTime
--   conn <- connectTLS tlsConfig brokerAddress
--   endTime <- getCurrentTime
--   close conn
--   return (diffUTCTime endTime startTime)

{- IMPLEMENTATION NOTES:

When implementing these benchmarks, consider:

1. **Test Environment Setup**
   - Use Docker Compose to spin up Kafka cluster
   - Pre-create topics with test data
   - Consider using testcontainers-hs for automatic lifecycle management
   - May want separate benchmark executable that requires --enable-integration flag

2. **Timing Considerations**
   - Network I/O makes timing more variable than pure computation
   - May need more samples for statistical significance
   - Consider using criterion's --time-limit flag
   - Warm up connections before benchmarking

3. **Resource Management**
   - Ensure proper cleanup of connections/producers/consumers
   - Use bracket patterns for resource safety
   - Consider connection pooling to avoid setup overhead skewing results

4. **Benchmark Isolation**
   - Each benchmark should use separate topics to avoid interference
   - Clean up topics between runs if possible
   - Consider using unique topic names with timestamps

5. **Realistic Workloads**
   - Test with realistic message sizes (1KB, 10KB, 100KB)
   - Test with realistic batch sizes
   - Consider testing under load (concurrent producers/consumers)

6. **Configuration**
   - Make broker addresses configurable via environment variables
   - Allow skipping benchmarks if Kafka is not available
   - Consider separate "quick" vs "full" benchmark suites

7. **Comparison Baselines**
   - Consider benchmarking against other Kafka clients (Java client via JNI?)
   - Document expected performance characteristics
   - Track performance over time using JSON output
-}
