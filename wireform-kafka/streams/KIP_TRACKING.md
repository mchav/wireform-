# Kafka Streams KIP Implementation Tracking

This document tracks the implementation status of **Kafka Streams-related KIPs** in the kafka-streams Haskell package.

**Note**: Core Kafka client KIPs (Producer, Consumer, AdminClient) are tracked in the [main kafka-native KIP_TRACKING.md](../KIP_TRACKING.md).

## Table of Contents

- [Status Indicators](#status-indicators)
- [Quick Statistics](#quick-statistics)
- [Implementation Priorities](#implementation-priorities)
- [Streams KIPs by Number](#streams-kips-by-number)

## Status Indicators

- ✅ **Implemented**: Feature is fully functional in the kafka-streams package
- 🔄 **Partial**: Some components implemented, full integration pending
- ❌ **Not Implemented**: Planned for kafka-streams but not yet implemented
- ⚪ **Deferred**: Low priority or not applicable to Haskell implementation

## Quick Statistics

**Total Streams KIPs Tracked**: ~150+

**Implementation Summary**:
- ✅ **Implemented**: 0 KIPs (kafka-streams package is in early development)
- 🔄 **Partial**: 0 KIPs
- ❌ **Not Implemented**: ~150 KIPs (Streams DSL, state stores, windowing, joins, etc.)
- ⚪ **Deferred**: TBD (some Java-specific features may not be relevant)

**Streams Capabilities**:
- **DSL**: ❌ Stream processing DSL not yet implemented
- **State Stores**: ❌ State store abstraction not yet implemented
- **Windowing**: ❌ Windowing operations not yet implemented
- **Joins**: ❌ Stream-stream and stream-table joins not yet implemented
- **Interactive Queries**: ❌ State store querying not yet implemented
- **Processor API**: ❌ Low-level processor API not yet implemented

## Implementation Priorities

### Phase 1: Foundation (Current Focus)
1. **Basic Streams DSL** - Stream/KStream abstraction
2. **Simple Transformations** - map, filter, flatMap
3. **State Store Interface** - Key-value store abstraction
4. **Topology Builder** - Define processing topology

### Phase 2: Core Features
5. **Windowing** - Time-based windowing operations
6. **Aggregations** - groupBy, count, reduce, aggregate
7. **Joins** - KStream-KStream, KStream-KTable joins
8. **Global State Stores** - Replicated state across instances

### Phase 3: Advanced Features
9. **Interactive Queries** - Query state stores
10. **Exactly-Once Semantics** - Transactional processing
11. **Timestamping** - Custom timestamp extractors
12. **Rebalancing** - Dynamic partition assignment

## Streams KIPs by Number

### KIP-28: Add a processor client
**Status**: ❌ Not Implemented  
Foundation for Kafka Streams - low-level processor API for stateful stream processing.

### KIP-63: Unify store and downstream caching in streams
**Status**: ❌ Not Implemented  
Unified caching layer for state stores to reduce write amplification.

### KIP-67: Queryable state for Kafka Streams
**Status**: ❌ Not Implemented  
Interactive queries - query state stores from outside the Streams application.

### KIP-77: Improve Kafka Streams Join Semantics
**Status**: ❌ Not Implemented  
Enhanced join semantics for stream-stream and stream-table joins.

### KIP-82: Add Record Headers
**Status**: ⚪ Deferred  
Record headers are handled by core client, Streams DSL needs to expose them.

### KIP-87: Add Compaction Tombstone Flag
**Status**: ⚪ Deferred  
Log compaction feature, handled at broker/client level.

### KIP-94: Add per-partition lag metrics to Kafka Consumer
**Status**: ⚪ Deferred  
Consumer metrics, tracked in core client.

### KIP-95: Incremental Batch Processing for Kafka Streams
**Status**: ❌ Not Implemented  
Optimization for batch processing in Streams.

### KIP-99: Add Global Tables to Kafka Streams
**Status**: ❌ Not Implemented  
Global state stores replicated to all instances for lookups.

### KIP-100: Change default stream grouping behavior
**Status**: ❌ Not Implemented  
Improved default partitioning for groupBy operations.

### KIP-101: Alter Replication Protocol to use Leader Epoch
**Status**: ⚪ Deferred  
Protocol-level change, handled by core client.

### KIP-105: Add --close-repartition-topic option to streams app reset tool
**Status**: ⚪ Deferred  
Tool-level feature, not library concern.

### KIP-106: Add recordContext to Transformer and ValueTransformer APIs
**Status**: ❌ Not Implemented  
Enhanced transformer API with access to record metadata.

### KIP-114: KTable materialization and improved semantics
**Status**: ❌ Not Implemented  
Improved KTable semantics and explicit materialization control.

### KIP-120: Cleanup Kafka Streams builder API
**Status**: ❌ Not Implemented  
Improved DSL API design.

### KIP-129: Streams Exactly-Once Semantics
**Status**: ❌ Not Implemented  
Exactly-once processing guarantees for Streams applications.

### KIP-138: Change punctuate semantics
**Status**: ❌ Not Implemented  
Improved punctuation (scheduled callbacks) in processor API.

### KIP-150: Add UUID serializer and deserializer
**Status**: ⚪ Deferred  
Serialization concern, can be implemented as separate serializers.

### KIP-153: Include only client traffic in BytesConsumed and BytesProduced metrics
**Status**: ⚪ Deferred  
Metrics refinement, tracked in core client.

### KIP-154: Add a new metric for total number of active tasks
**Status**: ❌ Not Implemented  
Streams task metrics.

### KIP-155: Add range scan for windowed state stores
**Status**: ❌ Not Implemented  
Windowed state store query API.

### KIP-159: Introducing Rich functions to Streams
**Status**: ❌ Not Implemented  
Enhanced DSL with richer function interfaces.

### KIP-160: Augment Streams DSL with Convenience Methods
**Status**: ❌ Not Implemented  
Additional convenience methods in DSL.

### KIP-161: Enhance Stream DSL with Returning Processor Names
**Status**: ❌ Not Implemented  
DSL methods return processor names for topology inspection.

### KIP-162: Global StreamsMetrics
**Status**: ❌ Not Implemented  
Global metrics accessor for Streams applications.

### KIP-169: Add Global Table to Kafka Connect
**Status**: ⚪ Deferred  
Kafka Connect feature, not Streams.

### KIP-171: Extend Consumer Group Reset Offset for Stream Application
**Status**: ⚪ Deferred  
Tool-level feature.

### KIP-172: Add Streams API Version Endpoint
**Status**: ❌ Not Implemented  
REST API for querying Streams version (optional feature).

### KIP-179: Rack aware partition assignment for Kafka Streams
**Status**: ❌ Not Implemented  
Rack-aware task assignment to minimize cross-rack traffic.

### KIP-182: Reduce Streams DSL overloads and allow easier use of custom storage engines
**Status**: ❌ Not Implemented  
Simplified DSL and pluggable state store backends.

### KIP-202: Add close(Duration) to Streams
**Status**: ❌ Not Implemented  
Graceful shutdown with timeout.

### KIP-205: Add getAllKeys() API to ReadOnlyKeyValueStore
**Status**: ❌ Not Implemented  
State store query API enhancement.

### KIP-213: Introduce TaskId and StreamsMetadata API changes
**Status**: ❌ Not Implemented  
Improved task identification and metadata APIs.

### KIP-220: Add AdminClient into Kafka Streams
**Status**: ❌ Not Implemented  
Expose AdminClient API within Streams for administrative operations.

### KIP-227: Introduce incremental FetchRequests to increase partition scalability
**Status**: ⚪ Deferred  
Protocol optimization, handled by core client.

### KIP-239: Add StreamsConfig prefix for Consumer and Producer properties
**Status**: ❌ Not Implemented  
Configuration namespace for embedded consumer/producer settings.

### KIP-244: Add Record Header support to Kafka Streams Processor API
**Status**: ❌ Not Implemented  
Processor API support for record headers.

### KIP-245: Use Properties instead of StreamsConfig in KafkaStreams constructor
**Status**: ❌ Not Implemented  
API design for configuration.

### KIP-251: Allow streams to wait for topics to be created
**Status**: ❌ Not Implemented  
Improved startup behavior when topics don't exist yet.

### KIP-258: Allow to Store Record Timestamps in RocksDB
**Status**: ❌ Not Implemented  
Timestamp storage in state stores.

### KIP-261: Add Single Value Fetch in Window Stores
**Status**: ❌ Not Implemented  
State store query optimization.

### KIP-270: Topology Naming in Streams DSL
**Status**: ❌ Not Implemented  
Named sub-topologies for better observability.

### KIP-274: Kafka Streams Skipping Corrupted Records
**Status**: ❌ Not Implemented  
Deserialization error handling strategy.

### KIP-276: Add StreamsBuilder overloads accepting Duration
**Status**: ❌ Not Implemented  
Use Duration type instead of long milliseconds.

### KIP-277: Fine Grained User Control of Streams Partition Assignment
**Status**: ❌ Not Implemented  
Custom partition assignment strategies.

### KIP-283: Allow users to start KafkaStreams from any point
**Status**: ❌ Not Implemented  
Flexible offset reset strategy on startup.

### KIP-284: Expose TopologyTestDriver to public API
**Status**: ❌ Not Implemented  
Testing utility for Streams topologies.

### KIP-289: Improve Streams DSL with Overloaded Functions
**Status**: ❌ Not Implemented  
DSL API improvements.

### KIP-291: Separating key and value deserializers in Kafka Streams
**Status**: ❌ Not Implemented  
Independent key and value deserializer configuration.

### KIP-307: Allow negative record timestamp
**Status**: ⚪ Deferred  
Timestamp handling, affects Streams timestamp extractors.

### KIP-308: Support for delegation tokens in Kafka Streams
**Status**: ⚪ Deferred  
Authentication feature, handled by core client.

### KIP-311: Kafka Streams configuration errors
**Status**: ❌ Not Implemented  
Better configuration validation and error messages.

### KIP-312: Add Overloads Accepting Duration to KafkaStreams
**Status**: ❌ Not Implemented  
Duration-based API overloads.

### KIP-319: Replace endOfCurrBatch with FixedSize batching strategy
**Status**: ❌ Not Implemented  
Buffering strategy for windowed operations.

### KIP-320: Allow fetchers to detect and handle log truncation
**Status**: ⚪ Deferred  
Consumer-level feature, handled by core client.

### KIP-321: Update Streams FSM to Allow for Smoother State Transitions
**Status**: ❌ Not Implemented  
State machine improvements for Streams application lifecycle.

### KIP-322: Add Consumer Group Id to StreamsBuilder
**Status**: ❌ Not Implemented  
Explicit consumer group configuration in DSL.

### KIP-328: Ability to suppress updates for KTables
**Status**: ❌ Not Implemented  
KTable suppression for controlling downstream updates.

### KIP-334: Update Kafka Streams Topology to support max.task.idle.ms
**Status**: ❌ Not Implemented  
Task idle configuration for better join behavior.

### KIP-345: Reduce number of rebalances in Kafka Streams
**Status**: ⚪ Deferred  
Related to cooperative rebalancing, tracked in core client.

### KIP-353: Improve Kafka Streams Configuration Documentation
**Status**: ❌ Not Implemented  
Documentation improvements.

### KIP-354: Add a Maximum Time to Wait for a KafkaStreams Instance to Be Ready
**Status**: ❌ Not Implemented  
Startup readiness check with timeout.

### KIP-356: Improve handling of unknown partitions in Streams
**Status**: ❌ Not Implemented  
Better error handling for partition metadata changes.

### KIP-358: Migrate Streams API to Duration instead of long ms times
**Status**: ❌ Not Implemented  
API modernization using Duration type throughout.

### KIP-363: Improve Kafka Streams Timestamp Synchronization
**Status**: ❌ Not Implemented  
Better timestamp handling across multiple input streams.

### KIP-372: Naming Repartition Topics for Joins and GroupBys
**Status**: ❌ Not Implemented  
Control over internal topic naming.

### KIP-378: Add support for null values in ToStream operation
**Status**: ❌ Not Implemented  
Allow null values in KTable-to-KStream conversion.

### KIP-401: Provide a configurable log appender in KafkaProducer
**Status**: ⚪ Deferred  
Producer feature, tracked in core client.

### KIP-405: Kafka Tiered Storage
**Status**: ⚪ Deferred  
Broker feature, not Streams-specific.

### KIP-418: A method-chaining way to branch KStream
**Status**: ❌ Not Implemented  
Improved branching API in DSL.

### KIP-425: Expose state stores via state stores factory
**Status**: ❌ Not Implemented  
Pluggable state store implementation.

### KIP-428: Add in-memory Streams State Stores
**Status**: ❌ Not Implemented  
Memory-backed state stores (alternative to RocksDB).

### KIP-429: Incremental Cooperative Rebalancing for Kafka Streams
**Status**: ⚪ Deferred  
Protocol feature, tracked in core client.

### KIP-437: Improve Stream Thread Exception Handling
**Status**: ❌ Not Implemented  
Better exception handling and error propagation.

### KIP-438: Add custom Partitioner API to Kafka Streams DSL
**Status**: ❌ Not Implemented  
Custom partitioning for produced records.

### KIP-441: Improve API for KafkaStreams Metrics and State
**Status**: ❌ Not Implemented  
Improved metrics and state query APIs.

### KIP-444: Augment Metrics for Kafka Streams
**Status**: ❌ Not Implemented  
Additional metrics for observability.

### KIP-447: Add cogroup in Kafka Streams DSL
**Status**: ❌ Not Implemented  
Co-grouping multiple streams for aggregation.

### KIP-450: Improve Streams Task Assignment for Large State
**Status**: ❌ Not Implemented  
Better task assignment strategy for stateful applications.

### KIP-457: Add support for versioned state stores
**Status**: ❌ Not Implemented  
State stores with versioning/time-travel capabilities.

### KIP-461: Add retry logic and handling for RocksDB unavailability
**Status**: ❌ Not Implemented  
Improved resilience for RocksDB state stores.

### KIP-463: Handling of Null Values in Streams Aggregations
**Status**: ❌ Not Implemented  
Explicit null value handling in aggregations.

### KIP-470: Add Java Serializers for UUID
**Status**: ⚪ Deferred  
Serializer utility, can be implemented separately.

### KIP-471: Expose RocksDB Metrics in Kafka Streams
**Status**: ❌ Not Implemented  
RocksDB internal metrics exposure.

### KIP-475: Expose Metrics in Streams Static Membership
**Status**: ❌ Not Implemented  
Static membership metrics for Streams.

### KIP-479: Add Materialized to TimeWindowedKStream
**Status**: ❌ Not Implemented  
Explicit state store materialization for windowed streams.

### KIP-481: SerDe improvements for POJOs
**Status**: ⚪ Deferred  
Java POJO serialization, Haskell will use different approach.

### KIP-482: Kafka Streams DSL for Hopping Windows
**Status**: ❌ Not Implemented  
Hopping window operations.

### KIP-485: Update KeystoreType default to JKS to match Java defaults
**Status**: ⚪ Deferred  
Java-specific default.

### KIP-491: Preferred Replica Fetching
**Status**: ⚪ Deferred  
Consumer feature, tracked in core client (KIP-392).

### KIP-500: Replace ZooKeeper with a Self-Managed Metadata Quorum
**Status**: ⚪ Deferred  
Broker architecture change (KRaft).

### KIP-535: Allow state stores to serve stale reads during rebalance
**Status**: ❌ Not Implemented  
Improved availability during rebalancing.

### KIP-557: Add emit on change support for Kafka Streams
**Status**: ❌ Not Implemented  
Control when to emit updates from stateful operations.

### KIP-562: Allow fetching a key from a single partition
**Status**: ❌ Not Implemented  
State store query optimization.

### KIP-563: Add close with timeout to Producer, Consumer and Admin
**Status**: ⚪ Deferred  
Core client feature, tracked separately.

### KIP-571: Add option to force a rebalance in Kafka Streams
**Status**: ❌ Not Implemented  
Programmatic rebalance trigger.

### KIP-572: Improve timeouts and retries in Kafka Streams Task shutdown
**Status**: ❌ Not Implemented  
Better shutdown behavior.

### KIP-591: Add Kafka Streams config to set default state store
**Status**: ❌ Not Implemented  
Default state store type configuration.

### KIP-613: Add end-to-end latency metrics to Streams
**Status**: ❌ Not Implemented  
Latency metrics for stream processing.

### KIP-614: Add Tombstone Support to KTable Suppress
**Status**: ❌ Not Implemented  
Tombstone handling in suppression.

### KIP-617: Allow Kafka Streams State Stores to be Iterated Backwards
**Status**: ❌ Not Implemented  
Reverse iteration over state stores.

### KIP-618: Exactly-Once Support for Source Connectors
**Status**: ⚪ Deferred  
Kafka Connect feature.

### KIP-623: Add the ability to run StreamsResetter on a topic
**Status**: ⚪ Deferred  
Tool feature.

### KIP-634: Allow configurations that allow a metric to be pushed to both client and cluster reporter
**Status**: ⚪ Deferred  
Metrics infrastructure.

### KIP-638: Serve Full List of Registered Custom SerDes
**Status**: ❌ Not Implemented  
SerDe registry query API.

### KIP-644: Rename Kafka Streams Config UPGRADE_FROM_CONFIG
**Status**: ❌ Not Implemented  
Configuration naming improvement.

### KIP-663: API to Start and Shut Down Stream Threads
**Status**: ❌ Not Implemented  
Dynamic thread management API.

### KIP-667: Add Strict API for defining Stream Windowing semantics
**Status**: ❌ Not Implemented  
Improved windowing API design.

### KIP-680: Semantics for TopologyTestDriver Producers and Consumers
**Status**: ❌ Not Implemented  
Testing framework enhancements.

### KIP-690: Add additional configuration to control MirrorMaker 2 internal topics naming convention
**Status**: ⚪ Deferred  
MirrorMaker feature.

### KIP-698: Add maxIdleTime configuration to IQv2
**Status**: ❌ Not Implemented  
Interactive queries v2 configuration.

### KIP-699: Streams Standby Task Consistency
**Status**: ❌ Not Implemented  
Standby task behavior improvements.

### KIP-715: Expose committed offsets in streams
**Status**: ❌ Not Implemented  
Expose consumer offsets to Streams application.

### KIP-725: Streamlining configurations for TimeWindowedDeserializer
**Status**: ❌ Not Implemented  
Configuration simplification.

### KIP-729: Add prefixScan to State Stores
**Status**: ❌ Not Implemented  
Prefix scan query for state stores.

### KIP-732: Deprecate eos-alpha and replace it with eos-v2
**Status**: ❌ Not Implemented  
Exactly-once semantics v2.

### KIP-740: Clean up public API for StreamsConfig
**Status**: ❌ Not Implemented  
Configuration API cleanup.

### KIP-761: Add Total Blocked Time Metric to Streams
**Status**: ❌ Not Implemented  
Thread blocking metrics.

### KIP-766: Add custom streams state query interface
**Status**: ❌ Not Implemented  
Custom query API for state stores.

### KIP-770: Replace request.timeout.ms with separate config entries for KafkaProducer and KafkaConsumer
**Status**: ⚪ Deferred  
Core client configuration.

### KIP-775: Custom partitioners in Kafka Streams
**Status**: ❌ Not Implemented  
Custom partition assignment for internal topics.

### KIP-779: Allow PromQL-like queries for State Store for StreamsMetadataState
**Status**: ❌ Not Implemented  
Query language for state metadata.

### KIP-796: Interactive Query v2
**Status**: ❌ Not Implemented  
Next-generation interactive queries API.

### KIP-800: Expose version of features in ApiVersionsResponse
**Status**: ⚪ Deferred  
Protocol feature.

### KIP-813: Shared State Stores
**Status**: ❌ Not Implemented  
Share state stores across multiple tasks.

### KIP-817: Expand log exception handler to tombstone records
**Status**: ❌ Not Implemented  
Exception handling for tombstones.

### KIP-820: Extend StreamJoined to allow more store configs
**Status**: ❌ Not Implemented  
Join configuration enhancements.

### KIP-826: Expose metrics for punctuate call latency
**Status**: ❌ Not Implemented  
Punctuation metrics.

### KIP-830: Allow disabling JMX in Kafka
**Status**: ⚪ Deferred  
Metrics infrastructure, Haskell won't use JMX.

### KIP-834: Pause and Resume KafkaStreams Topologies
**Status**: ❌ Not Implemented  
Runtime topology control.

### KIP-837: Allow MultiCasting A Result Record
**Status**: ❌ Not Implemented  
Send result to multiple downstream processors.

### KIP-841: Kafka Streams Cooperative Rebalancing Protocol Upgrade
**Status**: ⚪ Deferred  
Protocol upgrade, tracked in core client.

### KIP-844: Querying Partition Lag in Kafka Streams
**Status**: ❌ Not Implemented  
Partition lag metrics/queries.

### KIP-854: Separate network thread for Kafka Streams
**Status**: ❌ Not Implemented  
Threading model improvement.

### KIP-860: Add allowUpdates option for Kafka Streams log compaction
**Status**: ❌ Not Implemented  
Changelog topic configuration.

### KIP-862: Introduce SubscriptionPattern to allow dynamically providing topics for subscription
**Status**: ❌ Not Implemented  
Dynamic topic subscription patterns.

### KIP-869: Enable follower fetching in Kafka Streams
**Status**: ⚪ Deferred  
Related to rack-aware fetching (KIP-392), tracked in core client.

### KIP-874: Kafka Streams Sticky Group Assignment
**Status**: ❌ Not Implemented  
Sticky task assignment to minimize rebalancing.

### KIP-875: First-class offsets support in Kafka Connect
**Status**: ⚪ Deferred  
Kafka Connect feature.

### KIP-889: Versioned State Stores
**Status**: ❌ Not Implemented  
Time-travel and versioning for state stores.

### KIP-900: Deprecate WindowStore#put(K key, V value, long windowStartTimestamp)
**Status**: ❌ Not Implemented  
API deprecation and replacement.

### KIP-904: Automatic Configuration of Number of Kafka Streams Threads
**Status**: ❌ Not Implemented  
Auto-scaling thread count based on partition count.

### KIP-924: customizable task assignment for Kafka Streams
**Status**: ❌ Not Implemented  
Pluggable task assignment strategies.

### KIP-932: Queues for Kafka
**Status**: ⚪ Deferred  
Share groups feature, tracked in core client.

### KIP-950: Serde for Java Records
**Status**: ⚪ Deferred  
Java-specific serialization.

### KIP-954: expand default DSL store configuration to custom types
**Status**: ❌ Not Implemented  
Extended store configuration in DSL.

### KIP-955: Add stream-table join on foreign key
**Status**: ❌ Not Implemented  
Foreign key joins between streams and tables.

### KIP-960: Support single-key interactive queries (IQv2) for versioned state stores
**Status**: ❌ Not Implemented  
Versioned state store queries.

### KIP-962: Relax non-null key requirement in Kafka Streams
**Status**: ❌ Not Implemented  
Allow null keys in certain operations.

### KIP-968: Support custom metadata for state stores
**Status**: ❌ Not Implemented  
Attach custom metadata to state stores.

### KIP-969: Introduce TaskAssignor callback for state (partition) restoration
**Status**: ❌ Not Implemented  
Task assignment callback during restoration.

### KIP-976: Streams IQ exception handler
**Status**: ❌ Not Implemented  
Exception handling for interactive queries.

### KIP-978: Allow disabling all client-side metrics
**Status**: ⚪ Deferred  
Metrics configuration, tracked in core client.

### KIP-983: Client Customizable DNS Lookup
**Status**: ⚪ Deferred  
Core client feature.

### KIP-985: Add reverseRange and reverseAll query over kv-store in IQv2
**Status**: ❌ Not Implemented  
Reverse queries for interactive queries.

### KIP-992: Proposal: Support Materialized Views in Streams
**Status**: ❌ Not Implemented  
Materialized view abstraction.

### KIP-1002: Allow unregister Task Restore Listener in Streams
**Status**: ❌ Not Implemented  
Lifecycle management for restore listeners.

### KIP-1019: Expose metrics when an instance is not in rebalancing state
**Status**: ❌ Not Implemented  
Metrics during stable state.

### KIP-1022: Configurable limit to number of recorded exceptions in streams
**Status**: ❌ Not Implemented  
Exception history configuration.

### KIP-1023: An alternative to StreamsPartitionAssignor
**Status**: ❌ Not Implemented  
New partition assignment strategy.

### KIP-1034: Expose Materialized Serde Methods
**Status**: ❌ Not Implemented  
Access configured SerDes from materialized stores.

### KIP-1035: StateStore managed changelog
**Status**: ❌ Not Implemented  
Streams manages changelog topics for state stores.

### KIP-1037: Allow GlobalKTable to inherit TIMESTAMP
**Status**: ❌ Not Implemented  
Timestamp handling for global tables.

### KIP-1038: Add Custom Error Handler for Task
**Status**: ❌ Not Implemented  
Per-task error handling.

### KIP-1047: Add Sliding Window support for Stream-Stream join
**Status**: ❌ Not Implemented  
Sliding window joins.

### KIP-1048: Delegation token creation and renewal should be principal aware
**Status**: ⚪ Deferred  
Authentication feature, tracked in core client.

### KIP-1068: Expand support for null values in changelog and repartition topics
**Status**: ❌ Not Implemented  
Null value handling in internal topics.

### KIP-1084: Expose Streams internal topics naming convention
**Status**: ❌ Not Implemented  
Programmatic access to internal topic names.

### KIP-1098: Asynchronous Processing Support for Kafka Streams
**Status**: ❌ Not Implemented  
Async processing model for Streams.

### KIP-1133: Refresh Streams version probing in cooperative assignor
**Status**: ❌ Not Implemented  
Version negotiation improvements.

### KIP-1179: Introduce TaskId#subtopology to StreamsMetadata
**Status**: ❌ Not Implemented  
Subtopology information in metadata.

### KIP-1185: Add getRangeAll API to WindowStore in IQv2
**Status**: ❌ Not Implemented  
Range query API for window stores.

## Summary

The kafka-streams package is in **early development**. This tracking document will be updated as features are implemented.

**Current Focus**: Establishing the foundation for Streams processing in Haskell, starting with basic DSL and state store abstractions.

**Architecture Notes**:
- Haskell Streams implementation will be idiomatic while maintaining compatibility with Kafka protocol
- State stores will leverage Haskell's strong type system and immutability
- DSL design will use Haskell's functional programming patterns (monads, applicatives)
- Performance will leverage lazy evaluation and efficient data structures

**Contributing**: See the main [README.md](../README.md) for contribution guidelines.

