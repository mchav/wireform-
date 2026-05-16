-- |
-- Module      : Kafka.Streams.Serde
-- Description : Re-export of the shared @Kafka.Serde@ machinery
--
-- The base 'Serde' type and the standard built-ins live in
-- "Kafka.Serde" so that the client-side producer / consumer can
-- use them too. This module re-exports the whole surface so
-- existing @import Kafka.Streams.Serde@ call sites continue to
-- compile unchanged.
--
-- Streams-specific extensions (Confluent Schema Registry, Avro /
-- Protobuf / JSON-Schema envelopes, windowed-key framing) live in
-- the @Kafka.Streams.Serde.*@ sub-modules.
module Kafka.Streams.Serde
  ( module Kafka.Serde
  ) where

import Kafka.Serde
