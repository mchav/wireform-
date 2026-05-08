{-|
Module      : Kafka
Description : Pure-Haskell native Kafka client umbrella module
Copyright   : (c) 2025
License     : BSD-3-Clause

This is the convenience entry point for the @wireform-kafka@ package — a
fully native, pure-Haskell client implementation of the Apache Kafka
wire protocol.

The package is layered:

* "Kafka.Protocol.Primitives" — wire primitives (varints, compact strings,
  fixed-width integers, tagged fields).
* "Kafka.Protocol.RecordBatch" — RecordBatch v2 framing (CRC, header, key /
  value framing, transaction markers).
* "Kafka.Protocol.CRC32C" — Castagnoli CRC32C with hardware acceleration.
* "Kafka.Protocol.Encoding" — typeclasses and helpers shared by both
  hand-written and code-generated message modules.
* "Kafka.Protocol.ApiVersions" — version negotiation against the broker's
  @ApiVersionsResponse@.
* @Kafka.Protocol.Generated.*@ — code-generated request \/ response message
  modules, one per Kafka API key. These are emitted by the
  @kafka-codegen@ executable from the upstream JSON message definitions
  in @kafka\/clients\/src\/main\/resources\/common\/message@.
* "Kafka.Network.Connection" — TCP \/ TLS \/ SASL connection state.
* @Kafka.Network.Auth.*@ — SASL mechanisms (PLAIN, SCRAM-SHA-256\/512).
* @Kafka.Compression.*@ — gzip \/ snappy \/ lz4 \/ zstd record-batch codecs.
* @Kafka.Client.*@ — high-level @Producer@ \/ @Consumer@ \/ @AdminClient@ \/
  @Transaction@ surface re-exported below.
* "Kafka.Telemetry.OpenTelemetry" — semantic-convention instrumentation.

= Quick start: producer

@
import qualified Kafka

main :: IO ()
main = do
  Right p <- Kafka.createProducer [\"localhost:9092\"] Kafka.defaultProducerConfig
  _ <- Kafka.sendMessage p \"my-topic\" Nothing \"hello\"
  Kafka.closeProducer p
@

= Quick start: consumer (high-level group API)

For just-give-me-records consumption, use "Kafka.Client.Group" (also
re-exported as @Wireform.Kafka.Group@ from the umbrella package). It
hides FindCoordinator \/ JoinGroup \/ SyncGroup \/ OffsetFetch \/
heartbeat \/ commit behind a single bracket:

@
import qualified Kafka.Client.Group as Group

main :: IO ()
main =
  Group.runConsumer
    Group.defaultGroupConfig
      { Group.gcBootstrapBrokers = [\"localhost:9092\"]
      , Group.gcGroupId          = \"my-service\"
      , Group.gcTopics           = [\"events\"]
      }
    $ \\rec -> putStrLn (show (Kafka.crKey rec, Kafka.crValue rec))
@

The lower-level "Kafka.Client.Consumer" (re-exported below) is still
available for cases where you want to drive the poll loop or own
offset management yourself.
-}
module Kafka
  ( -- * High-level producer
    module Kafka.Client.Producer
    -- * High-level consumer
  , module Kafka.Client.Consumer
    -- * Transactional producer
  , module Kafka.Client.Transaction
  ) where

import Kafka.Client.Producer
import Kafka.Client.Consumer
import Kafka.Client.Transaction
