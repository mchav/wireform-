{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{- |
Module      : Conformance.T0006.Symbols
Description : librdkafka @tests\/0006-symbols.c@ — exported symbol smoke test

librdkafka's @0006-symbols@ links a tiny TU that calls every public
@rd_kafka_*@ function once, just to confirm that the dynamic library
exposes them. Our analogue is 'Kafka' (the umbrella module) plus the
individual @Kafka.Client.*@ entry points: the equivalent check is
"every API surface advertised in our README compiles when imported".

This test is therefore a build-time check disguised as a runtime test:
if any of the imports below fail to resolve, the suite won't link.
-}
module Conformance.T0006.Symbols (tests) where

-- Top-level umbrella (re-exports producer / consumer / transaction
-- entry points the user is most likely to reach for):
import Kafka qualified
-- Lower-level surface, one import per top-level namespace; these
-- imports are the smoke test.
import Kafka.Client.AdminClient qualified ()
import Kafka.Client.Consumer qualified ()
import Kafka.Client.Group qualified ()
import Kafka.Client.Metadata qualified ()
import Kafka.Client.Pipeline qualified ()
import Kafka.Client.Producer qualified ()
import Kafka.Client.Transaction qualified ()
import Kafka.Compression.Gzip qualified ()
import Kafka.Compression.Lz4 qualified ()
import Kafka.Compression.Snappy qualified ()
import Kafka.Compression.Types qualified ()
import Kafka.Compression.Zstd qualified ()
import Kafka.Network.Auth.AwsMskIam qualified ()
import Kafka.Network.Auth.OAuthBearer qualified ()
import Kafka.Network.Auth.Plain qualified ()
import Kafka.Network.Auth.SASL qualified ()
import Kafka.Network.Auth.Scram qualified ()
import Kafka.Network.Connection qualified ()
import Kafka.Protocol.ApiVersions qualified ()
import Kafka.Protocol.CRC32C qualified ()
import Kafka.Protocol.RecordBatch qualified ()
import Kafka.Telemetry.OpenTelemetry qualified ()
import Test.Syd
import "wireform-kafka-protocol" Kafka.Protocol.Message qualified ()
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified ()
import "wireform-kafka-protocol" Kafka.Protocol.Wire qualified ()


tests :: Spec
tests =
  describe "0006-symbols" $
    sequence_
      [ it "umbrella module is importable" $ do
          -- 'Kafka' is the user-facing entry point; touching it forces
          -- GHC to resolve its re-exports at link time. If any of the
          -- imports above are missing, the file would not have compiled.
          let _ = Kafka.defaultProducerConfig
              _ = Kafka.defaultConsumerConfig
          pure () :: IO ()
      ]
