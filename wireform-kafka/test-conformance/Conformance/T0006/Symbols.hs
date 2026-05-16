{-# LANGUAGE OverloadedStrings #-}

{-|
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

import Test.Tasty
import Test.Tasty.HUnit

-- Top-level umbrella (re-exports producer / consumer / transaction
-- entry points the user is most likely to reach for):
import qualified Kafka

-- Lower-level surface, one import per top-level namespace; these
-- imports are the smoke test.
import qualified Kafka.Client.AdminClient ()
import qualified Kafka.Client.Consumer ()
import qualified Kafka.Client.Group ()
import qualified Kafka.Client.Metadata ()
import qualified Kafka.Client.Pipeline ()
import qualified Kafka.Client.Producer ()
import qualified Kafka.Client.Transaction ()
import qualified Kafka.Compression.Gzip ()
import qualified Kafka.Compression.Lz4 ()
import qualified Kafka.Compression.Snappy ()
import qualified Kafka.Compression.Types ()
import qualified Kafka.Compression.Zstd ()
import qualified Kafka.Network.Auth.AwsMskIam ()
import qualified Kafka.Network.Auth.OAuthBearer ()
import qualified Kafka.Network.Auth.Plain ()
import qualified Kafka.Network.Auth.SASL ()
import qualified Kafka.Network.Auth.Scram ()
import qualified Kafka.Network.Connection ()
import qualified Kafka.Protocol.ApiVersions ()
import qualified Kafka.Protocol.CRC32C ()
import qualified Kafka.Protocol.Wire ()
import qualified Kafka.Protocol.Message ()
import qualified Kafka.Protocol.Primitives ()
import qualified Kafka.Protocol.RecordBatch ()
import qualified Kafka.Telemetry.OpenTelemetry ()

tests :: TestTree
tests = testGroup "0006-symbols"
  [ testCase "umbrella module is importable" $ do
      -- 'Kafka' is the user-facing entry point; touching it forces
      -- GHC to resolve its re-exports at link time. If any of the
      -- imports above are missing, the file would not have compiled.
      let _ = Kafka.defaultProducerConfig
          _ = Kafka.defaultConsumerConfig
      pure ()
  ]
