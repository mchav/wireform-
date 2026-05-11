{-|
Module      : Kafka
Description : One-stop import for the wireform-kafka client.
Copyright   : (c) 2025
License     : BSD-3-Clause

This umbrella module is the recommended starting point. It
re-exports the high-level client surface — producers, consumers,
consumer groups, and transactions — so that a typical application
only needs

@
import qualified Kafka
@

= If you're new to Kafka

Kafka is an append-only log service. You /produce/ records into
named /topics/, and one or more independent /consumers/ /poll/ those
records back out in order. There are two long-running roles you can
play:

  * __producer__ — opens a connection, holds onto it, and publishes
    records (key, value, headers, optionally a partition) into one or
    more topics. See "Kafka.Client.Producer".
  * __consumer__ — opens a connection, joins a /consumer group/ that
    shares the work across many processes, and reads records out one
    at a time. See "Kafka.Client.Group" for the high-level
    \"call this handler per record\" API, or "Kafka.Client.Consumer"
    if you want to drive the poll loop yourself.

If you need stateful stream processing on top — windowed
aggregations, joins, exactly-once side effects — see the
"Kafka.Streams" package.

= Sending a record (producer)

@
import qualified Kafka

main :: IO ()
main =
  Kafka.'withProducer' [\"localhost:9092\"] Kafka.'defaultProducerConfig' $ \\p -> do
    _ \<- Kafka.'sendMessage' p \"my-topic\" Nothing \"hello\"
    pure ()
@

'withProducer' is a 'Control.Exception.bracket': it builds the
producer, runs your body, and flushes + closes on the way out even
if you throw.

= Receiving records (high-level consumer)

@
import qualified Kafka

main :: IO ()
main =
  Kafka.'runConsumer'
    Kafka.'defaultGroupConfig'
      { Kafka.'bootstrapBrokers' = [\"localhost:9092\"]
      , Kafka.'groupId'          = \"my-service\"
      , Kafka.'topics'           = [\"events\"]
      }
    $ \\rec ->
        putStrLn (show (Kafka.'crKey' rec, Kafka.'crValue' rec))
@

'runConsumer' joins the group, runs your handler once per record,
commits offsets when you return successfully, and leaves the group
on the way out. For higher throughput where per-record overhead
matters, use 'runBatchedConsumer'; for full control of the poll
loop, drop down to 'withConsumer' from "Kafka.Client.Consumer".

= Transactions

To make 'sendMessage' calls part of an atomic group with consumer
offsets, see "Kafka.Client.Transaction".

= Where the rest of the package lives

  * "Kafka.Client.AdminClient" — create / delete / describe topics,
    groups, ACLs, configs.
  * "Kafka.Streams" — Streams DSL (KStream, KTable, joins, windows,
    transactional state).
  * "Kafka.Compression.*" — gzip / snappy / lz4 / zstd codecs.
  * "Kafka.Network.Connection" / "Kafka.Network.Auth.*" — TLS,
    SASL\/PLAIN\/SCRAM\/OAUTHBEARER\/AWS-MSK-IAM, low-level
    connection control.
  * "Kafka.Telemetry.OpenTelemetry" — W3C Trace Context
    propagation across producer \/ consumer hops (SDK-independent;
    bring your own tracer for span creation).
  * "Kafka.Protocol.*" / @Kafka.Protocol.Generated.*@ — raw wire
    primitives and one module per Kafka request \/ response (for
    custom tooling).
  * "Kafka.Client.Simple" — a single-broker, single-record client
    intended for debugging and unit-test scaffolding, not for
    production.

The full guided tour lives in @TUTORIAL.md@; a plain-language Kafka
primer is in @CONCEPTS.md@.
-}
module Kafka
  ( -- * High-level producer
    module Kafka.Client.Producer
    -- * High-level consumer
  , module Kafka.Client.Consumer
    -- * \"Call this handler per record\" consumer group runner
  , module Kafka.Client.Group
    -- * Transactional producer
  , module Kafka.Client.Transaction
  ) where

import Kafka.Client.Consumer
import Kafka.Client.Group hiding
  -- 'Kafka.Client.Group.currentAssignment' takes a 'GroupConsumer';
  -- the 'Kafka.Client.Consumer.currentAssignment' (re-exported above)
  -- takes the lower-level 'Consumer'. Both are useful, but two
  -- different functions can't share an unqualified name in the
  -- umbrella; users who want the high-level one should import
  -- "Kafka.Client.Group" qualified.
  ( currentAssignment
  )
import Kafka.Client.Producer
import Kafka.Client.Transaction
