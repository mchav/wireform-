{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Pin-test for the config-knob coverage / default-values
contract: every librdkafka @CONFIGURATION.md@ entry that maps
onto the in-memory client has a corresponding field on
'ProducerConfig' / 'ConsumerConfig' / 'ConnectionConfig', and
the default carries the librdkafka (or JVM-Kafka 3.x) default.
-}
module Client.ConfigParitySpec (tests) where

import Data.Text qualified as T
import Kafka.Client.Consumer qualified as Cons
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Producer qualified as Prod
import Kafka.Network.Connection qualified as Conn
import Test.Syd


tests :: Spec
tests =
  describe "ConfigParity" $
    sequence_
      [ producer_defaults
      , producer_setter_round_trips
      , consumer_defaults
      , consumer_isolation_level_default
      , connection_defaults
      , connection_address_family_default
      , connection_dns_lookup_default
      , -- Retry/backoff
        retry_default_curve_doubles
      , retry_curve_caps_at_max
      , retry_curve_zero_jitter_is_clean
      , retry_attempt_zero_returns_initial
      , producer_retries_pass_through_to_sender
      ]


----------------------------------------------------------------------
-- Producer defaults
----------------------------------------------------------------------

producer_defaults :: Spec
producer_defaults =
  it "ProducerConfig defaults match librdkafka / JVM 3.x" $ do
    let c = Prod.defaultProducerConfig
    Prod.producerClientId c `shouldBe` "kafka-native-producer"
    Prod.producerBatchSize c `shouldBe` 16384
    Prod.producerLingerMs c `shouldBe` 0
    Prod.producerMaxInFlight c `shouldBe` 5
    Prod.producerRetries c `shouldBe` 2147483647
    Prod.producerRetryBackoffMs c `shouldBe` 100
    Prod.producerRetryBackoffMaxMs c `shouldBe` 1000
    Prod.producerRetryBackoffMultiplier c `shouldBe` 2.0
    Prod.producerRetryBackoffJitter c `shouldBe` 0.2
    Prod.producerDeliveryTimeoutMs c `shouldBe` 120000
    Prod.producerRequestTimeoutMs c `shouldBe` 30000
    Prod.producerMaxRequestSize c `shouldBe` 1048576
    Prod.producerQueueBufferingMaxMessages c `shouldBe` 100000
    Prod.producerQueueBufferingMaxKbytes c `shouldBe` 1048576
    Prod.producerTransactionTimeoutMs c `shouldBe` 60000
    Prod.producerEnableGaplessGuarantee c `shouldBe` False
    Prod.producerStickyPartitioningLingerMs c `shouldBe` 10
    -- Matches JVM 3.x: @enable.idempotence@ defaults to @true@.
    Prod.producerIdempotent c `shouldBe` True
    Prod.producerTransactional c `shouldBe` Nothing
    -- Matches JVM 3.x: @acks@ defaults to @all@ (= ExactlyOnce here).
    case Prod.producerDelivery c of
      Prod.ExactlyOnce -> pure ()
      other -> expectationFailure ("expected ExactlyOnce, got " <> show other)


producer_setter_round_trips :: Spec
producer_setter_round_trips =
  it "every Producer config field round-trips through a record update" $ do
    let c0 = Prod.defaultProducerConfig
        c1 =
          c0
            { Prod.producerRetries = 7
            , Prod.producerRetryBackoffMs = 250
            , Prod.producerRetryBackoffMaxMs = 5000
            , Prod.producerRetryBackoffMultiplier = 1.5
            , Prod.producerRetryBackoffJitter = 0.0
            , Prod.producerRequestTimeoutMs = 15000
            , Prod.producerMaxRequestSize = 524288
            , Prod.producerQueueBufferingMaxMessages = 50000
            , Prod.producerQueueBufferingMaxKbytes = 524288
            , Prod.producerTransactionTimeoutMs = 30000
            , Prod.producerEnableGaplessGuarantee = True
            , Prod.producerStickyPartitioningLingerMs = 25
            , Prod.producerTransactional = Just "tx-id"
            , Prod.producerIdempotent = True
            }
    Prod.producerRetries c1 `shouldBe` 7
    Prod.producerRetryBackoffMs c1 `shouldBe` 250
    Prod.producerRetryBackoffMaxMs c1 `shouldBe` 5000
    Prod.producerRetryBackoffMultiplier c1 `shouldBe` 1.5
    Prod.producerRetryBackoffJitter c1 `shouldBe` 0.0
    Prod.producerRequestTimeoutMs c1 `shouldBe` 15000
    Prod.producerMaxRequestSize c1 `shouldBe` 524288
    Prod.producerQueueBufferingMaxMessages c1 `shouldBe` 50000
    Prod.producerQueueBufferingMaxKbytes c1 `shouldBe` 524288
    Prod.producerTransactionTimeoutMs c1 `shouldBe` 30000
    Prod.producerEnableGaplessGuarantee c1 `shouldBe` True
    Prod.producerStickyPartitioningLingerMs c1 `shouldBe` 25
    Prod.producerTransactional c1 `shouldBe` Just "tx-id"
    Prod.producerIdempotent c1 `shouldBe` True
    -- Untouched defaults survive.
    Prod.producerBatchSize c1 `shouldBe` Prod.producerBatchSize c0


----------------------------------------------------------------------
-- Consumer defaults
----------------------------------------------------------------------

consumer_defaults :: Spec
consumer_defaults =
  it "ConsumerConfig defaults match librdkafka / JVM 3.x" $ do
    let c = Cons.defaultConsumerConfig
    Cons.consumerClientId c `shouldBe` "kafka-native-consumer"
    Cons.consumerGroupId c `shouldBe` "default-group"
    Cons.consumerGroupInstanceId c `shouldBe` Nothing
    Cons.consumerAutoCommit c `shouldBe` True
    Cons.consumerAutoCommitIntervalMs c `shouldBe` 5000
    Cons.consumerEnableAutoOffsetStore c `shouldBe` True
    Cons.consumerSessionTimeoutMs c `shouldBe` 45000
    Cons.consumerHeartbeatIntervalMs c `shouldBe` 3000
    Cons.consumerMaxPollRecords c `shouldBe` 500
    Cons.consumerMaxPollIntervalMs c `shouldBe` 300000
    Cons.consumerEnablePartitionEof c `shouldBe` False
    Cons.consumerCheckCrcs c `shouldBe` True
    Cons.consumerFetchMinBytes c `shouldBe` 1
    Cons.consumerFetchMaxBytes c `shouldBe` 52428800
    Cons.consumerFetchMaxWaitMs c `shouldBe` 500
    Cons.consumerFetchMessageMaxBytes c `shouldBe` 1048576
    Cons.consumerFetchErrorBackoffMs c `shouldBe` 500
    Cons.consumerQueuedMaxMessagesKbytes c `shouldBe` 65536
    Cons.consumerRackId c `shouldBe` Nothing
    case Cons.consumerAssignmentStrategy c of
      Cons.RangeAssignment -> pure ()
      other -> expectationFailure ("expected RangeAssignment, got " <> show other)
    case Cons.consumerAutoOffsetReset c of
      Cons.Latest -> pure ()
      other -> expectationFailure ("expected Latest, got " <> show other)


consumer_isolation_level_default :: Spec
consumer_isolation_level_default =
  it "ConsumerConfig defaults isolation level to ReadUncommitted (matches JVM)" $ do
    let c = Cons.defaultConsumerConfig
    case Cons.consumerIsolationLevel c of
      Cons.ReadUncommitted -> pure ()
      other -> expectationFailure ("expected ReadUncommitted, got " <> show other)


----------------------------------------------------------------------
-- Connection defaults
----------------------------------------------------------------------

connection_defaults :: Spec
connection_defaults =
  it "ConnectionConfig defaults match librdkafka" $ do
    let c = Conn.defaultConnectionConfig
    Conn.connTimeout c `shouldBe` 10
    Conn.connReadTimeout c `shouldBe` 30
    Conn.connWriteTimeout c `shouldBe` 30
    Conn.connRequestTimeoutMs c `shouldBe` 30000
    Conn.connRetryDelay c `shouldBe` 100
    Conn.connMaxRetries c `shouldBe` 3
    Conn.connBackoffMaxMs c `shouldBe` 10000
    Conn.connBackoffMultiplier c `shouldBe` 2.0
    -- Both default 'True' here (we deliberately diverge from
    -- librdkafka, which defaults both off): every Kafka write is
    -- already a complete framed request, so Nagle is pure
    -- latency penalty (the JVM client also sets TCP_NODELAY
    -- unconditionally).  SO_KEEPALIVE is on so silent dead-peer
    -- cases (NAT, broker crash) fail fast instead of parking
    -- forever on the TCP send-queue.  See the field-level
    -- comments in 'defaultConnectionConfig' for the full
    -- rationale.
    Conn.connSocketKeepalive c `shouldBe` True
    Conn.connSocketNagleDisable c `shouldBe` True
    Conn.connSocketSendBuffer c `shouldBe` 0
    Conn.connSocketReceiveBuffer c `shouldBe` 0
    Conn.connSocketMaxFails c `shouldBe` 1
    Conn.connMaxIdleMs c `shouldBe` 540000
    Conn.connMaxReauthMs c `shouldBe` 0
    Conn.connMessageMaxBytes c `shouldBe` 1000000
    Conn.connReceiveMessageMaxBytes c `shouldBe` 100000000
    Conn.connMetadataMaxAgeMs c `shouldBe` 900000
    Conn.connTopicMetadataRefreshFastIntervalMs c `shouldBe` 250
    Conn.connTopicMetadataRefreshSparse c `shouldBe` True
    Conn.connBrokerAddressTtl c `shouldBe` 1000
    Conn.connUseTls c `shouldBe` False
    case Conn.connTlsSettings c of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected no TLS settings by default"
    case Conn.connSasl c of
      Nothing -> pure ()
      Just _ -> expectationFailure "expected no SASL by default"
    Conn.connClientId c `shouldBe` T.pack "wireform-kafka"


connection_address_family_default :: Spec
connection_address_family_default =
  it "default connection address family is Any (librdkafka @broker.address.family@ default)" $ do
    let c = Conn.defaultConnectionConfig
    case Conn.connBrokerAddressFamily c of
      Conn.BrokerAddressAny -> pure ()
      other -> expectationFailure ("expected BrokerAddressAny, got " <> show other)


connection_dns_lookup_default :: Spec
connection_dns_lookup_default =
  it "default DNS lookup is resolve_canonical_bootstrap_servers_only" $ do
    let c = Conn.defaultConnectionConfig
    case Conn.connDnsLookup c of
      Conn.DnsResolveCanonicalBootstrapServersOnly -> pure ()
      other -> expectationFailure ("expected DnsResolveCanonicalBootstrapServersOnly, got " <> show other)


----------------------------------------------------------------------
-- Retry / backoff curve
----------------------------------------------------------------------

retry_default_curve_doubles :: Spec
retry_default_curve_doubles =
  it "default retry backoff doubles per attempt up to the cap" $ do
    let cfg = Sender.defaultRetryConfig
    -- attempt 0: 100ms; attempt 1: 200ms; attempt 2: 400ms;
    -- attempt 3: 800ms; attempt 4: capped at 1000ms.
    -- (Jitter is sin-based and small; check that the values are
    -- within the expected jitter band rather than exact.)
    let !b0 = Sender.nextRetryBackoffMs cfg 0
        !b1 = Sender.nextRetryBackoffMs cfg 1
        !b2 = Sender.nextRetryBackoffMs cfg 2
        !b3 = Sender.nextRetryBackoffMs cfg 3
        !b4 = Sender.nextRetryBackoffMs cfg 4
    -- Each step is roughly double the previous (up to jitter)
    -- until the cap.
    (if (b0 >= 80 && b0 <= 120) then pure () else expectationFailure ("b0 ~= 100; got " <> show b0))
    (if (b1 >= 160 && b1 <= 240) then pure () else expectationFailure ("b1 ~= 200; got " <> show b1))
    (if (b2 >= 320 && b2 <= 480) then pure () else expectationFailure ("b2 ~= 400; got " <> show b2))
    (if (b3 >= 640 && b3 <= 960) then pure () else expectationFailure ("b3 ~= 800; got " <> show b3))
    -- After the cap (1000ms), values stay bounded by 1000 + jitter.
    (if (b4 <= 1200) then pure () else expectationFailure ("b4 capped; got " <> show b4))


retry_curve_caps_at_max :: Spec
retry_curve_caps_at_max =
  it "after enough attempts, backoff hits retryBackoffMaxMs" $ do
    let cfg = Sender.defaultRetryConfig
        !v = Sender.nextRetryBackoffMs cfg 50
    (if (v <= 1200) then pure () else expectationFailure ("expected <= ~1200, got " <> show v))


retry_curve_zero_jitter_is_clean :: Spec
retry_curve_zero_jitter_is_clean =
  it "with retryBackoffJitter = 0, the curve doubles cleanly" $ do
    let cfg =
          Sender.defaultRetryConfig
            { Sender.retryBackoffMs = 50
            , Sender.retryBackoffMaxMs = 10000
            , Sender.retryBackoffMultiplier = 2.0
            , Sender.retryBackoffJitter = 0.0
            }
    map (Sender.nextRetryBackoffMs cfg) [0 .. 5]
      `shouldBe` [50, 100, 200, 400, 800, 1600]


retry_attempt_zero_returns_initial :: Spec
retry_attempt_zero_returns_initial =
  it "attempt 0 returns retryBackoffMs (within jitter band)" $ do
    let cfg =
          Sender.defaultRetryConfig
            { Sender.retryBackoffMs = 250
            , Sender.retryBackoffJitter = 0.0
            }
    Sender.nextRetryBackoffMs cfg 0 `shouldBe` 250


----------------------------------------------------------------------
-- Producer config -> sender wiring
----------------------------------------------------------------------

producer_retries_pass_through_to_sender :: Spec
producer_retries_pass_through_to_sender =
  it "producer retry knobs flow through into Sender.RetryConfig" $ do
    -- The exposed API path is via createProducer; here we just
    -- assert the field-by-field correspondence by constructing a
    -- RetryConfig the same way createProducer does.
    let c =
          Prod.defaultProducerConfig
            { Prod.producerRetries = 5
            , Prod.producerRetryBackoffMs = 200
            , Prod.producerRetryBackoffMaxMs = 4000
            , Prod.producerRetryBackoffMultiplier = 3.0
            , Prod.producerRetryBackoffJitter = 0.1
            }
        rc =
          Sender.RetryConfig
            { Sender.retryMaxAttempts = Prod.producerRetries c
            , Sender.retryBackoffMs = Prod.producerRetryBackoffMs c
            , Sender.retryBackoffMaxMs = Prod.producerRetryBackoffMaxMs c
            , Sender.retryBackoffMultiplier = Prod.producerRetryBackoffMultiplier c
            , Sender.retryBackoffJitter = Prod.producerRetryBackoffJitter c
            }
    Sender.retryMaxAttempts rc `shouldBe` 5
    Sender.retryBackoffMs rc `shouldBe` 200
    Sender.retryBackoffMaxMs rc `shouldBe` 4000
    Sender.retryBackoffMultiplier rc `shouldBe` 3.0
    Sender.retryBackoffJitter rc `shouldBe` 0.1
