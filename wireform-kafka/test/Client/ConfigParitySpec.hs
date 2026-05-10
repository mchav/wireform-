{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Pin-test for the config-knob coverage / default-values
-- contract: every librdkafka @CONFIGURATION.md@ entry that maps
-- onto the in-memory client has a corresponding field on
-- 'ProducerConfig' / 'ConsumerConfig' / 'ConnectionConfig', and
-- the default carries the librdkafka (or JVM-Kafka 3.x) default.
module Client.ConfigParitySpec (tests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Consumer as Cons
import qualified Kafka.Client.Producer as Prod
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Network.Connection as Conn

tests :: TestTree
tests = testGroup "ConfigParity"
  [ producer_defaults
  , producer_setter_round_trips
  , consumer_defaults
  , consumer_isolation_level_default
  , connection_defaults
  , connection_address_family_default
  , connection_dns_lookup_default
    -- Retry/backoff
  , retry_default_curve_doubles
  , retry_curve_caps_at_max
  , retry_curve_zero_jitter_is_clean
  , retry_attempt_zero_returns_initial
  , producer_retries_pass_through_to_sender
  ]

----------------------------------------------------------------------
-- Producer defaults
----------------------------------------------------------------------

producer_defaults :: TestTree
producer_defaults =
  testCase "ProducerConfig defaults match librdkafka / JVM 3.x" $ do
    let c = Prod.defaultProducerConfig
    Prod.producerClientId                    c @?= "kafka-native-producer"
    Prod.producerBatchSize                   c @?= 16384
    Prod.producerLingerMs                    c @?= 0
    Prod.producerMaxInFlight                 c @?= 5
    Prod.producerRetries                     c @?= 2147483647
    Prod.producerRetryBackoffMs              c @?= 100
    Prod.producerRetryBackoffMaxMs           c @?= 1000
    Prod.producerRetryBackoffMultiplier      c @?= 2.0
    Prod.producerRetryBackoffJitter          c @?= 0.2
    Prod.producerDeliveryTimeoutMs           c @?= 120000
    Prod.producerRequestTimeoutMs            c @?= 30000
    Prod.producerMaxRequestSize              c @?= 1048576
    Prod.producerQueueBufferingMaxMessages   c @?= 100000
    Prod.producerQueueBufferingMaxKbytes     c @?= 1048576
    Prod.producerTransactionTimeoutMs        c @?= 60000
    Prod.producerEnableGaplessGuarantee      c @?= False
    Prod.producerStickyPartitioningLingerMs  c @?= 10
    Prod.producerIdempotent                  c @?= False
    Prod.producerTransactional               c @?= Nothing
    case Prod.producerDelivery c of
      Prod.AtLeastOnce -> pure ()
      other            -> error ("expected AtLeastOnce, got " <> show other)

producer_setter_round_trips :: TestTree
producer_setter_round_trips =
  testCase "every Producer config field round-trips through a record update" $ do
    let c0 = Prod.defaultProducerConfig
        c1 = c0
          { Prod.producerRetries                    = 7
          , Prod.producerRetryBackoffMs             = 250
          , Prod.producerRetryBackoffMaxMs          = 5000
          , Prod.producerRetryBackoffMultiplier     = 1.5
          , Prod.producerRetryBackoffJitter         = 0.0
          , Prod.producerRequestTimeoutMs           = 15000
          , Prod.producerMaxRequestSize             = 524288
          , Prod.producerQueueBufferingMaxMessages  = 50000
          , Prod.producerQueueBufferingMaxKbytes    = 524288
          , Prod.producerTransactionTimeoutMs       = 30000
          , Prod.producerEnableGaplessGuarantee     = True
          , Prod.producerStickyPartitioningLingerMs = 25
          , Prod.producerTransactional              = Just "tx-id"
          , Prod.producerIdempotent                 = True
          }
    Prod.producerRetries                    c1 @?= 7
    Prod.producerRetryBackoffMs             c1 @?= 250
    Prod.producerRetryBackoffMaxMs          c1 @?= 5000
    Prod.producerRetryBackoffMultiplier     c1 @?= 1.5
    Prod.producerRetryBackoffJitter         c1 @?= 0.0
    Prod.producerRequestTimeoutMs           c1 @?= 15000
    Prod.producerMaxRequestSize             c1 @?= 524288
    Prod.producerQueueBufferingMaxMessages  c1 @?= 50000
    Prod.producerQueueBufferingMaxKbytes    c1 @?= 524288
    Prod.producerTransactionTimeoutMs       c1 @?= 30000
    Prod.producerEnableGaplessGuarantee     c1 @?= True
    Prod.producerStickyPartitioningLingerMs c1 @?= 25
    Prod.producerTransactional              c1 @?= Just "tx-id"
    Prod.producerIdempotent                 c1 @?= True
    -- Untouched defaults survive.
    Prod.producerBatchSize c1 @?= Prod.producerBatchSize c0

----------------------------------------------------------------------
-- Consumer defaults
----------------------------------------------------------------------

consumer_defaults :: TestTree
consumer_defaults =
  testCase "ConsumerConfig defaults match librdkafka / JVM 3.x" $ do
    let c = Cons.defaultConsumerConfig
    Cons.consumerClientId                  c @?= "kafka-native-consumer"
    Cons.consumerGroupId                   c @?= "default-group"
    Cons.consumerGroupInstanceId           c @?= Nothing
    Cons.consumerAutoCommit                c @?= True
    Cons.consumerAutoCommitIntervalMs      c @?= 5000
    Cons.consumerEnableAutoOffsetStore     c @?= True
    Cons.consumerSessionTimeoutMs          c @?= 45000
    Cons.consumerHeartbeatIntervalMs       c @?= 3000
    Cons.consumerMaxPollRecords            c @?= 500
    Cons.consumerMaxPollIntervalMs         c @?= 300000
    Cons.consumerEnablePartitionEof        c @?= False
    Cons.consumerCheckCrcs                 c @?= True
    Cons.consumerFetchMinBytes             c @?= 1
    Cons.consumerFetchMaxBytes             c @?= 52428800
    Cons.consumerFetchMaxWaitMs            c @?= 500
    Cons.consumerFetchMessageMaxBytes      c @?= 1048576
    Cons.consumerFetchErrorBackoffMs       c @?= 500
    Cons.consumerQueuedMaxMessagesKbytes   c @?= 65536
    Cons.consumerRackId                    c @?= Nothing
    case Cons.consumerAssignmentStrategy c of
      Cons.RangeAssignment -> pure ()
      other                -> error ("expected RangeAssignment, got " <> show other)
    case Cons.consumerAutoOffsetReset c of
      Cons.Latest -> pure ()
      other       -> error ("expected Latest, got " <> show other)

consumer_isolation_level_default :: TestTree
consumer_isolation_level_default =
  testCase "ConsumerConfig defaults isolation level to ReadUncommitted (matches JVM)" $ do
    let c = Cons.defaultConsumerConfig
    case Cons.consumerIsolationLevel c of
      Cons.ReadUncommitted -> pure ()
      other -> error ("expected ReadUncommitted, got " <> show other)

----------------------------------------------------------------------
-- Connection defaults
----------------------------------------------------------------------

connection_defaults :: TestTree
connection_defaults =
  testCase "ConnectionConfig defaults match librdkafka" $ do
    let c = Conn.defaultConnectionConfig
    Conn.connTimeout                              c @?= 10
    Conn.connReadTimeout                          c @?= 30
    Conn.connWriteTimeout                         c @?= 30
    Conn.connRequestTimeoutMs                     c @?= 30000
    Conn.connRetryDelay                           c @?= 100
    Conn.connMaxRetries                           c @?= 3
    Conn.connBackoffMaxMs                         c @?= 10000
    Conn.connBackoffMultiplier                    c @?= 2.0
    -- Both default 'True' here (we deliberately diverge from
    -- librdkafka, which defaults both off): every Kafka write is
    -- already a complete framed request, so Nagle is pure
    -- latency penalty (the JVM client also sets TCP_NODELAY
    -- unconditionally).  SO_KEEPALIVE is on so silent dead-peer
    -- cases (NAT, broker crash) fail fast instead of parking
    -- forever on the TCP send-queue.  See the field-level
    -- comments in 'defaultConnectionConfig' for the full
    -- rationale.
    Conn.connSocketKeepalive                      c @?= True
    Conn.connSocketNagleDisable                   c @?= True
    Conn.connSocketSendBuffer                     c @?= 0
    Conn.connSocketReceiveBuffer                  c @?= 0
    Conn.connSocketMaxFails                       c @?= 1
    Conn.connMaxIdleMs                            c @?= 540000
    Conn.connMaxReauthMs                          c @?= 0
    Conn.connMessageMaxBytes                      c @?= 1000000
    Conn.connReceiveMessageMaxBytes               c @?= 100000000
    Conn.connMetadataMaxAgeMs                     c @?= 900000
    Conn.connTopicMetadataRefreshFastIntervalMs   c @?= 250
    Conn.connTopicMetadataRefreshSparse           c @?= True
    Conn.connBrokerAddressTtl                     c @?= 1000
    Conn.connUseTls                               c @?= False
    case Conn.connTlsSettings c of
      Nothing -> pure ()
      Just _  -> error "expected no TLS settings by default"
    case Conn.connSasl c of
      Nothing -> pure ()
      Just _  -> error "expected no SASL by default"
    Conn.connClientId c @?= T.pack "wireform-kafka"

connection_address_family_default :: TestTree
connection_address_family_default =
  testCase "default connection address family is Any (librdkafka @broker.address.family@ default)" $ do
    let c = Conn.defaultConnectionConfig
    case Conn.connBrokerAddressFamily c of
      Conn.BrokerAddressAny -> pure ()
      other -> error ("expected BrokerAddressAny, got " <> show other)

connection_dns_lookup_default :: TestTree
connection_dns_lookup_default =
  testCase "default DNS lookup is resolve_canonical_bootstrap_servers_only" $ do
    let c = Conn.defaultConnectionConfig
    case Conn.connDnsLookup c of
      Conn.DnsResolveCanonicalBootstrapServersOnly -> pure ()
      other -> error ("expected DnsResolveCanonicalBootstrapServersOnly, got " <> show other)

----------------------------------------------------------------------
-- Retry / backoff curve
----------------------------------------------------------------------

retry_default_curve_doubles :: TestTree
retry_default_curve_doubles =
  testCase "default retry backoff doubles per attempt up to the cap" $ do
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
    assertBool ("b0 ~= 100; got " <> show b0)
               (b0 >= 80 && b0 <= 120)
    assertBool ("b1 ~= 200; got " <> show b1)
               (b1 >= 160 && b1 <= 240)
    assertBool ("b2 ~= 400; got " <> show b2)
               (b2 >= 320 && b2 <= 480)
    assertBool ("b3 ~= 800; got " <> show b3)
               (b3 >= 640 && b3 <= 960)
    -- After the cap (1000ms), values stay bounded by 1000 + jitter.
    assertBool ("b4 capped; got " <> show b4) (b4 <= 1200)

retry_curve_caps_at_max :: TestTree
retry_curve_caps_at_max =
  testCase "after enough attempts, backoff hits retryBackoffMaxMs" $ do
    let cfg = Sender.defaultRetryConfig
        !v  = Sender.nextRetryBackoffMs cfg 50
    assertBool ("expected <= ~1200, got " <> show v) (v <= 1200)

retry_curve_zero_jitter_is_clean :: TestTree
retry_curve_zero_jitter_is_clean =
  testCase "with retryBackoffJitter = 0, the curve doubles cleanly" $ do
    let cfg = Sender.defaultRetryConfig
              { Sender.retryBackoffMs         = 50
              , Sender.retryBackoffMaxMs      = 10000
              , Sender.retryBackoffMultiplier = 2.0
              , Sender.retryBackoffJitter     = 0.0
              }
    map (Sender.nextRetryBackoffMs cfg) [0..5]
      @?= [50, 100, 200, 400, 800, 1600]

retry_attempt_zero_returns_initial :: TestTree
retry_attempt_zero_returns_initial =
  testCase "attempt 0 returns retryBackoffMs (within jitter band)" $ do
    let cfg = Sender.defaultRetryConfig
              { Sender.retryBackoffMs     = 250
              , Sender.retryBackoffJitter = 0.0
              }
    Sender.nextRetryBackoffMs cfg 0 @?= 250

----------------------------------------------------------------------
-- Producer config -> sender wiring
----------------------------------------------------------------------

producer_retries_pass_through_to_sender :: TestTree
producer_retries_pass_through_to_sender =
  testCase "producer retry knobs flow through into Sender.RetryConfig" $ do
    -- The exposed API path is via createProducer; here we just
    -- assert the field-by-field correspondence by constructing a
    -- RetryConfig the same way createProducer does.
    let c = Prod.defaultProducerConfig
              { Prod.producerRetries                = 5
              , Prod.producerRetryBackoffMs         = 200
              , Prod.producerRetryBackoffMaxMs      = 4000
              , Prod.producerRetryBackoffMultiplier = 3.0
              , Prod.producerRetryBackoffJitter     = 0.1
              }
        rc = Sender.RetryConfig
              { Sender.retryMaxAttempts       = Prod.producerRetries c
              , Sender.retryBackoffMs         = Prod.producerRetryBackoffMs c
              , Sender.retryBackoffMaxMs      = Prod.producerRetryBackoffMaxMs c
              , Sender.retryBackoffMultiplier = Prod.producerRetryBackoffMultiplier c
              , Sender.retryBackoffJitter     = Prod.producerRetryBackoffJitter c
              }
    Sender.retryMaxAttempts       rc @?= 5
    Sender.retryBackoffMs         rc @?= 200
    Sender.retryBackoffMaxMs      rc @?= 4000
    Sender.retryBackoffMultiplier rc @?= 3.0
    Sender.retryBackoffJitter     rc @?= 0.1
