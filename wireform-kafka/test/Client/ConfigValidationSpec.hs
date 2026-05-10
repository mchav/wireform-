{-# LANGUAGE OverloadedStrings #-}

-- | Tests for KIP-360 client-side config validation.
--
-- The intent here is *not* to retest every default value (that's
-- 'Client.ConsumerConfigSpec' / 'Client.ConfigParitySpec' territory)
-- but to make sure that obviously broken configs are rejected
-- before any socket is opened, and that the validator's surface
-- matches what the JVM client checks in its
-- @ProducerConfig.postProcessAndValidateIdempotenceConfigs@ flow.
module Client.ConfigValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=), assertBool)

import qualified Data.Text as T

import Kafka.Client.ConfigValidation
  ( ConfigError (..), renderConfigErrors )
import Kafka.Client.Producer
  ( ProducerConfig (..)
  , DeliveryGuarantee (..)
  , defaultProducerConfig
  , validateProducerConfig
  )
import Kafka.Client.Consumer
  ( ConsumerConfig (..)
  , defaultConsumerConfig
  , validateConsumerConfig
  )

tests :: TestTree
tests = testGroup "KIP-360 config validation"
  [ testGroup "Producer"
      [ testCase "default config is valid" prop_producerDefaultsValid
      , testCase "empty client.id is rejected"
          prop_producerEmptyClientIdRejected
      , testCase "negative batch.size is rejected"
          prop_producerNegativeBatchSizeRejected
      , testCase "max.in.flight = 0 is rejected"
          prop_producerZeroInFlightRejected
      , testCase "delivery.timeout.ms < request.timeout.ms is rejected"
          prop_producerDeliveryShorterThanRequestRejected
      , testCase "idempotent producer with in-flight > 5 is rejected"
          prop_producerIdempotentInFlightCapEnforced
      , testCase "transactional producer requires idempotence"
          prop_producerTxnRequiresIdempotence
      , testCase "transactional producer requires acks=all"
          prop_producerTxnRequiresAcksAll
      , testCase "all errors are accumulated, not short-circuited"
          prop_producerErrorsAccumulated
      ]
  , testGroup "Consumer"
      [ testCase "default config is valid" prop_consumerDefaultsValid
      , testCase "heartbeat.interval.ms >= session.timeout.ms is rejected"
          prop_consumerHeartbeatTooHighRejected
      , testCase "max.poll.interval.ms < session.timeout.ms is rejected"
          prop_consumerPollIntervalTooLowRejected
      , testCase "fetch.min.bytes > fetch.max.bytes is rejected"
          prop_consumerFetchMinExceedsMaxRejected
      , testCase "auto-commit interval must be > 0 when auto-commit is on"
          prop_consumerAutoCommitIntervalRequiredWhenEnabled
      , testCase "auto-commit interval is ignored when auto-commit is off"
          prop_consumerAutoCommitIntervalIgnoredWhenDisabled
      ]
  , testCase "renderConfigErrors prints field + message" prop_renderConfigErrors
  ]

------------------------------------------------------------------
-- Producer rules
------------------------------------------------------------------

prop_producerDefaultsValid :: Assertion
prop_producerDefaultsValid =
  validateProducerConfig defaultProducerConfig @?= []

prop_producerEmptyClientIdRejected :: Assertion
prop_producerEmptyClientIdRejected = do
  let cfg = defaultProducerConfig { producerClientId = "" }
      errs = validateProducerConfig cfg
  fmap configErrorField errs @?= ["client.id"]

prop_producerNegativeBatchSizeRejected :: Assertion
prop_producerNegativeBatchSizeRejected = do
  let cfg = defaultProducerConfig { producerBatchSize = -1 }
      errs = validateProducerConfig cfg
  fmap configErrorField errs @?= ["batch.size"]

prop_producerZeroInFlightRejected :: Assertion
prop_producerZeroInFlightRejected = do
  let cfg = defaultProducerConfig { producerMaxInFlight = 0 }
      errs = validateProducerConfig cfg
  fmap configErrorField errs
    @?= ["max.in.flight.requests.per.connection"]

prop_producerDeliveryShorterThanRequestRejected :: Assertion
prop_producerDeliveryShorterThanRequestRejected = do
  let cfg = defaultProducerConfig
        { producerRequestTimeoutMs   = 30_000
        , producerDeliveryTimeoutMs  = 10_000
        , producerLingerMs           = 0
        }
      errs = validateProducerConfig cfg
  fmap configErrorField errs @?= ["delivery.timeout.ms"]

prop_producerIdempotentInFlightCapEnforced :: Assertion
prop_producerIdempotentInFlightCapEnforced = do
  let cfg = defaultProducerConfig
        { producerIdempotent = True
        , producerMaxInFlight = 6
        }
      fields = configErrorField <$> validateProducerConfig cfg
  assertBool ("expected in-flight cap, got " <> show fields)
    ("max.in.flight.requests.per.connection" `elem` fields)

prop_producerTxnRequiresIdempotence :: Assertion
prop_producerTxnRequiresIdempotence = do
  let cfg = defaultProducerConfig
        { producerTransactional = Just "txn-1"
        , producerIdempotent    = False
        , producerDelivery      = ExactlyOnce
        }
      fields = configErrorField <$> validateProducerConfig cfg
  assertBool
    ("expected enable.idempotence error, got " <> show fields)
    ("enable.idempotence" `elem` fields)

prop_producerTxnRequiresAcksAll :: Assertion
prop_producerTxnRequiresAcksAll = do
  let cfg = defaultProducerConfig
        { producerTransactional = Just "txn-1"
        , producerIdempotent    = True
        , producerDelivery      = AtLeastOnce
        }
      fields = configErrorField <$> validateProducerConfig cfg
  assertBool ("expected acks error, got " <> show fields)
    ("acks" `elem` fields)

prop_producerErrorsAccumulated :: Assertion
prop_producerErrorsAccumulated = do
  let cfg = defaultProducerConfig
        { producerClientId          = ""
        , producerBatchSize         = -1
        , producerMaxInFlight       = 0
        }
      fields = configErrorField <$> validateProducerConfig cfg
  -- All three errors should surface in one pass; the validator must
  -- not bail on the first failure.
  assertBool ("expected three errors, got " <> show fields)
    (length fields >= 3)

------------------------------------------------------------------
-- Consumer rules
------------------------------------------------------------------

prop_consumerDefaultsValid :: Assertion
prop_consumerDefaultsValid =
  validateConsumerConfig defaultConsumerConfig @?= []

prop_consumerHeartbeatTooHighRejected :: Assertion
prop_consumerHeartbeatTooHighRejected = do
  let cfg = defaultConsumerConfig
        { consumerSessionTimeoutMs    = 10_000
        , consumerHeartbeatIntervalMs = 10_000
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  assertBool ("expected heartbeat error, got " <> show fields)
    ("heartbeat.interval.ms" `elem` fields)

prop_consumerPollIntervalTooLowRejected :: Assertion
prop_consumerPollIntervalTooLowRejected = do
  let cfg = defaultConsumerConfig
        { consumerSessionTimeoutMs   = 60_000
        , consumerMaxPollIntervalMs  = 30_000
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  assertBool ("expected max.poll.interval.ms, got " <> show fields)
    ("max.poll.interval.ms" `elem` fields)

prop_consumerFetchMinExceedsMaxRejected :: Assertion
prop_consumerFetchMinExceedsMaxRejected = do
  let cfg = defaultConsumerConfig
        { consumerFetchMinBytes = 1024
        , consumerFetchMaxBytes = 512
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  assertBool ("expected fetch.min.bytes, got " <> show fields)
    ("fetch.min.bytes" `elem` fields)

prop_consumerAutoCommitIntervalRequiredWhenEnabled :: Assertion
prop_consumerAutoCommitIntervalRequiredWhenEnabled = do
  let cfg = defaultConsumerConfig
        { consumerAutoCommit = True
        , consumerAutoCommitIntervalMs = 0
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  fields @?= ["auto.commit.interval.ms"]

prop_consumerAutoCommitIntervalIgnoredWhenDisabled :: Assertion
prop_consumerAutoCommitIntervalIgnoredWhenDisabled = do
  let cfg = defaultConsumerConfig
        { consumerAutoCommit = False
        , consumerAutoCommitIntervalMs = 0
        }
  validateConsumerConfig cfg @?= []

------------------------------------------------------------------
-- Render
------------------------------------------------------------------

prop_renderConfigErrors :: Assertion
prop_renderConfigErrors = do
  let rendered = renderConfigErrors
        [ ConfigError "batch.size" "must be >= 0"
        , ConfigError "client.id" "must be non-empty"
        ]
  assertBool ("rendered: " <> rendered)
    ("batch.size: must be >= 0"   `T.isInfixOf` T.pack rendered)
  assertBool ("rendered: " <> rendered)
    ("client.id: must be non-empty" `T.isInfixOf` T.pack rendered)
