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

import Test.Syd

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

tests :: Spec
tests = describe "KIP-360 config validation" $ sequence_
  [ describe "Producer" $ sequence_
      [ it "default config is valid" prop_producerDefaultsValid
      , it "empty client.id is rejected"
          prop_producerEmptyClientIdRejected
      , it "negative batch.size is rejected"
          prop_producerNegativeBatchSizeRejected
      , it "max.in.flight = 0 is rejected"
          prop_producerZeroInFlightRejected
      , it "delivery.timeout.ms < request.timeout.ms is rejected"
          prop_producerDeliveryShorterThanRequestRejected
      , it "idempotent producer with in-flight > 5 is rejected"
          prop_producerIdempotentInFlightCapEnforced
      , it "transactional producer requires idempotence"
          prop_producerTxnRequiresIdempotence
      , it "transactional producer requires acks=all"
          prop_producerTxnRequiresAcksAll
      , it "all errors are accumulated, not short-circuited"
          prop_producerErrorsAccumulated
      ]
  , describe "Consumer" $ sequence_
      [ it "default config is valid" prop_consumerDefaultsValid
      , it "heartbeat.interval.ms >= session.timeout.ms is rejected"
          prop_consumerHeartbeatTooHighRejected
      , it "max.poll.interval.ms < session.timeout.ms is rejected"
          prop_consumerPollIntervalTooLowRejected
      , it "fetch.min.bytes > fetch.max.bytes is rejected"
          prop_consumerFetchMinExceedsMaxRejected
      , it "auto-commit interval must be > 0 when auto-commit is on"
          prop_consumerAutoCommitIntervalRequiredWhenEnabled
      , it "auto-commit interval is ignored when auto-commit is off"
          prop_consumerAutoCommitIntervalIgnoredWhenDisabled
      ]
  , it "renderConfigErrors prints field + message" prop_renderConfigErrors
  ]

------------------------------------------------------------------
-- Producer rules
------------------------------------------------------------------

prop_producerDefaultsValid :: IO ()
prop_producerDefaultsValid =
  validateProducerConfig defaultProducerConfig `shouldBe` []

prop_producerEmptyClientIdRejected :: IO ()
prop_producerEmptyClientIdRejected = do
  let cfg = defaultProducerConfig { producerClientId = "" }
      errs = validateProducerConfig cfg
  fmap configErrorField errs `shouldBe` ["client.id"]

prop_producerNegativeBatchSizeRejected :: IO ()
prop_producerNegativeBatchSizeRejected = do
  let cfg = defaultProducerConfig { producerBatchSize = -1 }
      errs = validateProducerConfig cfg
  fmap configErrorField errs `shouldBe` ["batch.size"]

prop_producerZeroInFlightRejected :: IO ()
prop_producerZeroInFlightRejected = do
  let cfg = defaultProducerConfig { producerMaxInFlight = 0 }
      errs = validateProducerConfig cfg
  fmap configErrorField errs
    `shouldBe` ["max.in.flight.requests.per.connection"]

prop_producerDeliveryShorterThanRequestRejected :: IO ()
prop_producerDeliveryShorterThanRequestRejected = do
  let cfg = defaultProducerConfig
        { producerRequestTimeoutMs   = 30_000
        , producerDeliveryTimeoutMs  = 10_000
        , producerLingerMs           = 0
        }
      errs = validateProducerConfig cfg
  fmap configErrorField errs `shouldBe` ["delivery.timeout.ms"]

prop_producerIdempotentInFlightCapEnforced :: IO ()
prop_producerIdempotentInFlightCapEnforced = do
  let cfg = defaultProducerConfig
        { producerIdempotent = True
        , producerMaxInFlight = 6
        }
      fields = configErrorField <$> validateProducerConfig cfg
  (if ("max.in.flight.requests.per.connection" `elem` fields) then pure () else expectationFailure ("expected in-flight cap, got " <> show fields))

prop_producerTxnRequiresIdempotence :: IO ()
prop_producerTxnRequiresIdempotence = do
  let cfg = defaultProducerConfig
        { producerTransactional = Just "txn-1"
        , producerIdempotent    = False
        , producerDelivery      = ExactlyOnce
        }
      fields = configErrorField <$> validateProducerConfig cfg
  (if ("enable.idempotence" `elem` fields) then pure () else expectationFailure ("expected enable.idempotence error, got " <> show fields))

prop_producerTxnRequiresAcksAll :: IO ()
prop_producerTxnRequiresAcksAll = do
  let cfg = defaultProducerConfig
        { producerTransactional = Just "txn-1"
        , producerIdempotent    = True
        , producerDelivery      = AtLeastOnce
        }
      fields = configErrorField <$> validateProducerConfig cfg
  (if ("acks" `elem` fields) then pure () else expectationFailure ("expected acks error, got " <> show fields))

prop_producerErrorsAccumulated :: IO ()
prop_producerErrorsAccumulated = do
  let cfg = defaultProducerConfig
        { producerClientId          = ""
        , producerBatchSize         = -1
        , producerMaxInFlight       = 0
        }
      fields = configErrorField <$> validateProducerConfig cfg
  -- All three errors should surface in one pass; the validator must
  -- not bail on the first failure.
  (if (length fields >= 3) then pure () else expectationFailure ("expected three errors, got " <> show fields))

------------------------------------------------------------------
-- Consumer rules
------------------------------------------------------------------

prop_consumerDefaultsValid :: IO ()
prop_consumerDefaultsValid =
  validateConsumerConfig defaultConsumerConfig `shouldBe` []

prop_consumerHeartbeatTooHighRejected :: IO ()
prop_consumerHeartbeatTooHighRejected = do
  let cfg = defaultConsumerConfig
        { consumerSessionTimeoutMs    = 10_000
        , consumerHeartbeatIntervalMs = 10_000
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  (if ("heartbeat.interval.ms" `elem` fields) then pure () else expectationFailure ("expected heartbeat error, got " <> show fields))

prop_consumerPollIntervalTooLowRejected :: IO ()
prop_consumerPollIntervalTooLowRejected = do
  let cfg = defaultConsumerConfig
        { consumerSessionTimeoutMs   = 60_000
        , consumerMaxPollIntervalMs  = 30_000
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  (if ("max.poll.interval.ms" `elem` fields) then pure () else expectationFailure ("expected max.poll.interval.ms, got " <> show fields))

prop_consumerFetchMinExceedsMaxRejected :: IO ()
prop_consumerFetchMinExceedsMaxRejected = do
  let cfg = defaultConsumerConfig
        { consumerFetchMinBytes = 1024
        , consumerFetchMaxBytes = 512
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  (if ("fetch.min.bytes" `elem` fields) then pure () else expectationFailure ("expected fetch.min.bytes, got " <> show fields))

prop_consumerAutoCommitIntervalRequiredWhenEnabled :: IO ()
prop_consumerAutoCommitIntervalRequiredWhenEnabled = do
  let cfg = defaultConsumerConfig
        { consumerAutoCommit = True
        , consumerAutoCommitIntervalMs = 0
        }
      fields = configErrorField <$> validateConsumerConfig cfg
  fields `shouldBe` ["auto.commit.interval.ms"]

prop_consumerAutoCommitIntervalIgnoredWhenDisabled :: IO ()
prop_consumerAutoCommitIntervalIgnoredWhenDisabled = do
  let cfg = defaultConsumerConfig
        { consumerAutoCommit = False
        , consumerAutoCommitIntervalMs = 0
        }
  validateConsumerConfig cfg `shouldBe` []

------------------------------------------------------------------
-- Render
------------------------------------------------------------------

prop_renderConfigErrors :: IO ()
prop_renderConfigErrors = do
  let rendered = renderConfigErrors
        [ ConfigError "batch.size" "must be >= 0"
        , ConfigError "client.id" "must be non-empty"
        ]
  (if ("batch.size: must be >= 0"   `T.isInfixOf` T.pack rendered) then pure () else expectationFailure ("rendered: " <> rendered))
  (if ("client.id: must be non-empty" `T.isInfixOf` T.pack rendered) then pure () else expectationFailure ("rendered: " <> rendered))
