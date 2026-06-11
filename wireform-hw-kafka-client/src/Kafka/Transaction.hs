{- |
Module      : Kafka.Transaction
Description : Legacy transaction API placeholder for migration.

@hw-kafka-client@ exposed librdkafka transaction functions over the
producer handle. The native wireform implementation models transactions
through "Kafka.Client.Transaction" and explicit transaction handles.
This module is present so legacy imports continue to compile during a
transition; real transactional applications should move to the native
module.
-}
module Kafka.Transaction (
  initTransactions,
  beginTransaction,
  commitTransaction,
  abortTransaction,
  commitOffsetMessageTransaction,
  TxError,
  getKafkaError,
  kafkaErrorIsFatal,
  kafkaErrorIsRetriable,
  kafkaErrorTxnRequiresAbort,
) where

import Control.Monad.IO.Class (MonadIO)
import Kafka.Consumer (ConsumerRecord, KafkaConsumer)
import Kafka.Producer (KafkaProducer)
import Kafka.Types (KafkaError (..), Timeout)


-- | Transaction error classification from the legacy API.
data TxError = TxError
  { txErrorKafka :: !KafkaError
  -- ^ Underlying Kafka error.
  , txErrorFatal :: !Bool
  -- ^ Whether the error is fatal.
  , txErrorRetriable :: !Bool
  -- ^ Whether retrying may succeed.
  , txErrorTxnReqAbort :: !Bool
  -- ^ Whether the current transaction requires abort.
  }


{- | Initialise Kafka transactions.

Compatibility stub; use "Kafka.Client.Transaction".
-}
initTransactions :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe KafkaError)
initTransactions _ _ = pure (Just transactionUnsupported)


{- | Begin a transaction.

Compatibility stub; use "Kafka.Client.Transaction".
-}
beginTransaction :: MonadIO m => KafkaProducer -> m (Maybe KafkaError)
beginTransaction _ = pure (Just transactionUnsupported)


{- | Commit a transaction.

Compatibility stub; use "Kafka.Client.Transaction".
-}
commitTransaction :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe TxError)
commitTransaction _ _ = pure (Just (txUnsupported True))


{- | Abort a transaction.

Compatibility stub; use "Kafka.Client.Transaction".
-}
abortTransaction :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe KafkaError)
abortTransaction _ _ = pure (Just transactionUnsupported)


{- | Commit a consumed offset inside a transaction.

Compatibility stub; use "Kafka.Client.Transaction".
-}
commitOffsetMessageTransaction
  :: MonadIO m
  => KafkaProducer
  -> KafkaConsumer
  -> ConsumerRecord k v
  -> Timeout
  -> m (Maybe TxError)
commitOffsetMessageTransaction _ _ _ _ =
  pure (Just (txUnsupported True))


-- | Extract the underlying Kafka error.
getKafkaError :: TxError -> KafkaError
getKafkaError = txErrorKafka


-- | Whether the transaction error is fatal.
kafkaErrorIsFatal :: TxError -> Bool
kafkaErrorIsFatal = txErrorFatal


-- | Whether the transaction error is retriable.
kafkaErrorIsRetriable :: TxError -> Bool
kafkaErrorIsRetriable = txErrorRetriable


-- | Whether the transaction error requires abort.
kafkaErrorTxnRequiresAbort :: TxError -> Bool
kafkaErrorTxnRequiresAbort = txErrorTxnReqAbort


transactionUnsupported :: KafkaError
transactionUnsupported =
  KafkaBadSpecification "Kafka.Transaction is present for source compatibility; use Kafka.Client.Transaction for native wireform transactions"


txUnsupported :: Bool -> TxError
txUnsupported fatal =
  TxError
    { txErrorKafka = transactionUnsupported
    , txErrorFatal = fatal
    , txErrorRetriable = False
    , txErrorTxnReqAbort = False
    }
