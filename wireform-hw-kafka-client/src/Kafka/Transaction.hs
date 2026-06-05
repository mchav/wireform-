module Kafka.Transaction
  ( initTransactions
  , beginTransaction
  , commitTransaction
  , abortTransaction
  , commitOffsetMessageTransaction
  , TxError
  , getKafkaError
  , kafkaErrorIsFatal
  , kafkaErrorIsRetriable
  , kafkaErrorTxnRequiresAbort
  ) where

import Control.Monad.IO.Class (MonadIO)
import Kafka.Consumer (ConsumerRecord, KafkaConsumer)
import Kafka.Producer (KafkaProducer)
import Kafka.Types (KafkaError (..), Timeout)

data TxError = TxError
  { txErrorKafka :: !KafkaError
  , txErrorFatal :: !Bool
  , txErrorRetriable :: !Bool
  , txErrorTxnReqAbort :: !Bool
  }

initTransactions :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe KafkaError)
initTransactions _ _ = pure (Just transactionUnsupported)

beginTransaction :: MonadIO m => KafkaProducer -> m (Maybe KafkaError)
beginTransaction _ = pure (Just transactionUnsupported)

commitTransaction :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe TxError)
commitTransaction _ _ = pure (Just (txUnsupported True))

abortTransaction :: MonadIO m => KafkaProducer -> Timeout -> m (Maybe KafkaError)
abortTransaction _ _ = pure (Just transactionUnsupported)

commitOffsetMessageTransaction
  :: MonadIO m
  => KafkaProducer
  -> KafkaConsumer
  -> ConsumerRecord k v
  -> Timeout
  -> m (Maybe TxError)
commitOffsetMessageTransaction _ _ _ _ =
  pure (Just (txUnsupported True))

getKafkaError :: TxError -> KafkaError
getKafkaError = txErrorKafka

kafkaErrorIsFatal :: TxError -> Bool
kafkaErrorIsFatal = txErrorFatal

kafkaErrorIsRetriable :: TxError -> Bool
kafkaErrorIsRetriable = txErrorRetriable

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
