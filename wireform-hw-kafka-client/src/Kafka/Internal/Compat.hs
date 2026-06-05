module Kafka.Internal.Compat
  ( Callback (..)
  , Kafka (..)
  , KafkaConf (..)
  , TopicConf (..)
  , HasKafka (..)
  , HasKafkaConf (..)
  , HasTopicConf (..)
  , RdKafkaRespErrT (..)
  , kafkaConf
  , topicConf
  , kafkaError
  , errorToKafkaError
  , maybeError
  , textDecimal
  , decimalText
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Kafka.Client.Consumer as WFConsumer
import qualified Kafka.Client.Producer as WFProducer

data RdKafkaRespErrT
  = RdKafkaRespErrBegin
  | RdKafkaRespErrBadMsg
  | RdKafkaRespErrBadCompression
  | RdKafkaRespErrDestroy
  | RdKafkaRespErrFail
  | RdKafkaRespErrTransport
  | RdKafkaRespErrCritSysResource
  | RdKafkaRespErrResolve
  | RdKafkaRespErrMsgTimedOut
  | RdKafkaRespErrPartitionEof
  | RdKafkaRespErrUnknownPartition
  | RdKafkaRespErrFs
  | RdKafkaRespErrUnknownTopic
  | RdKafkaRespErrAllBrokersDown
  | RdKafkaRespErrInvalidArg
  | RdKafkaRespErrTimedOut
  | RdKafkaRespErrQueueFull
  | RdKafkaRespErrIsrInsuff
  | RdKafkaRespErrNodeUpdate
  | RdKafkaRespErrSsl
  | RdKafkaRespErrWaitCoord
  | RdKafkaRespErrUnknownGroup
  | RdKafkaRespErrInProgress
  | RdKafkaRespErrPrevInProgress
  | RdKafkaRespErrExistingSubscription
  | RdKafkaRespErrAssignPartitions
  | RdKafkaRespErrRevokePartitions
  | RdKafkaRespErrConflict
  | RdKafkaRespErrState
  | RdKafkaRespErrUnknownProtocol
  | RdKafkaRespErrNotImplemented
  | RdKafkaRespErrAuthentication
  | RdKafkaRespErrNoOffset
  | RdKafkaRespErrOutdated
  | RdKafkaRespErrTimedOutQueue
  | RdKafkaRespErrUnsupportedFeature
  | RdKafkaRespErrWaitCache
  | RdKafkaRespErrIntr
  | RdKafkaRespErrKeySerialization
  | RdKafkaRespErrValueSerialization
  | RdKafkaRespErrKeyDeserialization
  | RdKafkaRespErrValueDeserialization
  | RdKafkaRespErrPartial
  | RdKafkaRespErrReadOnly
  | RdKafkaRespErrNoent
  | RdKafkaRespErrUnderflow
  | RdKafkaRespErrInvalidType
  | RdKafkaRespErrRetry
  | RdKafkaRespErrPurgeQueue
  | RdKafkaRespErrPurgeInflight
  | RdKafkaRespErrFatal
  | RdKafkaRespErrInconsistent
  | RdKafkaRespErrGaplessGuarantee
  | RdKafkaRespErrMaxPollExceeded
  | RdKafkaRespErrUnknownBroker
  | RdKafkaRespErrNotConfigured
  | RdKafkaRespErrFenced
  | RdKafkaRespErrApplication
  | RdKafkaRespErrAssignmentLost
  | RdKafkaRespErrNoop
  | RdKafkaRespErrAutoOffsetReset
  | RdKafkaRespErrLogTruncation
  | RdKafkaRespErrEnd
  | RdKafkaRespErrUnknown
  | RdKafkaRespErrNoError
  | RdKafkaRespErrOffsetOutOfRange
  | RdKafkaRespErrInvalidMsg
  | RdKafkaRespErrUnknownTopicOrPart
  | RdKafkaRespErrInvalidMsgSize
  | RdKafkaRespErrLeaderNotAvailable
  | RdKafkaRespErrNotLeaderForPartition
  | RdKafkaRespErrRequestTimedOut
  | RdKafkaRespErrBrokerNotAvailable
  | RdKafkaRespErrReplicaNotAvailable
  | RdKafkaRespErrMsgSizeTooLarge
  | RdKafkaRespErrStaleCtrlEpoch
  | RdKafkaRespErrOffsetMetadataTooLarge
  | RdKafkaRespErrNetworkException
  | RdKafkaRespErrCoordinatorLoadInProgress
  | RdKafkaRespErrCoordinatorNotAvailable
  | RdKafkaRespErrNotCoordinator
  | RdKafkaRespErrTopicException
  | RdKafkaRespErrRecordListTooLarge
  | RdKafkaRespErrNotEnoughReplicas
  | RdKafkaRespErrNotEnoughReplicasAfterAppend
  | RdKafkaRespErrInvalidRequiredAcks
  | RdKafkaRespErrIllegalGeneration
  | RdKafkaRespErrInconsistentGroupProtocol
  | RdKafkaRespErrInvalidGroupId
  | RdKafkaRespErrUnknownMemberId
  | RdKafkaRespErrInvalidSessionTimeout
  | RdKafkaRespErrRebalanceInProgress
  | RdKafkaRespErrInvalidCommitOffsetSize
  | RdKafkaRespErrTopicAuthorizationFailed
  | RdKafkaRespErrGroupAuthorizationFailed
  | RdKafkaRespErrClusterAuthorizationFailed
  | RdKafkaRespErrInvalidTimestamp
  | RdKafkaRespErrUnsupportedSaslMechanism
  | RdKafkaRespErrIllegalSaslState
  | RdKafkaRespErrUnsupportedVersion
  | RdKafkaRespErrTopicAlreadyExists
  | RdKafkaRespErrInvalidPartitions
  | RdKafkaRespErrInvalidReplicationFactor
  | RdKafkaRespErrInvalidReplicaAssignment
  | RdKafkaRespErrInvalidConfig
  | RdKafkaRespErrNotController
  | RdKafkaRespErrInvalidRequest
  | RdKafkaRespErrUnsupportedForMessageFormat
  | RdKafkaRespErrPolicyViolation
  | RdKafkaRespErrOutOfOrderSequenceNumber
  | RdKafkaRespErrDuplicateSequenceNumber
  | RdKafkaRespErrInvalidProducerEpoch
  | RdKafkaRespErrInvalidTxnState
  | RdKafkaRespErrInvalidProducerIdMapping
  | RdKafkaRespErrInvalidTransactionTimeout
  | RdKafkaRespErrConcurrentTransactions
  | RdKafkaRespErrTransactionCoordinatorFenced
  | RdKafkaRespErrTransactionalIdAuthorizationFailed
  | RdKafkaRespErrSecurityDisabled
  | RdKafkaRespErrOperationNotAttempted
  | RdKafkaRespErrKafkaStorageError
  | RdKafkaRespErrLogDirNotFound
  | RdKafkaRespErrSaslAuthenticationFailed
  | RdKafkaRespErrUnknownProducerId
  | RdKafkaRespErrReassignmentInProgress
  | RdKafkaRespErrDelegationTokenAuthDisabled
  | RdKafkaRespErrDelegationTokenNotFound
  | RdKafkaRespErrDelegationTokenOwnerMismatch
  | RdKafkaRespErrDelegationTokenRequestNotAllowed
  | RdKafkaRespErrDelegationTokenAuthorizationFailed
  | RdKafkaRespErrDelegationTokenExpired
  | RdKafkaRespErrInvalidPrincipalType
  | RdKafkaRespErrNonEmptyGroup
  | RdKafkaRespErrGroupIdNotFound
  | RdKafkaRespErrFetchSessionIdNotFound
  | RdKafkaRespErrInvalidFetchSessionEpoch
  | RdKafkaRespErrListenerNotFound
  | RdKafkaRespErrTopicDeletionDisabled
  | RdKafkaRespErrFencedLeaderEpoch
  | RdKafkaRespErrUnknownLeaderEpoch
  | RdKafkaRespErrUnsupportedCompressionType
  | RdKafkaRespErrStaleBrokerEpoch
  | RdKafkaRespErrOffsetNotAvailable
  | RdKafkaRespErrMemberIdRequired
  | RdKafkaRespErrPreferredLeaderNotAvailable
  | RdKafkaRespErrGroupMaxSizeReached
  | RdKafkaRespErrFencedInstanceId
  | RdKafkaRespErrEligibleLeadersNotAvailable
  | RdKafkaRespErrElectionNotNeeded
  | RdKafkaRespErrNoReassignmentInProgress
  | RdKafkaRespErrGroupSubscribedToTopic
  | RdKafkaRespErrInvalidRecord
  | RdKafkaRespErrUnstableOffsetCommit
  | RdKafkaRespErrThrottlingQuotaExceeded
  | RdKafkaRespErrProducerFenced
  | RdKafkaRespErrResourceNotFound
  | RdKafkaRespErrDuplicateResource
  | RdKafkaRespErrUnacceptableCredential
  | RdKafkaRespErrInconsistentVoterSet
  | RdKafkaRespErrInvalidUpdateVersion
  | RdKafkaRespErrFeatureUpdateFailed
  | RdKafkaRespErrPrincipalDeserializationFailure
  | RdKafkaRespErrEndAll
  deriving (Eq, Show, Enum, Bounded, Typeable, Generic)

data Callback = Callback

data Kafka
  = KafkaProducerHandle !WFProducer.Producer
  | KafkaConsumerHandle !WFConsumer.Consumer

data KafkaConf = KafkaConf
  { kcfgKafkaProps :: !(Map Text Text)
  , kcfgBufferedRecords :: !(IORef [WFConsumer.ConsumerRecord])
  }

newtype TopicConf = TopicConf
  { topicConfProps :: Map Text Text
  }

class HasKafka a where
  getKafka :: a -> Kafka

class HasKafkaConf a where
  getKafkaConf :: a -> KafkaConf

class HasTopicConf a where
  getTopicConf :: a -> TopicConf

kafkaConf :: Map Text Text -> IO KafkaConf
kafkaConf props = do
  buffered <- newIORef []
  pure KafkaConf
    { kcfgKafkaProps = props
    , kcfgBufferedRecords = buffered
    }

topicConf :: Map Text Text -> TopicConf
topicConf = TopicConf

kafkaError :: Text -> err
kafkaError = error . T.unpack

errorToKafkaError :: Text -> KafkaErrorLike
errorToKafkaError = KafkaErrorLike

maybeError :: Either String a -> Maybe KafkaErrorLike
maybeError = either (Just . KafkaErrorLike . T.pack) (const Nothing)

textDecimal :: Integral a => Text -> Maybe a
textDecimal t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

decimalText :: Integral a => a -> Text
decimalText = toStrict . toLazyText . decimal

newtype KafkaErrorLike = KafkaErrorLike Text
  deriving stock (Eq, Show, Typeable, Generic)

instance Exception KafkaErrorLike
