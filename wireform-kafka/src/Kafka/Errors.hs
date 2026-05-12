{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Errors
Description : The exception hierarchy used by every public Kafka operation.
Copyright   : (c) 2025
License     : BSD-3-Clause

Every operation on a producer, consumer, admin client, or
transaction throws a 'KafkaException' on failure. The exception
carries a structured 'KafkaErrorKind' that names the failure
category (connection lost, authentication failed, record too
large, topic not found, …) plus a human-readable message and an
optional underlying 'SomeException' cause.

= Catching

@
import Control.Exception (try)
import Kafka.Errors (KafkaException(..), KafkaErrorKind(..))

main = do
  r <- try (Kafka.'sendMessage' p \"events\" Nothing \"hello\")
  case (r :: Either KafkaException RecordMetadata) of
    Right md             -> ...
    Left e | isRetriable e -> 'retry'
           | otherwise     -> 'crash'
@

= Categories

The categories mirror the JVM client's
@org.apache.kafka.common.errors.*@ hierarchy:

  * 'ConnectError'                 — TCP / TLS handshake failed.
  * 'AuthenticationError'          — SASL / TLS rejected.
  * 'AuthorizationError'           — broker refused an authorized
    operation (insufficient ACL).
  * 'ConfigurationError'           — pre-flight config validation
    rejected the supplied 'ProducerConfig' \/ 'ConsumerConfig'.
  * 'TimeoutError'                 — request \/ delivery exceeded
    its deadline.
  * 'NetworkError'                 — broker dropped the socket
    mid-request.
  * 'InvalidTopicError'            — topic name violates
    @[a-zA-Z0-9._-]@.
  * 'TopicAlreadyExistsError'      — @CreateTopics@ collided.
  * 'UnknownTopicOrPartitionError' — the broker has no metadata
    for the requested (topic, partition).
  * 'RecordTooLargeError'          — record exceeds the broker's
    @max.message.bytes@.
  * 'SerializationError'           — a 'Serde' failed to encode
    \/ decode.
  * 'ProducerFencedError'          — another producer with the
    same @transactional.id@ took over (transactional fencing).
  * 'TransactionAbortedError'      — the in-flight transaction was
    aborted.
  * 'OffsetOutOfRangeError'        — fetch position is no longer
    available on the broker.
  * 'NotInTransactionError'        — a transactional operation
    was attempted outside an open transaction.
  * 'UnsupportedVersionError'      — the broker doesn't speak the
    minimum API version this client requires.
  * 'DeliveryFailedError'          — catch-all for produce
    delivery failures the classifier couldn't bucket.
  * 'UnknownError'                 — last-resort bucket; carries
    the original error string for diagnostics.

= 'isRetriable'

A KIP-487-aligned predicate. 'True' iff the failure category is
transient and a backoff + retry might succeed (network blip,
timeout, leader-change). Use it to gate retry loops.
-}
module Kafka.Errors
  ( -- * Exception type
    KafkaException (..)
  , KafkaErrorKind (..)
    -- * Construction
  , kafkaException
  , configurationError
  , connectError
  , timeoutError
  , authenticationError
  , unknownError
    -- * Bridging from legacy 'Either String' code
  , orThrow
  , orThrowWith
    -- * Classification
  , isRetriable
  , isFatal
  ) where

import           Control.Exception (Exception, SomeException, throwIO)
import           Data.Int          (Int32, Int64)
import qualified Data.Text         as T
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | Every public Kafka operation throws this on failure. Mirrors
-- the JVM @org.apache.kafka.common.KafkaException@ hierarchy via
-- the 'KafkaErrorKind' sum type.
data KafkaException = KafkaException
  { keMessage :: !Text
    -- ^ Human-readable summary; safe to log.
  , keKind    :: !KafkaErrorKind
    -- ^ Structured failure category; pattern-match on this when
    --   recovering specific errors.
  , keCause   :: !(Maybe SomeException)
    -- ^ Optional underlying exception when one fired.
  }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | What category of failure happened. Pattern-match on this to
-- decide whether to retry, fail the request, or fail the whole
-- producer / consumer.
data KafkaErrorKind
  = ConnectError
  | AuthenticationError
  | AuthorizationError
  | ConfigurationError ![Text]
    -- ^ Carries the list of validation messages.
  | TimeoutError
  | NetworkError
  | InvalidTopicError !Text
  | TopicAlreadyExistsError !Text
  | UnknownTopicOrPartitionError !Text !Int32
  | RecordTooLargeError !Int !Int
    -- ^ Carries @(actualBytes, maxBytes)@.
  | SerializationError
  | ProducerFencedError
  | TransactionAbortedError
  | OffsetOutOfRangeError !Text !Int32 !Int64
    -- ^ Carries @(topic, partition, requestedOffset)@.
  | NotInTransactionError
  | UnsupportedVersionError !Int !Int
    -- ^ Carries @(brokerMaxVersion, clientMinVersion)@.
  | DeliveryFailedError
  | UnknownError
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- Construction helpers
----------------------------------------------------------------------

kafkaException :: KafkaErrorKind -> Text -> KafkaException
kafkaException k msg = KafkaException
  { keMessage = msg
  , keKind    = k
  , keCause   = Nothing
  }

configurationError :: [Text] -> KafkaException
configurationError errs = kafkaException (ConfigurationError errs)
  ("invalid configuration: " <> T.intercalate "; " errs)

connectError :: Text -> KafkaException
connectError = kafkaException ConnectError

timeoutError :: Text -> KafkaException
timeoutError = kafkaException TimeoutError

authenticationError :: Text -> KafkaException
authenticationError = kafkaException AuthenticationError

-- | Last-resort wrapper; prefer one of the typed constructors above
-- when the call site knows the category.
unknownError :: Text -> KafkaException
unknownError = kafkaException UnknownError

----------------------------------------------------------------------
-- Bridging
----------------------------------------------------------------------

-- | Throw an 'UnknownError' carrying the 'Left' string, or return
-- the 'Right' payload. Useful while migrating internal helpers that
-- still return 'Either String a' under the public throwing surface.
orThrow :: IO (Either String a) -> IO a
orThrow act = act >>= either (throwIO . unknownError . T.pack) pure

-- | Like 'orThrow' but builds a typed 'KafkaException' from the
-- 'Left' string instead of using 'UnknownError'.
orThrowWith
  :: (Text -> KafkaException)
  -> IO (Either String a)
  -> IO a
orThrowWith mkExc act = act >>= either (throwIO . mkExc . T.pack) pure

----------------------------------------------------------------------
-- Classification
----------------------------------------------------------------------

-- | 'True' iff the failure category is transient. Aligned with
-- KIP-487 / the JVM client's @RetriableException@ marker interface:
-- callers can use this to gate a backoff-and-retry loop.
isRetriable :: KafkaException -> Bool
isRetriable e = case keKind e of
  ConnectError                       -> True
  TimeoutError                       -> True
  NetworkError                       -> True
  UnknownTopicOrPartitionError {}    -> True
  OffsetOutOfRangeError {}           -> True
  -- Producer-side fencing / aborts / serialization / config errors
  -- are NOT retriable: re-issuing the same request can never
  -- succeed without operator intervention.
  _                                  -> False

-- | 'True' iff the producer \/ consumer must be torn down and
-- re-created (the kind of failure that can't be recovered by
-- retrying the request).
isFatal :: KafkaException -> Bool
isFatal e = case keKind e of
  AuthenticationError       -> True
  AuthorizationError        -> True
  ProducerFencedError       -> True
  UnsupportedVersionError {} -> True
  ConfigurationError {}     -> True
  _                         -> False
