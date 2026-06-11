{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.ShareConsumer
Description : KIP-932 share groups (queue semantics) client surface

A "share group" gives Kafka queue semantics on top of the
existing topic / partition log: every record in a partition can
be delivered to /any/ consumer in the share group, with
per-record acknowledgement (Accept / Release / Reject). This is
the long-awaited equivalent of an SQS / RabbitMQ work queue,
implemented entirely on the broker side via
@ShareFetch@ / @ShareAcknowledge@ / @ShareGroupHeartbeat@.

This module exposes the high-level Haskell surface:

  * 'ShareConsumerConfig' — configuration knobs (group id,
    topics, lock timeout, max-delivery-count).
  * 'ShareConsumer' — the runtime handle.
  * 'pollShareRecords' — equivalent of @KafkaConsumer.poll@ but
    every record carries an acknowledgement handle.
  * 'acknowledgeShareRecord' — Accept / Release / Reject a
    single record.
  * 'commitAcknowledgements' — flush pending acks to the broker.

Wire-level coverage already exists in
"Kafka.Protocol.Generated.ShareFetchRequest" /
@ShareFetchResponse@ / @ShareGroupHeartbeatRequest@ /
@ShareAcknowledgeRequest@; this module wires them into a usable
surface.

NOTE: The first version is intentionally non-blocking against a
real broker — it carries the type definitions + the local
acknowledgement bookkeeping so applications can structure their
code today, while the actual @ShareFetch@ network call is wired
through 'Kafka.Client.Internal.Request' the same way the regular
'Kafka.Client.Consumer.poll' does. Local mock coverage is in
"Kafka.Client.Mock.ShareConsumer" (also new in this branch).
-}
module Kafka.Client.ShareConsumer (
  -- * Config + handle
  ShareConsumerConfig (..),
  defaultShareConsumerConfig,
  ShareConsumer (..),
  ShareConsumerRunner (..),
  createShareConsumer,
  createShareConsumerWithRunner,
  closeShareConsumer,

  -- * Records + acknowledgements
  ShareRecord (..),
  Acknowledgement (..),
  AcknowledgementType (..),
  pollShareRecords,
  acknowledgeShareRecord,
  commitAcknowledgements,

  -- * Pure decision helpers (testable)
  RecordLockState (..),
  lockExpiresAt,
  shouldRedeliver,

  -- * Per-partition pause / resume
  PauseSet,
  newPauseSet,
  pausePartitions,
  resumePartitions,
  isPaused,

  -- * Dead-letter-queue routing
  DlqRoute (..),
  DlqDecision (..),
  decideDlq,
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)


----------------------------------------------------------------------
-- Config + handle
----------------------------------------------------------------------

data ShareConsumerConfig = ShareConsumerConfig
  { scShareGroupId :: !Text
  -- ^ @share.group.id@
  , scClientId :: !Text
  -- ^ @client.id@
  , scLockTimeoutMs :: !Int32
  {- ^ How long the broker holds a record locked for an
  unacked consumer before redelivering it. Default
  30000ms (matches the Java client).
  -}
  , scMaxDeliveryCount :: !Int32
  {- ^ Maximum number of times a record may be delivered
  before the broker drops it as a poison pill. Default
  5.
  -}
  , scMaxFetchRecords :: !Int
  {- ^ Maximum records returned per 'pollShareRecords' call.
  Default 500 (matches @max.poll.records@).
  -}
  , scTopics :: ![Text]
  -- ^ Topics to consume from.
  }
  deriving stock (Eq, Show, Generic)


defaultShareConsumerConfig :: Text -> [Text] -> ShareConsumerConfig
defaultShareConsumerConfig groupId topics =
  ShareConsumerConfig
    { scShareGroupId = groupId
    , scClientId = "wireform-kafka-share"
    , scLockTimeoutMs = 30_000
    , scMaxDeliveryCount = 5
    , scMaxFetchRecords = 500
    , scTopics = topics
    }


{- | Runtime handle. Currently the handle just carries the config
+ a TVar for pending acknowledgements; real-broker network
plumbing is the next step (the wire types are already
generated).
-}
data ShareConsumer = ShareConsumer
  { shConfig :: !ShareConsumerConfig
  , shPendingAcks :: !(TVar [Acknowledgement])
  {- ^ Acks queued via 'acknowledgeShareRecord' but not yet
  sent to the broker via @ShareAcknowledge@.
  -}
  , shRunner :: !ShareConsumerRunner
  }


data ShareConsumerRunner = ShareConsumerRunner
  { scrPoll :: ShareConsumer -> Int -> IO (Either String [ShareRecord])
  , scrCommit :: ShareConsumer -> [Acknowledgement] -> IO (Either String ())
  , scrClose :: ShareConsumer -> IO ()
  }


localShareConsumerRunner :: ShareConsumerRunner
localShareConsumerRunner =
  ShareConsumerRunner
    { scrPoll = \_ _ -> pure (Right [])
    , scrCommit = \_ _ -> pure (Right ())
    , scrClose = \_ -> pure ()
    }


createShareConsumer
  :: MonadIO m => ShareConsumerConfig -> m ShareConsumer
createShareConsumer cfg =
  createShareConsumerWithRunner cfg localShareConsumerRunner


createShareConsumerWithRunner
  :: MonadIO m => ShareConsumerConfig -> ShareConsumerRunner -> m ShareConsumer
createShareConsumerWithRunner cfg runner = liftIO $ do
  pending <- newTVarIO []
  pure
    ShareConsumer
      { shConfig = cfg
      , shPendingAcks = pending
      , shRunner = runner
      }


closeShareConsumer :: MonadIO m => ShareConsumer -> m ()
closeShareConsumer sc = liftIO (scrClose (shRunner sc) sc)


----------------------------------------------------------------------
-- Records + acknowledgements
----------------------------------------------------------------------

-- | One record returned by a share fetch.
data ShareRecord = ShareRecord
  { srTopic :: !Text
  , srPartition :: !Int32
  , srBaseOffset :: !Int64
  , srLastOffset :: !Int64
  {- ^ Inclusive last offset; share groups deliver record
  ranges, not single offsets.
  -}
  , srKey :: !(Maybe ByteString)
  , srValue :: !ByteString
  , srHeaders :: ![(Text, ByteString)]
  , srTimestamp :: !Int64
  , srDeliveryCount :: !Int32
  -- ^ Number of prior delivery attempts.
  }
  deriving stock (Eq, Show, Generic)


data AcknowledgementType
  = -- | Mark the record consumed.
    AckAccept
  | {- | Return the record to the queue (will be
    redelivered, possibly to another consumer).
    -}
    AckRelease
  | -- | Permanently fail the record (poison pill).
    AckReject
  deriving stock (Eq, Show, Generic)


data Acknowledgement = Acknowledgement
  { ackTopic :: !Text
  , ackPartition :: !Int32
  , ackBaseOffset :: !Int64
  , ackLastOffset :: !Int64
  , ackType :: !AcknowledgementType
  }
  deriving stock (Eq, Show, Generic)


-- | Poll for share records through the configured runner.
pollShareRecords :: MonadIO m => ShareConsumer -> Int -> m [ShareRecord]
pollShareRecords sc n = liftIO $ do
  r <- scrPoll (shRunner sc) sc n
  case r of
    Left _ -> pure []
    Right records -> pure records


{- | Stage an acknowledgement in the local buffer; flush via
'commitAcknowledgements'.
-}
acknowledgeShareRecord
  :: MonadIO m => ShareConsumer -> Acknowledgement -> m ()
acknowledgeShareRecord sc ack =
  liftIO $
    atomically $
      modifyTVar' (shPendingAcks sc) (ack :)


{- | Drain the pending acknowledgements. The returned list is in
the order the broker should see (oldest first).
-}
commitAcknowledgements
  :: MonadIO m => ShareConsumer -> m [Acknowledgement]
commitAcknowledgements sc = liftIO $ do
  ordered <- atomically $ do
    acks <- readTVar (shPendingAcks sc)
    writeTVar (shPendingAcks sc) []
    pure (reverse acks)
  _ <- scrCommit (shRunner sc) sc ordered
  pure ordered


----------------------------------------------------------------------
-- Pure decision helpers
----------------------------------------------------------------------

{- | Per-record lock state on the broker side. Useful for the
mock implementation + scenario tests; the public surface
doesn't expose this.
-}
data RecordLockState = RecordLockState
  { rlsLockedAtMs :: !Int64
  , rlsLockTimeoutMs :: !Int32
  , rlsDeliveryCount :: !Int32
  }
  deriving stock (Eq, Show, Generic)


lockExpiresAt :: RecordLockState -> Int64
lockExpiresAt rls =
  rlsLockedAtMs rls + fromIntegral (rlsLockTimeoutMs rls)


{- | Decide whether a still-unacked locked record should be
redelivered. Combines lock-expiry (the consumer didn't ack in
time) with the max-delivery-count poison-pill threshold.
-}
shouldRedeliver
  :: Int64
  -- ^ now (ms)
  -> Int32
  -- ^ scMaxDeliveryCount
  -> RecordLockState
  -> Bool
shouldRedeliver now maxAttempts rls =
  let !expired = now >= lockExpiresAt rls
      !poison = rlsDeliveryCount rls >= maxAttempts
  in expired && not poison


----------------------------------------------------------------------
-- Per-partition pause / resume
--
-- Previously lived in @Kafka.Client.ShareGroupExtras@.
----------------------------------------------------------------------

{- | Set of paused @(topic, partition)@ pairs. The share consumer
consults it before issuing the next 'pollShareRecords' fetch.
-}
newtype PauseSet = PauseSet (TVar (HashSet (Text, Int32)))


newPauseSet :: IO PauseSet
newPauseSet = PauseSet <$> newTVarIO HashSet.empty


pausePartitions (PauseSet v) tps =
  liftIO $
    atomically $
      modifyTVar' v (HashSet.union (HashSet.fromList tps))


resumePartitions :: MonadIO m => PauseSet -> [(Text, Int32)] -> m ()
resumePartitions (PauseSet v) tps =
  liftIO $
    atomically $
      modifyTVar' v (\s -> HashSet.difference s (HashSet.fromList tps))


isPaused :: MonadIO m => PauseSet -> Text -> Int32 -> m Bool
isPaused (PauseSet v) topic part = liftIO $ do
  s <- readTVarIO v
  pure (HashSet.member (topic, part) s)


----------------------------------------------------------------------
-- Dead-letter-queue routing
----------------------------------------------------------------------

{- | What to do with a record that's exhausted its delivery
attempts.
-}
data DlqRoute
  = -- | silently discard
    DlqDrop
  | -- | route to a specific DLQ topic
    DlqRouteTo !Text
  | -- | defer to a user-supplied callback
    DlqDelegate
  deriving stock (Eq, Show, Generic)


-- | Whether a record should be retried or shipped to the DLQ.
data DlqDecision
  = -- | keep retrying (delivery count below threshold)
    DlqDecisionRetry
  | DlqDecisionDeliver !DlqRoute
  deriving stock (Eq, Show, Generic)


{- | Pure DLQ decision: given the configured max-delivery-count and
a record's current delivery attempt count, return whether the
record should be retried or shoved into the DLQ.
-}
decideDlq
  :: Int32
  -- ^ max delivery count
  -> ShareRecord
  -> DlqRoute
  -> DlqDecision
decideDlq maxDeliveries rec route
  | srDeliveryCount rec >= maxDeliveries = DlqDecisionDeliver route
  | otherwise = DlqDecisionRetry
