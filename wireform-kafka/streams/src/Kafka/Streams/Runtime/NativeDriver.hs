{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Streams.Runtime.NativeDriver
Description : Driver that wires KafkaStreams runtime to the real client

The existing engine drives an in-process @MockStreamsDriver@; for
production we need a driver that uses 'Kafka.Client.Producer' and
'Kafka.Client.Consumer' against a real broker. This module is the
abstraction layer + a concrete implementation.

The shape mirrors @StreamsKafkaClients@ on the Java side:

@
data StreamDriver = StreamDriver
  { sdConsumerPoll       :: …
  , sdProducerSend       :: …
  , sdProducerBeginTxn   :: …
  , sdProducerCommitTxn  :: …
  , sdProducerAbortTxn   :: …
  , sdProducerSendOffsetsToTxn :: …
  , sdRebalanceListener  :: …
  }
@

The constructor 'newNativeDriver' wires every callback against an
already-built 'Producer' + 'Consumer'. EOS-V2 commit boundaries
fire when the engine's commit tick triggers
'sdProducerCommitTxn' (which goes through the producer's bound
'Transaction' from @Kafka.Client.Producer.bindTransaction@).

This driver is /not/ wired into the streaming runtime yet (that
would be a much larger refactor of @Kafka.Streams.Runtime@). It's
the transition point: tests can construct a 'StreamDriver' and
exercise it directly; once the engine refactor lands the module
becomes the bridge.
-}
module Kafka.Streams.Runtime.NativeDriver
  ( StreamDriver (..)
  , RebalanceEvent (..)
  , newNativeDriver
  , offsetResetForConsumer
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Producer as KP
import qualified Kafka.Client.Transaction as KT

-- | Rebalance signal handed to the driver's listener.
data RebalanceEvent
  = RebalanceAssigned   ![KC.TopicPartition]
  | RebalanceRevoked    ![KC.TopicPartition]
  | RebalanceLost       ![KC.TopicPartition]
  deriving stock (Eq, Show, Generic)

-- | The minimum surface the streams runtime needs to talk to a
-- real broker. Records of IO so the engine can swap in a mock
-- driver under test.
data StreamDriver = StreamDriver
  { -- | Poll the consumer for records.
    sdConsumerPoll
      :: !(Int -> IO (Either String [KC.ConsumerRecord]))
    -- | Send a record. Returns Right metadata on success.
  , sdProducerSend
      :: !(Text -> Maybe ByteString -> ByteString
           -> IO (Either String KP.RecordMetadata))
    -- | Begin a transaction (EOS-V2). 'Right' on success.
  , sdProducerBeginTxn
      :: !(IO (Either String ()))
    -- | Commit a transaction.
  , sdProducerCommitTxn
      :: !(IO (Either String ()))
    -- | Abort a transaction.
  , sdProducerAbortTxn
      :: !(IO (Either String ()))
    -- | Send consumer offsets as part of the open transaction
    --   (KIP-447).
  , sdProducerSendOffsetsToTxn
      :: !(Text -> Map KC.TopicPartition Int64
           -> IO (Either String ()))
    -- | Drain an event from the rebalance listener queue, if
    --   one is pending.
  , sdRebalanceEvent
      :: !(IO (Maybe RebalanceEvent))
  }

-- | Build a 'StreamDriver' that delegates to a live 'Producer' +
-- 'Consumer' pair. Optional 'KT.Transaction' is required for
-- EOS-V2 (binding it via 'KP.bindTransaction' is the caller's
-- responsibility).
newNativeDriver
  :: KP.Producer
  -> KC.Consumer
  -> Maybe KT.Transaction
  -> IO StreamDriver
newNativeDriver producer consumer mTxn = do
  -- The rebalance event queue would be populated by the
  -- onPartitionsAssigned / Revoked callbacks once the consumer
  -- exposes them. For now we just expose an empty queue so the
  -- caller can poll without blocking; the real callbacks land in
  -- a follow-up to 'Kafka.Client.Consumer'.
  rebalanceQ <- newTQueueIO
  pure StreamDriver
    { sdConsumerPoll = \timeoutMs -> KC.poll consumer timeoutMs
    , sdProducerSend = \topic key value ->
        KP.sendMessage producer topic key value
    , sdProducerBeginTxn = case mTxn of
        Nothing -> pure (Left "EOS not configured: producer has no bound Transaction")
        Just t -> do
          r <- KT.beginTransaction t
          pure $ case r of
            Left e   -> Left (show e)
            Right () -> Right ()
    , sdProducerCommitTxn = case mTxn of
        Nothing -> pure (Right ())  -- non-transactional: no-op
        Just t -> do
          r <- KT.commitTransaction t
          pure $ case r of
            Left e   -> Left (show e)
            Right () -> Right ()
    , sdProducerAbortTxn = case mTxn of
        Nothing -> pure (Right ())
        Just t -> do
          r <- KT.abortTransaction t
          pure $ case r of
            Left e   -> Left (show e)
            Right () -> Right ()
    , sdProducerSendOffsetsToTxn = \groupId offsets -> case mTxn of
        Nothing -> pure (Left "EOS not configured: producer has no bound Transaction")
        Just t -> do
          r <- KT.commitOffsetsInTransaction t groupId offsets
          pure $ case r of
            Left e   -> Left (show e)
            Right () -> Right ()
    , sdRebalanceEvent = atomically (tryReadTQueue rebalanceQ)
    }

-- | Translate a 'KC.OffsetResetStrategy' into the value the
-- runtime should use for @auto.offset.reset@ on the underlying
-- consumer. Wired by the engine when constructing the consumer
-- for a source topic.
offsetResetForConsumer :: KC.OffsetResetStrategy -> Text
offsetResetForConsumer = \case
  KC.Earliest -> "earliest"
  KC.Latest   -> "latest"
  KC.None     -> "none"
