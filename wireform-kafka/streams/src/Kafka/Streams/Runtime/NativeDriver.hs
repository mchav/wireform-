{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Streams.Runtime.NativeDriver
Description : Pluggable driver layer used by Kafka.Streams.Runtime

The streams runtime engine never talks to "Kafka.Client.Producer"
or "Kafka.Client.Consumer" directly. It talks to a 'StreamDriver'
record-of-IO that carries:

  * the consumer-side hooks ('sdConsumerSubscribe',
    'sdConsumerPoll', 'sdConsumerCommit', 'sdConsumerClose'),
  * the producer-side hooks ('sdProducerSend', 'sdProducerFlush',
    'sdProducerClose'),
  * the EOS-V2 transactional hooks
    ('sdProducerBeginTxn', 'sdProducerCommitTxn',
    'sdProducerAbortTxn', 'sdProducerSendOffsetsToTxn'),
  * the rebalance signal channel ('sdRebalanceEvent').

Two constructors ship out of the box:

  * 'newNativeDriver' wires the driver against a real
    'KP.Producer' + 'KC.Consumer' (and an optional bound
    'KT.Transaction' for EOS-V2). Production code uses this.
  * 'newMockDriver' returns a driver whose IO surface is
    in-memory. Tests use it to exercise the runtime
    deterministically without a broker or even a producer/consumer
    process.

The runtime treats the two as interchangeable; this module is the
seam that keeps the engine decoupled from the wire layer.
-}
module Kafka.Streams.Runtime.NativeDriver
  ( StreamDriver (..)
  , RebalanceEvent (..)
  , newNativeDriver
    -- * Mock driver (tests)
  , MockDriverHandle
  , newMockDriver
  , mockDriverInjectPoll
  , mockDriverDrainSends
  , mockDriverTxnLog
  , mockDriverCommittedOffsets
  , mockDriverCommitCount
  , MockSend (..)
  , MockTxnEvent (..)
  , offsetResetForConsumer
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Int (Int64)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
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

-- | The full surface the streams runtime needs to talk to a
-- broker. Records of IO so the engine can swap in a mock under
-- test or a native driver in production.
data StreamDriver = StreamDriver
  { -- ^ Subscribe the consumer to a list of source topics. Called
    -- once at startup.
    sdConsumerSubscribe
      :: !([Text] -> IO (Either String ()))
    -- | Poll the consumer for records.
  , sdConsumerPoll
      :: !(Int -> IO (Either String [KC.ConsumerRecord]))
    -- | Synchronously commit current consumer offsets (non-EOS
    -- path). Returns 'Right' on success.
  , sdConsumerCommit
      :: !(IO (Either String ()))
    -- | Tear down the consumer.
  , sdConsumerClose
      :: !(IO ())
    -- | Send a record. Returns Right metadata on success.
  , sdProducerSend
      :: !(Text -> Maybe ByteString -> ByteString
           -> IO (Either String KP.RecordMetadata))
    -- | Block until every previously sent record has been
    -- acknowledged by the broker.
  , sdProducerFlush
      :: !(IO (Either String ()))
    -- | Tear down the producer.
  , sdProducerClose
      :: !(IO ())
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
      :: !(Text -> HashMap KC.TopicPartition Int64
           -> IO (Either String ()))
    -- | Drain an event from the rebalance listener queue, if
    --   one is pending.
  , sdRebalanceEvent
      :: !(IO (Maybe RebalanceEvent))
  }

-- | Build a 'StreamDriver' that delegates to a live 'Producer' +
-- 'Consumer' pair. The optional 'KT.Transaction' is required for
-- EOS-V2 (binding it via 'KP.bindTransaction' is the caller's
-- responsibility before passing the producer in).
newNativeDriver
  :: KP.Producer
  -> KC.Consumer
  -> Maybe KT.Transaction
  -> IO StreamDriver
newNativeDriver producer consumer mTxn = do
  rebalanceQ <- newTQueueIO
  pure StreamDriver
    { sdConsumerSubscribe = KC.subscribe consumer
    , sdConsumerPoll      = \timeoutMs -> KC.poll consumer timeoutMs
    , sdConsumerCommit    = KC.commitSync consumer
    , sdConsumerClose     = KC.closeConsumer consumer
    , sdProducerSend      = \topic key value ->
        KP.sendMessage producer topic key value
    , sdProducerFlush     = KP.flushProducer producer
    , sdProducerClose     = KP.closeProducer producer
    , sdProducerBeginTxn  = case mTxn of
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

----------------------------------------------------------------------
-- Mock driver
----------------------------------------------------------------------

-- | A captured outbound record (what 'sdProducerSend' was called
-- with).
data MockSend = MockSend
  { mockSendTopic :: !Text
  , mockSendKey   :: !(Maybe ByteString)
  , mockSendValue :: !ByteString
  }
  deriving stock (Eq, Show, Generic)

-- | A captured EOS-V2 transactional event in call order.
data MockTxnEvent
  = MockTxnBegin
  | MockTxnCommit
  | MockTxnAbort
  | MockTxnSendOffsets !Text !(HashMap KC.TopicPartition Int64)
  deriving stock (Eq, Show, Generic)

-- | Test-side handle. Lets the test driver:
--
--   * push records the runtime will see on the next poll
--     ('mockDriverInjectPoll'),
--   * read everything the runtime tried to send
--     ('mockDriverDrainSends'),
--   * inspect the EOS call sequence ('mockDriverTxnLog'),
--   * inspect the offsets the runtime committed
--     ('mockDriverCommittedOffsets').
data MockDriverHandle = MockDriverHandle
  { mdhPollQueue  :: !(TVar (Seq [KC.ConsumerRecord]))
  , mdhSendsOut   :: !(TVar (Seq MockSend))
  , mdhFlushed    :: !(TVar Int)
  , mdhClosed     :: !(TVar (Bool, Bool))
    -- ^ (consumerClosed, producerClosed)
  , mdhSubscribed :: !(TVar [Text])
  , mdhCommits    :: !(TVar Int)
  , mdhTxn        :: !(TVar (Seq MockTxnEvent))
  , mdhSentOffs   :: !(IORef (Seq (Text, HashMap KC.TopicPartition Int64)))
  }

-- | Build a fresh mock driver. The driver starts out idle: every
-- poll returns @Right []@ until 'mockDriverInjectPoll' is called.
newMockDriver :: IO (StreamDriver, MockDriverHandle)
newMockDriver = do
  pollQ  <- newTVarIO Seq.empty
  sends  <- newTVarIO Seq.empty
  flushd <- newTVarIO 0
  closed <- newTVarIO (False, False)
  subs   <- newTVarIO []
  cmts   <- newTVarIO 0
  txn    <- newTVarIO Seq.empty
  offs   <- newIORef Seq.empty
  let h = MockDriverHandle pollQ sends flushd closed subs cmts txn offs
      drv = StreamDriver
        { sdConsumerSubscribe = \topics -> do
            atomically (writeTVar subs topics)
            pure (Right ())
        , sdConsumerPoll = \_timeoutMs -> atomically $ do
            q <- readTVar pollQ
            case Seq.viewl q of
              Seq.EmptyL -> pure (Right [])
              h_ Seq.:< rest -> do
                writeTVar pollQ rest
                pure (Right h_)
        , sdConsumerCommit = do
            atomically $ modifyTVar' cmts (+ 1)
            pure (Right ())
        , sdConsumerClose = atomically $
            modifyTVar' closed (\(_, p) -> (True, p))
        , sdProducerSend = \topic key value -> do
            atomically $ modifyTVar' sends
              (\s -> s |> MockSend topic key value)
            pure (Right KP.RecordMetadata
              { KP.metadataTopic     = topic
              , KP.metadataPartition = 0
              , KP.metadataOffset    = 0
              , KP.metadataTimestamp = 0
              })
        , sdProducerFlush = do
            atomically $ modifyTVar' flushd (+ 1)
            pure (Right ())
        , sdProducerClose = atomically $
            modifyTVar' closed (\(c, _) -> (c, True))
        , sdProducerBeginTxn  = do
            atomically $ modifyTVar' txn (\s -> s |> MockTxnBegin)
            pure (Right ())
        , sdProducerCommitTxn = do
            atomically $ modifyTVar' txn (\s -> s |> MockTxnCommit)
            pure (Right ())
        , sdProducerAbortTxn  = do
            atomically $ modifyTVar' txn (\s -> s |> MockTxnAbort)
            pure (Right ())
        , sdProducerSendOffsetsToTxn = \gid o -> do
            atomically $ modifyTVar' txn (\s -> s |> MockTxnSendOffsets gid o)
            modifyIORef' offs (\s -> s |> (gid, o))
            pure (Right ())
        , sdRebalanceEvent = pure Nothing
        }
  pure (drv, h)

-- | Push a batch of consumer records the runtime will receive on
-- the next 'sdConsumerPoll'.
mockDriverInjectPoll :: MockDriverHandle -> [KC.ConsumerRecord] -> IO ()
mockDriverInjectPoll h batch =
  atomically $ modifyTVar' (mdhPollQueue h) (\s -> s |> batch)

-- | Drain everything the runtime has sent so far. Subsequent
-- calls only see records produced after the previous drain.
mockDriverDrainSends :: MockDriverHandle -> IO [MockSend]
mockDriverDrainSends h = atomically $ do
  s <- readTVar (mdhSendsOut h)
  writeTVar (mdhSendsOut h) Seq.empty
  pure (Foldable.toList s)

-- | Read the captured EOS-V2 call sequence in order.
mockDriverTxnLog :: MockDriverHandle -> IO [MockTxnEvent]
mockDriverTxnLog h =
  Foldable.toList <$> atomically (readTVar (mdhTxn h))

-- | List every (groupId, offsets) pair that was committed inside
-- a transaction, in order.
mockDriverCommittedOffsets
  :: MockDriverHandle
  -> IO [(Text, HashMap KC.TopicPartition Int64)]
mockDriverCommittedOffsets h =
  Foldable.toList <$> readIORef (mdhSentOffs h)

-- | Number of times 'sdConsumerCommit' fired (non-EOS path).
mockDriverCommitCount :: MockDriverHandle -> IO Int
mockDriverCommitCount h = atomically $ readTVar (mdhCommits h)
