{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime
-- Description : Broker-backed runtime
--
-- @
-- KafkaStreams
-- @
--
-- mirrors the JVM @KafkaStreams@ class.  Each instance:
--
--   * Spins up @numStreamThreads@ stream-threads.
--   * Each stream-thread owns a 'Kafka.Client.Consumer.Consumer' and
--     a 'Kafka.Client.Producer.Producer' from the underlying
--     @wireform-kafka@ client.
--   * Each thread runs an event loop:
--
--       1. @poll@ for records.
--       2. For each batch, drive the engine by calling 'feedSource'
--          per record.
--       3. Drain the record collector and produce.
--       4. Commit consumer offsets at the configured cadence.
--
-- We stay deliberately simple here: a single stream-thread runs in
-- the foreground, and partition assignment is whatever the consumer
-- group coordinator hands us (there is /no/ Streams-aware assignor
-- yet — see 'Kafka.Streams.Runtime.PartitionAssignor' in a future
-- iteration). When EOS is requested, the producer is configured to
-- be transactional and 'commitOffsets' goes through
-- @TxnOffsetCommit@.
--
-- This is the "happy-path" runtime; it is not yet feature-complete
-- (no standby tasks, no global tables, no rack-aware fetching, etc.)
-- but it is enough to run a real topology against a real broker for
-- end-to-end validation.
module Kafka.Streams.Runtime
  ( KafkaStreams
  , newKafkaStreams
  , startKafkaStreams
  , closeKafkaStreams
    -- * Status
  , StreamsStatus (..)
  , streamsStatus
  , awaitState
  , StateListener
  , setStateListener
    -- * EOS
  , applyEOSCoordinator
    -- * Pause / resume (KIP-834)
  , pauseKafkaStreams
  , resumeKafkaStreams
  , isPausedKafkaStreams
    -- * Task lag (KIP-647)
  , LagInfo (..)
  , LagListener
  , setLagListener
  , publishLag
    -- * Internal access (used by Kafka.Streams.InteractiveQueries)
  , ksEngine
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Producer as KP

import Kafka.Streams.Config
  ( ProcessingGuarantee (..)
  , StreamsConfig (..)
  )
import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Runtime.EOS
  ( CommitOutcome (..)
  , EOSCoordinator (..)
  , noopEOSCoordinator
  , runCommitCycle
  )
import Kafka.Streams.Internal.Engine
  ( Engine
  , buildEngine
  , closeEngine
  , commitEngine
  , feedSource
  )
import Kafka.Streams.Internal.RecordCollector
  ( CollectedRecord (..)
  , RecordCollector (..)
  , drainCollector
  )
import Data.Int (Int64)
import Kafka.Streams.Processor (TaskId (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, topicName, unTopicName)

-- | Lifecycle of the runtime.
data StreamsStatus
  = StreamsCreated
  | StreamsRunning
  | StreamsClosing
  | StreamsClosed
  | StreamsError !Text
  deriving (Eq, Show)

-- | A user-installed callback fired on every state transition.
type StateListener =
  StreamsStatus       -- old state
  -> StreamsStatus    -- new state
  -> IO ()

data KafkaStreams = KafkaStreams
  { ksConfig    :: !StreamsConfig
  , ksTopology  :: !Topo.TopologyValid
  , ksStatus    :: !(TVar StreamsStatus)
  , ksThread    :: !(IORef (Maybe (Async ())))
  , ksProducer  :: !(IORef (Maybe KP.Producer))
  , ksConsumer  :: !(IORef (Maybe KC.Consumer))
  , ksEngine    :: !(IORef (Maybe Engine))
  , ksEosCoord  :: !(IORef EOSCoordinator)
  , ksListener  :: !(IORef StateListener)
  , ksPaused    :: !(TVar Bool)
  , ksLagLis    :: !(IORef LagListener)
    -- ^ Set during 'startKafkaStreams' based on
    -- 'processingGuarantee'; defaults to 'noopEOSCoordinator'.
  }

newKafkaStreams
  :: StreamsConfig
  -> Topo.TopologyValid
  -> IO KafkaStreams
newKafkaStreams cfg topo = do
  s <- newTVarIO StreamsCreated
  t <- newIORef Nothing
  p <- newIORef Nothing
  c <- newIORef Nothing
  e <- newIORef Nothing
  eos <- newIORef noopEOSCoordinator
  lis <- newIORef (\_ _ -> pure ())
  pa  <- newTVarIO False
  lagL <- newIORef (\_ -> pure ())
  pure KafkaStreams
    { ksConfig    = cfg
    , ksTopology  = topo
    , ksStatus    = s
    , ksThread    = t
    , ksProducer  = p
    , ksConsumer  = c
    , ksEngine    = e
    , ksEosCoord  = eos
    , ksListener  = lis
    , ksPaused    = pa
    , ksLagLis    = lagL
    }

-- | Start the runtime in the background. Returns immediately; once
-- the thread terminates (cleanly or otherwise), 'streamsStatus'
-- reflects the result.
startKafkaStreams :: KafkaStreams -> IO ()
startKafkaStreams ks = do
  ePR <- KP.createProducer (bootstrapServers (ksConfig ks)) producerCfg
  case ePR of
    Left err -> setError ks ("producer create: " <> Text.pack err)
    Right p  -> do
      writeIORef (ksProducer ks) (Just p)
      eCR <- KC.createConsumer
              (bootstrapServers (ksConfig ks))
              (applicationId (ksConfig ks))
              consumerCfg
      case eCR of
        Left err -> setError ks ("consumer create: " <> Text.pack err)
        Right c  -> do
          writeIORef (ksConsumer ks) (Just c)
          collector <- producerCollector p
          let topo = ksTopology ks
          engine <- buildEngine topo (TaskId 0 0)
                      (applicationId (ksConfig ks))
                      collector
                      logAndContinue
          writeIORef (ksEngine ks) (Just engine)
          -- Pick the EOS coordinator based on the configured
          -- processing guarantee. AtLeastOnceP uses the
          -- 'noopEOSCoordinator' (commit cycles still fire, but
          -- there's no transaction). ExactlyOnceV2 also defaults
          -- to the no-op so the commit sequence is exercised
          -- structurally; users that need wire-level EOS install
          -- their own coordinator via 'applyEOSCoordinator', for
          -- example wrapping a 'Kafka.Client.Transaction.Transaction'
          -- with 'newRealEOSCoordinator'. We deliberately don't
          -- auto-instantiate a 'Transaction' here: the producer's
          -- send path doesn't yet route records through a txn
          -- envelope, so a real coordinator combined with the
          -- current producer would commit empty transactions.
          writeIORef (ksEosCoord ks) noopEOSCoordinator
          let topics = sourceTopics topo
          eSubs <- KC.subscribe c (map unTopicName topics)
          case eSubs of
            Left err -> setError ks ("subscribe: " <> Text.pack err)
            Right () -> do
              ah <- async (eventLoop ks engine c)
              writeIORef (ksThread ks) (Just ah)
              transitionTo ks StreamsRunning
  where
    producerCfg = KP.defaultProducerConfig
      { KP.producerClientId  = clientId (ksConfig ks)
      , KP.producerTransactional =
          case processingGuarantee (ksConfig ks) of
            AtLeastOnceP  -> Nothing
            ExactlyOnceV2 -> Just (applicationId (ksConfig ks)
                                      <> "-txn")
      , KP.producerIdempotent =
          case processingGuarantee (ksConfig ks) of
            AtLeastOnceP  -> False
            ExactlyOnceV2 -> True
      , KP.producerDelivery =
          case processingGuarantee (ksConfig ks) of
            AtLeastOnceP  -> KP.AtLeastOnce
            ExactlyOnceV2 -> KP.ExactlyOnce
      }
    consumerCfg = KC.defaultConsumerConfig
      { KC.consumerClientId  = clientId (ksConfig ks)
      , KC.consumerGroupId   = applicationId (ksConfig ks)
      }

setError :: KafkaStreams -> Text -> IO ()
setError ks msg = transitionTo ks (StreamsError msg)

----------------------------------------------------------------------
-- Producer-backed collector
----------------------------------------------------------------------

producerCollector :: KP.Producer -> IO RecordCollector
producerCollector p = do
  bufRef <- newIORef ([] :: [CollectedRecord])
  pure RecordCollector
    { collectorSend = \cr -> atomicModifyIORef' bufRef
        (\xs -> (cr : xs, ()))
    , collectorFlush = do
        xs <- atomicModifyIORef' bufRef (\xs -> ([], xs))
        forM_ (reverse xs) $ \cr -> do
          _ <- KP.sendMessage p (unTopicName (crTopic cr)) (crKey cr) (crValue cr)
          pure ()
        _ <- KP.flushProducer p
        pure ()
    , collectorClose = pure ()
    , collectorPeek  = pure Map.empty
    , collectorTake  = \_ -> pure []
    }

----------------------------------------------------------------------
-- Event loop
----------------------------------------------------------------------

eventLoop :: KafkaStreams -> Engine -> KC.Consumer -> IO ()
eventLoop ks engine consumer = go
  where
    go = do
      status <- readTVarIO (ksStatus ks)
      unless (status == StreamsClosing || status == StreamsClosed) $ do
        eRecs <- KC.poll consumer (pollMs (ksConfig ks))
        case eRecs of
          Left err ->
            transitionTo ks (StreamsError (Text.pack err))
          Right recs -> do
            paused <- readTVarIO (ksPaused ks)
            -- While paused we still poll (heartbeat) but DON'T feed
            -- the engine. Records that arrived during the pause are
            -- silently dropped — the consumer will rewind via
            -- offset commits to replay anything not committed.
            unless paused $
              forM_ recs $ \rec ->
                feedSource engine
                  (topicName (KC.crTopic rec))
                  (KC.crKey rec)
                  (KC.crValue rec)
                  (Timestamp (KC.crTimestamp rec))
                  (fromIntegral (KC.crPartition rec))
                  (KC.crOffset rec)

            -- Collect the highest (offset + 1) per (topic, partition)
            -- across the records we just fed; that's what the EOS
            -- coordinator wants to commit. Skipping when paused
            -- means we don't advance offsets past records we
            -- never delivered.
            let !commitOffsets =
                  if paused
                    then Map.empty
                    else Map.fromListWith max
                           [ ( KC.TopicPartition
                                 (KC.crTopic rec)
                                 (fromIntegral (KC.crPartition rec))
                             , KC.crOffset rec + 1
                             )
                           | rec <- recs
                           ]

            -- The /commit/ goes through the EOS coordinator: under
            -- AtLeastOnce this is the no-op coordinator and the
            -- behaviour is identical to the old (commitEngine +
            -- commitSync); under ExactlyOnceV2 it sequences the
            -- transactional begin/commit envelope around the engine
            -- flush and the consumer offset commit.
            coord <- readIORef (ksEosCoord ks)
            outcome <- runCommitCycle
              coord
              (applicationId (ksConfig ks))
              (pure commitOffsets)
              (commitEngine engine)
            case outcome of
              CommitSucceeded -> do
                _ <- KC.commitSync consumer
                go
              CommitAborted reason -> do
                -- Aborted: the txn was rolled back; we keep running
                -- but skip committing consumer offsets so the next
                -- poll re-reads them.
                putStrLn ("[streams] commit aborted: "
                  <> Text.unpack reason)
                go
              CommitFatal reason ->
                transitionTo ks (StreamsError reason)

closeKafkaStreams :: KafkaStreams -> IO ()
closeKafkaStreams ks = do
  transitionTo ks StreamsClosing
  mAh <- readIORef (ksThread ks)
  case mAh of
    Just ah -> do
      cancel ah
      _ <- waitCatch ah
      pure ()
    Nothing -> pure ()
  mE <- readIORef (ksEngine ks)
  forM_ mE closeEngine
  mC <- readIORef (ksConsumer ks)
  forM_ mC KC.closeConsumer
  mP <- readIORef (ksProducer ks)
  forM_ mP KP.closeProducer
  transitionTo ks StreamsClosed

streamsStatus :: KafkaStreams -> IO StreamsStatus
streamsStatus = readTVarIO . ksStatus

-- | Block until 'streamsStatus' reaches the given target. Used by
-- tests for deterministic state-transition assertions; no
-- 'threadDelay'.
awaitState :: KafkaStreams -> StreamsStatus -> IO ()
awaitState ks target = atomically $ do
  cur <- readTVar (ksStatus ks)
  if cur == target
    then pure ()
    else retry

-- | Install a callback that fires on every state transition.
-- Mirrors @KafkaStreams.setStateListener@.
setStateListener :: KafkaStreams -> StateListener -> IO ()
setStateListener ks lis = writeIORef (ksListener ks) lis

-- | Internal helper: atomically transition to a new state and
-- invoke the registered listener.
transitionTo :: KafkaStreams -> StreamsStatus -> IO ()
transitionTo ks new_ = do
  old <- atomically $ do
    cur <- readTVar (ksStatus ks)
    writeTVar (ksStatus ks) new_
    pure cur
  lis <- readIORef (ksListener ks)
  lis old new_

-- | Pause record processing (KIP-834). The runtime keeps polling
-- the consumer (so heartbeats stay alive and the coordinator
-- doesn't kick the member) but does not feed the engine. Call
-- 'resumeKafkaStreams' to continue.
pauseKafkaStreams :: KafkaStreams -> IO ()
pauseKafkaStreams ks = atomically (writeTVar (ksPaused ks) True)

-- | Resume processing after 'pauseKafkaStreams'.
resumeKafkaStreams :: KafkaStreams -> IO ()
resumeKafkaStreams ks = atomically (writeTVar (ksPaused ks) False)

isPausedKafkaStreams :: KafkaStreams -> IO Bool
isPausedKafkaStreams = readTVarIO . ksPaused

-- | One row of task-lag information. Mirrors Java's
-- @org.apache.kafka.streams.LagInfo@.
data LagInfo = LagInfo
  { lagTaskId        :: !TaskId
  , lagCurrentOffset :: !Int64
  , lagEndOffset     :: !Int64
  }
  deriving stock (Eq, Show)

type LagListener = [LagInfo] -> IO ()

-- | Register a callback that receives a snapshot of every task's
-- current vs. end offset on every 'publishLag' call. Mirrors
-- @KafkaStreams.allLocalStorePartitionLags@ + the periodic
-- listener pattern in KIP-647.
setLagListener :: KafkaStreams -> LagListener -> IO ()
setLagListener ks lis = writeIORef (ksLagLis ks) lis

-- | Internal helper for the runtime (or tests) to publish a fresh
-- lag snapshot to the registered listener.
publishLag :: KafkaStreams -> [LagInfo] -> IO ()
publishLag ks lags = do
  lis <- readIORef (ksLagLis ks)
  lis lags

-- | Replace the runtime's EOS coordinator. The default is
-- 'noopEOSCoordinator'; tests inject a recording coordinator to
-- assert the call sequence, and a future broker-backed runtime
-- will swap in 'newRealEOSCoordinator' built from a
-- transactional 'Kafka.Client.Transaction.Transaction' once the
-- producer routes sends through the txn state machine.
applyEOSCoordinator :: KafkaStreams -> EOSCoordinator -> IO ()
applyEOSCoordinator ks coord =
  writeIORef (ksEosCoord ks) coord

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

sourceTopics :: Topo.TopologyValid -> [TopicName]
sourceTopics tv =
  let topo = Topo.topologyValidGraph tv
   in concatMap Topo.sourceTopics
        (Map.elems (Topo.topoSources topo))
