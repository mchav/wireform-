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

import Kafka.Streams.Config (StreamsConfig (..))
import Kafka.Streams.Errors (logAndContinue)
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

data KafkaStreams = KafkaStreams
  { ksConfig    :: !StreamsConfig
  , ksTopology  :: !Topo.TopologyValid
  , ksStatus    :: !(TVar StreamsStatus)
  , ksThread    :: !(IORef (Maybe (Async ())))
  , ksProducer  :: !(IORef (Maybe KP.Producer))
  , ksConsumer  :: !(IORef (Maybe KC.Consumer))
  , ksEngine    :: !(IORef (Maybe Engine))
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
  pure KafkaStreams
    { ksConfig    = cfg
    , ksTopology  = topo
    , ksStatus    = s
    , ksThread    = t
    , ksProducer  = p
    , ksConsumer  = c
    , ksEngine    = e
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
          let topics = sourceTopics topo
          eSubs <- KC.subscribe c (map unTopicName topics)
          case eSubs of
            Left err -> setError ks ("subscribe: " <> Text.pack err)
            Right () -> do
              ah <- async (eventLoop ks engine c)
              writeIORef (ksThread ks) (Just ah)
              atomically (writeTVar (ksStatus ks) StreamsRunning)
  where
    producerCfg = KP.defaultProducerConfig
      { KP.producerClientId  = clientId (ksConfig ks)
      }
    consumerCfg = KC.defaultConsumerConfig
      { KC.consumerClientId  = clientId (ksConfig ks)
      , KC.consumerGroupId   = applicationId (ksConfig ks)
      }

setError :: KafkaStreams -> Text -> IO ()
setError ks msg = atomically $
  writeTVar (ksStatus ks) (StreamsError msg)

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
            atomically (writeTVar (ksStatus ks) (StreamsError (Text.pack err)))
          Right recs -> do
            forM_ recs $ \rec ->
              feedSource engine
                (topicName (KC.crTopic rec))
                (KC.crKey rec)
                (KC.crValue rec)
                (Timestamp (KC.crTimestamp rec))
                (fromIntegral (KC.crPartition rec))
                (KC.crOffset rec)
            commitEngine engine
            _ <- KC.commitSync consumer
            go

closeKafkaStreams :: KafkaStreams -> IO ()
closeKafkaStreams ks = do
  atomically (writeTVar (ksStatus ks) StreamsClosing)
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
  atomically (writeTVar (ksStatus ks) StreamsClosed)

streamsStatus :: KafkaStreams -> IO StreamsStatus
streamsStatus = readTVarIO . ksStatus

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

sourceTopics :: Topo.TopologyValid -> [TopicName]
sourceTopics tv =
  let topo = Topo.topologyValidGraph tv
   in concatMap Topo.sourceTopics
        (Map.elems (Topo.topoSources topo))
