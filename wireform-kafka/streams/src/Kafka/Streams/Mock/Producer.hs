{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Producer
-- Description : Producer view over a 'MockCluster'
--
-- A 'MockProducer' is a thin wrapper that sends to the cluster's
-- partition logs, optionally inside a transaction, with a per-call
-- consultation of the 'FaultPolicy' for injected errors.
--
-- The producer's behaviour mirrors the Java
-- @org.apache.kafka.clients.producer.KafkaProducer@:
--
--   * non-transactional: each 'send' immediately appends to the log
--     and returns the assigned offset (after consulting the fault
--     policy);
--   * idempotent: a producer-id + epoch are issued, and out-of-order
--     epoch bumps fence the producer;
--   * transactional: 'beginTxn' opens a txn marker; subsequent
--     'send' calls stamp records with the producer id; 'commitTxn'
--     advances LSO past those records, 'abortTxn' marks them
--     read-aborted.
module Kafka.Streams.Mock.Producer
  ( MockProducer
  , newMockProducer
  , MockProduceResult (..)
  , sendMock
  , sendMockH
  , flushMock
    -- * Transactions
  , beginTxnMP
  , commitTxnMP
  , abortTxnMP
  , isInTxnMP
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Text

import Kafka.Streams.Mock.Cluster
  ( MockCluster
  , ProducerStamp (..)
  , TxnId
  , appendToPartition
  , beginTxn
  , commitTxn
  , abortTxn
  )
import qualified Kafka.Streams.Mock.Fault
import Kafka.Streams.Mock.Fault
  ( FaultPolicy
  , MockError
  , TxnOp (..)
  , takeProduceFault
  , takeTxnFaultFor
  )
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Producer
----------------------------------------------------------------------

data MockProducer = MockProducer
  { mpCluster      :: !MockCluster
  , mpFaults       :: !FaultPolicy
  , mpTxnId        :: !(Maybe TxnId)
  , mpProducerEpoch :: !(TVar Int32)
  , mpInTxn        :: !(TVar Bool)
  , mpFenced       :: !(TVar Bool)
  }

newMockProducer
  :: MockCluster
  -> FaultPolicy
  -> Maybe TxnId               -- ^ transactional id (or 'Nothing' for non-txn)
  -> IO MockProducer
newMockProducer c fp txnId = do
  ep <- newTVarIO 0
  it <- newTVarIO False
  fe <- newTVarIO False
  pure MockProducer
    { mpCluster       = c
    , mpFaults        = fp
    , mpTxnId         = txnId
    , mpProducerEpoch = ep
    , mpInTxn         = it
    , mpFenced        = fe
    }

----------------------------------------------------------------------
-- Send
----------------------------------------------------------------------

-- | Outcome of a single 'sendMock' call.
data MockProduceResult
  = MPSent !Int32 !Int      -- ^ partition + offset
  | MPFault !MockError      -- ^ injected fault
  | MPFenced                -- ^ producer was fenced earlier
  | MPNoSuchPartition !String
  deriving (Eq, Show)

-- | Send one record. The (topic, partition) is taken as an explicit
-- argument; the caller decides routing (round-robin, hash, etc.).
sendMock
  :: MockProducer
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> IO MockProduceResult
sendMock p topic part mk v ts = sendMockH p topic part mk v ts []

-- | 'sendMock' that also attaches headers to the stored record.
sendMockH
  :: MockProducer
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> [(Data.Text.Text, ByteString)]
  -> IO MockProduceResult
sendMockH p topic part mk v ts hdrs = do
  fenced <- readTVarIO (mpFenced p)
  if fenced
    then pure MPFenced
    else do
      mFault <- takeProduceFault (mpFaults p) topic part
      case mFault of
        Just e -> handleFault p e
        Nothing -> do
          stamp <- transactionStamp p
          r <- appendToPartition (mpCluster p) topic part mk v ts hdrs stamp
          case r of
            Left  err
              | "fenced" `Data.Text.isPrefixOf` Data.Text.pack err -> do
                  atomically $ writeTVar (mpFenced p) True
                  pure MPFenced
              | otherwise -> pure (MPNoSuchPartition err)
            Right off -> pure (MPSent part (fromIntegral off))

handleFault :: MockProducer -> MockError -> IO MockProduceResult
handleFault p e = do
  -- A fenced epoch is sticky: future sends keep failing until the
  -- producer is rebuilt. Mirrors the Java client's
  -- ProducerFencedException semantics.
  case e of
    err | isFenceError err -> atomically $ writeTVar (mpFenced p) True
    _                      -> pure ()
  pure (MPFault e)
  where
    isFenceError = \case
      _ -> False -- placeholder; fence is currently handled explicitly elsewhere

transactionStamp :: MockProducer -> IO (Maybe ProducerStamp)
transactionStamp p = do
  inTxn <- readTVarIO (mpInTxn p)
  case (inTxn, mpTxnId p) of
    (True, Just tid) -> do
      ep <- readTVarIO (mpProducerEpoch p)
      pure (Just (ProducerStamp tid ep))
    _ -> pure Nothing

flushMock :: MockProducer -> IO ()
flushMock _ = pure ()
  -- Sends are synchronous in this mock; nothing to flush.

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

-- | Open a transaction. Mirrors @KafkaProducer.beginTransaction@.
beginTxnMP :: MockProducer -> IO (Either MockError ())
beginTxnMP p = case mpTxnId p of
  Nothing -> pure (Left (errCustom "non-transactional producer"))
  Just tid -> do
    mFault <- takeTxnFaultFor (mpFaults p) tid TxnBegin
    case mFault of
      Just e  -> pure (Left e)
      Nothing -> do
        beginTxn (mpCluster p) tid
        atomically (writeTVar (mpInTxn p) True)
        pure (Right ())
  where
    errCustom msg =
      Kafka.Streams.Mock.Fault.ErrCustom msg

-- | Commit a transaction. Bumps the producer epoch, advances LSO on
-- every partition the txn touched. Mirrors
-- @KafkaProducer.commitTransaction@.
commitTxnMP :: MockProducer -> IO (Either MockError ())
commitTxnMP p = case mpTxnId p of
  Nothing  -> pure (Left (Kafka.Streams.Mock.Fault.ErrCustom "non-transactional producer"))
  Just tid -> do
    mFault <- takeTxnFaultFor (mpFaults p) tid TxnCommit
    case mFault of
      Just e  -> pure (Left e)
      Nothing -> do
        commitTxn (mpCluster p) tid
        atomically $ do
          writeTVar (mpInTxn p) False
          modifyTVar' (mpProducerEpoch p) (+ 1)
        pure (Right ())

abortTxnMP :: MockProducer -> IO (Either MockError ())
abortTxnMP p = case mpTxnId p of
  Nothing  -> pure (Left (Kafka.Streams.Mock.Fault.ErrCustom "non-transactional producer"))
  Just tid -> do
    mFault <- takeTxnFaultFor (mpFaults p) tid TxnAbort
    case mFault of
      Just e  -> pure (Left e)
      Nothing -> do
        abortTxn (mpCluster p) tid
        atomically $ do
          writeTVar (mpInTxn p) False
          modifyTVar' (mpProducerEpoch p) (+ 1)
        pure (Right ())

isInTxnMP :: MockProducer -> IO Bool
isInTxnMP p = readTVarIO (mpInTxn p)

-- 'Kafka.Streams.Mock.Fault' kept imported via qualified-prefix
-- usages above; this dummy keeps GHC happy when the file is
-- compiled alone.
_keepFault :: Kafka.Streams.Mock.Fault.MockError
_keepFault = Kafka.Streams.Mock.Fault.ErrUnknownServerError
