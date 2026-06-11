{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Mock.Idempotent
Description : Idempotent-producer mechanics layered on the mock
              cluster

Wraps a 'MockProducer' with idempotent-producer state — a
(producerId, epoch) pair plus a per-(topic, partition) sequence
counter — that lets tests reproduce KIP-98 idempotent-producer
failure modes (duplicate detection, sequence gap, fence on
unknown producer epoch).

Mirrors librdkafka 0144_idempotence_mock.c.
-}
module Kafka.Client.Mock.Idempotent (
  -- * State
  IdempotentState,
  ProducerId (..),
  newIdempotentState,
  initProducerId,
  currentProducerId,
  currentProducerEpoch,

  -- * Sending
  IdempotentSendResult (..),
  sendIdempotent,
  nextSequence,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Client.Mock.Cluster qualified as C
import Kafka.Client.Mock.Producer (
  MockProduceResult (..),
  MockProducer,
  sendMock,
 )
import Kafka.Client.Mock.Producer qualified as P


----------------------------------------------------------------------
-- State
----------------------------------------------------------------------

newtype ProducerId = ProducerId {unProducerId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)


{- | Per-producer idempotent state. Owned by the test (or a future
'Kafka.Client.Producer' wrapper) and threaded through 'sendIdempotent'.
-}
data IdempotentState = IdempotentState
  { isProducerId :: !(TVar (Maybe ProducerId))
  , isEpoch :: !(TVar Int32)
  , isSequences :: !(TVar (Map (Text, Int32) Int32))
  -- ^ next sequence number to send per (topic, partition).
  , isSentSeqs :: !(TVar (Map (Text, Int32) (Map Int32 Int64)))
  {- ^ Sequence -> assigned offset for every record this producer
  has successfully sent. Used to detect duplicates: if the
  same sequence is presented twice on the same (topic,
  partition), the second send is /deduplicated/ — the broker
  ACKs but does not write a new copy.
  -}
  }


newIdempotentState :: IO IdempotentState
newIdempotentState = do
  p <- newTVarIO Nothing
  e <- newTVarIO 0
  sq <- newTVarIO Map.empty
  ss <- newTVarIO Map.empty
  pure
    IdempotentState
      { isProducerId = p
      , isEpoch = e
      , isSequences = sq
      , isSentSeqs = ss
      }


{- | Issue a producer id + initial epoch. Mirrors
@KafkaProducer.initTransactions@ for non-transactional idempotent
producers, and @InitProducerId@ on the wire.
-}
initProducerId :: IdempotentState -> ProducerId -> Int32 -> IO ()
initProducerId st pid ep = atomically $ do
  writeTVar (isProducerId st) (Just pid)
  writeTVar (isEpoch st) ep


currentProducerId :: IdempotentState -> IO (Maybe ProducerId)
currentProducerId = readTVarIO . isProducerId


currentProducerEpoch :: IdempotentState -> IO Int32
currentProducerEpoch = readTVarIO . isEpoch


{- | Read the next sequence the idempotent state will assign for
@(topic, partition)@. Useful for tests that want to assert the
sequence progression without actually sending.
-}
nextSequence :: IdempotentState -> Text -> Int32 -> IO Int32
nextSequence st t p = do
  m <- readTVarIO (isSequences st)
  pure (Map.findWithDefault 0 (t, p) m)


----------------------------------------------------------------------
-- Sends
----------------------------------------------------------------------

data IdempotentSendResult
  = ISSent
      { isAssignedOffset :: !Int64
      , isAssignedSeq :: !Int32
      }
  | {- | Sequence already seen; broker ACKs without writing
    (mirrors KIP-98 dedup on retry).
    -}
    ISDuplicateAcked
      { isOriginalOffset :: !Int64
      , isAssignedSeq :: !Int32
      }
  | -- | Underlying producer fault, fence, or "no such partition".
    ISFault !P.MockProduceResult
  | -- | initProducerId hasn't been called.
    ISUninitialised
  deriving (Eq, Show)


{- | Idempotent-aware send. The caller still owns the
'MockProducer' (so faults / fencing work the same way); we
decorate the call with a per-(topic, partition) sequence counter
and short-circuit on duplicates.
-}
sendIdempotent
  :: IdempotentState
  -> MockProducer
  -> Text
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Int64
  -> IO IdempotentSendResult
sendIdempotent st prod topic part mk v ts = do
  mPid <- currentProducerId st
  case mPid of
    Nothing -> pure ISUninitialised
    Just _ -> do
      -- 1. Check the dedup table for the pending sequence.
      seqs <- readTVarIO (isSequences st)
      sent <- readTVarIO (isSentSeqs st)
      let !nextSeq = Map.findWithDefault 0 (topic, part) seqs
      case Map.lookup (topic, part) sent >>= Map.lookup nextSeq of
        Just oldOff ->
          pure
            ISDuplicateAcked
              { isOriginalOffset = oldOff
              , isAssignedSeq = nextSeq
              }
        Nothing -> do
          -- 2. Forward to the underlying producer.
          r <- sendMock prod topic part mk v ts
          case r of
            MPSent _ off -> do
              atomically $ do
                modifyTVar'
                  (isSequences st)
                  (Map.insert (topic, part) (nextSeq + 1))
                modifyTVar'
                  (isSentSeqs st)
                  ( Map.alter
                      ( Just
                          . Map.insert nextSeq (fromIntegral off)
                          . maybe Map.empty id
                      )
                      (topic, part)
                  )
              pure
                ISSent
                  { isAssignedOffset = fromIntegral off
                  , isAssignedSeq = nextSeq
                  }
            other -> pure (ISFault other)
