{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Streams.State.Transactional
Description : KIP-892 EOS-V3 transactional buffer for state stores

Today the engine commits the producer transaction and the
changelog write atomically, but state-store puts happen
/before/ the transaction commit — if the commit fails after a
put, the store is left in a state inconsistent with the
broker-side log.

KIP-892 introduces a /store transaction/ layer: every put goes
into a per-task buffer first, the buffer is replayed onto the
underlying store only when the producer transaction commits
(immediately, atomically), and an abort discards the buffer.

This module provides the wrapper. It works on any
'KeyValueStore'; the buffer is a per-task in-memory
@Map@ overlay that:

  * intercepts 'kvsPut' / 'kvsPutIfAbsent' / 'kvsDelete' and
    records the change rather than applying it;
  * falls back to the underlying store on 'kvsGet' so
    /reads see writes/ within the same transaction (matches
    the JVM EOS-V3 read-your-writes property);
  * exposes 'commit' and 'abort' that the engine driver calls
    when the producer transaction commits / aborts.

Note: this is a /single-writer/ overlay. The engine guarantees
each task's store is written from one thread (the task's
processor); concurrent writers would need extra synchronisation.
-}
module Kafka.Streams.State.Transactional
  ( TransactionalStore
  , newTransactionalStore
  , txnStore
  , txnCommit
  , txnAbort
  , txnPendingCount
  ) where

import Control.Concurrent.STM
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified ListT
import qualified StmContainers.Map as StmMap

import Kafka.Streams.State.Store
  ( KeyValueIterator (..)
  , KeyValueStore (..)
  , kvIteratorFromList
  )

-- | A wrapper around a 'KeyValueStore' that buffers writes until
-- 'txnCommit' (or discards them on 'txnAbort').
data TransactionalStore k v = TransactionalStore
  { tsUnderlying :: !(KeyValueStore k v)
  , tsBuffer     :: !(StmMap.Map k (BufferedOp v))
  }

-- | A single deferred operation. We collapse repeated puts /
-- deletes for the same key by overwrite — only the latest survives
-- to commit time, matching the semantics the active store would
-- end up with anyway.
data BufferedOp v
  = BufPut    !v
  | BufDelete

-- | Wrap an existing store with a transactional buffer.
newTransactionalStore
  :: KeyValueStore k v
  -> IO (TransactionalStore k v)
newTransactionalStore kvs = do
  buf <- StmMap.newIO
  pure TransactionalStore { tsUnderlying = kvs, tsBuffer = buf }

-- | Project the transactional wrapper as a 'KeyValueStore' that
-- the engine can hand to processor code. Reads see /pending
-- writes/ first; writes are buffered.
txnStore
  :: forall k v
   . (Eq k, Hashable k)
  => TransactionalStore k v
  -> KeyValueStore k v
txnStore ts@TransactionalStore{..} = KeyValueStore
  { kvsBase            = kvsBase tsUnderlying
  , kvsApproxEntries   = kvsApproxEntries tsUnderlying
  , kvsRange           = kvsRange tsUnderlying
  , kvsAll             = kvsAll tsUnderlying
  , kvsReverseRange    = kvsReverseRange tsUnderlying
  , kvsReverseAll      = kvsReverseAll tsUnderlying
  , kvsGet = \k -> do
      -- Read-your-writes: the buffer wins.
      mPending <- atomically $ StmMap.lookup k tsBuffer
      case mPending of
        Just BufDelete  -> pure Nothing
        Just (BufPut v) -> pure (Just v)
        Nothing         -> kvsGet tsUnderlying k
  , kvsPut = \k v -> atomically $
      StmMap.insert (BufPut v) k tsBuffer
  , kvsPutIfAbsent = \k v -> do
      -- Honour the underlying store's existing value if any
      -- (consulting the buffer first for read-your-writes).
      mExisting <- kvsGet (txnStore ts) k
      case mExisting of
        Just _  -> pure mExisting
        Nothing -> do
          atomically (StmMap.insert (BufPut v) k tsBuffer)
          pure Nothing
  , kvsDelete = \k -> do
      mExisting <- kvsGet (txnStore ts) k
      atomically (StmMap.insert BufDelete k tsBuffer)
      pure mExisting
  }

-- | Drain the buffer onto the underlying store.
--
-- The pending operations are ordered by 'StmMap''s iteration order
-- (which is /not/ guaranteed across runs but is stable for any
-- single transaction). Since each (key) at most appears once in
-- the buffer (by overwrite collapse), order doesn't affect the
-- final state.
txnCommit
  :: (Eq k, Hashable k)
  => TransactionalStore k v -> IO ()
txnCommit TransactionalStore{..} = do
  pairs <- atomically $ ListT.toList (StmMap.listT tsBuffer)
  mapM_ apply pairs
  atomically $ resetBuffer tsBuffer
  where
    apply (k, BufPut v)  = kvsPut tsUnderlying k v
    apply (k, BufDelete) = () <$ kvsDelete tsUnderlying k

-- | Discard the buffered writes.
txnAbort
  :: (Eq k, Hashable k)
  => TransactionalStore k v -> IO ()
txnAbort TransactionalStore{..} = atomically (resetBuffer tsBuffer)

-- | How many uncommitted operations are buffered. Useful for
-- assertions / observability.
txnPendingCount :: TransactionalStore k v -> IO Int
txnPendingCount TransactionalStore{..} = atomically $ do
  pairs <- ListT.toList (StmMap.listT tsBuffer)
  pure (length pairs)

resetBuffer
  :: (Eq k, Hashable k)
  => StmMap.Map k v
  -> STM ()
resetBuffer m = do
  pairs <- ListT.toList (StmMap.listT m)
  mapM_ (\(k, _) -> StmMap.delete k m) pairs
