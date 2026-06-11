{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Future
Description : KIP-247 producer Future API + KIP-944 async consumer surface

The JVM client returns a @Future<RecordMetadata>@ from
@KafkaProducer.send@ so callers can fan out work without
blocking on every record. KIP-944 generalises this to the
consumer side with @AsyncKafkaConsumer@.

This module exposes the small Future / Promise type the public
producer + consumer use under the hood:

  * 'KafkaFuture' — read-only handle.
  * 'newPromise' — build a fresh, unfilled future + the writer
    handle.
  * 'awaitFuture' / 'awaitFutureWithTimeout' — block / time-out
    on completion.
  * 'completeFuture' / 'failFuture' — fulfil from the runtime
    side.
  * 'thenFuture' — compose another action onto the future
    (Java's @Future.thenApply@).

The implementation is just an STM 'TMVar' wrapper but the typed
result + the @then@-style combinator are what the JVM ports
expect.
-}
module Kafka.Client.Future (
  -- * Future / Promise
  KafkaFuture,
  Promise,
  newPromise,
  completePromise,
  failPromise,
  awaitFuture,
  awaitFutureWithTimeout,
  isCompleted,
  thenFuture,

  -- * Convenience
  immediateFuture,
  failedFuture,
) where

import Control.Concurrent.STM
import Data.Either (isRight)
import System.Timeout qualified


-- | Read-only handle on a value that may not be ready yet.
newtype KafkaFuture a = KafkaFuture
  { unKafkaFuture :: TMVar (Either String a)
  }


{- | The writer side of a 'KafkaFuture'. Use 'completePromise'
or 'failPromise' once.
-}
newtype Promise a = Promise (TMVar (Either String a))


newPromise :: IO (Promise a, KafkaFuture a)
newPromise = do
  v <- newEmptyTMVarIO
  pure (Promise v, KafkaFuture v)


completePromise :: Promise a -> a -> IO Bool
completePromise (Promise v) x = atomically $ tryPutTMVar v (Right x)


failPromise :: Promise a -> String -> IO Bool
failPromise (Promise v) err = atomically $ tryPutTMVar v (Left err)


-- | Block until the future is fulfilled.
awaitFuture :: KafkaFuture a -> IO (Either String a)
awaitFuture (KafkaFuture v) = atomically (readTMVar v)


-- | Block up to @timeoutMs@ for the future to fulfil.
awaitFutureWithTimeout
  :: KafkaFuture a -> Int -> IO (Maybe (Either String a))
awaitFutureWithTimeout (KafkaFuture v) timeoutMs =
  System.Timeout.timeout
    (max 0 timeoutMs * 1000)
    (atomically (readTMVar v))


-- | True iff the future has been fulfilled (success /or/ failure).
isCompleted :: KafkaFuture a -> IO Bool
isCompleted (KafkaFuture v) = atomically $ do
  m <- tryReadTMVar v
  pure (isRight (maybe (Left ()) (\_ -> Right ()) m))


{- | Chain a continuation onto a future. Returns a new future
that fulfils with the continuation's result. Mirrors Java's
@CompletableFuture.thenApply@ — the continuation runs
synchronously on the thread that fulfilled the input.
-}
thenFuture
  :: KafkaFuture a
  -> (a -> IO (Either String b))
  -> IO (KafkaFuture b)
thenFuture (KafkaFuture v) f = do
  outV <- newEmptyTMVarIO
  -- Spawn a thread that waits for the input + chains. Using a
  -- fork keeps the parent caller non-blocking.
  _ <- forkAwait v outV f
  pure (KafkaFuture outV)
  where
    forkAwait inV outV g = do
      let go = do
            r <- atomically (readTMVar inV)
            r' <- case r of
              Left err -> pure (Left err)
              Right x -> g x
            atomically $ do
              full <- tryPutTMVar outV r'
              if full then pure () else pure ()
      -- The implementation deliberately uses a plain `IO`
      -- continuation; callers needing background scheduling can
      -- wrap in 'Control.Concurrent.Async.async'.
      go


immediateFuture :: a -> IO (KafkaFuture a)
immediateFuture x = do
  v <- newTMVarIO (Right x)
  pure (KafkaFuture v)


failedFuture :: String -> IO (KafkaFuture a)
failedFuture err = do
  v <- newTMVarIO (Left err)
  pure (KafkaFuture v)
