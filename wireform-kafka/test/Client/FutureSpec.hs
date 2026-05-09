{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the producer/consumer Future API (KIP-247 / 944).
module Client.FutureSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Future as F

tests :: TestTree
tests = testGroup "KafkaFuture"
  [ testCase "completePromise -> awaitFuture returns Right"
      complete_path
  , testCase "failPromise -> awaitFuture returns Left"
      fail_path
  , testCase "awaitFutureWithTimeout returns Nothing when nothing happens"
      timeout_returns_nothing
  , testCase "immediateFuture is already-completed"
      immediate
  , testCase "failedFuture is already-failed"
      failed
  , testCase "completing twice is rejected (returns False)"
      double_complete
  ]

complete_path :: IO ()
complete_path = do
  (p, f) <- F.newPromise
  ok <- F.completePromise p (42 :: Int)
  ok @?= True
  r <- F.awaitFuture f
  r @?= Right 42

fail_path :: IO ()
fail_path = do
  (p, f :: F.KafkaFuture Int) <- F.newPromise
  _ <- F.failPromise p "broken"
  r <- F.awaitFuture f
  r @?= Left "broken"

timeout_returns_nothing :: IO ()
timeout_returns_nothing = do
  (_p, f :: F.KafkaFuture Int) <- F.newPromise
  m <- F.awaitFutureWithTimeout f 50
  m @?= Nothing

immediate :: IO ()
immediate = do
  f <- F.immediateFuture (5 :: Int)
  r <- F.awaitFuture f
  r @?= Right 5

failed :: IO ()
failed = do
  f :: F.KafkaFuture Int <- F.failedFuture "no"
  r <- F.awaitFuture f
  r @?= Left "no"

double_complete :: IO ()
double_complete = do
  (p, _f :: F.KafkaFuture Int) <- F.newPromise
  _ <- F.completePromise p 1
  -- Second attempt fails (TMVar is full).
  ok <- F.completePromise p 2
  ok @?= False
