{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the producer/consumer Future API (KIP-247 / 944).
module Client.FutureSpec (tests) where

import Kafka.Client.Future qualified as F
import Test.Syd


tests :: Spec
tests =
  describe "KafkaFuture" $
    sequence_
      [ it
          "completePromise -> awaitFuture returns Right"
          complete_path
      , it
          "failPromise -> awaitFuture returns Left"
          fail_path
      , it
          "awaitFutureWithTimeout returns Nothing when nothing happens"
          timeout_returns_nothing
      , it
          "immediateFuture is already-completed"
          immediate
      , it
          "failedFuture is already-failed"
          failed
      , it
          "completing twice is rejected (returns False)"
          double_complete
      ]


complete_path :: IO ()
complete_path = do
  (p, f) <- F.newPromise
  ok <- F.completePromise p (42 :: Int)
  ok `shouldBe` True
  r <- F.awaitFuture f
  r `shouldBe` Right 42


fail_path :: IO ()
fail_path = do
  (p, f :: F.KafkaFuture Int) <- F.newPromise
  _ <- F.failPromise p "broken"
  r <- F.awaitFuture f
  r `shouldBe` Left "broken"


timeout_returns_nothing :: IO ()
timeout_returns_nothing = do
  (_p, f :: F.KafkaFuture Int) <- F.newPromise
  m <- F.awaitFutureWithTimeout f 50
  m `shouldBe` Nothing


immediate :: IO ()
immediate = do
  f <- F.immediateFuture (5 :: Int)
  r <- F.awaitFuture f
  r `shouldBe` Right 5


failed :: IO ()
failed = do
  f :: F.KafkaFuture Int <- F.failedFuture "no"
  r <- F.awaitFuture f
  r `shouldBe` Left "no"


double_complete :: IO ()
double_complete = do
  (p, _f :: F.KafkaFuture Int) <- F.newPromise
  _ <- F.completePromise p 1
  -- Second attempt fails (TMVar is full).
  ok <- F.completePromise p 2
  ok `shouldBe` False
