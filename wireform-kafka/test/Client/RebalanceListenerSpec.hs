{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-415 / 429 rebalance listener.
module Client.RebalanceListenerSpec (tests) where

import Data.IORef
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.RebalanceListener as RL

tests :: TestTree
tests = testGroup "RebalanceListener (KIP-415 / 429)"
  [ testCase "noop listener doesn't throw"
      noop
  , testCase "dispatchAssigned / Revoked / Lost route to right callbacks"
      routes
  , testCase "combineListeners runs both in order"
      combine
  , testCase "exceptions in listeners are swallowed"
      swallowed
  ]

mkRecorder :: IO (RL.RebalanceListener, IORef [(String, [C.TopicPartition])])
mkRecorder = do
  ref <- newIORef []
  let l = RL.RebalanceListener
        { RL.rlOnAssigned = \tps -> modifyIORef' ref (++ [("assigned", tps)])
        , RL.rlOnRevoked  = \tps -> modifyIORef' ref (++ [("revoked", tps)])
        , RL.rlOnLost     = \tps -> modifyIORef' ref (++ [("lost", tps)])
        }
  pure (l, ref)

noop :: IO ()
noop = do
  RL.dispatchAssigned RL.noopRebalanceListener []
  RL.dispatchRevoked  RL.noopRebalanceListener []
  RL.dispatchLost     RL.noopRebalanceListener []

routes :: IO ()
routes = do
  (l, ref) <- mkRecorder
  let tp = C.TopicPartition "t" 0
  RL.dispatchAssigned l [tp]
  RL.dispatchRevoked  l [tp]
  RL.dispatchLost     l [tp]
  log_ <- readIORef ref
  map fst log_ @?= ["assigned", "revoked", "lost"]

combine :: IO ()
combine = do
  (l1, ref1) <- mkRecorder
  (l2, ref2) <- mkRecorder
  let combined = RL.combineListeners l1 l2
      tp = C.TopicPartition "t" 1
  RL.dispatchAssigned combined [tp]
  log1 <- readIORef ref1
  log2 <- readIORef ref2
  map fst log1 @?= ["assigned"]
  map fst log2 @?= ["assigned"]

swallowed :: IO ()
swallowed = do
  let l = RL.RebalanceListener
        { RL.rlOnAssigned = \_ -> error "boom"
        , RL.rlOnRevoked  = \_ -> pure ()
        , RL.rlOnLost     = \_ -> pure ()
        }
  -- Must NOT throw.
  RL.dispatchAssigned l []
