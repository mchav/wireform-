{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-415 / 429 rebalance listener.
module Client.RebalanceListenerSpec (tests) where

import Data.IORef
import qualified Data.Text as T
import Test.Syd

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.RebalanceListener as RL

tests :: Spec
tests = describe "RebalanceListener (KIP-415 / 429)" $ sequence_
  [ it "noop listener doesn't throw"
      noop
  , it "dispatchAssigned / Revoked / Lost route to right callbacks"
      routes
  , it "combineListeners runs both in order"
      combine
  , it "exceptions in listeners are swallowed"
      swallowed
  , it "computeAssignmentDelta: revoked + added are correct sets"
      deltaCorrect
  , it "computeAssignmentDelta: identical assignments yield empty deltas"
      deltaIdentity
  , it "computeAssignmentDelta: deterministic ascending order"
      deltaOrdered
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
  map fst log_ `shouldBe` ["assigned", "revoked", "lost"]

combine :: IO ()
combine = do
  (l1, ref1) <- mkRecorder
  (l2, ref2) <- mkRecorder
  let combined = RL.combineListeners l1 l2
      tp = C.TopicPartition "t" 1
  RL.dispatchAssigned combined [tp]
  log1 <- readIORef ref1
  log2 <- readIORef ref2
  map fst log1 `shouldBe` ["assigned"]
  map fst log2 `shouldBe` ["assigned"]

swallowed :: IO ()
swallowed = do
  let l = RL.RebalanceListener
        { RL.rlOnAssigned = \_ -> error "boom"
        , RL.rlOnRevoked  = \_ -> pure ()
        , RL.rlOnLost     = \_ -> pure ()
        }
  -- Must NOT throw.
  RL.dispatchAssigned l []

tp :: String -> Int -> C.TopicPartition
tp t p = C.TopicPartition (T.pack t) (fromIntegral p)

deltaCorrect :: IO ()
deltaCorrect = do
  let prev = [tp "in" 0, tp "in" 1, tp "in" 2]
      now  = [tp "in" 1, tp "in" 2, tp "in" 3]
      (revoked, added) = C.computeAssignmentDelta prev now
  revoked `shouldBe` [tp "in" 0]
  added   `shouldBe` [tp "in" 3]

deltaIdentity :: IO ()
deltaIdentity = do
  let asg = [tp "in" 0, tp "out" 7]
      (revoked, added) = C.computeAssignmentDelta asg asg
  revoked `shouldBe` []
  added   `shouldBe` []

deltaOrdered :: IO ()
deltaOrdered = do
  -- Inputs in random-ish order; deltas must come back sorted.
  let prev = [tp "z" 9, tp "a" 1, tp "m" 5]
      now  = [tp "b" 0, tp "a" 1, tp "z" 9]
      (revoked, added) = C.computeAssignmentDelta prev now
  revoked `shouldBe` [tp "m" 5]
  added   `shouldBe` [tp "b" 0]
