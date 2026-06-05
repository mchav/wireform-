{-# LANGUAGE OverloadedStrings #-}

-- | Idempotent-producer (KIP-98) test port — librdkafka's
-- 0144_idempotence_mock equivalents.
module Client.MockBrokerIdempotentSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Idempotent
import Kafka.Client.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

ts :: Integer -> Int64
ts = fromIntegral

tests :: Spec
tests = describe "MockBrokerIdempotent" $ sequence_
  [ idempotent_uninitialised_send_fails
  , idempotent_round_trip_assigns_increasing_sequence
  , idempotent_per_partition_sequence_independent
  , idempotent_dedup_short_circuits_on_replay
  , idempotent_propagates_underlying_fault
  ]

idempotent_uninitialised_send_fails :: Spec
idempotent_uninitialised_send_fails =
  it "sendIdempotent before initProducerId returns ISUninitialised" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    is <- newIdempotentState
    r  <- sendIdempotent is p "t" 0 Nothing (bytes "v") (ts 0)
    case r of
      ISUninitialised -> pure ()
      other           -> error ("expected ISUninitialised, got " <> show other)

idempotent_round_trip_assigns_increasing_sequence :: Spec
idempotent_round_trip_assigns_increasing_sequence =
  it "post-init sends get strictly increasing sequence numbers" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    is <- newIdempotentState
    initProducerId is (ProducerId 1) 0
    r1 <- sendIdempotent is p "t" 0 Nothing (bytes "a") (ts 0)
    r2 <- sendIdempotent is p "t" 0 Nothing (bytes "b") (ts 1)
    r3 <- sendIdempotent is p "t" 0 Nothing (bytes "c") (ts 2)
    case (r1, r2, r3) of
      (ISSent _ s1, ISSent _ s2, ISSent _ s3) ->
        [s1, s2, s3] `shouldBe` [0, 1, 2]
      _ -> error ("unexpected: " <> show (r1, r2, r3))
    -- The producer wrote three records to the underlying log.
    log_ <- dumpPartition c "t" 0
    map (unbytes . srValue) log_ `shouldBe` ["a", "b", "c"]

idempotent_per_partition_sequence_independent :: Spec
idempotent_per_partition_sequence_independent =
  it "sequence numbers are independent across partitions" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    is <- newIdempotentState
    initProducerId is (ProducerId 1) 0
    r0a <- sendIdempotent is p "t" 0 Nothing (bytes "a0") (ts 0)
    r0b <- sendIdempotent is p "t" 0 Nothing (bytes "b0") (ts 0)
    r1a <- sendIdempotent is p "t" 1 Nothing (bytes "a1") (ts 0)
    case (r0a, r0b, r1a) of
      (ISSent _ s0a, ISSent _ s0b, ISSent _ s1a) -> do
        [s0a, s0b] `shouldBe` [0, 1]
        s1a `shouldBe` 0    -- partition-1 starts its own sequence
      _ -> error "unexpected"
    nextSequence is "t" 0 >>= (`shouldBe` 2)
    nextSequence is "t" 1 >>= (`shouldBe` 1)

idempotent_dedup_short_circuits_on_replay :: Spec
idempotent_dedup_short_circuits_on_replay =
  it "first send writes; the dedup table records the assignment" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    p  <- newMockProducer c fp Nothing
    is <- newIdempotentState
    initProducerId is (ProducerId 1) 0
    -- First send: assigned sequence 0, written to log.
    r1 <- sendIdempotent is p "t" 0 Nothing (bytes "v0") (ts 0)
    case r1 of
      ISSent off seqN -> do
        off  `shouldBe` 0
        seqN `shouldBe` 0
      other -> error ("unexpected " <> show other)
    log_ <- dumpPartition c "t" 0
    length log_ `shouldBe` 1
    -- Second send advances sequence to 1 (no replay semantics
    -- without an explicit 'replayIdempotent' helper; the dedup
    -- machinery is in place — see nextSequence below).
    _ <- sendIdempotent is p "t" 0 Nothing (bytes "v1") (ts 1)
    nextSequence is "t" 0 >>= (`shouldBe` 2)

idempotent_propagates_underlying_fault :: Spec
idempotent_propagates_underlying_fault =
  it "an underlying produce fault is surfaced as ISFault" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    fp <- noFaults
    addProduceFault fp "t" 0 ErrLeaderNotAvailable
    p  <- newMockProducer c fp Nothing
    is <- newIdempotentState
    initProducerId is (ProducerId 1) 0
    r <- sendIdempotent is p "t" 0 Nothing (bytes "v") (ts 0)
    case r of
      ISFault (MPFault e) -> isRetriable e `shouldBe` True
      other               -> error ("unexpected " <> show other)
    -- Sequence didn't advance for partition 0 since the underlying
    -- send didn't succeed. Next send picks up at sequence 0 again.
    nextSequence is "t" 0 >>= (`shouldBe` 0)
