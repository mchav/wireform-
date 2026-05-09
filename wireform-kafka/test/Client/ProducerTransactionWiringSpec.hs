{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @Kafka.Client.Transaction@ ↔
-- @Kafka.Client.Producer@ wiring landed alongside FEATURE_PARITY's
-- top-of-queue S0 item ("Wire @Transaction@ into @Producer@").
--
-- The end-to-end story (transactional records become visible to a
-- read-committed consumer iff the producer commits) requires a
-- live broker and lives in
-- @test-integration/Integration/BasicSpec.hs@; what we cover here
-- are the pure pieces that drove the integration:
--
--   * 'producerTxnGate' — the state guard that rejects
--     'sendMessage' calls outside @InTransaction@;
--   * 'BatchAccumulator.appendRecordStamped' — that a freshly
--     created batch gets stamped with the supplied
--     'BatchStamp', including the @isTransactional@ bit;
--   * 'ProducerSender.buildRecordBatch' — that a batch carrying
--     @batchIsTransactional = True@ encodes a wire-level
--     'Attributes' word with the transactional bit set.
module Client.ProducerTransactionWiringSpec (tests) where

import Control.Concurrent.STM
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Client.Producer as Producer
import qualified Kafka.Client.Transaction as Txn
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

tests :: TestTree
tests = testGroup "Producer ↔ Transaction wiring"
  [ testGroup "producerTxnGate"
      [ gate_uninitialized
      , gate_ready_rejects
      , gate_in_txn_with_pid_passes
      , gate_in_txn_without_pid_rejects
      , gate_fenced
      , gate_aborting
      , gate_error
      ]
  , testGroup "BatchAccumulator stamping"
      [ stamped_records_carry_stamp_into_new_batch
      , stamped_records_share_existing_batch_stamp
      , noStamp_matches_default_sentinels
      , distinct_partitions_get_distinct_stamps
      ]
  , testGroup "RecordBatch transactional flag"
      [ buildRecordBatch_propagates_isTransactional
      , buildRecordBatch_default_is_non_transactional
      ]
  ]

----------------------------------------------------------------------
-- producerTxnGate
----------------------------------------------------------------------

gate_uninitialized :: TestTree
gate_uninitialized = testCase
  "Uninitialized: rejects with 'must call initTransactions'" $
  case Producer.producerTxnGate Txn.Uninitialized Nothing Nothing of
    Left msg -> assertBool ("got: " <> msg)
                  ("initTransactions" `infix'` msg)
    Right _  -> error "expected rejection"

gate_ready_rejects :: TestTree
gate_ready_rejects = testCase
  "Ready: rejects with 'must call beginTransaction'" $
  case Producer.producerTxnGate Txn.Ready
         (Just (Txn.ProducerId 1)) (Just (Txn.ProducerEpoch 0)) of
    Left msg -> assertBool ("got: " <> msg)
                  ("beginTransaction" `infix'` msg)
    Right _  -> error "expected rejection"

gate_in_txn_with_pid_passes :: TestTree
gate_in_txn_with_pid_passes = testCase
  "InTransaction + (pid, epoch): returns the pair to stamp the batch" $
  Producer.producerTxnGate Txn.InTransaction
    (Just (Txn.ProducerId 12345)) (Just (Txn.ProducerEpoch 7))
    @?= Right (12345, 7)

gate_in_txn_without_pid_rejects :: TestTree
gate_in_txn_without_pid_rejects = testCase
  "InTransaction + Nothing pid: rejects (initTransactions never ran)" $
  case Producer.producerTxnGate Txn.InTransaction Nothing Nothing of
    Left _ -> pure ()
    Right _ -> error "expected rejection without populated pid/epoch"

gate_fenced :: TestTree
gate_fenced = testCase
  "Fenced: surfaces 'producer fenced'" $
  case Producer.producerTxnGate Txn.Fenced
         (Just (Txn.ProducerId 1)) (Just (Txn.ProducerEpoch 0)) of
    Left msg -> assertBool ("got: " <> msg) ("fenced" `infix'` msg)
    Right _  -> error "expected rejection"

gate_aborting :: TestTree
gate_aborting = testCase
  "Aborting: rejects (commit/abort already in flight)" $
  case Producer.producerTxnGate Txn.Aborting
         (Just (Txn.ProducerId 1)) (Just (Txn.ProducerEpoch 0)) of
    Left _ -> pure ()
    Right _ -> error "expected rejection"

gate_error :: TestTree
gate_error = testCase
  "Error: surfaces the underlying message" $
  case Producer.producerTxnGate (Txn.Error "boom")
         (Just (Txn.ProducerId 1)) (Just (Txn.ProducerEpoch 0)) of
    Left msg -> assertBool ("got: " <> msg) ("boom" `infix'` msg)
    Right _  -> error "expected rejection"

----------------------------------------------------------------------
-- BatchAccumulator stamping
----------------------------------------------------------------------

mkAcc :: IO BA.BatchAccumulator
mkAcc = BA.createBatchAccumulator
  16384  -- batch size
  100000 -- linger ms (very high so nothing rolls over by time)
  Compression.NoCompression
  (Compression.defaultLevel Compression.NoCompression)

stamped_records_carry_stamp_into_new_batch :: TestTree
stamped_records_carry_stamp_into_new_batch = testCase
  "appendRecordStamped: a freshly created batch carries the stamp's \
  \(producerId, epoch, baseSeq, isTransactional)" $ do
  acc <- mkAcc
  let tp    = BA.TopicPartition "t" 0
      stamp = BA.BatchStamp
        { BA.stampProducerId      = 999
        , BA.stampProducerEpoch   = 3
        , BA.stampBaseSequence    = 17
        , BA.stampIsTransactional = True
        }
      rec_ = RB.Record 0 0 Nothing "value" []
  ok <- BA.appendRecordStamped acc tp rec_ (\_ -> pure ()) stamp
  ok @?= True
  BA.closeBatchAccumulator acc
  batches <- BA.drainReadyBatches acc
  case batches of
    [b] -> do
      BA.batchProducerId      b @?= 999
      BA.batchProducerEpoch   b @?= 3
      BA.batchBaseSequence    b @?= 17
      BA.batchIsTransactional b @?= True
      length (toList (BA.batchRecords b)) @?= 1
    _ -> error $ "expected exactly one ready batch, got " <> show (length batches)

stamped_records_share_existing_batch_stamp :: TestTree
stamped_records_share_existing_batch_stamp = testCase
  "appendRecordStamped: records appended to a /filling/ batch \
  \inherit that batch's stamp; producer-side seq advancement is the \
  \producer's responsibility" $ do
  acc <- mkAcc
  let tp = BA.TopicPartition "t" 0
      stampA = BA.BatchStamp 999 3 17 True
      stampB = BA.BatchStamp 999 3 18 True  -- next per-producer seq
      stampC = BA.BatchStamp 999 3 19 True
      rec_ = RB.Record 0 0 Nothing "v" []
  _ <- BA.appendRecordStamped acc tp rec_ (\_ -> pure ()) stampA
  _ <- BA.appendRecordStamped acc tp rec_ (\_ -> pure ()) stampB
  _ <- BA.appendRecordStamped acc tp rec_ (\_ -> pure ()) stampC
  BA.closeBatchAccumulator acc
  batches <- BA.drainReadyBatches acc
  case batches of
    [b] -> do
      -- All three records ended up in the single filling batch;
      -- the batch's base_sequence is whatever the /first/ stamp
      -- carried.
      length (toList (BA.batchRecords b)) @?= 3
      BA.batchBaseSequence b @?= 17
      BA.batchIsTransactional b @?= True
    other -> error $
      "expected one batch, got " <> show (length other)

noStamp_matches_default_sentinels :: TestTree
noStamp_matches_default_sentinels = testCase
  "noStamp produces no-producer-id / no-epoch / no-sequence sentinels" $ do
  BA.stampProducerId      BA.noStamp @?= RB.noProducerId
  BA.stampProducerEpoch   BA.noStamp @?= RB.noProducerEpoch
  BA.stampBaseSequence    BA.noStamp @?= RB.noSequence
  BA.stampIsTransactional BA.noStamp @?= False

distinct_partitions_get_distinct_stamps :: TestTree
distinct_partitions_get_distinct_stamps = testCase
  "Stamps on distinct (topic, partition) tracks are independent" $ do
  acc <- mkAcc
  let tp1 = BA.TopicPartition "t" 0
      tp2 = BA.TopicPartition "t" 1
      stamp1 = BA.BatchStamp 999 3  0 True
      stamp2 = BA.BatchStamp 999 3 17 True
      rec_ = RB.Record 0 0 Nothing "v" []
  _ <- BA.appendRecordStamped acc tp1 rec_ (\_ -> pure ()) stamp1
  _ <- BA.appendRecordStamped acc tp2 rec_ (\_ -> pure ()) stamp2
  BA.closeBatchAccumulator acc
  batches <- BA.drainReadyBatches acc
  -- One batch per partition; both should be transactional with their
  -- own base sequence.
  let baseSeqs =
        Seq.sort $
          Seq.fromList [BA.batchBaseSequence b | b <- batches]
  toList baseSeqs @?= [0, 17]
  assertBool "every drained batch is transactional"
    (all BA.batchIsTransactional batches)

----------------------------------------------------------------------
-- buildRecordBatch
----------------------------------------------------------------------

mkBatchWith :: Bool -> BA.ProducerBatch
mkBatchWith isTxn = BA.ProducerBatch
  { BA.batchTopicPartition = BA.TopicPartition "t" 0
  , BA.batchRecords        = Seq.fromList
                               [RB.Record 0 0 Nothing "v" []]
  , BA.batchSizeBytes      = 1
  , BA.batchCreateTime     = 0
  , BA.batchBaseTimestamp  = 0
  , BA.batchState          = BA.Ready
  , BA.batchCompression    = Compression.NoCompression
  , BA.batchCompressionLevel =
      Compression.defaultLevel Compression.NoCompression
  , BA.batchCallbacks      = Seq.empty
  , BA.batchAttempts       = 0
  , BA.batchProducerId     = if isTxn then 12345 else RB.noProducerId
  , BA.batchProducerEpoch  = if isTxn then 7     else RB.noProducerEpoch
  , BA.batchBaseSequence   = if isTxn then 42    else RB.noSequence
  , BA.batchIsTransactional = isTxn
  }

buildRecordBatch_propagates_isTransactional :: TestTree
buildRecordBatch_propagates_isTransactional = testCase
  "buildRecordBatch: batchIsTransactional flips attrIsTransactional \
  \and the producer-id / epoch / base-seq carry through" $ do
  let rb = Sender.buildRecordBatch (mkBatchWith True)
  RB.attrIsTransactional (RB.batchAttributes rb) @?= True
  RB.batchProducerId rb    @?= 12345
  RB.batchProducerEpoch rb @?= 7
  RB.batchBaseSequence rb  @?= 42

buildRecordBatch_default_is_non_transactional :: TestTree
buildRecordBatch_default_is_non_transactional = testCase
  "buildRecordBatch: default batch encodes attrIsTransactional = False \
  \and the no-producer-id / no-epoch / no-seq sentinels" $ do
  let rb = Sender.buildRecordBatch (mkBatchWith False)
  RB.attrIsTransactional (RB.batchAttributes rb) @?= False
  RB.batchProducerId rb    @?= RB.noProducerId
  RB.batchProducerEpoch rb @?= RB.noProducerEpoch
  RB.batchBaseSequence rb  @?= RB.noSequence

----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

-- | Tiny @isInfixOf@ for 'String' so the tests don't need
-- 'Data.List.Extra' / 'Data.Text'.
infix' :: String -> String -> Bool
infix' needle haystack =
  any (\i -> take (length needle) (drop i haystack) == needle)
      [0 .. length haystack - length needle]
