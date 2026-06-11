{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.KVStoreSMSpec
Description : State-machine property test for 'KeyValueStore' backends

Models a key-value store as a 'Data.Map.Strict.Map' and generates
arbitrary command sequences via Hedgehog. Each command is run
against both the model and the real store; their observable
results must agree.

The test runs against two generic backends:

  * 'inMemoryKeyValueStore' (canonical baseline)
  * 'TransactionalStore' (KIP-892 buffer wrapping the baseline,
    drained by 'txnCommit' between batches)

A single property iterates 1..60 random commands per case, and
Hedgehog shrinks on failure to the minimum reproducer.

The persistent backend ('Kafka.Streams.State.KeyValue.Persistent')
is 'ByteString'-keyed and exercised separately in
'Streams.PersistentStoreSpec'. The caching wrapper has its
own coverage in 'Streams.CacheSpec'.
-}
module Streams.Properties.KVStoreSMSpec (tests) where

import Data.Int (Int64)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStore,
 )
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  kvIteratorToList,
  storeName,
 )
import Kafka.Streams.State.Transactional qualified as Txn
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------

type K = Int


type V = Int


-- | A single observable operation on the store.
data Cmd
  = CmdPut !K !V
  | CmdPutIfAbsent !K !V
  | CmdGet !K
  | CmdDelete !K
  | CmdRange !K !K
  | CmdReverseRange !K !K
  | CmdAll
  | CmdReverseAll
  | CmdApproxEntries
  deriving stock (Eq, Show)


{- | Result of a command. We capture only the side-effect-free
/observable/ portion: 'CmdPut' has no return value so it
shows up as 'ResUnit'.
-}
data Result
  = ResUnit
  | ResMaybe !(Maybe V)
  | ResList ![(K, V)]
  | ResInt !Int64
  deriving stock (Eq, Show)


----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

{- | Small key alphabet so commands collide on the same key
frequently — the most interesting case for a KV store.
-}
genKey :: H.Gen K
genKey = Gen.int (Range.linear 0 7)


genValue :: H.Gen V
genValue = Gen.int (Range.linear (-1000) 1000)


genCmd :: H.Gen Cmd
genCmd =
  Gen.frequency
    [ (5, CmdPut <$> genKey <*> genValue)
    , (3, CmdPutIfAbsent <$> genKey <*> genValue)
    , (4, CmdGet <$> genKey)
    , (3, CmdDelete <$> genKey)
    , (2, mkOrderedRange CmdRange)
    , (2, mkOrderedRange CmdReverseRange)
    , (1, pure CmdAll)
    , (1, pure CmdReverseAll)
    , (1, pure CmdApproxEntries)
    ]
  where
    mkOrderedRange ctor = do
      a <- genKey
      b <- genKey
      pure (ctor (min a b) (max a b))


----------------------------------------------------------------------
-- Model
----------------------------------------------------------------------

{- | The model is just a 'Data.Map.Strict.Map K V'. Each command
maps to one or two map operations.
-}
applyModel :: Map K V -> Cmd -> (Map K V, Result)
applyModel m = \case
  CmdPut k v -> (Map.insert k v m, ResUnit)
  CmdPutIfAbsent k v ->
    case Map.lookup k m of
      Just existing -> (m, ResMaybe (Just existing))
      Nothing -> (Map.insert k v m, ResMaybe Nothing)
  CmdGet k -> (m, ResMaybe (Map.lookup k m))
  CmdDelete k ->
    case Map.lookup k m of
      Nothing -> (m, ResMaybe Nothing)
      Just v -> (Map.delete k m, ResMaybe (Just v))
  CmdRange lo hi ->
    let xs =
          List.takeWhile (\(k, _) -> k <= hi) $
            List.dropWhile (\(k, _) -> k < lo) (Map.toAscList m)
    in (m, ResList xs)
  CmdReverseRange lo hi ->
    let asc =
          List.takeWhile (\(k, _) -> k <= hi) $
            List.dropWhile (\(k, _) -> k < lo) (Map.toAscList m)
    in (m, ResList (reverse asc))
  CmdAll -> (m, ResList (Map.toAscList m))
  CmdReverseAll -> (m, ResList (Map.toDescList m))
  CmdApproxEntries -> (m, ResInt (fromIntegral (Map.size m)))


----------------------------------------------------------------------
-- Real store harness
----------------------------------------------------------------------

applyReal :: KeyValueStore K V -> Cmd -> IO Result
applyReal store = \case
  CmdPut k v -> kvsPut store k v >> pure ResUnit
  CmdPutIfAbsent k v -> ResMaybe <$> kvsPutIfAbsent store k v
  CmdGet k -> ResMaybe <$> kvsGet store k
  CmdDelete k -> ResMaybe <$> kvsDelete store k
  CmdRange lo hi -> do
    it <- kvsRange store lo hi
    ResList <$> kvIteratorToList it
  CmdReverseRange lo hi -> do
    it <- kvsReverseRange store lo hi
    ResList <$> kvIteratorToList it
  CmdAll -> do
    it <- kvsAll store
    ResList <$> kvIteratorToList it
  CmdReverseAll -> do
    it <- kvsReverseAll store
    ResList <$> kvIteratorToList it
  CmdApproxEntries -> ResInt <$> kvsApproxEntries store


----------------------------------------------------------------------
-- Property driver
----------------------------------------------------------------------

{- | Build the supplied store, run the command sequence against
it and a pure 'Map' model in lockstep, return whichever step
(if any) had a divergence.
-}
runAgainst
  :: IO (KeyValueStore K V)
  -> [Cmd]
  -> IO (Either (Cmd, Result, Result) ())
runAgainst mkStore cmds = do
  store <- mkStore
  let go _ [] = pure (Right ())
      go m (c : rest) = do
        let (m', expected) = applyModel m c
        observed <- applyReal store c
        if observed == expected
          then go m' rest
          else pure (Left (c, expected, observed))
  go Map.empty cmds


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "KVStore state-machine" $
    sequence_
      [ it "in-memory backend matches Data.Map" $
          H.withTests 80 $
            propAgainstModel inMemoryStore
      , it "transactional buffer (read-your-writes) matches Data.Map" $
          H.withTests 60 $
            propAgainstModel txnStoreReadYourWrites
      , it "transactional commit drains buffer onto underlying" $
          H.withTests 40 propTxnCommitBatched
      , it "transactional abort discards buffered writes" $
          H.withTests 40 propTxnAbortDiscards
      ]


propAgainstModel
  :: IO (KeyValueStore K V) -> H.Property
propAgainstModel mkStore = H.property $ do
  cmds <- H.forAll (Gen.list (Range.linear 1 60) genCmd)
  outcome <- H.evalIO (runAgainst mkStore cmds)
  case outcome of
    Right () -> pure ()
    Left (cmd, expected, actual) -> do
      H.annotate ("command:  " <> show cmd)
      H.annotate ("expected: " <> show expected)
      H.annotate ("actual:   " <> show actual)
      H.failure


----------------------------------------------------------------------
-- Store constructors used by the property
----------------------------------------------------------------------

inMemoryStore :: IO (KeyValueStore K V)
inMemoryStore = inMemoryKeyValueStore (storeName "im")


{- | The transactional wrapper's read-your-writes API matches a
direct 'KeyValueStore' for sequential single-thread access,
so we treat it as one of the backends in the same property.
-}
txnStoreReadYourWrites :: IO (KeyValueStore K V)
txnStoreReadYourWrites = do
  inner <- inMemoryStore
  ts <- Txn.newTransactionalStore inner
  pure (Txn.txnStore ts)


----------------------------------------------------------------------
-- Transactional semantics specifically
----------------------------------------------------------------------

{- | After a sequence of @batch1@ followed by 'txnCommit', then
@batch2@ followed by 'txnCommit', the underlying store
contains exactly what running both batches sequentially
against a 'Data.Map.Strict.Map' would produce.
-}
propTxnCommitBatched :: H.Property
propTxnCommitBatched = H.property $ do
  batch1 <- H.forAll (Gen.list (Range.linear 0 20) genCmd)
  batch2 <- H.forAll (Gen.list (Range.linear 0 20) genCmd)
  outcome <- H.evalIO $ do
    inner <- inMemoryStore
    ts <- Txn.newTransactionalStore inner
    let api = Txn.txnStore ts
    mapM_ (applyReal api) batch1
    Txn.txnCommit ts
    mapM_ (applyReal api) batch2
    Txn.txnCommit ts
    -- After both commits, the inner store should agree with
    -- the model run over @batch1 ++ batch2@.
    let (modelEnd, _) =
          foldl
            (\(m, _) c -> applyModel m c)
            (Map.empty, ResUnit)
            (batch1 ++ batch2)
    innerAll <-
      kvsAll inner >>= kvIteratorToList
    pure (Map.toAscList modelEnd, innerAll)
  let (expected, observed) = outcome
  expected H.=== observed


{- | A buffered batch followed by 'txnAbort' must leave the
underlying store identical to its pre-batch state.
-}
propTxnAbortDiscards :: H.Property
propTxnAbortDiscards = H.property $ do
  warmup <- H.forAll (Gen.list (Range.linear 0 10) genCmd)
  batch <- H.forAll (Gen.list (Range.linear 0 20) genCmd)
  outcome <- H.evalIO $ do
    inner <- inMemoryStore
    ts <- Txn.newTransactionalStore inner
    let api = Txn.txnStore ts
    -- Warmup commits durably.
    mapM_ (applyReal api) warmup
    Txn.txnCommit ts
    -- Snapshot underlying state before the buffered batch.
    pre <- kvsAll inner >>= kvIteratorToList
    mapM_ (applyReal api) batch
    Txn.txnAbort ts
    post <- kvsAll inner >>= kvIteratorToList
    pure (pre, post)
  let (pre, post) = outcome
  -- Abort leaves the underlying state unchanged.
  pre H.=== post
