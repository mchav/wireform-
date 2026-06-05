{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Properties.ChangelogReplaySpec
-- Description : Failover + replay-equivalence properties
--               for the active/standby state-store machinery
--
-- The standby task is supposed to be a perfect, eventually-consistent
-- replica of the active task: every successful write on a logged
-- store is durably reflected in the changelog topic, and a fresh
-- store that replays the topic from offset 0 must converge to the
-- same logical state as the active.
--
-- This module checks that contract under randomised replication
-- schedules:
--
--   1. Sequential consistency under interleaved replays.
--      Active receives random op sequences; the standby is
--      advanced at random points. At every advance the standby's
--      observable state must equal a pure 'Data.Map' model run
--      over exactly the op prefix that's been durably published.
--
--   2. Multi-replica convergence.
--      Two standbys with independent advance schedules must reach
--      the same final state (each other and the active) once both
--      are advanced past 'currentChangelogOffset'.
--
--   3. Failover (active crash, standby promoted to active).
--      Run a prefix of ops on the active; advance standby;
--      "crash" the active (drop the handle); promote standby's
--      underlying store to the new active by re-wrapping with a
--      fresh 'loggedKeyValueStore' against the same topic; run a
--      suffix; spin up a /second-generation/ standby that replays
--      the topic from offset 0; it must converge to the
--      post-suffix active state.
--
--   4. Per-store isolation on a shared changelog.
--      Two active stores publish to the same changelog topic;
--      one standby is registered for each. Each standby's state
--      must match its own active and ignore the other store's
--      entries entirely.
--
-- These properties exercise the replication contract at a
-- significantly stronger level than the existing unit tests in
-- "Streams.StandbySpec".
module Streams.Properties.ChangelogReplaySpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Runtime.Standby
  ( StandbyTask
  , advanceStandby
  , currentChangelogOffset
  , loggedKeyValueStore
  , newInMemoryChangelogTopic
  , newStandbyTask
  , sbStore
  )
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )

----------------------------------------------------------------------
-- Commands & model
----------------------------------------------------------------------

type K = Text
type V = Text

-- | Workload op. We do not advance the standby with these — that's
-- a separate axis ('AdvanceSchedule') so we can shrink the workload
-- and the schedule independently.
data Op
  = OpPut !K !V
  | OpPutIfAbsent !K !V
  | OpDelete !K
  deriving stock (Eq, Show)

-- | Apply an op to the pure model. Mirrors 'loggedKeyValueStore'
-- semantics: the standby observes exactly the writes that were
-- /actually applied/ on the active.
--
--   * 'OpPut' always writes.
--   * 'OpPutIfAbsent' writes only when the key is absent.
--   * 'OpDelete' is a no-op (no changelog entry) when the key is
--     absent, matching 'loggedKeyValueStore.kvsDelete'.
applyOp :: Op -> Map K V -> Map K V
applyOp op m = case op of
  OpPut k v          -> Map.insert k v m
  OpPutIfAbsent k v  ->
    case Map.lookup k m of
      Just _  -> m
      Nothing -> Map.insert k v m
  OpDelete k         ->
    if Map.member k m
      then Map.delete k m
      else m

runReal :: KeyValueStore K V -> Op -> IO ()
runReal store op = case op of
  OpPut k v          -> kvsPut store k v
  OpPutIfAbsent k v  -> () <$ kvsPutIfAbsent store k v
  OpDelete k         -> () <$ kvsDelete store k

----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

-- Keep the key set small so we get plenty of collisions; that's
-- where put/delete/putIfAbsent interactions get interesting.
genKey :: H.Gen K
genKey = Gen.element ["k0", "k1", "k2", "k3", "k4"]

genValue :: H.Gen V
genValue = Gen.element ["v0", "v1", "v2"]

genOp :: H.Gen Op
genOp = Gen.frequency
  [ (4, OpPut          <$> genKey <*> genValue)
  , (2, OpPutIfAbsent  <$> genKey <*> genValue)
  , (2, OpDelete       <$> genKey)
  ]

-- | A list of (op index after which to advance the standby).
-- Indices are 0-based and clamped into the op range. Duplicates
-- and out-of-order entries are fine — extra advances are no-ops.
genAdvanceSchedule :: Int -> H.Gen [Int]
genAdvanceSchedule numOps = do
  let upper = max 0 (numOps - 1)
  n <- Gen.int (Range.linear 0 (max 0 (numOps `div` 2)))
  Gen.list (Range.singleton n) (Gen.int (Range.linear 0 upper))

----------------------------------------------------------------------
-- Snapshots
----------------------------------------------------------------------

snapshot :: KeyValueStore K V -> IO [(K, V)]
snapshot kvs = kvsAll kvs >>= kvIteratorToList

----------------------------------------------------------------------
-- Property 1: sequential consistency under interleaved replays
----------------------------------------------------------------------

prop_sequential_consistency :: H.Property
prop_sequential_consistency = H.property $ do
  ops      <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  schedule <- H.forAll (genAdvanceSchedule (length ops))
  outcome  <- H.evalIO $ do
    topic <- newInMemoryChangelogTopic
    underActive <- inMemoryKeyValueStore @K @V (storeName "s")
    active <- loggedKeyValueStore underActive topic (storeName "s")
                textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @K @V (storeName "sb")
    sb <- newStandbyTask standbyStore topic (storeName "s")
            textSerde textSerde

    -- For each op index, list the advance steps to take afterwards.
    let scheduleAt i =
          length (filter (== i) schedule)

    let go !modelAcc [] _ = pure (Right modelAcc)
        go !modelAcc (op : rest) i = do
          runReal active op
          let modelAcc' = applyOp op modelAcc
          let advances = scheduleAt i
          mismatch <- replicateAdvanceAndCheck sb modelAcc' advances
          case mismatch of
            Just (expected, observed) ->
              pure (Left (op, i, expected, observed))
            Nothing -> go modelAcc' rest (i + 1)

    finalEither <- go Map.empty ops 0
    case finalEither of
      Left x -> pure (Left x)
      Right modelEnd -> do
        -- Final drain: advance past everything and assert
        -- equality with the model.
        _ <- advanceStandby sb
        observed <- snapshot (sbStore sb)
        let expected = Map.toAscList modelEnd
        if observed == expected
          then pure (Right ())
          else pure (Left (OpPut "final" "final", -1, expected, observed))
  case outcome of
    Right () -> pure ()
    Left (op, i, expected, observed) -> do
      H.annotate ("op:       " <> show op)
      H.annotate ("op index: " <> show i)
      H.annotate ("expected: " <> show expected)
      H.annotate ("observed: " <> show observed)
      H.failure

-- | After every op we may run zero or more advances. The standby's
-- visible state after any advance must equal the model, because
-- 'loggedKeyValueStore' publishes the changelog entry synchronously
-- with the underlying write — there is no in-flight gap to lose.
replicateAdvanceAndCheck
  :: StandbyTask K V
  -> Map K V
  -> Int
  -> IO (Maybe ([(K, V)], [(K, V)]))
replicateAdvanceAndCheck _ _ 0 = pure Nothing
replicateAdvanceAndCheck sb model k = do
  _ <- advanceStandby sb
  observed <- snapshot (sbStore sb)
  let expected = Map.toAscList model
  if observed == expected
    then replicateAdvanceAndCheck sb model (k - 1)
    else pure (Just (expected, observed))

----------------------------------------------------------------------
-- Property 2: multi-replica convergence
----------------------------------------------------------------------

prop_multi_replica_convergence :: H.Property
prop_multi_replica_convergence = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  -- Independent advance schedules for two standbys. They can fire
  -- at the same index or at completely disjoint indices.
  let n = length ops
  schedA <- H.forAll (genAdvanceSchedule n)
  schedB <- H.forAll (genAdvanceSchedule n)
  outcome <- H.evalIO $ do
    topic <- newInMemoryChangelogTopic
    underActive <- inMemoryKeyValueStore @K @V (storeName "s")
    active <- loggedKeyValueStore underActive topic (storeName "s")
                textSerde textSerde

    sbAStore <- inMemoryKeyValueStore @K @V (storeName "sb-a")
    sbA <- newStandbyTask sbAStore topic (storeName "s") textSerde textSerde
    sbBStore <- inMemoryKeyValueStore @K @V (storeName "sb-b")
    sbB <- newStandbyTask sbBStore topic (storeName "s") textSerde textSerde

    let countAt sched i = length (filter (== i) sched)

    let drive !modelAcc [] _ = pure modelAcc
        drive !modelAcc (op : rest) i = do
          runReal active op
          let modelAcc' = applyOp op modelAcc
          let advA = countAt schedA i
              advB = countAt schedB i
          replicateAdvance_ sbA advA
          replicateAdvance_ sbB advB
          drive modelAcc' rest (i + 1)

    modelEnd <- drive Map.empty ops 0
    -- Final drain on both standbys; they must converge to the model.
    _ <- advanceStandby sbA
    _ <- advanceStandby sbB

    obsA <- snapshot (sbStore sbA)
    obsB <- snapshot (sbStore sbB)
    let expected = Map.toAscList modelEnd
    pure (expected, obsA, obsB)

  let (expected, obsA, obsB) = outcome
  H.annotate ("expected: " <> show expected)
  H.annotate ("obsA:     " <> show obsA)
  H.annotate ("obsB:     " <> show obsB)
  obsA H.=== expected
  obsB H.=== expected
  -- Mutual agreement is implied by transitivity but assert it
  -- separately so a failure shrinks toward the smallest pair-wise
  -- counterexample if both diverge from the model.
  obsA H.=== obsB

replicateAdvance_ :: StandbyTask K V -> Int -> IO ()
replicateAdvance_ _  0 = pure ()
replicateAdvance_ sb k = do
  _ <- advanceStandby sb
  replicateAdvance_ sb (k - 1)

----------------------------------------------------------------------
-- Property 3: failover with promote-on-crash
----------------------------------------------------------------------

prop_failover_promote :: H.Property
prop_failover_promote = H.property $ do
  pre  <- H.forAll (Gen.list (Range.linear 1 20) genOp)
  post <- H.forAll (Gen.list (Range.linear 1 20) genOp)
  outcome <- H.evalIO $ do
    topic <- newInMemoryChangelogTopic

    -- Phase 1: original active + standby.
    underActive <- inMemoryKeyValueStore @K @V (storeName "s")
    active <- loggedKeyValueStore underActive topic (storeName "s")
                textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @K @V (storeName "sb")
    sb <- newStandbyTask standbyStore topic (storeName "s")
            textSerde textSerde

    mapM_ (runReal active) pre
    _ <- advanceStandby sb

    -- Crash: drop the original active handle. We model "crash" as
    -- the standby's underlying store becoming the new source of
    -- truth; the JVM does the same on a "lost task" reassignment.
    -- The standby's underlying KV store is reused as the new
    -- active's underlying. The old active's underlying is
    -- discarded.
    let newActiveUnder = standbyStore
    newActive <- loggedKeyValueStore newActiveUnder topic
                   (storeName "s") textSerde textSerde

    -- Phase 2: continue ops on the new active.
    mapM_ (runReal newActive) post

    -- Spin up a fresh second-generation standby that has never seen
    -- the topic. Replay from offset 0. It must converge to the
    -- same logical state as the new active.
    sgStore <- inMemoryKeyValueStore @K @V (storeName "sb2")
    sg <- newStandbyTask sgStore topic (storeName "s")
            textSerde textSerde
    _ <- advanceStandby sg

    -- Reference: the model run over (pre ++ post).
    let modelEnd = foldl (flip applyOp) Map.empty (pre ++ post)

    activeSnap <- snapshot newActiveUnder
    sgSnap     <- snapshot (sbStore sg)
    endOff     <- currentChangelogOffset topic
    pure (Map.toAscList modelEnd, activeSnap, sgSnap, endOff)

  let (expected, activeSnap, sgSnap, endOff) = outcome
  H.annotate ("changelog tip offset: " <> show endOff)
  H.annotate ("expected: " <> show expected)
  H.annotate ("active:   " <> show activeSnap)
  H.annotate ("standby2: " <> show sgSnap)
  activeSnap H.=== expected
  sgSnap     H.=== expected

----------------------------------------------------------------------
-- Property 4: per-store isolation on a shared changelog
----------------------------------------------------------------------

prop_per_store_isolation :: H.Property
prop_per_store_isolation = H.property $ do
  opsA <- H.forAll (Gen.list (Range.linear 1 25) genOp)
  opsB <- H.forAll (Gen.list (Range.linear 1 25) genOp)
  -- Interleave the two op streams with a random Bool schedule.
  let n = length opsA + length opsB
  schedule <- H.forAll (Gen.list (Range.singleton n) Gen.bool)
  outcome <- H.evalIO $ do
    topic <- newInMemoryChangelogTopic

    underA <- inMemoryKeyValueStore @K @V (storeName "sa")
    activeA <- loggedKeyValueStore underA topic (storeName "sa")
                 textSerde textSerde
    underB <- inMemoryKeyValueStore @K @V (storeName "sb")
    activeB <- loggedKeyValueStore underB topic (storeName "sb")
                 textSerde textSerde

    let go _      _      []           = pure ()
        go remA   remB   (next : rest)
          | next, op : remA' <- remA = do
              runReal activeA op
              go remA' remB rest
          | not next, op : remB' <- remB = do
              runReal activeB op
              go remA remB' rest
          -- Stream exhausted; route to whatever's left.
          | op : remA' <- remA = do
              runReal activeA op
              go remA' remB rest
          | op : remB' <- remB = do
              runReal activeB op
              go remA remB' rest
          | otherwise = pure ()
    go opsA opsB schedule

    -- Standbys for each store.
    sbAStore <- inMemoryKeyValueStore @K @V (storeName "sa-sb")
    sbA <- newStandbyTask sbAStore topic (storeName "sa") textSerde textSerde
    sbBStore <- inMemoryKeyValueStore @K @V (storeName "sb-sb")
    sbB <- newStandbyTask sbBStore topic (storeName "sb") textSerde textSerde
    _ <- advanceStandby sbA
    _ <- advanceStandby sbB

    obsA <- snapshot (sbStore sbA)
    obsB <- snapshot (sbStore sbB)
    refA <- snapshot underA
    refB <- snapshot underB
    pure (refA, refB, obsA, obsB)
  let (refA, refB, obsA, obsB) = outcome
  H.annotate ("refA: " <> show refA)
  H.annotate ("refB: " <> show refB)
  H.annotate ("obsA: " <> show obsA)
  H.annotate ("obsB: " <> show obsB)
  obsA H.=== refA
  obsB H.=== refB

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests = describe "Changelog replay" $ sequence_
  [ it "standby replay stays in sync after every advance" $
      H.withTests 120 prop_sequential_consistency
  , it "two standbys with independent schedules converge" $
      H.withTests 80 prop_multi_replica_convergence
  , it "promote-on-failover: 2nd-gen standby converges via replay" $
      H.withTests 80 prop_failover_promote
  , it "per-store isolation on a shared changelog topic" $
      H.withTests 60 prop_per_store_isolation
  ]
