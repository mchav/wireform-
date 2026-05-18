{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Antithesis.TTLSpec
-- Description : Property suite for event-time TTL KV wrapper
--
-- Properties:
--
--   1. /Read filtering/: 'kvsGet' returns 'Nothing' for any
--      entry whose @expireAt <= clock@.
--   2. /Range filtering/: 'kvsRange' / 'kvsAll' skip expired
--      entries.
--   3. /Sweep correctness/: 'expireBefore now' deletes exactly
--      the entries whose @expireAt <= now@; subsequent reads
--      reflect the deletion.
--   4. /Idempotent sweep/: a second 'expireBefore now' reaps 0
--      entries.
--   5. /Write-then-sweep model match/: after any randomised
--      sequence of @put@s and @expireBefore@s, the live entries
--      match a pure 'Data.Map' model whose semantics are
--      \"key -\> (value, expiresAt)\".
module Streams.Antithesis.TTLSpec (tests) where

import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStore
  )
import Kafka.Streams.State.KeyValue.TTL
  ( TTLConfig (..)
  , expireBefore
  , ttlEntryCount
  , ttlKeyValueStore
  )
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )
import Kafka.Streams.Time
  ( Duration
  , Timestamp (..)
  , addDuration
  , millis
  )

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

type K = Int
type V = Int

mkStore
  :: Duration
  -> IO ((Timestamp -> IO ()), KeyValueStore K (V, Timestamp),
         KeyValueStore K V)
mkStore dur = do
  clockRef <- newIORef (Timestamp 0)
  let advance t = writeIORef clockRef t
  inner <- inMemoryKeyValueStore @K @(V, Timestamp) (storeName "ttl-under")
  let cfg = TTLConfig
        { ttlDuration = dur
        , ttlClock    = readIORef clockRef
        }
  (wrapped, _) <- ttlKeyValueStore cfg inner
  pure (advance, inner, wrapped)

----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_put_then_read_before_expiry :: TestTree
unit_put_then_read_before_expiry =
  testCase "put at t=0, read at t<ttl: visible" $ do
    (advance, _, kv) <- mkStore (millis 1000)
    advance (Timestamp 0)
    kvsPut kv 1 100
    advance (Timestamp 500)
    kvsGet kv 1 >>= (@?= Just 100)

unit_put_then_read_after_expiry :: TestTree
unit_put_then_read_after_expiry =
  testCase "put at t=0, read at t>ttl: invisible" $ do
    (advance, _, kv) <- mkStore (millis 1000)
    advance (Timestamp 0)
    kvsPut kv 1 100
    advance (Timestamp 2000)
    kvsGet kv 1 >>= (@?= Nothing)

unit_expireBefore_purges :: TestTree
unit_expireBefore_purges =
  testCase "expireBefore removes underlying entries" $ do
    (advance, inner, kv) <- mkStore (millis 1000)
    advance (Timestamp 0)
    kvsPut kv 1 100
    kvsPut kv 2 200
    advance (Timestamp 2000)
    n <- expireBefore inner (Timestamp 2000)
    n @?= 2
    -- Underlying really empty now.
    ttlEntryCount inner >>= (@?= 0)

unit_kvsAll_skips_expired :: TestTree
unit_kvsAll_skips_expired =
  testCase "kvsAll skips expired entries even before a sweep" $ do
    (advance, _, kv) <- mkStore (millis 1000)
    advance (Timestamp 0)
    kvsPut kv 1 100
    advance (Timestamp 500)
    kvsPut kv 2 200
    advance (Timestamp 1200)
    -- key 1 expired at 1000; key 2 expires at 1500.
    rs <- kvsAll kv >>= kvIteratorToList
    rs @?= [(2, 200)]

----------------------------------------------------------------------
-- Property: model match
----------------------------------------------------------------------

data Op
  = OpPut !K !V
  | OpAdvance !Int     -- ^ delta in millis
  | OpSweep
  deriving stock (Eq, Show)

genOp :: H.Gen Op
genOp = Gen.frequency
  [ (4, OpPut <$> Gen.int (Range.linear 0 5)
              <*> Gen.int (Range.linear 0 999))
  , (3, OpAdvance <$> Gen.int (Range.linear 0 600))
  , (1, pure OpSweep)
  ]

-- | Pure model. State is the underlying 'Map K (V, Timestamp)'
-- (i.e. every entry that's still in the inner store, regardless
-- of whether it would be visible through the TTL wrapper).
applyOp :: Duration -> Op -> (Timestamp, Map K (V, Timestamp))
       -> (Timestamp, Map K (V, Timestamp))
applyOp ttl op (clock, m) = case op of
  OpPut k v ->
    let !expireAt = addDuration clock ttl
    in (clock, Map.insert k (v, expireAt) m)
  OpAdvance delta ->
    let Timestamp t = clock
    in (Timestamp (t + fromIntegral delta), m)
  OpSweep ->
    let alive = Map.filter (\(_, e) -> e > clock) m
    in (clock, alive)

-- | Project the model down to the "visible" map from the TTL
-- wrapper's perspective at the current clock.
visible :: Timestamp -> Map K (V, Timestamp) -> Map K V
visible clock m =
  Map.map fst $ Map.filter (\(_, e) -> e > clock) m

prop_ttl_visibility_matches_pure_model :: H.Property
prop_ttl_visibility_matches_pure_model = H.property $ do
  ttlMs <- H.forAll (Gen.int64 (Range.linear 100 5000))
  ops   <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  observed <- H.evalIO $ do
    clockRef <- newIORef (Timestamp 0)
    inner <- inMemoryKeyValueStore @K @(V, Timestamp) (storeName "ttl-prop")
    let cfg = TTLConfig
          { ttlDuration = millis ttlMs
          , ttlClock    = readIORef clockRef
          }
    (wrapped, _) <- ttlKeyValueStore cfg inner
    let go [] = pure ()
        go (op : rest) = do
          case op of
            OpPut k v -> kvsPut wrapped k v
            OpAdvance delta -> do
              Timestamp ms_ <- readIORef clockRef
              writeIORef clockRef (Timestamp (ms_ + fromIntegral delta))
            OpSweep -> do
              now <- readIORef clockRef
              _ <- expireBefore inner now
              pure ()
          go rest
    go ops
    rs <- kvsAll wrapped >>= kvIteratorToList
    Timestamp finalT <- readIORef clockRef
    pure (Map.fromList rs, finalT)
  let (live, finalT) = observed
      (_, model) = foldl
        (\acc op -> applyOp (millis ttlMs) op acc)
        (Timestamp 0, Map.empty)
        ops
      expected = visible (Timestamp finalT) model
  live H.=== expected

----------------------------------------------------------------------
-- Property: sweep is idempotent
----------------------------------------------------------------------

prop_sweep_idempotent :: H.Property
prop_sweep_idempotent = H.property $ do
  ttlMs <- H.forAll (Gen.int64 (Range.linear 100 5000))
  ops   <- H.forAll (Gen.list (Range.linear 1 30) genOp)
  delta <- H.forAll (Gen.int (Range.linear 0 10_000))
  (n1, n2) <- H.evalIO $ do
    clockRef <- newIORef (Timestamp 0)
    inner <- inMemoryKeyValueStore @K @(V, Timestamp) (storeName "ttl-idem")
    let cfg = TTLConfig
          { ttlDuration = millis ttlMs
          , ttlClock    = readIORef clockRef
          }
    (wrapped, _) <- ttlKeyValueStore cfg inner
    forM_ ops $ \op -> case op of
      OpPut k v -> kvsPut wrapped k v
      OpAdvance d -> do
        Timestamp ms_ <- readIORef clockRef
        writeIORef clockRef (Timestamp (ms_ + fromIntegral d))
      OpSweep -> do
        now <- readIORef clockRef
        _ <- expireBefore inner now
        pure ()
    -- Now advance further and sweep twice.
    Timestamp ms_ <- readIORef clockRef
    let finalNow = Timestamp (ms_ + fromIntegral delta)
    a <- expireBefore inner finalNow
    b <- expireBefore inner finalNow
    pure (a, b)
  n2 H.=== 0
  H.assert (n1 >= 0)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Event-time TTL"
  [ unit_put_then_read_before_expiry
  , unit_put_then_read_after_expiry
  , unit_expireBefore_purges
  , unit_kvsAll_skips_expired
  , testProperty "kvsAll visibility matches the pure (clock,map) model" $
      H.withTests 120 prop_ttl_visibility_matches_pure_model
  , testProperty "second expireBefore reaps zero entries" $
      H.withTests 80 prop_sweep_idempotent
  ]
