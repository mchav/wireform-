{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Properties.CDCSourceSpec
-- Description : Property suite for the CDC source primitive
--
-- Properties:
--
--   1. /Mapping correctness/: any sequence of CDC events applied
--      to a KV store produces the same final state as the
--      \"last-write-wins\" projection over the event sequence,
--      with deletes pruning the key.
--   2. /FIFO polling/: events pushed in order are polled in order.
--   3. /Tombstone semantics/: Insert/Update with after = Nothing
--      acts as a delete.
--   4. /Drain step model match/: 'cdcToKTableStep' applied
--      repeatedly equals 'applyCDCToKVStore' folded over all
--      pushed events.
module Streams.Properties.CDCSourceSpec (tests) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Sources.CDC
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )
import Kafka.Streams.Time (Timestamp (..))

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

type K = Int
type V = Int

----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_insert_then_get :: TestTree
unit_insert_then_get =
  testCase "CDCInsert: after-image lands in the store" $ do
    (src, q) <- inMemoryCDCSource @K @V "u"
    kvs <- inMemoryKeyValueStore @K @V (storeName "u")
    pushCDC q (CDCEvent CDCInsert 1 Nothing (Just 100) 0 (Timestamp 0))
    _ <- cdcToKTableStep src kvs
    kvsGet kvs 1 >>= (@?= Just 100)

unit_update_overwrites :: TestTree
unit_update_overwrites =
  testCase "CDCUpdate: after-image overwrites the value" $ do
    (src, q) <- inMemoryCDCSource @K @V "u"
    kvs <- inMemoryKeyValueStore @K @V (storeName "u")
    pushCDC q (CDCEvent CDCInsert 1 Nothing (Just 100) 0 (Timestamp 0))
    pushCDC q (CDCEvent CDCUpdate 1 (Just 100) (Just 200) 1 (Timestamp 1))
    _ <- cdcToKTableStep src kvs
    kvsGet kvs 1 >>= (@?= Just 200)

unit_delete_removes :: TestTree
unit_delete_removes =
  testCase "CDCDelete: removes the key" $ do
    (src, q) <- inMemoryCDCSource @K @V "u"
    kvs <- inMemoryKeyValueStore @K @V (storeName "u")
    pushCDC q (CDCEvent CDCInsert 1 Nothing (Just 100) 0 (Timestamp 0))
    pushCDC q (CDCEvent CDCDelete 1 (Just 100) Nothing 1 (Timestamp 1))
    _ <- cdcToKTableStep src kvs
    kvsGet kvs 1 >>= (@?= Nothing)

unit_tombstone_after_image :: TestTree
unit_tombstone_after_image =
  testCase "CDCUpdate with after=Nothing is a logical delete" $ do
    (src, q) <- inMemoryCDCSource @K @V "u"
    kvs <- inMemoryKeyValueStore @K @V (storeName "u")
    pushCDC q (CDCEvent CDCInsert 1 Nothing (Just 100) 0 (Timestamp 0))
    pushCDC q (CDCEvent CDCUpdate 1 (Just 100) Nothing 1 (Timestamp 1))
    _ <- cdcToKTableStep src kvs
    kvsGet kvs 1 >>= (@?= Nothing)

----------------------------------------------------------------------
-- Property: arbitrary event sequence matches the pure projection
----------------------------------------------------------------------

genOp :: H.Gen CDCOp
genOp = Gen.element [CDCInsert, CDCUpdate, CDCDelete]

genEvent :: H.Gen (CDCEvent K V)
genEvent = do
  op <- genOp
  k  <- Gen.int (Range.linear 0 4)
  -- after-image: Insert / Update may have Nothing (logical delete);
  -- Delete always has Nothing.
  after <- case op of
    CDCDelete -> pure Nothing
    _         -> Gen.choice
                    [ pure Nothing
                    , Just <$> Gen.int (Range.linear 0 999)
                    ]
  off <- Gen.int64 (Range.linear 0 1_000_000)
  pure (CDCEvent op k Nothing after off (Timestamp 0))

-- | Pure projection of an event sequence onto a Map.
projectEvents :: [CDCEvent K V] -> Map K V
projectEvents = foldl step Map.empty
  where
    step m e = case cdcOp e of
      CDCInsert -> case cdcAfter e of
        Just v  -> Map.insert (cdcKey e) v m
        Nothing -> Map.delete (cdcKey e) m
      CDCUpdate -> case cdcAfter e of
        Just v  -> Map.insert (cdcKey e) v m
        Nothing -> Map.delete (cdcKey e) m
      CDCDelete -> Map.delete (cdcKey e) m

prop_apply_matches_projection :: H.Property
prop_apply_matches_projection = H.property $ do
  events <- H.forAll (Gen.list (Range.linear 1 40) genEvent)
  observed <- H.evalIO $ do
    (src, q) <- inMemoryCDCSource @K @V "p"
    kvs <- inMemoryKeyValueStore @K @V (storeName "p")
    forM_ events (pushCDC q)
    _ <- cdcToKTableStep src kvs
    rs <- kvsAll kvs >>= kvIteratorToList
    pure (Map.fromList rs)
  observed H.=== projectEvents events

----------------------------------------------------------------------
-- Property: drain step splits cleanly into multiple polls
----------------------------------------------------------------------

prop_drain_in_chunks :: H.Property
prop_drain_in_chunks = H.property $ do
  chunks <- H.forAll
              (Gen.list (Range.linear 1 6)
                (Gen.list (Range.linear 1 8) genEvent))
  observed <- H.evalIO $ do
    (src, q) <- inMemoryCDCSource @K @V "c"
    kvs <- inMemoryKeyValueStore @K @V (storeName "c")
    forM_ chunks $ \batch -> do
      forM_ batch (pushCDC q)
      _ <- cdcToKTableStep src kvs
      pure ()
    rs <- kvsAll kvs >>= kvIteratorToList
    pure (Map.fromList rs)
  observed H.=== projectEvents (concat chunks)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "CDC source"
  [ unit_insert_then_get
  , unit_update_overwrites
  , unit_delete_removes
  , unit_tombstone_after_image
  , testProperty "applyCDCToKVStore matches the pure projection" $
      H.withTests 150 prop_apply_matches_projection
  , testProperty "polling in chunks does not change the final state" $
      H.withTests 100 prop_drain_in_chunks
  ]
