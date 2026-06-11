{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Streams.Properties.SnapshotSpec
Description : Property suite for snapshot-aware KV store +
              recovery flow (Riffle \xc2\xa71)

Properties:

  1. Round-trip: snapshotting a store then restoring it onto
     a fresh store yields the same kvsAll view.
  2. Manifest pointers: 'readLatestManifest' reflects the
     most recent snapshot id + advancedTo.
  3. Multiple snapshots: each call replaces the manifest;
     the latest one is the one restoreFromSnapshot picks up.
  4. 'recoverStore' returns 'Nothing' when no snapshot has
     ever been published, and 'Just advancedTo' otherwise.
  5. 'shouldSnapshot' fires by both interval and record-count
     triggers per the policy.
  6. 'pruneOldSnapshots keep' leaves exactly @keep@ snapshots
     in the object store.
  7. Object-store reference impls (in-memory + filesystem)
     behave the same way on the round-trip.
-}
module Streams.Properties.SnapshotSpec (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Runtime.ObjectStore
import Kafka.Streams.Runtime.Snapshot
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.KeyValue.Snapshot
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  kvIteratorToList,
  storeName,
 )
import Kafka.Streams.Time (Timestamp (..), millis, seconds)
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Test helpers
----------------------------------------------------------------------

type K = Text


type V = Text


encK :: K -> ByteString
encK = TE.encodeUtf8


encV :: V -> ByteString
encV = TE.encodeUtf8


decK :: ByteString -> Either Text K
decK b = case TE.decodeUtf8' b of
  Left e -> Left (T.pack (show e))
  Right t -> Right t


decV :: ByteString -> Either Text V
decV b = case TE.decodeUtf8' b of
  Left e -> Left (T.pack (show e))
  Right t -> Right t


mkSeededStore :: [(K, V)] -> IO (KeyValueStore K V)
mkSeededStore pairs = do
  kvs <- inMemoryKeyValueStore (storeName "seeded")
  forM_ pairs (\(k, v) -> kvsPut kvs k v)
  pure kvs


----------------------------------------------------------------------
-- 1. Snapshot + restore round-trip
----------------------------------------------------------------------

unit_round_trip_in_memory :: Spec
unit_round_trip_in_memory =
  it "in-memory object store: snapshot then restore = identity" $ do
    os <- inMemoryObjectStore "test"
    src <- mkSeededStore [("a", "1"), ("b", "2"), ("c", "3")]
    Right () <-
      snapshotStore
        os
        (storeName "s")
        (SnapshotId 100)
        100
        encK
        encV
        src
    dst <- inMemoryKeyValueStore @K @V (storeName "dst")
    Right (Just mf) <-
      restoreFromSnapshot
        os
        (storeName "s")
        decK
        decV
        dst
    manifestAdvancedTo mf `shouldBe` 100
    snap <- kvsAll dst >>= kvIteratorToList
    Map.fromList snap `shouldBe` Map.fromList [("a", "1"), ("b", "2"), ("c", "3")]


unit_round_trip_filesystem :: Spec
unit_round_trip_filesystem =
  it "filesystem object store: snapshot then restore = identity" $ do
    withSystemTempDirectory "snap-test" $ \root -> do
      os <- filesystemObjectStore "test" root
      src <- mkSeededStore [("x", "100"), ("y", "200")]
      Right () <-
        snapshotStore
          os
          (storeName "s")
          (SnapshotId 7)
          7
          encK
          encV
          src
      dst <- inMemoryKeyValueStore @K @V (storeName "dst")
      Right (Just _) <-
        restoreFromSnapshot
          os
          (storeName "s")
          decK
          decV
          dst
      snap <- kvsAll dst >>= kvIteratorToList
      Map.fromList snap `shouldBe` Map.fromList [("x", "100"), ("y", "200")]


----------------------------------------------------------------------
-- 2. Manifest reflects latest snapshot
----------------------------------------------------------------------

unit_latest_manifest_pointer :: Spec
unit_latest_manifest_pointer =
  it "readLatestManifest tracks the most recent publish" $ do
    os <- inMemoryObjectStore "t"
    src <- mkSeededStore [("k", "v")]
    Right () <-
      snapshotStore
        os
        (storeName "s")
        (SnapshotId 1)
        1
        encK
        encV
        src
    Right () <-
      snapshotStore
        os
        (storeName "s")
        (SnapshotId 5)
        5
        encK
        encV
        src
    Right (Just mf) <- readLatestManifest os (storeName "s")
    manifestSnapshotId mf `shouldBe` SnapshotId 5
    manifestAdvancedTo mf `shouldBe` 5


----------------------------------------------------------------------
-- 3. recoverStore returns Nothing on a fresh object store
----------------------------------------------------------------------

unit_recover_returns_nothing_on_fresh :: Spec
unit_recover_returns_nothing_on_fresh =
  it "recoverStore returns Nothing when no snapshot exists" $ do
    os <- inMemoryObjectStore "t"
    dst <- inMemoryKeyValueStore @K @V (storeName "dst")
    Right mAdv <- recoverStore os (storeName "s") decK decV dst
    mAdv `shouldBe` Nothing


unit_recover_returns_advanced_to :: Spec
unit_recover_returns_advanced_to =
  it "recoverStore returns Just advancedTo after a publish" $ do
    os <- inMemoryObjectStore "t"
    src <- mkSeededStore [("k", "v")]
    _ <-
      snapshotStore
        os
        (storeName "s")
        (SnapshotId 12)
        12
        encK
        encV
        src
    dst <- inMemoryKeyValueStore @K @V (storeName "dst")
    Right (Just adv) <- recoverStore os (storeName "s") decK decV dst
    adv `shouldBe` 12


----------------------------------------------------------------------
-- 4. shouldSnapshot policy
----------------------------------------------------------------------

unit_shouldSnapshot_interval :: Spec
unit_shouldSnapshot_interval =
  it "shouldSnapshot fires once the interval has elapsed" $ do
    let plan =
          SnapshotPlan
            { spInterval = seconds 30
            , spMaxRecordsBetween = Nothing
            , spRetention = 3
            }
    shouldSnapshot plan (Timestamp 1_000) (Timestamp 0) 0
      `shouldBe` Nothing
    shouldSnapshot plan (Timestamp 31_000) (Timestamp 0) 0
      `shouldBe` Just TriggerInterval


unit_shouldSnapshot_record_count :: Spec
unit_shouldSnapshot_record_count =
  it "shouldSnapshot fires once record-count threshold passes" $ do
    let plan =
          SnapshotPlan
            { spInterval = seconds 600
            , spMaxRecordsBetween = Just 1000
            , spRetention = 3
            }
    shouldSnapshot plan (Timestamp 0) (Timestamp 0) 500
      `shouldBe` Nothing
    shouldSnapshot plan (Timestamp 0) (Timestamp 0) 1500
      `shouldBe` Just TriggerRecordCount


----------------------------------------------------------------------
-- 5. Retention
----------------------------------------------------------------------

unit_pruneOldSnapshots_keeps_n :: Spec
unit_pruneOldSnapshots_keeps_n =
  it "pruneOldSnapshots keeps the most recent N" $ do
    os <- inMemoryObjectStore "t"
    src <- mkSeededStore [("k", "v")]
    forM_ [1, 2, 3, 4, 5 :: Int] $ \i ->
      snapshotStore
        os
        (storeName "s")
        (SnapshotId (fromIntegral i))
        (fromIntegral i)
        encK
        encV
        src
    Right deleted <- pruneOldSnapshots os (storeName "s") 2
    deleted `shouldBe` 3
    Right remaining <- listSnapshots os (storeName "s")
    map unSnapshotId remaining `shouldBe` [4, 5]
    -- The latest snapshot's manifest still resolves.
    Right (Just mf) <- readLatestManifest os (storeName "s")
    manifestSnapshotId mf `shouldBe` SnapshotId 5


----------------------------------------------------------------------
-- 6. publishIfDue triggers + advances state
----------------------------------------------------------------------

unit_publishIfDue_advances_state :: Spec
unit_publishIfDue_advances_state =
  it "publishIfDue: interval trigger publishes + resets state" $ do
    os <- inMemoryObjectStore "t"
    src <- mkSeededStore [("k", "v")]
    let plan = SnapshotPlan (seconds 1) Nothing 5
    st <- newSnapshotState (Timestamp 0)
    -- Not yet due.
    r0 <-
      publishIfDue
        plan
        st
        os
        (storeName "s")
        (Timestamp 500)
        5
        encK
        encV
        src
    r0 `shouldBe` Nothing
    -- Now due.
    Just (trig, Right sid) <-
      publishIfDue
        plan
        st
        os
        (storeName "s")
        (Timestamp 2_000)
        17
        encK
        encV
        src
    trig `shouldBe` TriggerInterval
    sid `shouldBe` SnapshotId 17


----------------------------------------------------------------------
-- 7. Property: random KV pairs round-trip
----------------------------------------------------------------------

prop_snapshot_round_trip :: H.Property
prop_snapshot_round_trip = H.property $ do
  pairs <-
    H.forAll $
      Gen.list (Range.linear 0 30) $
        (,)
          <$> Gen.element (map T.singleton ['a' .. 'z'])
          <*> Gen.text (Range.linear 0 8) Gen.unicode
  observed <- H.evalIO $ do
    os <- inMemoryObjectStore "p"
    src <- mkSeededStore (Map.toAscList (Map.fromList pairs))
    _ <-
      snapshotStore
        os
        (storeName "s")
        (SnapshotId 1)
        1
        encK
        encV
        src
    dst <- inMemoryKeyValueStore @K @V (storeName "dst")
    _ <- restoreFromSnapshot os (storeName "s") decK decV dst
    rs <- kvsAll dst >>= kvIteratorToList
    pure (Map.fromList rs)
  observed H.=== Map.fromList pairs


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Snapshot (Riffle \xc2\xa71)" $
    sequence_
      [ unit_round_trip_in_memory
      , unit_round_trip_filesystem
      , unit_latest_manifest_pointer
      , unit_recover_returns_nothing_on_fresh
      , unit_recover_returns_advanced_to
      , unit_shouldSnapshot_interval
      , unit_shouldSnapshot_record_count
      , unit_pruneOldSnapshots_keeps_n
      , unit_publishIfDue_advances_state
      , it "snapshot round-trip preserves arbitrary KV pairs" $
          H.withTests 120 prop_snapshot_round_trip
      ]
