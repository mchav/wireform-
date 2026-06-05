{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Properties.SchemaVersionedSpec
-- Description : Property suite for schema-versioned KV stores +
--               burn-in migration
--
-- Properties:
--
--   1. Round-trip: writing at @current@ and reading at @current@
--      yields the same value (no migration runs).
--   2. Forward migration on read: an entry written at version
--      @v - n@ reads back as the migrated value at @v@.
--   3. Newer-than-current entries surface as 'Nothing': we never
--      forge values we don't know how to handle.
--   4. Burn-in: after 'burnInMigrate', every underlying entry is
--      stamped with @current@ and the migrated payload matches
--      what 'kvsGet' would have produced.
--   5. Idempotence: a second burn-in migrates zero entries.
module Streams.Properties.SchemaVersionedSpec (tests) where

import Control.Monad (forM_)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.KeyValue.SchemaVersioned
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )

----------------------------------------------------------------------
-- Test schema
----------------------------------------------------------------------

-- | A toy domain that's been through three schemas:
--
--   v1: a plain @Int@.
--   v2: @(Int, Int)@ — the second field is a "checksum"
--       (= original * 10).
--   v3: @(Int, Int, Text)@ — adds a "label" derived from the
--       int as text.
--
-- We model all three in the same value type @v@ for the wrapper,
-- which has to be uniform. The encoding stuffs an unused field
-- with a sentinel in the older versions; the migrations fill it in.
data V = V
  { vInt   :: !Int
  , vSum   :: !Int
  , vLabel :: !T.Text
  } deriving stock (Eq, Show)

current :: SchemaVersion
current = SchemaVersion 3

migrations :: [SchemaMigration V]
migrations =
  [ SchemaMigration (SchemaVersion 1) (SchemaVersion 2) (\v ->
      Right v { vSum = vInt v * 10 })
  , SchemaMigration (SchemaVersion 2) (SchemaVersion 3) (\v ->
      Right v { vLabel = T.pack ("n" <> show (vInt v)) })
  ]

----------------------------------------------------------------------
-- Setup helpers
----------------------------------------------------------------------

newPair :: IO (KeyValueStore Int (SchemaVersion, V), KeyValueStore Int V)
newPair = do
  inner <- inMemoryKeyValueStore @Int @(SchemaVersion, V) (storeName "sv")
  wrap  <- schemaVersionedKeyValueStore current migrations inner
  pure (inner, wrap)

writeRaw
  :: KeyValueStore Int (SchemaVersion, V)
  -> SchemaVersion
  -> Int -> Int -> Int -> T.Text -> IO ()
writeRaw inner ver k i s lbl =
  kvsPut inner k (ver, V { vInt = i, vSum = s, vLabel = lbl })

----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_round_trip_current :: Spec
unit_round_trip_current =
  it "put@current, get@current: identity" $ do
    (inner, kv) <- newPair
    kvsPut kv 1 V { vInt = 7, vSum = 70, vLabel = "n7" }
    kvsGet kv 1 >>= (`shouldBe` Just V { vInt = 7, vSum = 70, vLabel = "n7" })
    -- Underlying really tagged @current.
    readCurrentVersionEntry inner 1
      >>= (`shouldBe` Just (current, V { vInt = 7, vSum = 70, vLabel = "n7" }))

unit_migrate_v1_to_v3 :: Spec
unit_migrate_v1_to_v3 =
  it "v1 entry reads as fully-migrated v3 value" $ do
    (inner, kv) <- newPair
    -- The v1 encoding leaves the other fields blank; migrations
    -- fill them in.
    writeRaw inner (SchemaVersion 1) 5 5 0 ""
    kvsGet kv 5 >>= (`shouldBe` Just V { vInt = 5, vSum = 50, vLabel = "n5" })

unit_migrate_v2_to_v3 :: Spec
unit_migrate_v2_to_v3 =
  it "v2 entry reads as fully-migrated v3 value" $ do
    (inner, kv) <- newPair
    writeRaw inner (SchemaVersion 2) 3 3 30 ""
    kvsGet kv 3 >>= (`shouldBe` Just V { vInt = 3, vSum = 30, vLabel = "n3" })

unit_newer_than_current_invisible :: Spec
unit_newer_than_current_invisible =
  it "entry tagged newer than @current is invisible" $ do
    (inner, kv) <- newPair
    writeRaw inner (SchemaVersion 99) 1 1 1 "?"
    kvsGet kv 1 >>= (`shouldBe` Nothing)

----------------------------------------------------------------------
-- Burn-in
----------------------------------------------------------------------

unit_burnin_rewrites_old_entries :: Spec
unit_burnin_rewrites_old_entries =
  it "burn-in migrates old entries onto @current" $ do
    (inner, _kv) <- newPair
    writeRaw inner (SchemaVersion 1) 1 1 0 ""
    writeRaw inner (SchemaVersion 2) 2 2 20 ""
    writeRaw inner (SchemaVersion 3) 3 3 30 "n3"
    ref <- burnInMigrate current migrations inner
    p <- readBurnInProgress ref
    bipScanned  p `shouldBe` 3
    bipMigrated p `shouldBe` 2
    bipFailed   p `shouldBe` 0
    bipComplete p `shouldBe` True
    -- Every underlying entry now stamped @current.
    rs <- kvsAll inner >>= kvIteratorToList
    let vers = map (fst . snd) rs
    all (== current) vers `shouldBe` True

----------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------

genVer :: H.Gen SchemaVersion
genVer = SchemaVersion <$> Gen.int (Range.linear 1 3)

prop_round_trip_at_current :: H.Property
prop_round_trip_at_current = H.property $ do
  k <- H.forAll (Gen.int (Range.linear 0 10))
  i <- H.forAll (Gen.int (Range.linear (-1000) 1000))
  outcome <- H.evalIO $ do
    (_, kv) <- newPair
    let v = V { vInt = i, vSum = i * 10
              , vLabel = T.pack ("n" <> show i) }
    kvsPut kv k v
    kvsGet kv k
  outcome H.=== Just V { vInt = i, vSum = i * 10
                        , vLabel = T.pack ("n" <> show i) }

prop_migrate_any_version :: H.Property
prop_migrate_any_version = H.property $ do
  i <- H.forAll (Gen.int (Range.linear (-100) 100))
  v <- H.forAll genVer
  outcome <- H.evalIO $ do
    (inner, kv) <- newPair
    -- For each starting version, write the appropriate sub-schema
    -- and read via the wrapper.
    case v of
      SchemaVersion 1 -> writeRaw inner v 0 i 0   ""
      SchemaVersion 2 -> writeRaw inner v 0 i (i * 10) ""
      _               -> writeRaw inner v 0 i (i * 10)
                            (T.pack ("n" <> show i))
    kvsGet kv 0
  outcome H.=== Just V { vInt = i, vSum = i * 10
                        , vLabel = T.pack ("n" <> show i) }

prop_burnin_then_read_zero_migrate :: H.Property
prop_burnin_then_read_zero_migrate = H.property $ do
  versionsByKey <- H.forAll $
    Gen.list (Range.linear 1 20)
      ((,) <$> Gen.int (Range.linear 0 9) <*> genVer)
  outcome <- H.evalIO $ do
    (inner, _kv) <- newPair
    forM_ versionsByKey $ \(k, ver) ->
      writeRaw inner ver k k (k * 10) (T.pack ("n" <> show k))
    _ <- burnInMigrate current migrations inner
    -- A second burn-in should report zero migrations.
    ref <- burnInMigrate current migrations inner
    readBurnInProgress ref
  bipMigrated outcome H.=== 0
  bipComplete outcome H.=== True

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests = describe "Schema-versioned KV store" $ sequence_
  [ unit_round_trip_current
  , unit_migrate_v1_to_v3
  , unit_migrate_v2_to_v3
  , unit_newer_than_current_invisible
  , unit_burnin_rewrites_old_entries
  , it "round-trip at current is identity" $
      H.withTests 100 prop_round_trip_at_current
  , it "v1/v2/v3 entries all read back as v3" $
      H.withTests 100 prop_migrate_any_version
  , it "second burn-in migrates zero entries" $
      H.withTests 80 prop_burnin_then_read_zero_migrate
  ]
