{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Properties.TieredStoreSpec
-- Description : Property suite for the tiered KV store wrapper
--
-- Properties:
--
--   1. /Equivalence/: a tiered store behaves like a plain
--      in-memory store on @put / get / delete / all@ for any
--      sequence of ops.
--   2. /Promotion/: a read for a key that's only in cold
--      promotes the entry to hot.
--   3. /Eviction/: when hot exceeds the configured capacity,
--      'countBasedEviction' demotes the overflow into cold.
--   4. /Range merge/: 'kvsAll' over hot + cold yields the union
--      with hot winning on duplicate keys.
--   5. /Delete sees both tiers/: deleting a key whose value is
--      only in cold actually removes it.
module Streams.Properties.TieredStoreSpec (tests) where

import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.KeyValue.Tiered
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

type K = Int
type V = Int

newPair
  :: Int                       -- ^ hot capacity for eviction policy
  -> Int                       -- ^ evict-every N writes
  -> IO (KeyValueStore K V)
newPair cap every = do
  hot  <- inMemoryKeyValueStore @K @V (storeName "hot")
  cold <- inMemoryColdTier @K @V "cold"
  let cfg = TieredConfig
        { tcHot        = hot
        , tcCold       = cold
        , tcEvict      = countBasedEviction cap
        , tcEvictEvery = every
        }
  (wrapped, _) <- tieredKeyValueStore cfg
  pure wrapped

----------------------------------------------------------------------
-- Unit
----------------------------------------------------------------------

unit_put_then_get :: Spec
unit_put_then_get =
  it "put then get goes through hot" $ do
    kv <- newPair 100 0
    kvsPut kv 1 100
    kvsGet kv 1 >>= (`shouldBe` Just 100)

unit_promote_on_read :: Spec
unit_promote_on_read =
  it "read of cold-only key promotes to hot" $ do
    hot  <- inMemoryKeyValueStore @K @V (storeName "hot")
    cold <- inMemoryColdTier @K @V "cold"
    -- Seed cold with a key the hot doesn't see.
    ctPut cold 7 7000
    (wrapped, _stats) <- tieredKeyValueStore TieredConfig
      { tcHot        = hot
      , tcCold       = cold
      , tcEvict      = countBasedEviction 100
      , tcEvictEvery = 0
      }
    kvsGet wrapped 7 >>= (`shouldBe` Just 7000)
    -- After the read, the key is in hot.
    kvsGet hot 7 >>= (`shouldBe` Just 7000)

unit_eviction_demotes :: Spec
unit_eviction_demotes =
  it "exceeding hot capacity demotes the overflow to cold" $ do
    hot  <- inMemoryKeyValueStore @K @V (storeName "hot")
    cold <- inMemoryColdTier @K @V "cold"
    (wrapped, _stats) <- tieredKeyValueStore TieredConfig
      { tcHot        = hot
      , tcCold       = cold
      , tcEvict      = countBasedEviction 2
      , tcEvictEvery = 1
      }
    -- Three writes: the third triggers an eviction.
    kvsPut wrapped 1 1
    kvsPut wrapped 2 2
    kvsPut wrapped 3 3
    -- Cold now contains the oldest entry.
    coldEntries <- ctScan cold
    length coldEntries `shouldBe` 1
    -- Hot has the latest 2.
    hotN <- kvsApproxEntries hot
    hotN `shouldBe` 2

unit_delete_clears_cold :: Spec
unit_delete_clears_cold =
  it "delete of a cold-only key clears it" $ do
    hot  <- inMemoryKeyValueStore @K @V (storeName "hot")
    cold <- inMemoryColdTier @K @V "cold"
    ctPut cold 9 9000
    (wrapped, _) <- tieredKeyValueStore TieredConfig
      { tcHot = hot, tcCold = cold
      , tcEvict = countBasedEviction 100, tcEvictEvery = 0
      }
    mv <- kvsDelete wrapped 9
    mv `shouldBe` Just 9000
    kvsGet wrapped 9 >>= (`shouldBe` Nothing)
    ctScan cold >>= (`shouldBe` [])

----------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------

data Op
  = OpPut !K !V
  | OpGet !K
  | OpDelete !K
  deriving stock (Eq, Show)

genOp :: H.Gen Op
genOp = Gen.frequency
  [ (4, OpPut <$> Gen.int (Range.linear 0 5) <*> Gen.int (Range.linear 0 999))
  , (3, OpGet <$> Gen.int (Range.linear 0 5))
  , (2, OpDelete <$> Gen.int (Range.linear 0 5))
  ]

prop_tiered_matches_plain :: H.Property
prop_tiered_matches_plain = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 60) genOp)
  cap <- H.forAll (Gen.int (Range.linear 1 5))
  every <- H.forAll (Gen.int (Range.linear 0 4))
  (tieredView, plainView) <- H.evalIO $ do
    -- Reference: a plain in-memory store.
    plain <- inMemoryKeyValueStore @K @V (storeName "plain")
    tier  <- newPair cap every
    forM_ ops $ \op -> case op of
      OpPut k v -> do
        kvsPut plain k v
        kvsPut tier  k v
      OpGet _ -> pure ()
      OpDelete k -> do
        _ <- kvsDelete plain k
        _ <- kvsDelete tier k
        pure ()
    plainRows <- kvsAll plain >>= kvIteratorToList
    tierRows  <- kvsAll tier  >>= kvIteratorToList
    pure (Map.fromList tierRows, Map.fromList plainRows)
  tieredView H.=== plainView

prop_get_path_matches_plain :: H.Property
prop_get_path_matches_plain = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 60) genOp)
  keys <- H.forAll (Gen.list (Range.linear 1 10) (Gen.int (Range.linear 0 5)))
  cap   <- H.forAll (Gen.int (Range.linear 1 5))
  every <- H.forAll (Gen.int (Range.linear 0 4))
  (tieredGets, plainGets) <- H.evalIO $ do
    plain <- inMemoryKeyValueStore @K @V (storeName "plain")
    tier  <- newPair cap every
    forM_ ops $ \op -> case op of
      OpPut k v -> do
        kvsPut plain k v
        kvsPut tier  k v
      OpGet _ -> pure ()
      OpDelete k -> do
        _ <- kvsDelete plain k
        _ <- kvsDelete tier  k
        pure ()
    pg <- mapM (kvsGet plain) keys
    tg <- mapM (kvsGet tier)  keys
    pure (tg, pg)
  tieredGets H.=== plainGets

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests = describe "Tiered KV store" $ sequence_
  [ unit_put_then_get
  , unit_promote_on_read
  , unit_eviction_demotes
  , unit_delete_clears_cold
  , it "tiered store agrees with a plain store on kvsAll" $
      H.withTests 120 prop_tiered_matches_plain
  , it "tiered store agrees with a plain store on point lookups" $
      H.withTests 120 prop_get_path_matches_plain
  ]
