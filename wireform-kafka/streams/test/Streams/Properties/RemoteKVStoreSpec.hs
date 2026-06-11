{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Streams.Properties.RemoteKVStoreSpec
Description : Property suite for the remote-KV backend

Properties:

  1. /Equivalence with a plain store/: on any random op
     sequence, the remote-backed view of the store agrees
     with a plain in-memory store.
  2. /Fault propagation/: an injected 'RemoteRetryable' on
     'RcGet' surfaces as a 'RemoteError' thrown to the
     caller, and the next call (which has no injected fault)
     succeeds.
  3. /Fault is one-shot/: an injected fault only fires for one
     call; subsequent calls of the same kind succeed.
  4. /putIfAbsent semantics/: identical to a plain store.
  5. /Range/: covers the @[lo, hi]@ inclusive band.
-}
module Streams.Properties.RemoteKVStoreSpec (tests) where

import Control.Exception (Handler (..), catches, try)
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.KeyValue.Remote
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  kvIteratorToList,
  storeName,
 )
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

type K = Int


type V = Int


newRemoteBacked :: IO (KeyValueStore K V, RemoteFaultPolicy)
newRemoteBacked = do
  policy <- noRemoteFaults
  client <- inMemoryRemoteKVClient @K @V "rkv-test" policy
  store <- remoteKeyValueStore (storeName "rkv-test") client
  pure (store, policy)


----------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------

unit_put_then_get :: Spec
unit_put_then_get =
  it "remote: put / get round-trip" $ do
    (kv, _) <- newRemoteBacked
    kvsPut kv 1 100
    kvsGet kv 1 >>= (`shouldBe` Just 100)


unit_fault_surfaces :: Spec
unit_fault_surfaces =
  it "remote: injected RcGet fault throws RemoteError" $ do
    (kv, policy) <- newRemoteBacked
    kvsPut kv 1 100
    setRemoteFault policy RcGet (RemoteRetryable "boom")
    r <- try @RemoteError (kvsGet kv 1)
    case r of
      Left (RemoteRetryable _) -> pure ()
      _ -> (False) `shouldBe` True
    -- After consuming the queued fault, the next call succeeds.
    kvsGet kv 1 >>= (`shouldBe` Just 100)


unit_fault_is_one_shot :: Spec
unit_fault_is_one_shot =
  it "remote: faults pop FIFO; queue empties" $ do
    (kv, policy) <- newRemoteBacked
    setRemoteFault policy RcGet (RemoteRetryable "first")
    setRemoteFault policy RcGet (RemoteRetryable "second")
    r1 <- try @RemoteError (kvsGet kv 0)
    r2 <- try @RemoteError (kvsGet kv 0)
    r3 <- try @RemoteError (kvsGet kv 0)
    case (r1, r2, r3) of
      (Left (RemoteRetryable t1), Left (RemoteRetryable t2), Right Nothing) -> do
        t1 `shouldBe` "first"
        t2 `shouldBe` "second"
      _ -> (False) `shouldBe` True


unit_range_inclusive :: Spec
unit_range_inclusive =
  it "remote: kvsRange yields entries in [lo, hi] inclusive" $ do
    (kv, _) <- newRemoteBacked
    forM_ [(1, 100), (2, 200), (3, 300), (4, 400)] $ \(k, v) ->
      kvsPut kv k v
    rs <- kvsRange kv 2 3 >>= kvIteratorToList
    rs `shouldBe` [(2, 200), (3, 300)]


----------------------------------------------------------------------
-- Property: remote-backed store agrees with a plain store
----------------------------------------------------------------------

data Op
  = OpPut !K !V
  | OpGet !K
  | OpDelete !K
  | OpPutIfAbsent !K !V
  deriving stock (Eq, Show)


genOp :: H.Gen Op
genOp =
  Gen.frequency
    [ (4, OpPut <$> Gen.int (Range.linear 0 5) <*> Gen.int (Range.linear 0 999))
    , (3, OpGet <$> Gen.int (Range.linear 0 5))
    , (2, OpDelete <$> Gen.int (Range.linear 0 5))
    ,
      ( 2
      , OpPutIfAbsent
          <$> Gen.int (Range.linear 0 5)
          <*> Gen.int (Range.linear 0 999)
      )
    ]


prop_remote_matches_plain :: H.Property
prop_remote_matches_plain = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  observed <- H.evalIO $ do
    (rkv, _) <- newRemoteBacked
    plain <- inMemoryKeyValueStore @K @V (storeName "plain")
    forM_ ops $ \op -> case op of
      OpPut k v -> do
        kvsPut rkv k v
        kvsPut plain k v
      OpGet _ -> pure ()
      OpDelete k -> do
        _ <- kvsDelete rkv k
        _ <- kvsDelete plain k
        pure ()
      OpPutIfAbsent k v -> do
        _ <- kvsPutIfAbsent rkv k v
        _ <- kvsPutIfAbsent plain k v
        pure ()
    a <- Map.fromList <$> (kvsAll rkv >>= kvIteratorToList)
    b <- Map.fromList <$> (kvsAll plain >>= kvIteratorToList)
    pure (a, b)
  let (a, b) = observed
  a H.=== b


prop_remote_point_lookup_matches_plain :: H.Property
prop_remote_point_lookup_matches_plain = H.property $ do
  ops <- H.forAll (Gen.list (Range.linear 1 40) genOp)
  ks <-
    H.forAll
      ( Gen.list
          (Range.linear 1 12)
          (Gen.int (Range.linear 0 5))
      )
  observed <- H.evalIO $ do
    (rkv, _) <- newRemoteBacked
    plain <- inMemoryKeyValueStore @K @V (storeName "plain")
    forM_ ops $ \op -> case op of
      OpPut k v -> do
        kvsPut rkv k v
        kvsPut plain k v
      OpGet _ -> pure ()
      OpDelete k -> do
        _ <- kvsDelete rkv k
        _ <- kvsDelete plain k
        pure ()
      OpPutIfAbsent k v -> do
        _ <- kvsPutIfAbsent rkv k v
        _ <- kvsPutIfAbsent plain k v
        pure ()
    a <- mapM (kvsGet rkv) ks
    b <- mapM (kvsGet plain) ks
    pure (a, b)
  let (a, b) = observed
  a H.=== b


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Remote KV store" $
    sequence_
      [ unit_put_then_get
      , unit_fault_surfaces
      , unit_fault_is_one_shot
      , unit_range_inclusive
      , it "remote-backed store agrees with a plain store on kvsAll" $
          H.withTests 120 prop_remote_matches_plain
      , it "remote-backed point lookups agree with a plain store" $
          H.withTests 120 prop_remote_point_lookup_matches_plain
      ]
