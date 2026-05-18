{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Properties.StoreRefSpec
-- Description : Tests for the typed StoreRef wrapper
--
-- These verify the contract that 'StoreRef' really does pin the
-- store kind + key/value types at compile time and that the
-- runtime lookup returns the same store the builder declared.
module Streams.Properties.StoreRefSpec (tests) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Streams.Processor as Processor
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Processor.Mock
  ( newMockProcessorContext
  , mockContext
  , registerStateStore
  )
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import qualified Kafka.Streams.State.Store as Store
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , storeName
  )
import Kafka.Streams.State.Ref

tests :: TestTree
tests = testGroup "Typed StoreRef"
  [ kvref_round_trip
  , kvref_wrong_kind_returns_nothing
  , kvref_missing_store_returns_nothing
  , someref_projection
  ]

----------------------------------------------------------------------
-- Setup
----------------------------------------------------------------------

-- | Build a mock context with one in-memory KV store attached
-- under 'sName'. Returns the typed ref + the live context.
withKVStore
  :: T.Text
  -> (StoreRef 'SKKV T.Text Int -> Processor.ProcessorContext -> IO a)
  -> IO a
withKVStore sName k = do
  let nm = storeName sName
      builder = inMemoryKeyValueStoreBuilder nm :: Store.StoreBuilderKV T.Text Int
  -- Materialise the store directly (no engine).
  kvs <- Store.sbKvBuild builder
  mctx <- newMockProcessorContext "test-app" (TaskId 0 0)
  registerStateStore mctx nm (AnyKeyValueStore kvs)
  let ref = kvRefOfBuilder builder
  k ref (mockContext mctx)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

kvref_round_trip :: TestTree
kvref_round_trip =
  testCase "getKVStoreRef: builder ref -> registered store -> typed read/write" $ do
    withKVStore "counter" $ \ref ctx -> do
      mStore <- getKVStoreRef ctx ref
      let kv = fromJust mStore
      kvsPut kv "k1" 7
      kvsGet kv "k1" >>= (@?= Just 7)

kvref_wrong_kind_returns_nothing :: TestTree
kvref_wrong_kind_returns_nothing =
  testCase "getWindowStoreRef on a KV-attached store returns Nothing" $ do
    withKVStore "kv-only" $ \_ ctx -> do
      -- Forge a window ref pointing at the same name; the
      -- runtime can't possibly produce this ref legitimately, so
      -- this is the "what if someone tried to coerce" guard.
      let bogus :: StoreRef 'SKWindow T.Text Int
          bogus = storeRefOfBuilder (storeName "kv-only")
      mW <- getWindowStoreRef ctx bogus
      case mW of
        Nothing -> pure ()
        Just _  -> assertBool "expected Nothing for wrong-kind ref" False

kvref_missing_store_returns_nothing :: TestTree
kvref_missing_store_returns_nothing =
  testCase "getKVStoreRef returns Nothing when the topology forgot the store" $ do
    mctx <- newMockProcessorContext "test-app" (TaskId 0 0)
    let ref :: StoreRef 'SKKV T.Text Int
        ref = storeRefOfBuilder (storeName "never-registered")
    mS <- getKVStoreRef (mockContext mctx) ref
    case mS of
      Nothing -> pure ()
      Just _  -> assertBool "expected Nothing for missing store" False

someref_projection :: TestTree
someref_projection =
  testCase "someStoreRefName projects all three SomeStoreRef arms" $ do
    let kv :: StoreRef 'SKKV Int Int
        kv = storeRefOfBuilder (storeName "kv")
        wn :: StoreRef 'SKWindow Int Int
        wn = storeRefOfBuilder (storeName "win")
        ses :: StoreRef 'SKSession Int Int
        ses = storeRefOfBuilder (storeName "sess")
    someStoreRefName (SomeKVRef kv)      @?= storeName "kv"
    someStoreRefName (SomeWindowRef wn)  @?= storeName "win"
    someStoreRefName (SomeSessionRef ses) @?= storeName "sess"
