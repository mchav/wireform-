{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the wired KIP-213 foreign-key join DSL combinator.
--
-- The pure subscription/responder state machine has its own tests
-- in 'Streams.ForeignKeyJoinV2Spec'. This module exercises the
-- /topology-level/ semantics:
--
--   * Same-foreign-key value updates should re-emit using the new
--     left value (cache freshness, not just subscription
--     freshness).
--   * Switching foreign keys must unsubscribe from the old fk
--     and subscribe under the new fk.
--   * The right-side token check must accept the live
--     subscription unchanged across right-side updates.
--
-- These tests overlap with the broader FK tests in
-- 'Streams.JoinSpec' but are scoped to the token-verification
-- semantics introduced when KIP-213 was baked into the single
-- combinator.
module Streams.ForeignKeyJoinDSLSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams

tests :: TestTree
tests = testGroup "FK-join DSL (KIP-213 token verification)"
  [ same_fk_value_change_re_emits
  , rapid_fk_swap_then_old_fk_update_does_not_emit_stale
  , left_join_token_path_emits_with_no_right
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

-- | The left value's foreign key stays the same but the rest of
-- the value changes. The token therefore changes too. A fresh
-- right-side update must still re-emit the join, using the live
-- (post-change) left value.
same_fk_value_change_re_emits :: TestTree
same_fk_value_change_re_emits =
  testCase "same fk, value change: token cache is updated and right update emits live value" $ do
    b <- newStreamsBuilder
    tl <- tableFromTopic b (topicName "orders")
            (consumed textSerde textSerde)
            (materializedAs (storeName "orders-tk"))
    tr <- tableFromTopic b (topicName "users")
            (consumed textSerde textSerde)
            (materializedAs (storeName "users-tk"))
    out <- foreignKeyJoinKTable
            (\v -> T.takeWhile (/= '|') v)
            (\v u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
            (materializedAs (storeName "fk-out-tk"))
            tl
            tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-tk-app"

    -- Right table populated.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    -- Left record points at u1.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    -- Left record updated, same fk (u1), different value: token rotates.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|99") (t 2) 0
    -- Right side updates u1: must re-emit using the latest left value.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "ALICE2") (t 3) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    roKvGet rs "o1" >>= (@?= Just "ALICE2:99")
    closeDriver driver

-- | After a left record swaps from fk1 to fk2, an update on fk1
-- must NOT re-emit a join for the swapped record. The KIP-213
-- subscription store is supposed to drop the old subscription on
-- swap; this is the regression test that catches a missing
-- unsubscribe.
rapid_fk_swap_then_old_fk_update_does_not_emit_stale :: TestTree
rapid_fk_swap_then_old_fk_update_does_not_emit_stale =
  testCase "fk swap unsubscribes: later updates on the old fk don't re-emit" $ do
    b <- newStreamsBuilder
    tl <- tableFromTopic b (topicName "orders")
            (consumed textSerde textSerde)
            (materializedAs (storeName "orders-sw"))
    tr <- tableFromTopic b (topicName "users")
            (consumed textSerde textSerde)
            (materializedAs (storeName "users-sw"))
    out <- foreignKeyJoinKTable
            (\v -> T.takeWhile (/= '|') v)
            (\v u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
            (materializedAs (storeName "fk-out-sw"))
            tl
            tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-sw-app"

    -- Right side: u1 = alice, u2 = bob.
    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users")  (Just (bytes "u2")) (bytes "bob")   (t 0) 0
    -- Order originally at u1.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    -- Swap to u2. Should produce "bob:10".
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u2|10") (t 2) 0
    -- A right-side update on the OLD fk (u1) must not re-emit
    -- for o1: o1 has unsubscribed from u1.
    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "ALICE-NEW") (t 3) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    -- The output store still reflects the bob:10 join; updating u1
    -- did not overwrite it back to "ALICE-NEW:10".
    roKvGet rs "o1" >>= (@?= Just "bob:10")
    closeDriver driver

-- | Left-join tombstone path: the left value is present but the
-- right side is missing entirely. The token-verification path
-- must not regress this case; the joiner sees 'Nothing' and the
-- output is materialised.
left_join_token_path_emits_with_no_right :: TestTree
left_join_token_path_emits_with_no_right =
  testCase "left FK join: emits when no right value yet (token path)" $ do
    b <- newStreamsBuilder
    tl <- tableFromTopic b (topicName "orders")
            (consumed textSerde textSerde)
            (materializedAs (storeName "orders-lj"))
    tr <- tableFromTopic b (topicName "users")
            (consumed textSerde textSerde)
            (materializedAs (storeName "users-lj"))
    out <- leftForeignKeyJoinKTable
            (\v -> T.takeWhile (/= '|') v)
            (\v mu -> case mu of
                       Just u  -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v)
                       Nothing -> "<NONE>:" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
            (materializedAs (storeName "fk-out-lj"))
            tl
            tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-lj-app"

    -- Three left puts before the right side ever appears.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u9|x") (t 0) 0
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u9|y") (t 1) 0
    pipeInput driver (topicName "orders") (Just (bytes "o2")) (bytes "u9|z") (t 2) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    roKvGet rs "o1" >>= (@?= Just "<NONE>:y")
    roKvGet rs "o2" >>= (@?= Just "<NONE>:z")

    -- Right finally arrives: every subscriber re-emits with the
    -- live token (now matches the latest left value).
    pipeInput driver (topicName "users") (Just (bytes "u9")) (bytes "ENN") (t 3) 0
    roKvGet rs "o1" >>= (@?= Just "ENN:y")
    roKvGet rs "o2" >>= (@?= Just "ENN:z")

    closeDriver driver
