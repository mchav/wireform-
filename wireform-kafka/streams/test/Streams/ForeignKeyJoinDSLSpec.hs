{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the wired KIP-213 foreign-key join DSL combinator.
--
-- The combinator under test is
-- 'Kafka.Streams.Imperative.foreignKeyJoinKTable'
-- (and its @left@ variant). The internal subscription-token /
-- responder protocol is not exposed; we verify it indirectly by
-- driving the DSL with sequences of left and right updates and
-- asserting the materialised output store matches the expected
-- "live join cache" that a JVM Kafka Streams user would expect.
module Streams.ForeignKeyJoinDSLSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Imperative

tests :: Spec
tests = describe "FK-join DSL (KIP-213)" $ sequence_
  [ same_fk_value_change_re_emits
  , rapid_fk_swap_then_old_fk_update_does_not_emit_stale
  , left_join_token_path_emits_with_no_right
  , prop_dsl_permutation_invariance
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

----------------------------------------------------------------------
-- Targeted unit tests
----------------------------------------------------------------------

-- | The left value's foreign key stays the same but the rest of
-- the value changes. The token therefore changes too. A fresh
-- right-side update must still re-emit the join, using the live
-- (post-change) left value.
same_fk_value_change_re_emits :: Spec
same_fk_value_change_re_emits =
  it "same fk, value change: token rotates and right-side update emits live value" $ do
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

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    -- Same fk (u1), different value -> token rotates.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|99") (t 2) 0
    -- Right side updates u1: must re-emit using the latest left value.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "ALICE2") (t 3) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "ALICE2:99")
    closeDriver driver

-- | After a left record swaps from fk1 to fk2, an update on fk1
-- must NOT re-emit a join for the swapped record. The
-- subscription is supposed to migrate; this is the regression
-- test that catches a missing unsubscribe.
rapid_fk_swap_then_old_fk_update_does_not_emit_stale :: Spec
rapid_fk_swap_then_old_fk_update_does_not_emit_stale =
  it "fk swap unsubscribes: later updates on the old fk don't re-emit" $ do
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

    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users")  (Just (bytes "u2")) (bytes "bob")   (t 0) 0
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    -- Swap to u2.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u2|10") (t 2) 0
    -- A right-side update on the OLD fk (u1) must not re-emit for o1.
    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "ALICE-NEW") (t 3) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "bob:10")
    closeDriver driver

-- | Left-join tombstone path: the left value is present but the
-- right side is missing entirely. The token-verification path
-- must not regress this case; the joiner sees 'Nothing' and the
-- output is materialised.
left_join_token_path_emits_with_no_right :: Spec
left_join_token_path_emits_with_no_right =
  it "left FK join: emits when no right value yet" $ do
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

    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u9|x") (t 0) 0
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u9|y") (t 1) 0
    pipeInput driver (topicName "orders") (Just (bytes "o2")) (bytes "u9|z") (t 2) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "<NONE>:y")
    rs.roKvGet "o2" >>= (`shouldBe` Just "<NONE>:z")

    -- Right finally arrives: every subscriber re-emits with the
    -- live token (now matches the latest left value).
    pipeInput driver (topicName "users") (Just (bytes "u9")) (bytes "ENN") (t 3) 0
    rs.roKvGet "o1" >>= (`shouldBe` Just "ENN:y")
    rs.roKvGet "o2" >>= (`shouldBe` Just "ENN:z")

    closeDriver driver

----------------------------------------------------------------------
-- Property: permutation invariance over distinct keys
----------------------------------------------------------------------

-- | For an event sequence whose left keys are pairwise distinct
-- and whose right foreign keys are pairwise distinct (so the
-- final state is independent of order), the materialised output
-- store after replaying any permutation of those events must
-- match the join computed against the (left, right) caches that
-- result from the same set of (key, value) updates.
prop_dsl_permutation_invariance :: Spec
prop_dsl_permutation_invariance =
  it
    "DSL permutation invariance over distinct keys"
    $ property $ do
        leftKeys  <- forAll $ Gen.list (Range.linear 0 4) (Gen.int (Range.linear 1 4))
        rightKeys <- forAll $ Gen.list (Range.linear 0 4) (Gen.int (Range.linear 1 3))
        let !uniqueLefts  = uniq leftKeys
            !uniqueRights = uniq rightKeys
        -- Pair each left key with a foreign key chosen from the
        -- same right pool so we get some hits.
        let lefts  = zipWith3
                       (\k fk amt -> (k, T.pack (show fk) <> "|" <> T.pack (show amt)))
                       uniqueLefts
                       (cycle (1 : uniqueRights))
                       [10, 20 ..]
            rights = zipWith
                       (\fk u -> (fk, "user-" <> T.pack (show u)))
                       uniqueRights
                       [1 :: Int ..]
            events = map FkEvLeft lefts ++ map FkEvRight rights
            -- Reverse plays right-first.
            permuted = reverse events
        out1 <- evalIO (runDSL events)
        out2 <- evalIO (runDSL permuted)
        Map.toAscList out1 === Map.toAscList out2
        Map.toAscList out1 === Map.toAscList (expectedJoin lefts rights)
  where
    uniq :: Ord a => [a] -> [a]
    uniq = List.sort . List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []

data FkEvent = FkEvLeft  (Int, Text)
             | FkEvRight (Int, Text)
             deriving stock (Eq, Show)

-- Run the DSL combinator over an event sequence, return the
-- final state of the materialised output store as a Map.
runDSL :: [FkEvent] -> IO (Map Text Text)
runDSL events = do
  b <- newStreamsBuilder
  tl <- tableFromTopic b (topicName "orders")
          (consumed textSerde textSerde)
          (materializedAs (storeName "orders-prop"))
  tr <- tableFromTopic b (topicName "users")
          (consumed textSerde textSerde)
          (materializedAs (storeName "users-prop"))
  out <- foreignKeyJoinKTable
          (\v -> T.takeWhile (/= '|') v)
          (\v u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
          (materializedAs (storeName "fk-out-prop"))
          tl
          tr
  topo <- buildTopology b
  driver <- newDriver topo "fk-prop-app"
  let pump n ev = case ev of
        FkEvLeft (k, v) -> do
          pipeInput driver (topicName "orders")
            (Just (bytes (T.pack (show k))))
            (bytes v)
            (t (fromIntegral n))
            0
          pure (n + 1)
        FkEvRight (fk, v) -> do
          pipeInput driver (topicName "users")
            (Just (bytes (T.pack (show fk))))
            (bytes v)
            (t (fromIntegral n))
            0
          pure (n + 1)
  _ <- foldOverEvents pump (1 :: Int) events
  Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
  result <- collectStore rs (map (\(k, _) -> T.pack (show k)) (lefts events))
  closeDriver driver
  pure result
  where
    lefts :: [FkEvent] -> [(Int, Text)]
    lefts = foldr keepLeft []
    keepLeft (FkEvLeft x) acc = x : acc
    keepLeft _ acc            = acc

-- Sequential left-fold over IO. Avoids list-comprehension style.
foldOverEvents
  :: (s -> a -> IO s)
  -> s
  -> [a]
  -> IO s
foldOverEvents _ s []       = pure s
foldOverEvents step s (x:xs) = do
  s' <- step s x
  foldOverEvents step s' xs

collectStore
  :: ReadOnlyKeyValueStore Text Text
  -> [Text]
  -> IO (Map Text Text)
collectStore rs ks =
  foldOverEvents grab Map.empty ks
  where
    grab acc k = do
      mv <- rs.roKvGet k
      pure $ case mv of
        Just v  -> Map.insert k v acc
        Nothing -> acc

-- | Reference implementation: build the (left, right) caches and
-- compute the join the same way the DSL is supposed to.
expectedJoin :: [(Int, Text)] -> [(Int, Text)] -> Map Text Text
expectedJoin lefts rights =
  let !leftMap  = Map.fromList (map (\(k, v) -> (T.pack (show k), v)) lefts)
      !rightMap = Map.fromList (map (\(fk, v) -> (T.pack (show fk), v)) rights)
      step k v acc =
        let !fk      = T.takeWhile (/= '|') v
            !amt     = T.dropWhile (== '|') (T.dropWhile (/= '|') v)
        in case Map.lookup fk rightMap of
             Just u  -> Map.insert k (u <> ":" <> amt) acc
             Nothing -> acc
   in Map.foldrWithKey step Map.empty leftMap
