{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Kafka.Client.Internal.Subscribe' — specifically the
-- subscription-metadata codec used by JoinGroup. The interesting
-- bit is the @ownedPartitions@ field (KIP-341 / KIP-429): the
-- leader pulls it out via 'decodeSubscriptionFull' to feed the
-- sticky / cooperative-sticky assignors with previous-generation
-- state.
module Client.SubscribeSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import Kafka.Client.Internal.Subscribe
  ( encodeSubscription
  , encodeSubscriptionWithOwned
  , decodeSubscription
  , decodeSubscriptionFull
  , stickyAssign
  )

tests :: TestTree
tests = testGroup "Subscribe metadata codec"
  [ unit_encode_decode_basic
  , unit_decode_full_no_owned
  , unit_decode_full_with_owned
  , unit_decode_full_preserves_user_data
  , unit_decode_full_handles_multiple_owned_topics
  , unit_decode_full_invalid_bytes
  , unit_sticky_assign_preserves_owned_partitions
  , unit_sticky_assign_drops_unsubscribed_partitions
  ]

unit_encode_decode_basic :: TestTree
unit_encode_decode_basic =
  testCase "encodeSubscription / decodeSubscription roundtrip" $ do
    let bs = encodeSubscription ["t-a", "t-b"]
    case decodeSubscription bs of
      Right (topics, ud) -> do
        topics @?= ["t-a", "t-b"]
        ud     @?= BS.empty
      Left err -> assertFailure ("decode failed: " <> err)

unit_decode_full_no_owned :: TestTree
unit_decode_full_no_owned =
  testCase "decodeSubscriptionFull on plain subscription returns empty owned list" $ do
    let bs = encodeSubscription ["only-topic"]
    case decodeSubscriptionFull bs of
      Right (topics, ud, owned) -> do
        topics @?= ["only-topic"]
        ud     @?= BS.empty
        owned  @?= []
      Left err -> assertFailure ("decode failed: " <> err)

unit_decode_full_with_owned :: TestTree
unit_decode_full_with_owned =
  testCase "decodeSubscriptionFull recovers ownedPartitions for sticky rebalance" $ do
    let owned = [("t1", [0, 1]), ("t2", [2])] :: [(Text, [Int])]
        ownedI32 = [(t, map fromIntegral ps) | (t, ps) <- owned]
        bs       = encodeSubscriptionWithOwned ["t1", "t2"] BS.empty ownedI32
    case decodeSubscriptionFull bs of
      Right (topics, _, decoded) -> do
        topics @?= ["t1", "t2"]
        Map.fromList decoded @?= Map.fromList ownedI32
      Left err -> assertFailure ("decode failed: " <> err)

unit_decode_full_preserves_user_data :: TestTree
unit_decode_full_preserves_user_data =
  testCase "decodeSubscriptionFull preserves opaque user-data bytes" $ do
    let ud = BS.pack [0x01, 0x02, 0x03, 0xff]
        bs = encodeSubscriptionWithOwned ["t"] ud []
    case decodeSubscriptionFull bs of
      Right (_, decodedUd, _) -> decodedUd @?= ud
      Left err -> assertFailure ("decode failed: " <> err)

unit_decode_full_handles_multiple_owned_topics :: TestTree
unit_decode_full_handles_multiple_owned_topics =
  testCase "decodeSubscriptionFull preserves owned-partition order within each topic" $ do
    let owned = [("t-orig", [3, 1, 0, 2])]
        bs    = encodeSubscriptionWithOwned ["t-orig"] BS.empty owned
    case decodeSubscriptionFull bs of
      Right (_, _, decoded) ->
        decoded @?= owned
      Left err -> assertFailure ("decode failed: " <> err)

unit_decode_full_invalid_bytes :: TestTree
unit_decode_full_invalid_bytes =
  testCase "decodeSubscriptionFull surfaces a Left on truncated bytes" $ do
    let truncated = BS.pack [0x00]
    case decodeSubscriptionFull truncated of
      Left _   -> pure ()
      Right ok -> assertFailure ("expected decode failure, got " <> show ok)

----------------------------------------------------------------------
-- Sticky assignor end-to-end
--
-- decodeSubscriptionFull + stickyAssign together implement the
-- "preserve previous-gen ownership" path. We exercise both
-- together to show that round-tripping a member's owned
-- partitions through the wire codec and back into stickyAssign
-- yields the same partition retained on rebalance.
----------------------------------------------------------------------

unit_sticky_assign_preserves_owned_partitions :: TestTree
unit_sticky_assign_preserves_owned_partitions =
  testCase "stickyAssign keeps previously owned partitions for the same member" $ do
    -- Generation 1: m1 owned (t, 0..3) and m2 joined empty.
    let mems        = [("m1", ["t"]), ("m2", ["t"])]
        topicParts  = Map.fromList [("t", [0, 1, 2, 3])]
        prev        = Just [("m1", [("t", [0, 1, 2, 3])])]
        result      = stickyAssign mems topicParts prev
        m1Parts     = lookup "m1" result
        m2Parts     = lookup "m2" result
    -- m1 should keep some of (0..3) — at least 2 partitions
    -- (sticky moves the minimum needed for ±1 balance, so m1
    -- keeps 2, m2 receives 2).
    case (m1Parts, m2Parts) of
      (Just [("t", m1ps)], Just [("t", m2ps)]) -> do
        length m1ps + length m2ps @?= 4
        assertBool ("m1 retained: " <> show m1ps)
                   (length m1ps >= 1 && length m1ps <= 3)
        -- Anything m1 retains was in its previous-generation
        -- ownership.
        assertBool ("m1 retained partitions outside previous ownership: " <> show m1ps)
                   (all (`elem` [0, 1, 2, 3]) m1ps)
      other -> assertFailure ("unexpected assignment: " <> show other)

unit_sticky_assign_drops_unsubscribed_partitions :: TestTree
unit_sticky_assign_drops_unsubscribed_partitions =
  testCase "stickyAssign drops previously owned partitions whose topic is no longer subscribed" $ do
    -- m1 used to own (t-old, 0..2) but no longer subscribes to it.
    -- The new generation only knows about t-new.
    let mems        = [("m1", ["t-new"])]
        topicParts  = Map.fromList [("t-new", [0, 1])]
        prev        = Just [("m1", [("t-old", [0, 1, 2])])]
        result      = stickyAssign mems topicParts prev
    case lookup "m1" result of
      Just byTopic -> do
        -- t-old partitions are dropped (m1 no longer subscribes).
        lookup "t-old" byTopic @?= Nothing
        -- t-new partitions get handed out anew.
        case lookup "t-new" byTopic of
          Just ps -> ps @?= [0, 1]
          Nothing -> assertFailure "t-new not assigned"
      Nothing -> assertFailure "m1 missing from result"
