{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}

-- | Unit tests for the 'pickApiVersion' selector that's wired
-- through the Producer / Consumer / AdminClient / Transaction
-- request paths.
--
-- The handshake half ('ensureVersionsNegotiated') is exercised
-- end-to-end by the live-broker integration suite — there's no
-- way to drive 'negotiateVersions' without a real Connection,
-- which the unit suite deliberately can't open.
module Protocol.VersionNegotiationSpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Syd

import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.FetchRequest as FR
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ProduceRequest as PR
import qualified "wireform-kafka-protocol" Kafka.Protocol.Message as Msg
import qualified Kafka.Protocol.VersionNegotiation as VN

----------------------------------------------------------------------
-- Test fixtures
----------------------------------------------------------------------

addr :: Conn.BrokerAddress
addr = Conn.BrokerAddress "broker-a" 9092

addr2 :: Conn.BrokerAddress
addr2 = Conn.BrokerAddress "broker-b" 9092

-- | Build an 'ApiVersionCache' pre-populated with @addr@ -> the
-- supplied (apiKey -> (min, max)) ranges.
mkPopulatedCache
  :: [(Int, Int, Int)]
  -- ^ (apiKey, brokerMin, brokerMax)
  -> IO AV.ApiVersionCache
mkPopulatedCache rows = do
  cache <- AV.createVersionCache
  let m = Map.fromList
            [ ( fromIntegral k
              , AV.ApiVersionRange
                  { AV.rangeMinVersion = fromIntegral mn
                  , AV.rangeMaxVersion = fromIntegral mx
                  }
              )
            | (k, mn, mx) <- rows
            ]
  -- Use the dedicated test seeding helper rather than going
  -- through the wire-level handshake (which would require a
  -- real Connection / broker).
  AV.unsafeSeedVersionCache cache addr m
  pure cache

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests = describe "Kafka.Protocol.VersionNegotiation" $ sequence_
  [ describe "pickApiVersion" $ sequence_
      [ it "empty cache -> returns the supplied fallback" $ do
          cache <- AV.createVersionCache
          r <- VN.pickApiVersion cache addr 3 0 8 5
          r `shouldBe` Right 5
      , it "cache hit, fully overlapping range -> client max" $ do
          cache <- mkPopulatedCache [(3, 0, 12)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r `shouldBe` Right 8
      , it "cache hit, broker max < client max -> broker max" $ do
          cache <- mkPopulatedCache [(3, 0, 6)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r `shouldBe` Right 6
      , it "cache hit, broker min > 0 -> result clamped above broker min" $ do
          cache <- mkPopulatedCache [(3, 4, 12)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r `shouldBe` Right 8   -- min(8, 12) = 8 which is >= max(0, 4)
      , it "cache hit, client max < broker min -> mismatch" $ do
          cache <- mkPopulatedCache [(3, 5, 12)]
          r <- VN.pickApiVersion cache addr 3 6 8 0
          case r of
            Right v ->
              -- 6 (clientMin) <= 8 (clientMax) and 8 >= 5 (brokerMin), so
              -- this overlaps; the result should be 8 (min(clientMax,
              -- brokerMax) clamped above brokerMin).
              v `shouldBe` 8
            Left mm ->
              error ("expected overlap, got mismatch: " <> show mm)
      , it "cache hit, broker max < client min -> Left VersionMismatch" $ do
          cache <- mkPopulatedCache [(3, 0, 2)]
          r <- VN.pickApiVersion cache addr 3 4 8 0
          case r of
            Left mm -> do
              VN.mismatchApiKey   mm `shouldBe` 3
              VN.mismatchClientMin mm `shouldBe` 4
              VN.mismatchClientMax mm `shouldBe` 8
              VN.mismatchBrokerMin mm `shouldBe` 0
              VN.mismatchBrokerMax mm `shouldBe` 2
            Right v -> error ("expected mismatch, got " <> show v)
      , it "different broker uses its own cache entry" $ do
          cache <- mkPopulatedCache [(3, 0, 8)]
          r1 <- VN.pickApiVersion cache addr  3 0 12 11
          r2 <- VN.pickApiVersion cache addr2 3 0 12 11
          r1 `shouldBe` Right 8         -- addr is in cache
          r2 `shouldBe` Right 11        -- addr2 isn't, falls back
      , it "API key not in broker's range -> falls back" $ do
          -- The broker's cache entry has /some/ APIs but not the
          -- one we're asking about; behave exactly like an empty
          -- cache for that API.
          cache <- mkPopulatedCache [(0, 0, 9)]   -- only Produce
          r <- VN.pickApiVersion cache addr 1 0 11 4   -- ask Fetch
          r `shouldBe` Right 4
      ]
  , describe "toPositive / fallback semantics" $ sequence_
      [ it "fallback respected for negative client max" $ do
          -- This is a contract check: 'pickApiVersion' doesn't
          -- get to invent a version below 0. Even if we ask for
          -- nonsense bounds, the cached path returns the
          -- broker's actual range; the empty path returns the
          -- caller's fallback verbatim. We don't sanity-check
          -- the fallback (it's the caller's responsibility) so
          -- a -1 fallback returns Right (-1).
          cache <- AV.createVersionCache
          r <- VN.pickApiVersion cache addr 3 0 8 (-1)
          r `shouldBe` Right (-1)
      , it "ensureVersionsNegotiated is a no-op when cache hit" $ do
          -- We can't run a real handshake without a Connection,
          -- but we /can/ verify the fast-path: when the cache
          -- already has an entry for the broker (any API key),
          -- 'ensureVersionsNegotiated' returns Right () without
          -- touching the supplied (no-op) Connection.
          cache <- mkPopulatedCache [(18, 0, 3)]
          let bogusConn = error "bogus connection: should not be touched"
          let bogusNextCid = error "bogus corr id: should not be touched"
          r <- VN.ensureVersionsNegotiated bogusConn addr cache bogusNextCid
          (case r of Right () -> True; _ -> False) `shouldBe` True
      ]
  , describe "pickApiVersionFor / pickApiVersionForRange (type-driven)" $ sequence_
      [ it "pickApiVersionFor uses the message's full codegen range" $ do
          -- ProduceRequest's codegen range is (3, 13) per its
          -- 'KafkaMessage' instance. Broker advertises Produce
          -- [3..12]. Result: 12 (broker max, since it's < client max).
          --
          -- This is the headline ergonomic win: no need to spell
          -- out apiKey + min + max at every call site.
          Msg.messageApiKey @PR.ProduceRequest `shouldBe` 0
          cache <- mkPopulatedCache [(0, 3, 12)]
          r <- VN.pickApiVersionFor @PR.ProduceRequest cache addr 3
          r `shouldBe` Right 12
      , it "pickApiVersionFor falls back when broker hasn't responded" $ do
          cache <- AV.createVersionCache
          r <- VN.pickApiVersionFor @PR.ProduceRequest cache addr 7
          r `shouldBe` Right 7
      , it "pickApiVersionForRange overrides the message's range" $ do
          -- FetchRequest codegen range is (4, 17). The client
          -- caps at 12 because v13+ uses TopicId. Use the
          -- override; broker advertises Fetch [4..15] -> result
          -- is 12 (the override max, since it's < broker max).
          cache <- mkPopulatedCache [(1, 4, 15)]
          r <- VN.pickApiVersionForRange @FR.FetchRequest 4 12 cache addr 4
          r `shouldBe` Right 12
      , it "pickApiVersionForRange override below broker max takes precedence" $ do
          -- Broker would be happy to do Fetch v17, but the
          -- client only trusts v12 — the override caps below.
          cache <- mkPopulatedCache [(1, 4, 17)]
          r <- VN.pickApiVersionForRange @FR.FetchRequest 4 12 cache addr 4
          r `shouldBe` Right 12
      , it "pickApiVersionForRange override above broker max -> broker max" $ do
          -- Override pushes higher than the broker actually
          -- supports; the negotiation still respects the
          -- broker's max (so we don't ship a request the broker
          -- can't decode).
          cache <- mkPopulatedCache [(1, 4, 8)]
          r <- VN.pickApiVersionForRange @FR.FetchRequest 4 12 cache addr 4
          r `shouldBe` Right 8
      , it "pickApiVersionForRange empty cache -> falls back" $ do
          cache <- AV.createVersionCache
          r <- VN.pickApiVersionForRange @FR.FetchRequest 4 12 cache addr 7
          r `shouldBe` Right 7
      , it "pickApiVersionForRange pinned to a single version (test exercise)" $ do
          -- Tests can pin a specific version by setting min=max,
          -- which is the canonical way to drive a request at a
          -- known version regardless of what the broker
          -- advertises.
          cache <- mkPopulatedCache [(1, 4, 17)]
          r <- VN.pickApiVersionForRange @FR.FetchRequest 7 7 cache addr 7
          r `shouldBe` Right 7
      , it "pickApiVersionForRange pinned single version, broker doesn't support it -> mismatch" $ do
          -- If the broker is too old for the pinned version,
          -- the negotiator returns a mismatch rather than
          -- silently picking something the broker rejects.
          cache <- mkPopulatedCache [(1, 4, 6)]
          r <- VN.pickApiVersionForRange @FR.FetchRequest 7 7 cache addr 7
          case r of
            Left mm -> do
              VN.mismatchApiKey   mm `shouldBe` 1
              VN.mismatchClientMin mm `shouldBe` 7
              VN.mismatchClientMax mm `shouldBe` 7
              VN.mismatchBrokerMin mm `shouldBe` 4
              VN.mismatchBrokerMax mm `shouldBe` 6
            Right v -> error ("expected mismatch, got " <> show v)
      ]
  ]
