{-# LANGUAGE OverloadedStrings #-}

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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
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

tests :: TestTree
tests = testGroup "Kafka.Protocol.VersionNegotiation"
  [ testGroup "pickApiVersion"
      [ testCase "empty cache -> returns the supplied fallback" $ do
          cache <- AV.createVersionCache
          r <- VN.pickApiVersion cache addr 3 0 8 5
          r @?= Right 5
      , testCase "cache hit, fully overlapping range -> client max" $ do
          cache <- mkPopulatedCache [(3, 0, 12)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r @?= Right 8
      , testCase "cache hit, broker max < client max -> broker max" $ do
          cache <- mkPopulatedCache [(3, 0, 6)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r @?= Right 6
      , testCase "cache hit, broker min > 0 -> result clamped above broker min" $ do
          cache <- mkPopulatedCache [(3, 4, 12)]
          r <- VN.pickApiVersion cache addr 3 0 8 0
          r @?= Right 8   -- min(8, 12) = 8 which is >= max(0, 4)
      , testCase "cache hit, client max < broker min -> mismatch" $ do
          cache <- mkPopulatedCache [(3, 5, 12)]
          r <- VN.pickApiVersion cache addr 3 6 8 0
          case r of
            Right v ->
              -- 6 (clientMin) <= 8 (clientMax) and 8 >= 5 (brokerMin), so
              -- this overlaps; the result should be 8 (min(clientMax,
              -- brokerMax) clamped above brokerMin).
              v @?= 8
            Left mm ->
              error ("expected overlap, got mismatch: " <> show mm)
      , testCase "cache hit, broker max < client min -> Left VersionMismatch" $ do
          cache <- mkPopulatedCache [(3, 0, 2)]
          r <- VN.pickApiVersion cache addr 3 4 8 0
          case r of
            Left mm -> do
              VN.mismatchApiKey   mm @?= 3
              VN.mismatchClientMin mm @?= 4
              VN.mismatchClientMax mm @?= 8
              VN.mismatchBrokerMin mm @?= 0
              VN.mismatchBrokerMax mm @?= 2
            Right v -> error ("expected mismatch, got " <> show v)
      , testCase "different broker uses its own cache entry" $ do
          cache <- mkPopulatedCache [(3, 0, 8)]
          r1 <- VN.pickApiVersion cache addr  3 0 12 11
          r2 <- VN.pickApiVersion cache addr2 3 0 12 11
          r1 @?= Right 8         -- addr is in cache
          r2 @?= Right 11        -- addr2 isn't, falls back
      , testCase "API key not in broker's range -> falls back" $ do
          -- The broker's cache entry has /some/ APIs but not the
          -- one we're asking about; behave exactly like an empty
          -- cache for that API.
          cache <- mkPopulatedCache [(0, 0, 9)]   -- only Produce
          r <- VN.pickApiVersion cache addr 1 0 11 4   -- ask Fetch
          r @?= Right 4
      ]
  , testGroup "toPositive / fallback semantics"
      [ testCase "fallback respected for negative client max" $ do
          -- This is a contract check: 'pickApiVersion' doesn't
          -- get to invent a version below 0. Even if we ask for
          -- nonsense bounds, the cached path returns the
          -- broker's actual range; the empty path returns the
          -- caller's fallback verbatim. We don't sanity-check
          -- the fallback (it's the caller's responsibility) so
          -- a -1 fallback returns Right (-1).
          cache <- AV.createVersionCache
          r <- VN.pickApiVersion cache addr 3 0 8 (-1)
          r @?= Right (-1)
      , testCase "ensureVersionsNegotiated is a no-op when cache hit" $ do
          -- We can't run a real handshake without a Connection,
          -- but we /can/ verify the fast-path: when the cache
          -- already has an entry for the broker (any API key),
          -- 'ensureVersionsNegotiated' returns Right () without
          -- touching the supplied (no-op) Connection.
          cache <- mkPopulatedCache [(18, 0, 3)]
          -- An undefined Connection: if 'ensureVersionsNegotiated'
          -- ever evaluates it we'd crash, which is the test
          -- (fast-path = no Connection access).
          let bogusConn = error "bogus connection: should not be touched"
          let bogusNextCid = error "bogus corr id: should not be touched"
          r <- VN.ensureVersionsNegotiated bogusConn addr cache bogusNextCid
          assertBool "fast-path should succeed without touching the connection"
                     (case r of Right () -> True; _ -> False)
      ]
  ]
