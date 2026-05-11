{-# LANGUAGE OverloadedStrings #-}

module Network.BootstrapSpec (tests) where

import Data.IORef
import qualified Data.List as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Network.Bootstrap as B
import Kafka.Network.Connection (BrokerAddress (..))

tests :: TestTree
tests = testGroup "Bootstrap discoverer (KIP-580 / 899)"
  [ testCase "staticDiscoverer returns its argument verbatim"
      static
  , testCase "rotatingDiscoverer returns the first non-empty result"
      rotating
  , testCase "cachedDiscoverer reuses within the TTL window"
      cached
  , testCase "shuffledDiscoverer preserves the broker set"
      shuffled
  ]

bs :: [BrokerAddress]
bs = [BrokerAddress "h1" 9092, BrokerAddress "h2" 9092]

static :: IO ()
static = do
  r <- B.runDiscoverer (B.staticDiscoverer bs)
  r @?= bs

rotating :: IO ()
rotating = do
  let empty = B.staticDiscoverer []
      d = B.rotatingDiscoverer [empty, empty, B.staticDiscoverer bs]
  r <- B.runDiscoverer d
  r @?= bs

cached :: IO ()
cached = do
  -- Call a counter-based discoverer through a 60 s cache.
  countRef <- newIORef (0 :: Int)
  let underlying = B.Discoverer $ do
        modifyIORef' countRef (+ 1)
        pure bs
  d <- B.cachedDiscoverer 60_000 underlying
  _ <- B.runDiscoverer d
  _ <- B.runDiscoverer d
  _ <- B.runDiscoverer d
  n <- readIORef countRef
  n @?= 1   -- only the first call hit the underlying source

shuffled :: IO ()
shuffled = do
  -- The shuffled discoverer must preserve the broker set
  -- (just permute the order). Run it a few times and assert
  -- that the multiset matches the input.
  let d = B.shuffledDiscoverer (B.staticDiscoverer bs)
  results <- mapM (\_ -> B.runDiscoverer d) [1 .. 5 :: Int]
  let !setEq = all (\r -> L.sort r == L.sort bs) results
  assertBool "shuffled set matches input" setEq
