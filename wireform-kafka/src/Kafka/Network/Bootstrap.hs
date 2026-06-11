{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Network.Bootstrap
Description : KIP-580 (DNS lookup) + KIP-899 (pluggable cluster discovery)

KIP-580 expanded the DNS-lookup options for bootstrap servers
(any/v4/v6 + use-all-dns-ips + resolve-canonical-bootstrap).
KIP-899 generalised that further: callers can plug in their
own /cluster discoverer/ that returns the bootstrap-broker list
from whatever source they prefer (a service-discovery API, a
Kubernetes endpoint, a config-server JSON document, etc.). The
JVM client calls this @ClusterResourceListener@; we expose the
same surface as a record-of-IO.

Layered cake:

  * 'Discoverer' — the function the rest of the client consults
    to learn the current bootstrap broker list.
  * 'staticDiscoverer' — wraps the legacy
    @bootstrap.servers@ comma-separated list.
  * 'rotatingDiscoverer' — calls a list of underlying discoverers
    in turn, returning the first non-empty result. Useful for
    "primary HTTPS endpoint, fall back to DNS".
  * 'cachedDiscoverer' — memoises the result for a configurable
    TTL so high-frequency callers don't hammer the source.
  * 'shuffledDiscoverer' — randomises the order on every call so
    the producer / consumer don't always pin the same broker
    for their first connection.
-}
module Kafka.Network.Bootstrap (
  -- * Interface
  Discoverer (..),

  -- * Built-in implementations
  staticDiscoverer,
  rotatingDiscoverer,
  cachedDiscoverer,
  shuffledDiscoverer,

  -- * Convenience
  discoverBootstrap,
) where

import Control.Concurrent.STM
import Data.IORef
import Data.Int (Int64)
import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Time qualified as KafkaTime
import System.Random qualified as Rand


{- | The pluggable bootstrap-discovery interface. Returns the
current candidate broker list. The producer / consumer calls
this every time it has to /reconnect/ from scratch (i.e. has
no warm metadata cache).
-}
newtype Discoverer = Discoverer
  { runDiscoverer :: IO [BrokerAddress]
  }


{- | The legacy behaviour: the broker list is whatever was
configured.
-}
staticDiscoverer :: [BrokerAddress] -> Discoverer
staticDiscoverer bs = Discoverer (pure bs)


{- | Try each underlying discoverer in turn; return the first
non-empty result. If every discoverer returns an empty list,
return that.
-}
rotatingDiscoverer :: [Discoverer] -> Discoverer
rotatingDiscoverer ds = Discoverer (go ds)
  where
    go [] = pure []
    go (h : tl) = do
      bs <- runDiscoverer h
      if null bs then go tl else pure bs


{- | Memoise the result of an underlying discoverer for
@ttlMs@ milliseconds. Callers within the TTL window see the
cached value; the next call past the TTL re-fetches.
-}
cachedDiscoverer
  :: Int
  -- ^ TTL in ms
  -> Discoverer
  -> IO Discoverer
cachedDiscoverer ttlMs underlying = do
  ref <- newIORef (Nothing :: Maybe (Int64, [BrokerAddress]))
  pure
    Discoverer
      { runDiscoverer = do
          now <- nowMs
          cached <- readIORef ref
          case cached of
            Just (ts, bs) | now - ts < fromIntegral ttlMs -> pure bs
            _ -> do
              bs <- runDiscoverer underlying
              writeIORef ref (Just (now, bs))
              pure bs
      }


{- | Wrap a discoverer so its result is shuffled before return.
Useful for producers / consumers that always tried broker
index 0 first (a hot-spot risk on small clusters).
-}
shuffledDiscoverer :: Discoverer -> Discoverer
shuffledDiscoverer underlying = Discoverer $ do
  bs <- runDiscoverer underlying
  shuffle bs


-- | Convenience: discover + return; no transformation.
discoverBootstrap :: Discoverer -> IO [BrokerAddress]
discoverBootstrap = runDiscoverer


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

nowMs :: IO Int64
nowMs = KafkaTime.currentTimeMillis


{- | Fisher–Yates shuffle on a list. We use 'System.Random' so
the dependency tree stays small; callers needing a CSPRNG can
wrap their own shuffler around 'staticDiscoverer'.
-}
shuffle :: [a] -> IO [a]
shuffle xs = do
  let n = length xs
  swaps <- mapM (\i -> Rand.randomRIO (0, i)) [n - 1, n - 2 .. 1]
  pure (foldr applySwap xs (zip [n - 1, n - 2 ..] swaps))
  where
    applySwap (i, j) ys =
      let (h, t) = splitAt i ys
      in case t of
           [] -> ys
           (x : tl) ->
             let (a, b) = splitAt j h
             in case b of
                  [] -> ys
                  (y : tl') -> a ++ [x] ++ tl' ++ [y] ++ tl
