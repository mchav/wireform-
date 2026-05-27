{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Tests for "Network.HTTP.Client.Cache". Focused on the
'CacheStore' contract and on the behaviours that distinguish the
RFC 9111 cache from a passthrough middleware (freshness, ETag
revalidation, no-store / no-cache).

The tests build their own mock transport that records every
upstream call into an 'IORef'; that lets us assert exactly when
the cache hit vs missed.
-}
module Test.Cache (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import qualified Network.HTTP.Client.BodyStream as BSm
import           Network.HTTP.Client.Cache
import qualified Network.HTTP.Client.Request as WReq
import           Network.HTTP.Client.Request (Request, get)
import           Network.HTTP.Client.Response (RawResponse (..))
import qualified Network.HTTP.Client.Response as Resp
import           Network.HTTP.Client.Protocol  (ProtocolInfo (..))
import           Network.HTTP.Client.Send      (prepareRequest)
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI       as WURI

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- | A canned response factory: each call to 'mkTransport' takes a
-- list of @(status, headers, body)@ tuples and returns a
-- 'Transport' that returns them in order, recording every call
-- into a counter.
mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef Int, IORef [Request BSm.BodyStream])
mkTransport canned = do
  responsesRef <- newIORef canned
  callsRef     <- newIORef (0 :: Int)
  requestsRef  <- newIORef ([] :: [Request BSm.BodyStream])
  let go = Transport $ \req -> do
        atomicModifyIORef' callsRef (\n -> (n + 1, ()))
        atomicModifyIORef' requestsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' responsesRef $ \rs -> case rs of
          (r : rest) -> (rest, r)
          []         -> ([], (S.status500, [], "no more canned responses"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure RawResponse
          { Resp.statusCode    = st
          , Resp.headers       = hdrs
          , Resp.bodyPopper    = popper
          , Resp.protocolInfo  = HTTP1_1
          }
  pure (go, callsRef, requestsRef)

makeRequest :: ByteString -> IO (Request BSm.BodyStream)
makeRequest pathBs =
  case WURI.parseTemplate (BS8.unpack ("http://example.com" <> pathBs)) of
    Left err -> error ("makeRequest: bad URI: " <> show err)
    Right t  -> prepareRequest [] (get t)

drainBody :: RawResponse -> IO ByteString
drainBody = BSm.popperBytes . Resp.bodyPopper

-- ---------------------------------------------------------------------------
-- Hit + miss accounting
-- ---------------------------------------------------------------------------

unit_fresh_hits :: TestTree
unit_fresh_hits = testCase "fresh response satisfies subsequent reads" $ do
  let payload = "hello"
  (t, calls, _) <- mkTransport
    [ ( S.status200
      , [ (H.hCacheControl, "max-age=60")
        , (H.hContentLength, BS8.pack (show (BS.length payload)))
        ]
      , payload
      )
    ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  r1 <- makeRequest "/a" >>= withC
  body1 <- drainBody r1
  r2 <- makeRequest "/a" >>= withC
  body2 <- drainBody r2
  n <- readIORef calls
  assertEqual "upstream calls" 1 n
  assertEqual "first body"  payload body1
  assertEqual "second body" payload body2
  size <- cacheSize cache
  assertEqual "cache size" 1 size

unit_no_store_response :: TestTree
unit_no_store_response = testCase "no-store on response is not cached" $ do
  let payload = "secret"
  (t, calls, _) <- mkTransport
    [ ( S.status200, [(H.hCacheControl, "no-store, max-age=60")], payload )
    , ( S.status200, [(H.hCacheControl, "no-store, max-age=60")], payload )
    ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/x" >>= withC >>= drainBody
  _ <- makeRequest "/x" >>= withC >>= drainBody
  n <- readIORef calls
  assertEqual "upstream calls" 2 n
  size <- cacheSize cache
  assertEqual "cache size"     0 size

unit_etag_revalidation :: TestTree
unit_etag_revalidation = testCase "stale entry revalidates with ETag and replays on 304" $ do
  let payload = "etag-body"
      etag    = "\"v1\""
  (t, calls, reqsRef) <- mkTransport
    [ ( S.status200
      , [ (H.hCacheControl, "max-age=0")
        , (H.hETag,         etag)
        ]
      , payload
      )
    , ( S.status304
      , [ (H.hETag, etag) ]
      , ""
      )
    ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/e" >>= withC >>= drainBody
  r2 <- makeRequest "/e" >>= withC
  body2 <- drainBody r2
  n <- readIORef calls
  assertEqual "upstream calls (1 + revalidation)" 2 n
  -- The replayed response is the cached body, not the 304's empty body.
  assertEqual "replayed body" payload body2
  -- The second request should have carried If-None-Match.
  reqs <- readIORef reqsRef
  let recent  = head reqs  -- newest first
      hadInm  = case lookup H.hIfNoneMatch (WReq.headers recent) of
                  Just v  -> v == etag
                  Nothing -> False
  assertBool "If-None-Match carried" hadInm

-- ---------------------------------------------------------------------------
-- Pluggable store
-- ---------------------------------------------------------------------------

-- | A custom 'CacheStore' that records every operation it sees.
-- Used to verify the cache routes its writes / reads through
-- 'CacheStore' instead of touching internal state.
mkAuditedStore :: IO (CacheStore, IORef [String])
mkAuditedStore = do
  inner  <- newInMemoryStore 64
  audit  <- newIORef ([] :: [String])
  let log_ msg = atomicModifyIORef' audit (\xs -> (msg : xs, ()))
  pure
    ( CacheStore
        { csLookup = \k -> do
            log_ ("lookup " <> show (ckPath k))
            csLookup inner k
        , csInsert = \k v -> do
            log_ ("insert " <> show (ckPath k))
            csInsert inner k v
        , csDelete = \k -> do
            log_ ("delete " <> show (ckPath k))
            csDelete inner k
        , csClear = do
            log_ "clear"
            csClear inner
        , csSize = csSize inner
        }
    , audit
    )

unit_custom_store_is_used :: TestTree
unit_custom_store_is_used = testCase "withCache routes through a custom CacheStore" $ do
  let payload = "audit-body"
  (t, _, _) <- mkTransport
    [ ( S.status200
      , [(H.hCacheControl, "max-age=60"), (H.hContentLength, BS8.pack (show (BS.length payload)))]
      , payload
      )
    ]
  (store, audit) <- mkAuditedStore
  let cache = newCacheWith store defaultCacheConfig
      withC = sendRaw (withCache cache t)
  _ <- makeRequest "/p1" >>= withC >>= drainBody
  _ <- makeRequest "/p1" >>= withC >>= drainBody
  log_ <- reverse <$> readIORef audit
  -- Expected pattern: lookup (miss), insert (after store), lookup (hit).
  let summary = filter (\s -> "lookup" `isPrefix` s || "insert" `isPrefix` s) log_
  assertEqual "audit count" 3 (length summary)
  assertEqual "first op"  True ("lookup" `isPrefix` head summary)
  assertEqual "second op" True ("insert" `isPrefix` (summary !! 1))
  assertEqual "third op"  True ("lookup" `isPrefix` (summary !! 2))
  where
    isPrefix p s = take (length p) s == p

unit_custom_store_isolated :: TestTree
unit_custom_store_isolated = testCase "two caches with separate stores don't share state" $ do
  let payload = "isolation-body"
  (t, calls, _) <- mkTransport
    [ ( S.status200
      , [(H.hCacheControl, "max-age=60")]
      , payload
      )
    , ( S.status200
      , [(H.hCacheControl, "max-age=60")]
      , payload
      )
    ]
  storeA <- newInMemoryStore 16
  storeB <- newInMemoryStore 16
  let cacheA = newCacheWith storeA defaultCacheConfig
      cacheB = newCacheWith storeB defaultCacheConfig
  _ <- makeRequest "/iso" >>= sendRaw (withCache cacheA t) >>= drainBody
  _ <- makeRequest "/iso" >>= sendRaw (withCache cacheB t) >>= drainBody
  n <- readIORef calls
  assertEqual "each cache misses independently" 2 n

unit_in_memory_eviction :: TestTree
unit_in_memory_eviction = testCase "in-memory store honours the entry cap" $ do
  let body = "x"
  (t, _, _) <- mkTransport
    [ (S.status200, [(H.hCacheControl, "max-age=60")], body)
    | _ <- [(1 :: Int) .. 6]
    ]
  let cfg = defaultCacheConfig { ccMaxEntries = 3 }
  cache <- newCache cfg
  let withC = sendRaw (withCache cache t)
  -- Insert 5 entries into a 3-slot cache.
  mapM_ (\i -> makeRequest (BS8.pack ("/" <> show i)) >>= withC >>= drainBody)
        [(1 :: Int) .. 5]
  size <- cacheSize cache
  assertBool ("size " <> show size <> " > cap") (size <= 3)

-- | Verify that 'clearCache' on the Cache wrapper actually calls
-- the store's 'csClear' (not just the in-memory map).
unit_clear_threads_through :: TestTree
unit_clear_threads_through = testCase "clearCache invokes csClear on the store" $ do
  cleared <- newIORef (0 :: Int)
  inner   <- newInMemoryStore 16
  let store = CacheStore
        { csLookup = csLookup inner
        , csInsert = csInsert inner
        , csDelete = csDelete inner
        , csClear  = atomicModifyIORef' cleared (\n -> (n + 1, ())) >> csClear inner
        , csSize   = csSize inner
        }
      cache = newCacheWith store defaultCacheConfig
  clearCache cache
  clearCache cache
  n <- readIORef cleared
  assertEqual "csClear invocations" 2 n

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Cache"
  [ unit_fresh_hits
  , unit_no_store_response
  , unit_etag_revalidation
  , unit_custom_store_is_used
  , unit_custom_store_isolated
  , unit_in_memory_eviction
  , unit_clear_threads_through
  ]

