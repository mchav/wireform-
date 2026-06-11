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
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Cache
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

{- | A canned response factory: each call to 'mkTransport' takes a
list of @(status, headers, body)@ tuples and returns a
'Transport' that returns them in order, recording every call
into a counter.
-}
mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef Int, IORef [Request BSm.BodyStream])
mkTransport canned = do
  responsesRef <- newIORef canned
  callsRef <- newIORef (0 :: Int)
  requestsRef <- newIORef ([] :: [Request BSm.BodyStream])
  let go = Transport $ \req -> do
        atomicModifyIORef' callsRef (\n -> (n + 1, ()))
        atomicModifyIORef' requestsRef (\xs -> (req : xs, ()))
        next <- atomicModifyIORef' responsesRef $ \rs -> case rs of
          (r : rest) -> (rest, r)
          [] -> ([], (S.status500, [], "no more canned responses"))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (go, callsRef, requestsRef)


makeRequest :: ByteString -> IO (Request BSm.BodyStream)
makeRequest pathBs =
  case WURI.parseTemplate (BS8.unpack ("http://example.com" <> pathBs)) of
    Left err -> error ("makeRequest: bad URI: " <> show err)
    Right t -> prepareRequest [] (get t)


drainBody :: RawResponse -> IO ByteString
drainBody = BSm.popperBytes . Resp.bodyPopper


-- ---------------------------------------------------------------------------
-- Hit + miss accounting
-- ---------------------------------------------------------------------------

unit_fresh_hits :: Spec
unit_fresh_hits = it "fresh response satisfies subsequent reads" $ do
  let payload = "hello"
  (t, calls, _) <-
    mkTransport
      [
        ( S.status200
        ,
          [ (H.hCacheControl, "max-age=60")
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
  n `shouldBe` 1
  body1 `shouldBe` payload
  body2 `shouldBe` payload
  size <- cacheSize cache
  size `shouldBe` 1


unit_no_store_response :: Spec
unit_no_store_response = it "no-store on response is not cached" $ do
  let payload = "secret"
  (t, calls, _) <-
    mkTransport
      [ (S.status200, [(H.hCacheControl, "no-store, max-age=60")], payload)
      , (S.status200, [(H.hCacheControl, "no-store, max-age=60")], payload)
      ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/x" >>= withC >>= drainBody
  _ <- makeRequest "/x" >>= withC >>= drainBody
  n <- readIORef calls
  n `shouldBe` 2
  size <- cacheSize cache
  size `shouldBe` 0


unit_etag_revalidation :: Spec
unit_etag_revalidation = it "stale entry revalidates with ETag and replays on 304" $ do
  let payload = "etag-body"
      etag = "\"v1\""
  (t, calls, reqsRef) <-
    mkTransport
      [
        ( S.status200
        ,
          [ (H.hCacheControl, "max-age=0")
          , (H.hETag, etag)
          ]
        , payload
        )
      ,
        ( S.status304
        , [(H.hETag, etag)]
        , ""
        )
      ]
  cache <- newCache defaultCacheConfig
  let withC = sendRaw (withCache cache t)
  _ <- makeRequest "/e" >>= withC >>= drainBody
  r2 <- makeRequest "/e" >>= withC
  body2 <- drainBody r2
  n <- readIORef calls
  n `shouldBe` 2
  -- The replayed response is the cached body, not the 304's empty body.
  body2 `shouldBe` payload
  -- The second request should have carried If-None-Match.
  reqs <- readIORef reqsRef
  let recent = head reqs -- newest first
      hadInm = case lookup H.hIfNoneMatch (WReq.headers recent) of
        Just v -> v == etag
        Nothing -> False
  (hadInm) `shouldBe` True


-- ---------------------------------------------------------------------------
-- Pluggable store
-- ---------------------------------------------------------------------------

{- | A custom 'CacheStore' that records every operation it sees.
Used to verify the cache routes its writes / reads through
'CacheStore' instead of touching internal state.
-}
mkAuditedStore :: IO (CacheStore, IORef [String])
mkAuditedStore = do
  inner <- newInMemoryStore 64
  audit <- newIORef ([] :: [String])
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


unit_custom_store_is_used :: Spec
unit_custom_store_is_used = it "withCache routes through a custom CacheStore" $ do
  let payload = "audit-body"
  (t, _, _) <-
    mkTransport
      [
        ( S.status200
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
  (length summary) `shouldBe` 3
  ("lookup" `isPrefix` head summary) `shouldBe` True
  ("insert" `isPrefix` (summary !! 1)) `shouldBe` True
  ("lookup" `isPrefix` (summary !! 2)) `shouldBe` True
  where
    isPrefix p s = take (length p) s == p


unit_custom_store_isolated :: Spec
unit_custom_store_isolated = it "two caches with separate stores don't share state" $ do
  let payload = "isolation-body"
  (t, calls, _) <-
    mkTransport
      [
        ( S.status200
        , [(H.hCacheControl, "max-age=60")]
        , payload
        )
      ,
        ( S.status200
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
  n `shouldBe` 2


unit_in_memory_eviction :: Spec
unit_in_memory_eviction = it "in-memory store honours the entry cap" $ do
  let body = "x"
  (t, _, _) <-
    mkTransport
      [ (S.status200, [(H.hCacheControl, "max-age=60")], body)
      | _ <- [(1 :: Int) .. 6]
      ]
  let cfg = defaultCacheConfig {ccMaxEntries = 3}
  cache <- newCache cfg
  let withC = sendRaw (withCache cache t)
  -- Insert 5 entries into a 3-slot cache.
  mapM_
    (\i -> makeRequest (BS8.pack ("/" <> show i)) >>= withC >>= drainBody)
    [(1 :: Int) .. 5]
  size <- cacheSize cache
  (if (size <= 3) then pure () else expectationFailure ("size " <> show size <> " > cap"))


{- | Verify that 'clearCache' on the Cache wrapper actually calls
the store's 'csClear' (not just the in-memory map).
-}
unit_clear_threads_through :: Spec
unit_clear_threads_through = it "clearCache invokes csClear on the store" $ do
  cleared <- newIORef (0 :: Int)
  inner <- newInMemoryStore 16
  let store =
        CacheStore
          { csLookup = csLookup inner
          , csInsert = csInsert inner
          , csDelete = csDelete inner
          , csClear = atomicModifyIORef' cleared (\n -> (n + 1, ())) >> csClear inner
          , csSize = csSize inner
          }
      cache = newCacheWith store defaultCacheConfig
  clearCache cache
  clearCache cache
  n <- readIORef cleared
  n `shouldBe` 2


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Cache" $
    sequence_
      [ unit_fresh_hits
      , unit_no_store_response
      , unit_etag_revalidation
      , unit_custom_store_is_used
      , unit_custom_store_isolated
      , unit_in_memory_eviction
      , unit_clear_threads_through
      ]
