{- | A private (per-client) RFC 9111 HTTP cache.

This is the cache surface that the audit called out as missing
(§1.1). It implements the subset of RFC 9111 that applies to a
non-shared user-agent cache:

* Storage of GET \/ HEAD responses keyed by URI (scheme, host,
  port, path+query).
* Freshness from @Cache-Control: max-age@ then @Expires@.
* Conditional revalidation via @ETag@ → @If-None-Match@ and
  @Last-Modified@ → @If-Modified-Since@; on @304 Not Modified@
  the stored body is replayed.
* Honors @no-store@ on either the request or the response.
* Honors @no-cache@ (forces revalidation) and @must-revalidate@
  (refuses to serve stale).
* Honors @immutable@ (treats the response as never stale, even
  past @max-age@).
* Adds an @Age@ header to cache hits per RFC 9110 §5.6.2.

== Scope: private cache only

Shared-cache directives (@public@, @s-maxage@, @proxy-revalidate@)
are parsed and stored but not consulted; this module is designed
for the per-client use case. @Vary@-keyed secondary cache keys
are also not implemented yet — multi-variant responses fall
through to a single cache slot. Both are tracked as follow-ups.

== Pluggable storage

The cache decouples its decision logic (what's cacheable, when to
revalidate, how to apply Cache-Control) from its persistence
('CacheStore'). 'CacheStore' is a record-of-functions over
'CacheKey' and 'CacheEntry':

@
data 'CacheStore' = 'CacheStore'
  { 'csLookup' :: 'CacheKey' -> IO (Maybe 'CacheEntry')
  , 'csInsert' :: 'CacheKey' -> 'CacheEntry' -> IO ()
  , 'csDelete' :: 'CacheKey' -> IO ()
  , 'csClear'  :: IO ()
  , 'csSize'   :: IO Int
  }
@

The shipped 'newInMemoryStore' is a 'TVar'-backed 'HashMap' with
soft entry-count eviction. Custom backends (Redis, on-disk,
SQLite, …) just supply their own 'CacheStore'. Use
'newCacheWith' to plug one in; 'newCache' is the convenience
wrapper that allocates the in-memory store.

== Usage

@
cache <- 'newCache' 'defaultCacheConfig'
'withClient' 'defaultClientConfig'
  { 'ccExtra' = ['withCache' cache] } $ \\transport -> ...
@

The cache middleware is composed /below/ the auth and tracing
middlewares so that cache hits skip token-injection latency and
tracing sees cache-vs-network status. Compose /above/ the
decompression middleware so the cache stores already-decoded
bodies (smaller eviction sizes; faster replay).
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Client.Cache
  ( -- * Cache
    Cache
  , CacheConfig (..)
  , defaultCacheConfig
  , newCache
  , newCacheWith
  , clearCache
    -- * Storage backend
  , CacheStore (..)
  , newInMemoryStore
    -- * Cache primitives (for custom stores)
  , CacheKey (..)
  , CacheEntry (..)
    -- * Middleware
  , withCache
    -- * Inspection
  , cacheSize
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock
  ( NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime
  )

import qualified Network.HTTP.Headers.CacheControl as HCC
import qualified Network.HTTP.HttpDate              as HD

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import qualified Network.HTTP.Client.BodyStream as BSm
import qualified Network.HTTP.Client.Request    as WReq
import qualified Network.HTTP.Client.Response   as Resp
import           Network.HTTP.Client.Response   (RawResponse (..))
import           Network.HTTP.Client.Protocol   (ProtocolInfo (..))
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI        as WURI

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data CacheConfig = CacheConfig
  { ccMaxEntries :: !Int
    -- ^ Soft cap on the number of stored entries, honoured by
    --   'newInMemoryStore'. Custom 'CacheStore' backends decide
    --   their own eviction policy and may ignore this. Default
    --   1024.
  , ccMaxBodyBytes :: !Int
    -- ^ Maximum size of a response body the cache will store, in
    --   bytes. Larger responses pass through untouched. Default
    --   1 MiB.
  }
  deriving stock (Eq, Show)

defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { ccMaxEntries   = 1024
  , ccMaxBodyBytes = 1024 * 1024
  }

-- ---------------------------------------------------------------------------
-- Primitives (exposed so custom CacheStores can serialise them)
-- ---------------------------------------------------------------------------

-- | The cache key. Currently the primary key only — no
-- 'Vary'-derived secondary keys yet (RFC 9111 §4.1 second-step).
data CacheKey = CacheKey
  { ckScheme :: !WURI.Scheme
  , ckHost   :: !ByteString
  , ckPort   :: !Int
  , ckMethod :: !M.Method
  , ckPath   :: !ByteString
  }
  deriving stock (Eq, Show)

instance Hashable CacheKey where
  hashWithSalt s k =
    hashWithSalt s
      ( case ckScheme k of WURI.SchemeHttp -> (0 :: Int); WURI.SchemeHttps -> 1
      , ckHost k
      , ckPort k
      , M.fromMethod (ckMethod k)
      , ckPath k
      )

-- | One stored response plus the metadata needed to make freshness
-- and revalidation decisions later.
data CacheEntry = CacheEntry
  { ceStatus       :: !S.Status
  , ceHeaders      :: ![(H.HeaderName, H.HeaderValue)]
  , ceBody         :: !ByteString
  , ceRequestTime  :: !UTCTime
  , ceResponseTime :: !UTCTime
    -- ^ For the @Age@ calculation in RFC 9111 §4.2.3.
  , ceMaxAge       :: !(Maybe NominalDiffTime)
  , ceExpiresAt    :: !(Maybe UTCTime)
  , ceImmutable    :: !Bool
  , ceMustRevalidate :: !Bool
  , ceNoCache      :: !Bool
    -- ^ Response was tagged @no-cache@: stored but forces
    --   revalidation on every reuse.
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Pluggable storage backend
-- ---------------------------------------------------------------------------

-- | A record-of-functions abstraction over the cache's
-- persistence layer.  Bundled implementations:
--
-- * 'newInMemoryStore' — 'TVar' + 'Data.HashMap.Strict.HashMap'
--   with a soft entry-count cap.
--
-- Custom backends (Redis, disk, SQLite, …) implement their own
-- 'CacheStore' and pass it to 'newCacheWith'. The 'IO' actions
-- must be safe to call concurrently from multiple threads —
-- this module makes no internal locking attempt.
data CacheStore = CacheStore
  { csLookup :: !(CacheKey -> IO (Maybe CacheEntry))
    -- ^ Return the entry for @key@, or 'Nothing' if absent.
  , csInsert :: !(CacheKey -> CacheEntry -> IO ())
    -- ^ Store the entry, replacing any existing one for @key@.
    --   The store is responsible for any eviction needed to
    --   stay within its configured limits.
  , csDelete :: !(CacheKey -> IO ())
    -- ^ Remove @key@ if present. No-op when absent.
  , csClear  :: !(IO ())
    -- ^ Drop all stored entries.
  , csSize   :: !(IO Int)
    -- ^ Current entry count. Used by 'cacheSize' for
    --   diagnostics; cheap if possible but not required to be
    --   constant-time.
  }

-- | Allocate the bundled 'TVar' \/ 'HashMap' store with a soft
-- cap on the entry count. When the cap is exceeded, the
-- 'csInsert' implementation drops enough entries to bring it
-- back to the limit; the dropped set is unspecified (effectively
-- 'HashMap' iteration order), which is acceptable for a
-- best-effort fast-path cache.
newInMemoryStore :: Int -> IO CacheStore
newInMemoryStore cap = do
  ref <- newTVarIO HM.empty
  pure CacheStore
    { csLookup = \k -> HM.lookup k <$> readTVarIO ref
    , csInsert = \k v -> atomically $ modifyTVar' ref $ \m ->
        let m1 = HM.insert k v m
        in if HM.size m1 > cap
             -- Drop down to the cap. The eviction is
             -- HashMap-iteration-order, intentionally
             -- unspecified — a tighter LRU/LFU policy is the
             -- next refinement once a workload that needs it
             -- shows up.
             then HM.fromList (drop (HM.size m1 - cap) (HM.toList m1))
             else m1
    , csDelete = \k -> atomically $ modifyTVar' ref (HM.delete k)
    , csClear  = atomically $ writeTVar ref HM.empty
    , csSize   = HM.size <$> readTVarIO ref
    }

-- ---------------------------------------------------------------------------
-- Cache
-- ---------------------------------------------------------------------------

data Cache = Cache
  { cacheStore  :: !CacheStore
  , cacheConfig :: !CacheConfig
  }

-- | Allocate a 'Cache' backed by the bundled in-memory store
-- sized via 'ccMaxEntries'.
newCache :: CacheConfig -> IO Cache
newCache cfg = do
  store <- newInMemoryStore (ccMaxEntries cfg)
  pure (newCacheWith store cfg)

-- | Build a 'Cache' from a caller-supplied 'CacheStore'. The
-- 'CacheConfig' still controls cache-level policy
-- ('ccMaxBodyBytes' bounds the body size accepted on store);
-- 'ccMaxEntries' is up to the backend to honour (the bundled
-- 'newInMemoryStore' does, third-party stores may not).
newCacheWith :: CacheStore -> CacheConfig -> Cache
newCacheWith store cfg = Cache { cacheStore = store, cacheConfig = cfg }

clearCache :: Cache -> IO ()
clearCache cache = csClear (cacheStore cache)

cacheSize :: Cache -> IO Int
cacheSize cache = csSize (cacheStore cache)

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Cache middleware. On each request:
--
-- 1. Build a cache key from the request URI + method.
-- 2. If a cached entry exists and is fresh, return it (with an
--    @Age@ header).
-- 3. Else if a cached entry exists with a validator, attach
--    conditional headers and call the inner transport; on 304
--    replay the cached body.
-- 4. Else, call the inner transport and store the response if it
--    looks cacheable.
withCache :: Cache -> Middleware IO
withCache cache inner = Transport $ \req -> do
  mEntry <- lookupEntry cache req
  now    <- getCurrentTime
  case (M.methodToBytes (WReq.method req), mEntry) of
    -- Non-cacheable method: fall through.
    _ | not (cacheableMethod (WReq.method req)) -> sendRaw inner req
    _ | requestHasNoStore req -> sendRaw inner req
    (_, Just (key, entry))
      | entryFresh now entry && not (ceNoCache entry)
      , not (requestHasNoCache req)
        -> replayHit now entry
      | otherwise -> revalidateOrReplace cache inner req key entry
    (_, Nothing) -> fetchAndMaybeStore cache inner req

-- ---------------------------------------------------------------------------
-- Hit / miss paths
-- ---------------------------------------------------------------------------

-- | Stamp an 'Age' header and rebuild the popper for a cache hit.
replayHit :: UTCTime -> CacheEntry -> IO RawResponse
replayHit now entry = do
  let ageSec :: Int
      ageSec = max 0 (truncate (diffUTCTime now (ceResponseTime entry)))
      hdrs  =
        ( H.hAge
        , BS8.pack (show ageSec)
        )
        : filter ((/= H.hAge) . fst) (ceHeaders entry)
  popper <- BSm.popperFromStrict (ceBody entry)
  pure RawResponse
    { Resp.statusCode    = ceStatus entry
    , Resp.headers       = hdrs
    , Resp.bodyPopper    = popper
    , Resp.protocolInfo  = HTTP1_1
    }

-- | Cached entry exists but is stale or marked no-cache. Issue a
-- conditional request; on 304 replay the cached body with the new
-- response's headers merged on top.
revalidateOrReplace
  :: Cache
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> CacheKey
  -> CacheEntry
  -> IO RawResponse
revalidateOrReplace cache inner req key entry = do
  let withValidators = attachValidators entry req
  raw <- sendRaw inner withValidators
  now <- getCurrentTime
  case S.statusCode (Resp.statusCode raw) of
    304 -> do
      -- Replay the cached body but adopt the new headers (the
      -- 304 carries refreshed validators and Cache-Control).
      let mergedHeaders = mergeHeadersOn304 (ceHeaders entry) (Resp.headers raw)
          freshened     = freshenEntry now entry { ceHeaders = mergedHeaders }
      csInsert (cacheStore cache) key freshened
      -- Drain the 304's (empty) body so the connection can advance.
      BSm.drainPopper (Resp.bodyPopper raw)
      replayHit now freshened
    _ ->
      -- 200 / other: replace the cache entry from the fresh response
      -- if it looks storable.
      maybeStoreAndReturn cache key raw now

-- | No cached entry yet: forward to inner, then store if storable.
fetchAndMaybeStore
  :: Cache
  -> Transport IO
  -> WReq.Request BSm.BodyStream
  -> IO RawResponse
fetchAndMaybeStore cache inner req = do
  raw <- sendRaw inner req
  now <- getCurrentTime
  case keyOfRequest req of
    Just key -> maybeStoreAndReturn cache key raw now
    Nothing  -> pure raw

-- | Either store the response and pass it through, or pass it
-- through unchanged when it's not storable.
maybeStoreAndReturn
  :: Cache
  -> CacheKey
  -> RawResponse
  -> UTCTime
  -> IO RawResponse
maybeStoreAndReturn cache key raw now = do
  let canStore = responseIsStorable raw
                   && not (responseHasNoStore raw)
  if not canStore
    then pure raw
    else do
      bodyBs <- BSm.popperBytes (Resp.bodyPopper raw)
      if BS.length bodyBs > ccMaxBodyBytes (cacheConfig cache)
        then do
          newPopper <- BSm.popperFromStrict bodyBs
          pure raw { Resp.bodyPopper = newPopper }
        else do
          let entry = buildEntry now raw bodyBs
          csInsert (cacheStore cache) key entry
          newPopper <- BSm.popperFromStrict bodyBs
          pure raw { Resp.bodyPopper = newPopper }

-- ---------------------------------------------------------------------------
-- Storage helpers
-- ---------------------------------------------------------------------------

lookupEntry
  :: Cache
  -> WReq.Request BSm.BodyStream
  -> IO (Maybe (CacheKey, CacheEntry))
lookupEntry cache req = case keyOfRequest req of
  Nothing -> pure Nothing
  Just k  -> fmap (k,) <$> csLookup (cacheStore cache) k

keyOfRequest :: WReq.Request BSm.BodyStream -> Maybe CacheKey
keyOfRequest req = case WURI.renderRequestURI (WReq.requestURI req) of
  Left _  -> Nothing
  Right u -> Just CacheKey
    { ckScheme = WURI.uriScheme u
    , ckHost   = WURI.uriHost u
    , ckPort   = WURI.uriPort u
    , ckMethod = WReq.method req
    , ckPath   = WURI.uriPathAndQuery u
    }

buildEntry :: UTCTime -> RawResponse -> ByteString -> CacheEntry
buildEntry now raw bodyBs =
  let directives = parseCacheControl (Resp.headers raw)
      maxAge =
        case [s | HCC.MaxAge s <- directives] of
          (s : _) -> Just (fromIntegral s :: NominalDiffTime)
          []      -> Nothing
      expires =
        case lookup H.hExpires (Resp.headers raw) of
          Just bs -> HD.parseHttpDateMaybe bs
          Nothing -> Nothing
      immutable      = HCC.Immutable `elem` directives
      mustRevalidate = HCC.MustRevalidate `elem` directives
      noCacheTag     = any isNoCache directives
  in CacheEntry
       { ceStatus         = Resp.statusCode raw
       , ceHeaders        = Resp.headers raw
       , ceBody           = bodyBs
       , ceRequestTime    = now
       , ceResponseTime   = now
       , ceMaxAge         = maxAge
       , ceExpiresAt      = expires
       , ceImmutable      = immutable
       , ceMustRevalidate = mustRevalidate
       , ceNoCache        = noCacheTag
       }
  where
    isNoCache (HCC.NoCache _) = True
    isNoCache _               = False

freshenEntry :: UTCTime -> CacheEntry -> CacheEntry
freshenEntry now e =
  -- 304 means "these validators are still good" — bump
  -- response time so freshness computations restart now.
  e { ceResponseTime = now, ceRequestTime = now }

-- ---------------------------------------------------------------------------
-- Freshness
-- ---------------------------------------------------------------------------

entryFresh :: UTCTime -> CacheEntry -> Bool
entryFresh now entry
  | ceImmutable entry = True
  | otherwise = case explicitFreshness entry of
      Just lifetime ->
        diffUTCTime now (ceResponseTime entry) < lifetime
          && not (ceMustRevalidate entry && diffUTCTime now (ceResponseTime entry) >= lifetime)
      Nothing -> False

-- | The freshness lifetime from explicit signals only. We
-- deliberately do not implement RFC 9111 §4.2.2 heuristic freshness
-- yet; the audit asks for a minimal cache and heuristic freshness
-- is the next item to bolt on.
explicitFreshness :: CacheEntry -> Maybe NominalDiffTime
explicitFreshness entry = case ceMaxAge entry of
  Just m  -> Just m
  Nothing -> case ceExpiresAt entry of
    Just t  -> Just (diffUTCTime t (ceResponseTime entry))
    Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Header / directive helpers
-- ---------------------------------------------------------------------------

cacheableMethod :: M.Method -> Bool
cacheableMethod m = m == M.mGet || m == M.mHead

requestHasNoStore :: WReq.Request a -> Bool
requestHasNoStore req =
  case parseCacheControl (WReq.headers req) of
    ds -> HCC.NoStore `elem` ds

requestHasNoCache :: WReq.Request a -> Bool
requestHasNoCache req =
  any isNoCache (parseCacheControl (WReq.headers req))
  where
    isNoCache (HCC.NoCache _) = True
    isNoCache _               = False

responseHasNoStore :: RawResponse -> Bool
responseHasNoStore raw =
  HCC.NoStore `elem` parseCacheControl (Resp.headers raw)

-- | RFC 9111 §3: storable by default for 200, 203, 204, 300, 301,
-- 308, 404, 405, 410, 414, 501. We're conservative and only
-- store the codes that the audit list cares about (200, 203, 301,
-- 410) — the others rarely benefit a non-shared cache.
responseIsStorable :: RawResponse -> Bool
responseIsStorable raw =
  S.statusCode (Resp.statusCode raw) `elem` [200, 203, 301, 410]

parseCacheControl :: [(H.HeaderName, H.HeaderValue)] -> [HCC.CacheControlDirective]
parseCacheControl hdrs =
  concatMap parseOne (H.lookupHeaders H.hCacheControl hdrs)
  where
    parseOne bs = case HCC.parseCacheControlHeader bs of
      Right ds -> NE.toList ds
      Left _   -> []

attachValidators
  :: CacheEntry
  -> WReq.Request BSm.BodyStream
  -> WReq.Request BSm.BodyStream
attachValidators entry req =
  let hdrs0 = WReq.headers req
      mEtag = lookup H.hETag         (ceHeaders entry)
      mLm   = lookup H.hLastModified (ceHeaders entry)
      hdrs1 = case mEtag of
        Just t  -> H.insertHeader H.hIfNoneMatch     t hdrs0
        Nothing -> hdrs0
      hdrs2 = case mLm of
        Just l  -> H.insertHeader H.hIfModifiedSince l hdrs1
        Nothing -> hdrs1
  in req { WReq.headers = hdrs2 }

-- | RFC 9111 §3.2: when a 304 comes back, the cached entry is
-- refreshed by replacing the stored headers' values with those
-- in the 304 for any name that the 304 carries. Other cached
-- headers are kept.
mergeHeadersOn304
  :: [(H.HeaderName, H.HeaderValue)]
  -> [(H.HeaderName, H.HeaderValue)]
  -> [(H.HeaderName, H.HeaderValue)]
mergeHeadersOn304 cached fresh =
  let freshNames = map fst fresh
      kept = filter (\(n, _) -> n `notElem` freshNames) cached
  in kept <> fresh
