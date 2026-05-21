{- | Connection pool for the base transport.

A 'ConnectionPool' is a per-target ((scheme, host, port)) cache of
open 'Network.HTTP.Connection.Connection' handles. Acquiring a
connection prefers a still-warm idle one; if none is available the
pool opens a fresh one (subject to 'maxConnectionsPerHost').
Releasing a connection puts it back on the idle list, and a
background reaper closes connections that have been idle for
longer than 'maxIdleSeconds'.

== Scope of real reuse

The wireform-http1 low-level API exposes 'openClientConnection' \/
'closeClientConnection' out of bracket scope, so plaintext
HTTP\/1.x connections genuinely get reused across requests.

TLS connections and HTTP\/2 connections still go through the
bracketed 'Network.HTTP.Connection.withConnection' (the low-level
HTTP\/2 client doesn't expose open\/close — it spawns a recv-loop
thread that's tied to the bracket). For those targets the pool
falls back to a fresh connection per request. The API surface
doesn't change; only the wins do.

When wireform-http2's TLS \/ multiplexing surface grows
non-bracketed open\/close hooks, the pool will automatically pick
those up.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Client.Pool
  ( -- * Configuration
    PoolConfig (..)
  , defaultPoolConfig
    -- * The pool
  , ConnectionPool
  , newPool
  , closePool
  , withPool
    -- * Transport
  , pooledTransport
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, mask, throwIO, try)
import Control.Monad (forever, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Hashable (Hashable (..))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)

import qualified Network.HTTP.Connection    as Conn
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as LB
import qualified Network.HTTP.Types.Version as LV
import qualified Network.HTTP.VersionRange  as VR

import           Network.HTTP.Client.BodyStream
                  (BodyStream, bodyStreamBytes, popperFromStrict)
import           Network.HTTP.Client.Protocol
import qualified Network.HTTP.Client.Request   as WReq
import           Network.HTTP.Client.Response
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI       as WURI

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data PoolConfig = PoolConfig
  { maxConnectionsPerHost :: !Int
    -- ^ Cap on simultaneously open connections per
    --   @(scheme, host, port)@. Requests that exceed the cap block
    --   until a slot frees up.
  , maxIdleSeconds        :: !Double
    -- ^ Idle connections older than this are closed by the reaper.
  , reaperIntervalSeconds :: !Double
    -- ^ Reaper sweep cadence.
  , versionRange          :: !VR.VersionRange
    -- ^ Negotiated version range for newly opened connections.
  }

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
  { maxConnectionsPerHost = 8
  , maxIdleSeconds        = 60
  , reaperIntervalSeconds = 5
  , versionRange          = VR.preferHttp1
  }

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

data Target = Target
  { tgtScheme :: !TargetScheme
  , tgtHost   :: !ByteString
  , tgtPort   :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

data TargetScheme = TargetHttp | TargetHttps
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | One cached connection plus its last-used timestamp.
data Slot = Slot
  { slotConn   :: !Conn.Connection
  , slotLastUsed :: !UTCTime
  }

data PerTarget = PerTarget
  { ptIdle  :: ![Slot]     -- available connections (LIFO so warmest first)
  , ptInUse :: !Int        -- count of checked-out connections
  }

emptyPerTarget :: PerTarget
emptyPerTarget = PerTarget [] 0

data PoolState = PoolState
  { psPerTarget :: !(HashMap Target PerTarget)
  , psShutdown  :: !Bool
  }

data ConnectionPool = ConnectionPool
  { poolState   :: !(TVar PoolState)
  , poolConfig  :: !PoolConfig
  , poolReaper  :: !ThreadId
  }

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

newPool :: PoolConfig -> IO ConnectionPool
newPool cfg = do
  stateVar <- newTVarIO PoolState
    { psPerTarget = HM.empty
    , psShutdown  = False
    }
  rtid <- forkIO (reaperLoop cfg stateVar)
  pure ConnectionPool
    { poolState  = stateVar
    , poolConfig = cfg
    , poolReaper = rtid
    }

closePool :: ConnectionPool -> IO ()
closePool pool = do
  killThread (poolReaper pool)
  victims <- atomically $ do
    s <- readTVar (poolState pool)
    writeTVar (poolState pool) s { psShutdown = True, psPerTarget = HM.empty }
    pure (concatMap (map slotConn . ptIdle) (HM.elems (psPerTarget s)))
  mapM_ (\c -> try @SomeException (Conn.closeConnection c)) victims

withPool :: PoolConfig -> (ConnectionPool -> IO a) -> IO a
withPool cfg = bracket (newPool cfg) closePool

-- ---------------------------------------------------------------------------
-- Reaper
-- ---------------------------------------------------------------------------

reaperLoop :: PoolConfig -> TVar PoolState -> IO ()
reaperLoop cfg stateVar = forever $ do
  let micros = max 1_000_000 (round (reaperIntervalSeconds cfg * 1_000_000))
  threadDelay micros
  now <- getCurrentTime
  victims <- atomically $ do
    s <- readTVar stateVar
    if psShutdown s
      then pure []
      else do
        let limit = realToFrac (maxIdleSeconds cfg) :: NominalDiffTime
            sweep pt =
              let (live, dead) = List.partition
                    (\sl -> diffUTCTime now (slotLastUsed sl) < limit)
                    (ptIdle pt)
              in (pt { ptIdle = live }, dead)
            (newMap, allDead) = HM.foldrWithKey step (HM.empty, []) (psPerTarget s)
            step k pt (acc, deadAcc) =
              let (pt', dead) = sweep pt
                  acc' = if null (ptIdle pt') && ptInUse pt' == 0
                            then acc
                            else HM.insert k pt' acc
              in (acc', dead <> deadAcc)
        writeTVar stateVar s { psPerTarget = newMap }
        pure allDead
  mapM_ (\sl -> try @SomeException (Conn.closeConnection (slotConn sl))) victims

-- ---------------------------------------------------------------------------
-- Acquire / release
-- ---------------------------------------------------------------------------

-- | Try to grab an idle connection. If none is available and we're
-- below the per-host cap, return 'NeedNew'; otherwise 'STM.retry'.
data Acquire = ReuseIdle !Slot | NeedNew

acquireSTM :: ConnectionPool -> Target -> STM Acquire
acquireSTM pool target = do
  s <- readTVar (poolState pool)
  when (psShutdown s) (throwSTM PoolClosed)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
  case ptIdle pt of
    (slot : rest) -> do
      let pt' = pt { ptIdle = rest, ptInUse = ptInUse pt + 1 }
      writeTVar (poolState pool)
        s { psPerTarget = HM.insert target pt' (psPerTarget s) }
      pure (ReuseIdle slot)
    [] ->
      if ptInUse pt < maxConnectionsPerHost (poolConfig pool)
        then do
          let pt' = pt { ptInUse = ptInUse pt + 1 }
          writeTVar (poolState pool)
            s { psPerTarget = HM.insert target pt' (psPerTarget s) }
          pure NeedNew
        else retry

releaseIdle :: ConnectionPool -> Target -> Slot -> IO ()
releaseIdle pool target slot = atomically $ do
  s <- readTVar (poolState pool)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
      pt' = pt
        { ptIdle  = slot : ptIdle pt
        , ptInUse = max 0 (ptInUse pt - 1)
        }
  writeTVar (poolState pool)
    s { psPerTarget = HM.insert target pt' (psPerTarget s) }

releaseDead :: ConnectionPool -> Target -> IO ()
releaseDead pool target = atomically $ do
  s <- readTVar (poolState pool)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
      pt' = pt { ptInUse = max 0 (ptInUse pt - 1) }
  writeTVar (poolState pool)
    s { psPerTarget = HM.insert target pt' (psPerTarget s) }

-- ---------------------------------------------------------------------------
-- Transport
-- ---------------------------------------------------------------------------

-- | The pool's 'Transport'. Each request gets a connection from the
-- per-target slot, runs to completion, and the connection is
-- returned to the idle list (or closed, on failure or
-- TLS\/HTTP\/2 where reuse isn't supported yet).
pooledTransport :: ConnectionPool -> Transport IO
pooledTransport pool = Transport $ \req -> do
  target <- targetForRequest req
  let cfg = connectionConfigFor pool target
  if reuseOK cfg
    then runWithSlot pool target cfg req
    else runOneShot cfg req

-- | True iff a connection for this config can be opened \/ closed
-- out of bracket scope (HTTP\/1.x plaintext only, today).
reuseOK :: Conn.ConnectionConfig -> Bool
reuseOK cfg = case Conn.connectionTls cfg of
  Just _  -> False
  Nothing -> case VR.preferredVersion (Conn.connectionVersionRange cfg) of
    LV.HTTP2 -> False
    _        -> True

-- | Reusable path: grab a slot from the pool, send, return.
runWithSlot
  :: ConnectionPool
  -> Target
  -> Conn.ConnectionConfig
  -> WReq.Request BodyStream
  -> IO RawResponse
runWithSlot pool target cfg req = mask $ \restore -> do
  acquired <- atomically (acquireSTM pool target)
  conn <- case acquired of
    ReuseIdle slot -> pure (slotConn slot)
    NeedNew        -> do
      result <- restore (Conn.openConnection cfg)
      case result of
        Right c  -> pure c
        Left err -> do
          releaseDead pool target
          throwIO (PoolInvalidURI err)
  -- From here we own the connection; on success put it back idle,
  -- on failure close + decrement.
  result <- restore (try (sendAndMaterialise conn req))
  case result of
    Right raw -> do
      now <- getCurrentTime
      releaseIdle pool target (Slot conn now)
      pure raw
    Left (e :: SomeException) -> do
      void (try @SomeException (Conn.closeConnection conn))
      releaseDead pool target
      throwIO e

-- | One-shot path for connections we can't reuse yet (TLS, HTTP\/2):
-- bracket a fresh 'Conn.withConnection' for each request.
runOneShot
  :: Conn.ConnectionConfig
  -> WReq.Request BodyStream
  -> IO RawResponse
runOneShot cfg req = Conn.withConnection cfg $ \conn ->
  sendAndMaterialise conn req

sendAndMaterialise
  :: Conn.Connection
  -> WReq.Request BodyStream
  -> IO RawResponse
sendAndMaterialise conn req = do
  lowReq <- toLowLevelRequest req
  resp   <- Conn.sendOn conn lowReq
  drained <- materialise (Msg.responseBody resp)
  popper  <- popperFromStrict drained
  pure RawResponse
    { statusCode    = Msg.responseStatus resp
    , headers       = Msg.responseHeaders resp
    , bodyPopper    = popper
    , protocolInfo  = case Msg.responseVersion resp of
        LV.HTTP2 -> HTTP2 Http2Info { h2StreamId = 0, h2PushPromises = pure [] }
        _        -> HTTP1_1
    }
  where
    materialise = \case
      LB.BodyEmpty    -> pure BS.empty
      LB.BodyBytes b  -> pure b
      LB.BodyStream p -> drainMaybe p
    drainMaybe p = go []
      where
        go acc = p >>= \case
          Nothing -> pure $! BS.concat (reverse acc)
          Just b
            | BS.null b -> go acc
            | otherwise -> go (b : acc)

toLowLevelRequest :: WReq.Request BodyStream -> IO Msg.Request
toLowLevelRequest req = do
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u  -> pure u
    Left err -> throwIO (PoolInvalidURI err)
  bodyBytes <- bodyStreamBytes (WReq.body req)
  let lowScheme = case WURI.uriScheme uri_ of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp  -> Msg.SchemeHttp
      hostBs = WURI.uriHost uri_
      target = WURI.uriPathAndQuery uri_
      authority =
        Just (hostBs <> case (WURI.uriScheme uri_, WURI.uriPort uri_) of
                          (WURI.SchemeHttp,  80)  -> ""
                          (WURI.SchemeHttps, 443) -> ""
                          (_,                p)   -> ":" <> BS8.pack (show p))
  pure Msg.Request
    { Msg.requestMethod    = WReq.method req
    , Msg.requestTarget    = target
    , Msg.requestAuthority = authority
    , Msg.requestScheme    = lowScheme
    , Msg.requestHeaders   = WReq.headers req
    , Msg.requestBody      = if BS.null bodyBytes
                                then LB.BodyEmpty
                                else LB.BodyBytes bodyBytes
    , Msg.requestVersion   = VR.preferredVersion VR.preferHttp1
    , Msg.requestTrailers  = pure []
    }

connectionConfigFor :: ConnectionPool -> Target -> Conn.ConnectionConfig
connectionConfigFor pool tgt =
  let host = BS8.unpack (tgtHost tgt)
      tls  = case tgtScheme tgt of
        TargetHttps -> Just (Conn.defaultTlsConnectionConfig host)
        TargetHttp  -> Nothing
  in Conn.ConnectionConfig
       { Conn.connectionHost         = host
       , Conn.connectionPort         = show (tgtPort tgt)
       , Conn.connectionVersionRange = versionRange (poolConfig pool)
       , Conn.connectionTls          = tls
       }

targetForRequest :: WReq.Request BodyStream -> IO Target
targetForRequest req = case WURI.renderRequestURI (WReq.requestURI req) of
  Left err -> throwIO (PoolInvalidURI err)
  Right u  -> pure Target
    { tgtScheme = case WURI.uriScheme u of
        WURI.SchemeHttps -> TargetHttps
        WURI.SchemeHttp  -> TargetHttp
    , tgtHost = WURI.uriHost u
    , tgtPort = WURI.uriPort u
    }

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data PoolError
  = PoolInvalidURI !String
  | PoolClosed
  deriving stock (Show)

instance Exception PoolError