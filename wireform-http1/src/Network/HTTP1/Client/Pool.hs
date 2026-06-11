{- | Trivial per-host connection pool.

The pool is keyed on @(host, port)@ and holds idle connections in a
bounded STM queue. A client checks one out, runs a request, checks it
back in if the keep-alive state survived. Failed connections (parse
error or @Connection: close@ response) are dropped on the floor.

This is intentionally simple — the surface matches the @http-client@
manager's pool shape without adopting its TLS / SOCKS / cookie jar
machinery, which belongs in a separate layer.
-}
module Network.HTTP1.Client.Pool (
  Pool,
  PoolConfig (..),
  defaultPoolConfig,
  newPool,
  withPooledRequest,
  destroyPool,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Map.Strict qualified as Map
import Network.HTTP1.Client
import Network.HTTP1.Headers
import Network.HTTP1.Parser
import Network.HTTP1.Types


data PoolConfig = PoolConfig
  { poolPerHostIdle :: !Int
  -- ^ Maximum idle connections kept per @(host, port)@ key.
  }


defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig {poolPerHostIdle = 8}


type Key = (String, String)


data Pool = Pool
  { poolCfg :: !PoolConfig
  , poolMap :: !(TVar (Map.Map Key (TVar [ClientConnection])))
  }


newPool :: PoolConfig -> IO Pool
newPool cfg = do
  m <- newTVarIO Map.empty
  pure Pool {poolCfg = cfg, poolMap = m}


-- | Drop every cached connection. Safe to call multiple times.
destroyPool :: Pool -> IO ()
destroyPool pool = do
  conns <- atomically $ do
    m <- readTVar (poolMap pool)
    writeTVar (poolMap pool) Map.empty
    qs <- traverse readTVar (Map.elems m)
    pure (concat qs)
  mapM_ closeClientConnection conns


{- | Borrow a connection from the pool (or open a new one), run the
request, then return the connection if it remained usable. The
response's body MUST be fully consumed inside the callback because
we can't reuse the connection until the body is drained.
-}
withPooledRequest
  :: Pool
  -> ClientConfig
  -> Request
  -> ((Either ParseError Response) -> IO a)
  -> IO a
withPooledRequest pool cfg req action = do
  conn <- checkOut pool cfg
  result <- try @SomeException $ sendRequestOn conn req
  case result of
    Left _e -> do
      closeClientConnection conn
      action (Left ParseUnexpectedEof)
    Right resp -> do
      a <- action resp
      case resp of
        Right r | reusable r -> checkIn pool cfg conn
        _ -> closeClientConnection conn
      pure a
  where
    reusable r = case findConnection (responseHeaders r) of
      Just v | any (== ConnClose) (parseConnection v) -> False
      _ -> True


checkOut :: Pool -> ClientConfig -> IO ClientConnection
checkOut pool cfg = do
  mc <- atomically $ do
    m <- readTVar (poolMap pool)
    case Map.lookup (clientHost cfg, clientPort cfg) m of
      Nothing -> pure Nothing
      Just qVar -> do
        xs <- readTVar qVar
        case xs of
          [] -> pure Nothing
          (c : rest) -> do
            writeTVar qVar rest
            pure (Just c)
  case mc of
    Just c -> pure c
    Nothing -> openClientConnection cfg


checkIn :: Pool -> ClientConfig -> ClientConnection -> IO ()
checkIn pool cfg conn = do
  closed <- atomically $ do
    m <- readTVar (poolMap pool)
    let k = (clientHost cfg, clientPort cfg)
    qVar <- case Map.lookup k m of
      Just v -> pure v
      Nothing -> do
        v <- newTVar []
        writeTVar (poolMap pool) (Map.insert k v m)
        pure v
    xs <- readTVar qVar
    if length xs >= poolPerHostIdle (poolCfg pool)
      then pure True
      else do
        writeTVar qVar (conn : xs)
        pure False
  if closed
    then closeClientConnection conn
    else pure ()
