{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{- | Apples-to-apples benchmark: same WAI apps served over localhost
TCP via wireform-http's HTTP\/1 server (through the WAI adapter) vs
Warp, both hit by their respective HTTP\/1 clients.

Both paths do real TCP: bind, accept, parse HTTP\/1, serialize
response, read on the client side. The only variable is the
server+client implementation.
-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import qualified "http-types" Network.HTTP.Types as WAIHttp
import qualified "http-client" Network.HTTP.Client as HC
import qualified Network.Socket as NS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Criterion.Main

import qualified Network.HTTP.Message as U
import qualified "wireform-http" Network.HTTP.Types.Body as U
import qualified "wireform-http" Network.HTTP.Types.Header as U
import qualified "wireform-http" Network.HTTP.Types.Method as U
import qualified "wireform-http" Network.HTTP.Types.Version as U
import Network.HTTP.Connection
  (ConnectionConfig(..), defaultConnectionConfig, withConnection, sendOn)
import Network.HTTP.Server
  (ServerConfig(..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (http1Only)
import Network.HTTP.WAI (waiToHandler)

main :: IO ()
main = do
  mgr <- HC.newManager HC.defaultManagerSettings
  defaultMain
    [ bgroup "hello (GET → 5 B)"
        [ wireformBench "wireform-http1" helloApp doWfGet
        , warpBench mgr "warp" helloApp doWarpGet
        ]
    , bgroup "echo (POST 1 KiB)"
        [ wireformBench "wireform-http1" echoApp (`doWfPost` payload1k)
        , warpBench mgr "warp" echoApp (\m u -> doWarpPost m u payload1k)
        ]
    , bgroup "echo (POST 4 KiB)"
        [ wireformBench "wireform-http1" echoApp (`doWfPost` payload4k)
        , warpBench mgr "warp" echoApp (\m u -> doWarpPost m u payload4k)
        ]
    , bgroup "json-ish (GET → ~4 KiB)"
        [ wireformBench "wireform-http1" jsonApp doWfGet
        , warpBench mgr "warp" jsonApp doWarpGet
        ]
    , bgroup "stream (GET → 4×1 KiB)"
        [ wireformBench "wireform-http1" (streamApp 4 1024) doWfGet
        , warpBench mgr "warp" (streamApp 4 1024) doWarpGet
        ]
    , bgroup "headers (GET → 20 hdrs)"
        [ wireformBench "wireform-http1" headersApp doWfGet
        , warpBench mgr "warp" headersApp doWarpGet
        ]
    ]

------------------------------------------------------------------------
-- Payloads
------------------------------------------------------------------------

payload1k :: BS.ByteString
payload1k = BS.replicate 1024 0x61

payload4k :: BS.ByteString
payload4k = BS.replicate 4096 0x61

------------------------------------------------------------------------
-- WAI apps under test
------------------------------------------------------------------------

helloApp :: Wai.Application
helloApp _req respond =
  respond $ Wai.responseLBS WAIHttp.status200
    [("Content-Type", "text/plain")] "hello"

echoApp :: Wai.Application
echoApp req respond = do
  body <- Wai.consumeRequestBodyStrict req
  respond $ Wai.responseLBS WAIHttp.status200
    [("Content-Type", "application/octet-stream")] body

jsonApp :: Wai.Application
jsonApp _req respond =
  respond $ Wai.responseLBS WAIHttp.status200
    [("Content-Type", "application/json")]
    (LBS.fromStrict jsonPayload)

jsonPayload :: BS.ByteString
jsonPayload = BS.concat
  [ "{\"users\":["
  , BS.intercalate ","
      [ BS8.pack $ "{\"id\":" <> show i <> ",\"name\":\"user" <> show i
          <> "\",\"email\":\"user" <> show i <> "@example.com\"}"
      | i <- [1..20 :: Int]
      ]
  , "]}"
  ]

streamApp :: Int -> Int -> Wai.Application
streamApp nChunks chunkSize _req respond = do
  let chunk = BS.replicate chunkSize 0x78
  respond $ Wai.responseStream WAIHttp.status200
    [("Content-Type", "application/octet-stream")] $ \write flush -> do
      let loop 0 = flush
          loop n = do
            write (Builder.byteString chunk)
            loop (n - 1)
      loop nChunks

headersApp :: Wai.Application
headersApp _req respond =
  respond $ Wai.responseLBS WAIHttp.status200
    ([ (mk (BS8.pack ("X-Header-" <> show i)), BS8.pack ("value-" <> show i))
     | i <- [1..20 :: Int]
     ] <> [("Content-Type", "text/plain")])
    "ok"

------------------------------------------------------------------------
-- wireform-http1 server path (WAI app → waiToHandler → Server → TCP)
-- wireform Connection client on the other end
------------------------------------------------------------------------

wireformBench :: String -> Wai.Application -> (String -> IO BS.ByteString) -> Benchmark
wireformBench name app doReq =
  envWithCleanup (startWireform app) (\_ -> pure ()) $ \port ->
    bench name $ nfIO (doReq port)

startWireform :: Wai.Application -> IO String
startWireform app = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr"
    (addr:_) -> do
      listenSock <- NS.openSocket addr
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      bound <- NS.getSocketName listenSock
      let portStr = case bound of
            NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
            _ -> "0"
          handler = waiToHandler app
          cfg = defaultServerConfig
            { serverHost = "127.0.0.1"
            , serverPort = portStr
            , serverVersionRange = http1Only
            , serverHandler = handler
            }
      _ <- forkIO $ do
        putMVar readyVar ()
        runServerOnListener cfg listenSock `finally` NS.close listenSock
      takeMVar readyVar
      pure portStr

doWfGet :: String -> IO BS.ByteString
doWfGet port = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = http1Only
        , connectionTls = Nothing
        }
  withConnection cfg $ \c -> do
    r <- sendOn c U.Request
      { U.requestMethod    = U.methodFromBytes "GET"
      , U.requestTarget    = "/"
      , U.requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
      , U.requestScheme    = U.SchemeHttp
      , U.requestHeaders   = [(U.hHost, BS8.pack ("127.0.0.1:" <> port))]
      , U.requestBody      = U.BodyEmpty
      , U.requestVersion   = U.HTTP1_1
      , U.requestTrailers  = pure []
      }
    drainBody (U.responseBody r)

doWfPost :: String -> BS.ByteString -> IO BS.ByteString
doWfPost port payload = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = http1Only
        , connectionTls = Nothing
        }
  withConnection cfg $ \c -> do
    r <- sendOn c U.Request
      { U.requestMethod    = U.methodFromBytes "POST"
      , U.requestTarget    = "/"
      , U.requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
      , U.requestScheme    = U.SchemeHttp
      , U.requestHeaders   =
          [ (U.hHost, BS8.pack ("127.0.0.1:" <> port))
          , (U.hContentType, "application/octet-stream")
          ]
      , U.requestBody      = U.BodyBytes payload
      , U.requestVersion   = U.HTTP1_1
      , U.requestTrailers  = pure []
      }
    drainBody (U.responseBody r)

drainBody :: U.Body -> IO BS.ByteString
drainBody U.BodyEmpty = pure BS.empty
drainBody (U.BodyBytes bs) = pure bs
drainBody (U.BodyStream p) = BS.concat <$> go
  where
    go = do
      mc <- p
      case mc of
        Nothing -> pure []
        Just c  -> (c :) <$> go

------------------------------------------------------------------------
-- Warp server path (WAI app → Warp → TCP)
-- http-client on the other end
------------------------------------------------------------------------

warpBench :: HC.Manager -> String -> Wai.Application -> (HC.Manager -> String -> IO BS.ByteString) -> Benchmark
warpBench mgr name app doReq =
  envWithCleanup (startWarp app) (\_ -> pure ()) $ \url ->
    bench name $ nfIO (doReq mgr url)

startWarp :: Wai.Application -> IO String
startWarp app = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr"
    (addr:_) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.addrAddress addr)
      NS.listen sock 128
      bound <- NS.getSocketName sock
      let port = case bound of
            NS.SockAddrInet p _ -> fromIntegral p :: Int
            _ -> 0
          settings = Warp.setPort port
                   $ Warp.setHost "127.0.0.1"
                   $ Warp.defaultSettings
      _ <- forkIO $ Warp.runSettingsSocket settings sock app
      threadDelay 50000
      pure ("http://127.0.0.1:" <> show port)

doWarpGet :: HC.Manager -> String -> IO BS.ByteString
doWarpGet mgr url = do
  req <- HC.parseRequest url
  resp <- HC.httpLbs req mgr
  pure $! LBS.toStrict (HC.responseBody resp)

doWarpPost :: HC.Manager -> String -> BS.ByteString -> IO BS.ByteString
doWarpPost mgr url payload = do
  req0 <- HC.parseRequest url
  let req = req0
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyBS payload
        , HC.requestHeaders = [("Content-Type", "application/octet-stream")]
        }
  resp <- HC.httpLbs req mgr
  pure $! LBS.toStrict (HC.responseBody resp)
