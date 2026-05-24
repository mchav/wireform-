{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{- | Apples-to-apples benchmark: same WAI apps served over localhost
TCP via wireform-http's HTTP\/1 server (through the WAI adapter) vs
Warp, both hit by their respective HTTP\/1 clients.

Both sides reuse connections (keep-alive): wireform via a persistent
'Connection', http-client via its connection pool. Each criterion
iteration is a single request\/response round-trip on an already-open
TCP connection.
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
import Data.IORef
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
  (Connection, ConnectionConfig(..), defaultConnectionConfig,
   openConnection, closeConnection, sendOn)
import Network.HTTP.Server
  (ServerConfig(..), defaultServerConfig, runServerOnListener)
import Network.HTTP.VersionRange (http1Only)
import Network.HTTP.WAI (waiToHandler)

main :: IO ()
main = do
  mgr <- HC.newManager HC.defaultManagerSettings

  -- Set up wireform servers + persistent connections for each app
  wfHello   <- setupWireform helloApp
  wfEcho    <- setupWireform echoApp
  wfJson    <- setupWireform jsonApp
  wfStream  <- setupWireform (streamApp 16 4096)
  wfHeaders <- setupWireform headersApp

  -- Set up warp servers
  warpHelloUrl   <- startWarp helloApp
  warpEchoUrl    <- startWarp echoApp
  warpJsonUrl    <- startWarp jsonApp
  warpStreamUrl  <- startWarp (streamApp 16 4096)
  warpHeadersUrl <- startWarp headersApp

  defaultMain
    [ bgroup "hello (GET → 5 B)"
        [ bench "wireform-http1" $ nfIO (wfGet wfHello)
        , bench "warp"           $ nfIO (warpGet mgr warpHelloUrl)
        ]
    , bgroup "echo (POST 1 KiB)"
        [ bench "wireform-http1" $ nfIO (wfPost wfEcho payload1k)
        , bench "warp"           $ nfIO (warpPost mgr warpEchoUrl payload1k)
        ]
    , bgroup "echo (POST 64 KiB)"
        [ bench "wireform-http1" $ nfIO (wfPost wfEcho payload64k)
        , bench "warp"           $ nfIO (warpPost mgr warpEchoUrl payload64k)
        ]
    , bgroup "json-ish (GET → ~4 KiB)"
        [ bench "wireform-http1" $ nfIO (wfGet wfJson)
        , bench "warp"           $ nfIO (warpGet mgr warpJsonUrl)
        ]
    , bgroup "stream (GET → 16×4 KiB)"
        [ bench "wireform-http1" $ nfIO (wfGet wfStream)
        , bench "warp"           $ nfIO (warpGet mgr warpStreamUrl)
        ]
    , bgroup "headers (GET → 20 hdrs)"
        [ bench "wireform-http1" $ nfIO (wfGet wfHeaders)
        , bench "warp"           $ nfIO (warpGet mgr warpHeadersUrl)
        ]
    ]

------------------------------------------------------------------------
-- Payloads
------------------------------------------------------------------------

payload1k :: BS.ByteString
payload1k = BS.replicate 1024 0x61

payload64k :: BS.ByteString
payload64k = BS.replicate (64 * 1024) 0x61

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
-- wireform-http1: persistent keep-alive connection
------------------------------------------------------------------------

data WfEnv = WfEnv
  { wfConn :: !Connection
  , wfPort :: !String
  }

setupWireform :: Wai.Application -> IO WfEnv
setupWireform app = do
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
      let connCfg = defaultConnectionConfig
            { connectionHost = "127.0.0.1"
            , connectionPort = portStr
            , connectionVersionRange = http1Only
            , connectionTls = Nothing
            }
      eConn <- openConnection connCfg
      case eConn of
        Left err -> error ("openConnection failed: " <> err)
        Right conn -> pure WfEnv { wfConn = conn, wfPort = portStr }

wfGet :: WfEnv -> IO BS.ByteString
wfGet env = do
  let port = wfPort env
  r <- sendOn (wfConn env) U.Request
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

wfPost :: WfEnv -> BS.ByteString -> IO BS.ByteString
wfPost env payload = do
  let port = wfPort env
  r <- sendOn (wfConn env) U.Request
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
-- Warp: http-client keeps connections alive via its pool
------------------------------------------------------------------------

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

warpGet :: HC.Manager -> String -> IO BS.ByteString
warpGet mgr url = do
  req <- HC.parseRequest url
  resp <- HC.httpLbs req mgr
  pure $! LBS.toStrict (HC.responseBody resp)

warpPost :: HC.Manager -> String -> BS.ByteString -> IO BS.ByteString
warpPost mgr url payload = do
  req0 <- HC.parseRequest url
  let req = req0
        { HC.method = "POST"
        , HC.requestBody = HC.RequestBodyBS payload
        , HC.requestHeaders = [("Content-Type", "application/octet-stream")]
        }
  resp <- HC.httpLbs req mgr
  pure $! LBS.toStrict (HC.responseBody resp)
