{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
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

import Network.HTTP.WAI

main :: IO ()
main = do
  mgr <- HC.newManager HC.defaultManagerSettings
  defaultMain
    [ bgroup "hello (GET, 5 B)"
        [ bench "in-process" $ nfIO (inProcessGet helloApp)
        , withWarp helloApp mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpGet mgr url)
        ]
    , bgroup "echo (POST 1 KiB)"
        [ bench "in-process" $ nfIO (inProcessPost echoApp payload1k)
        , withWarp echoApp mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpPost mgr url payload1k)
        ]
    , bgroup "echo (POST 64 KiB)"
        [ bench "in-process" $ nfIO (inProcessPost echoApp payload64k)
        , withWarp echoApp mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpPost mgr url payload64k)
        ]
    , bgroup "json-ish (GET, ~4 KiB)"
        [ bench "in-process" $ nfIO (inProcessGet jsonApp)
        , withWarp jsonApp mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpGet mgr url)
        ]
    , bgroup "stream (GET, 16×4 KiB)"
        [ bench "in-process" $ nfIO (inProcessGet (streamApp 16 4096))
        , withWarp (streamApp 16 4096) mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpGet mgr url)
        ]
    , bgroup "headers (GET, 20 resp hdrs)"
        [ bench "in-process" $ nfIO (inProcessGet headersApp)
        , withWarp headersApp mgr $ \url ->
            bench "warp+http-client" $ nfIO (warpGet mgr url)
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
-- In-process via waiToHandler
------------------------------------------------------------------------

inProcessGet :: Wai.Application -> IO BS.ByteString
inProcessGet app = do
  let handler = waiToHandler app
  resp <- handler getReq
  drainBody resp

inProcessPost :: Wai.Application -> BS.ByteString -> IO BS.ByteString
inProcessPost app payload = do
  let handler = waiToHandler app
  resp <- handler (postReq payload)
  drainBody resp

getReq :: U.Request
getReq = U.Request
  { U.requestMethod    = U.mGet
  , U.requestTarget    = "/"
  , U.requestAuthority = Just "localhost"
  , U.requestScheme    = U.SchemeHttp
  , U.requestHeaders   = [(U.hHost, "localhost")]
  , U.requestBody      = U.BodyEmpty
  , U.requestVersion   = U.HTTP1_1
  , U.requestTrailers  = pure []
  }

postReq :: BS.ByteString -> U.Request
postReq payload = U.Request
  { U.requestMethod    = U.mPost
  , U.requestTarget    = "/"
  , U.requestAuthority = Just "localhost"
  , U.requestScheme    = U.SchemeHttp
  , U.requestHeaders   =
      [ (U.hHost, "localhost")
      , (U.hContentType, "application/octet-stream")
      ]
  , U.requestBody      = U.BodyBytes payload
  , U.requestVersion   = U.HTTP1_1
  , U.requestTrailers  = pure []
  }

drainBody :: U.Response -> IO BS.ByteString
drainBody r = case U.responseBody r of
  U.BodyEmpty      -> pure BS.empty
  U.BodyBytes bs   -> pure bs
  U.BodyStream src -> BS.concat <$> go src
    where
      go p = do
        mc <- p
        case mc of
          Nothing -> pure []
          Just c  -> (c :) <$> go p

------------------------------------------------------------------------
-- Warp over TCP via http-client
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Warp lifecycle
------------------------------------------------------------------------

withWarp :: Wai.Application -> HC.Manager -> (String -> Benchmark) -> Benchmark
withWarp app _mgr mkBench =
  envWithCleanup (startWarp app) (\_ -> pure ()) $ \url ->
    mkBench url

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
