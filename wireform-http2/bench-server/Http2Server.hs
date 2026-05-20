{-# LANGUAGE OverloadedStrings #-}
-- | Minimal HTTP/2 server using the http2 Hackage package for comparison.
module Main (main) where

import Data.ByteString.Builder (byteString)
import Network.HTTP2.Server
import Network.HTTP.Types (status200)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.Environment (getArgs)
import Control.Exception (bracket, catch, SomeException)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p:_) -> p
        _ -> "8081"
  putStrLn $ "http2 (Hackage) server listening on port " <> port
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
  case addrs of
    [] -> error "No address found"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 1024
        acceptLoop sock

acceptLoop :: NS.Socket -> IO ()
acceptLoop listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ handleConn clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop listenSock

handleConn :: NS.Socket -> IO ()
handleConn sock = do
  config <- allocSimpleConfig sock 4096
  run defaultServerConfig config server
  freeSimpleConfig config
  NS.close sock

server :: Server
server _req _aux sendResponse = sendResponse resp []
  where
    resp = responseBuilder status200 headers body
    headers = [ ("content-type", "text/plain")
              , ("content-length", "13")
              ]
    body = byteString "Hello, World!"
