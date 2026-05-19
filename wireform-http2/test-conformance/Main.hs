module Main (main) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Network.Socket (Socket)
import qualified Network.Socket as NS
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith, exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Process (readProcessWithExitCode, proc, createProcess, waitForProcess, CreateProcess(..))

import Network.HTTP2.Server
import Network.HTTP2.Types

main :: IO ()
main = do
  h2specPath <- lookupEnv "H2SPEC" >>= \case
    Just p -> pure p
    Nothing -> pure "h2spec"
  port <- lookupEnv "H2SPEC_PORT" >>= \case
    Just p -> pure p
    Nothing -> pure "9090"
  let serverCfg = defaultServerConfig
        { serverPort = port
        , serverHandler = conformanceHandler
        , serverSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 100
            }
        }
  serverReady <- newEmptyMVar
  serverTid <- forkIO $ do
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
          NS.listen sock 128
          putMVar serverReady ()
          acceptLoopConformance serverCfg sock
  takeMVar serverReady
  (exitCode, stdout', stderr') <- readProcessWithExitCode h2specPath
    [ "-h", "127.0.0.1"
    , "-p", port
    , "--strict"
    , "--timeout", "3"
    ] ""
  killThread serverTid
  putStrLn stdout'
  hPutStrLn stderr stderr'
  exitWith exitCode

acceptLoopConformance :: ServerConfig -> Socket -> IO ()
acceptLoopConformance cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ runServerOnSocket cfg clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoopConformance cfg listenSock

conformanceHandler :: Request -> (Response -> IO ()) -> IO ()
conformanceHandler _req respond = respond Response
  { responseStatus = 200
  , responseHeaders = [("content-type", "text/plain")]
  , responseBody = ResponseBodyBS "ok"
  }
