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
  -- Bind to an ephemeral port so back-to-back runs don't trip over
  -- TIME_WAIT sockets from the previous run.
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  serverReady <- newEmptyMVar
  (listenSock, boundPort) <- case addrs of
    [] -> error "No address found"
    (addr:_) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.setSocketOption sock NS.NoDelay 1
      NS.bind sock (NS.addrAddress addr)
      NS.listen sock 128
      port <- NS.socketPort sock
      pure (sock, port)
  let portStr = show (fromIntegral boundPort :: Int)
      serverCfg = defaultServerConfig
        { serverPort = portStr
        , serverHandler = conformanceHandler
        , serverSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 100
            }
        }
  serverTid <- forkIO $ do
    putMVar serverReady ()
    acceptLoopConformance serverCfg listenSock
      `finally` NS.close listenSock
  takeMVar serverReady
  (exitCode, stdout', stderr') <- readProcessWithExitCode h2specPath
    [ "-h", "127.0.0.1"
    , "-p", portStr
    , "--timeout", "5"
    ] ""
  killThread serverTid
  NS.close listenSock `catch` (\(_ :: SomeException) -> pure ())
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
conformanceHandler req respond = do
  hPutStrLn stderr $ "conformance handler invoked stream=" <> show (requestStreamId req)
  respond Response
    { responseStatus = 200
    , responseHeaders = [("content-type", "text/plain")]
    , responseBody = ResponseBodyBS "ok"
    , responseTrailers = []
    }
  hPutStrLn stderr $ "conformance handler done stream=" <> show (requestStreamId req)
