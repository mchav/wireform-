-- | HTTP/2 benchmark server using the real wireform-http2 library API.
-- No shortcuts — exercises the full frame encode/decode, HPACK, and connection stack.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, catch, bracket, finally)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.Environment (getArgs)

import Network.HTTP2.Connection
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p:_) -> p
        _ -> "8080"
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
        putStrLn $ "wireform-http2 server listening on port " <> port
        acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ handleClient clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop listenSock

handleClient :: Socket -> IO ()
handleClient sock = do
  preface <- recvExact sock 24
  if preface /= connectionPreface
    then NS.close sock
    else do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 1000
            , settingsInitialWindowSize = 1048576
            }
        , ccSocket = sock
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      -- Send server settings
      let params = encodeSettings defaultSettings
            { settingsMaxConcurrentStreams = Just 1000
            , settingsInitialWindowSize = 1048576
            }
      sendFrame conn $ Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
      -- Boost connection window
      sendFrame conn $ Frame
        (FrameHeader 4 FrameWindowUpdate 0 0)
        (WindowUpdateFrame 10485760)
      serverLoop conn `finally` NS.close sock

serverLoop :: Connection -> IO ()
serverLoop conn = do
  result <- recvFrame conn
  case result of
    Left _ -> pure ()
    Right (Frame hdr payload) -> do
      handleFrame conn hdr payload
      serverLoop conn

handleFrame :: Connection -> FrameHeader -> FramePayload -> IO ()
handleFrame conn hdr payload = case payload of
  SettingsFrame _
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise ->
        sendFrame conn $ Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])

  PingFrame opaqueData
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise ->
        sendFrame conn $ Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaqueData)

  WindowUpdateFrame _ -> pure ()

  HeadersFrame _ headerBlock
    | testFlag (fhFlags hdr) flagEndHeaders -> do
        -- Full HPACK decode (real work)
        decoder <- readMVar (connHpackDecoder conn)
        _ <- decodeHeaderBlock decoder headerBlock
        -- Full HPACK encode response (real work)
        sendResponse conn (fhStreamId hdr)
    | otherwise -> pure ()

  DataFrame _ -> do
    let len = fhLength hdr
    if len > 0
      then sendFrame conn $ Frame (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
      else pure ()

  GoAwayFrame _ _ _ -> pure ()
  RSTStreamFrame _ -> pure ()
  _ -> pure ()

sendResponse :: Connection -> StreamId -> IO ()
sendResponse conn sid = do
  encoder <- readMVar (connHpackEncoder conn)
  -- Real HPACK encode with dynamic table
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder
    [(":status", "200"), ("content-type", "text/plain"), ("content-length", "13")]
  -- Batch both frames into one syscall
  sendFrames conn
    [ Frame (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders flagEndHeaders sid)
        (HeadersFrame Nothing headerBlock)
    , Frame (FrameHeader 13 FrameData flagEndStream sid)
        (DataFrame "Hello, World!")
    ]

recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 16384)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
