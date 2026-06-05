module Test.Connection (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import Test.Syd

import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.Types

tests :: Spec
tests = describe "Connection" $ sequence_
  [ describe "Settings" $ sequence_
      [ it "apply valid settings" $ do
          let params = [(0x1, 8192), (0x3, 200), (0x4, 131072), (0x5, 32768)]
          case applySettingsParams defaultSettings params of
            Left err -> expectationFailure (show err)
            Right s -> do
              settingsHeaderTableSize s `shouldBe` 8192
              settingsMaxConcurrentStreams s `shouldBe` Just 200
              settingsInitialWindowSize s `shouldBe` 131072
              settingsMaxFrameSize s `shouldBe` 32768
      , it "reject invalid max frame size (too small)" $ do
          let params = [(0x5, 100)]
          case applySettingsParams defaultSettings params of
            Left (InvalidMaxFrameSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidMaxFrameSize"
      , it "reject invalid max frame size (too large)" $ do
          let params = [(0x5, 20000000)]
          case applySettingsParams defaultSettings params of
            Left (InvalidMaxFrameSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidMaxFrameSize"
      , it "reject invalid initial window size" $ do
          let params = [(0x4, 0x80000000)]
          case applySettingsParams defaultSettings params of
            Left (InvalidInitialWindowSize _) -> pure ()
            _ -> expectationFailure "Expected InvalidInitialWindowSize"
      , it "reject invalid enable push" $ do
          let params = [(0x2, 2)]
          case applySettingsParams defaultSettings params of
            Left (InvalidEnablePush _) -> pure ()
            _ -> expectationFailure "Expected InvalidEnablePush"
      , it "unknown settings ignored" $ do
          let params = [(0xFF, 42)]
          case applySettingsParams defaultSettings params of
            Left err -> expectationFailure (show err)
            Right s -> s `shouldBe` defaultSettings
      ]
  , describe "FlowControl" $ sequence_
      [ it "consume and release" $ do
          fc <- atomically $ newFlowControl 65535
          consumed <- atomically $ consumeWindow fc 1000
          consumed `shouldBe` True
          avail <- atomically $ availableWindow fc
          avail `shouldBe` 64535
          result <- atomically $ releaseWindow fc 1000
          case result of
            Right () -> pure ()
            Left _ -> expectationFailure "Should not overflow"
          avail2 <- atomically $ availableWindow fc
          avail2 `shouldBe` 65535
      , it "refuse over-consume" $ do
          fc <- atomically $ newFlowControl 100
          consumed <- atomically $ consumeWindow fc 200
          not consumed `shouldBe` True
      , it "detect overflow" $ do
          fc <- atomically $ newFlowControl 2147483647
          result <- atomically $ releaseWindow fc 1
          case result of
            Left _ -> pure ()
            Right () -> expectationFailure "Should overflow"
      ]
  , describe "Client/Server handshake" $ sequence_
      [ it "basic preface exchange" $ do
          (clientSock, serverSock) <- socketPair
          serverDone <- newEmptyMVar
          _ <- forkIO $ do
            preface <- recvExact' serverSock 24
            preface `shouldBe` connectionPreface
            let settingsAck = encodeFrame $ Frame
                  (FrameHeader 0 FrameSettings flagAck 0)
                  (SettingsFrame [])
                serverSettings = encodeFrame $ Frame
                  (FrameHeader 0 FrameSettings 0 0)
                  (SettingsFrame [])
            NBS.sendMany serverSock [serverSettings, settingsAck]
            putMVar serverDone ()
          NBS.sendMany clientSock [connectionPreface]
          let clientSettings = encodeFrame $ Frame
                (FrameHeader 0 FrameSettings 0 0)
                (SettingsFrame [])
          NBS.sendMany clientSock [clientSettings]
          takeMVar serverDone
          NS.close clientSock
          NS.close serverSock
      ]
  ]

socketPair :: IO (Socket, Socket)
socketPair = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "No address"
    (addr:_) -> do
      listenSock <- NS.openSocket addr
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 1
      boundAddr <- NS.getSocketName listenSock
      clientSock <- NS.openSocket addr
      NS.connect clientSock boundAddr
      (serverSock, _) <- NS.accept listenSock
      NS.close listenSock
      pure (clientSock, serverSock)

recvExact' :: Socket -> Int -> IO ByteString
recvExact' sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
