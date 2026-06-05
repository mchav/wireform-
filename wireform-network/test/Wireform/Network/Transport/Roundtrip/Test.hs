{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end roundtrip tests: send bytes through sendBuilderDirect /
-- sendByteString / withSendCork on one side of a DuplexPipe, receive
-- and verify on the other side via the ReceiveTransport.
--
-- This exercises the full pipeline: builder → RingSink → send ring →
-- inline drain → in-memory queue → recv ring → consumer read.
module Wireform.Network.Transport.Roundtrip.Test (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (try, SomeException)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import Test.Syd
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Wireform.Builder
  ( Builder, word8, word32BE, word64BE
  , byteString, byteStringCopy
  , toStrictByteString
  )
import Wireform.Network
  ( DuplexTransport (..)
  , closeDuplexTransport
  , newDuplexPipe
  )
import Wireform.Transport.Config (defaultTransportConfig, TransportConfig (..), ringSizeHint)
import Wireform.Transport.Receive
import Wireform.Transport.Send

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

patternBytes :: Int -> BS.ByteString
patternBytes n = BS.pack (fmap (\i -> fromIntegral (i `mod` 251) :: Word8) [0..n-1])

mixedBuilder :: Int -> Builder
mixedBuilder n =
  let header  = word32BE 0xCAFEBABE <> word64BE (fromIntegral n)
      payload = byteString (patternBytes (max 0 (n - 12)))
  in header <> payload

mixedBuilderBS :: Int -> BS.ByteString
mixedBuilderBS = toStrictByteString . mixedBuilder

smallCfg :: TransportConfig
smallCfg = defaultTransportConfig { ringSizeHint = 4096 }

mediumCfg :: TransportConfig
mediumCfg = defaultTransportConfig { ringSizeHint = 16384 }

largeCfg :: TransportConfig
largeCfg = defaultTransportConfig { ringSizeHint = 65536 }

-- | Read exactly @n@ bytes from a ReceiveTransport.
recvExact :: ReceiveTransport -> IORef Word64 -> Int -> IO BS.ByteString
recvExact rx cursorRef n = go [] n
  where
    go acc 0 = pure (BS.concat (reverse acc))
    go acc remaining = do
      pos <- readIORef cursorRef
      w <- receiveWaitData rx pos
      case w of
        ReceiveMoreData hd -> do
          let !avail = fromIntegral (hd - pos)
              !take_ = min remaining avail
              !off   = fromIntegral pos .&. receiveRingMask rx
              !ptr   = receiveRingBase rx `plusPtr` off
          chunk <- BSI.create take_ $ \dst -> copyBytes dst ptr take_
          let !newPos = pos + fromIntegral take_
          writeIORef cursorRef newPos
          receiveAdvanceTail rx newPos
          go (chunk : acc) (remaining - take_)
        ReceiveEndOfInput -> pure (BS.concat (reverse acc))
        ReceiveFailed _ -> pure (BS.concat (reverse acc))

-- | Read all bytes until EOF from a ReceiveTransport.
recvAll :: ReceiveTransport -> IORef Word64 -> IO BS.ByteString
recvAll rx cursorRef = go []
  where
    go acc = do
      pos <- readIORef cursorRef
      w <- receiveWaitData rx pos
      case w of
        ReceiveMoreData hd -> do
          let !avail = fromIntegral (hd - pos)
              !off   = fromIntegral pos .&. receiveRingMask rx
              !ptr   = receiveRingBase rx `plusPtr` off
          chunk <- BSI.create avail $ \dst -> copyBytes dst ptr avail
          let !newPos = pos + fromIntegral avail
          writeIORef cursorRef newPos
          receiveAdvanceTail rx newPos
          go (chunk : acc)
        ReceiveEndOfInput -> pure (BS.concat (reverse acc))
        ReceiveFailed _ -> pure (BS.concat (reverse acc))

-- | Send on one side, receive on the other, verify.
roundtrip :: TransportConfig -> (SendTransport -> IO ()) -> IO BS.ByteString
roundtrip cfg sender = do
  (client, broker) <- newDuplexPipe cfg
  let tx = duplexSend client
      rx = duplexReceive broker
  cursor <- newIORef (0 :: Word64)
  resultVar <- newEmptyMVar
  _ <- forkIO $ do
    r <- try @SomeException $ do
      sender tx
      sendClose tx
      sendShutdownWrite tx
    putMVar resultVar r
  received <- recvAll rx cursor
  _ <- takeMVar resultVar
  closeDuplexTransport client
  closeDuplexTransport broker
  pure received

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Roundtrip (send → receive)" $ do

  -- ----------------------------------------------------------------
  -- sendBuilderDirect roundtrip
  -- ----------------------------------------------------------------
  describe "sendBuilderDirect" $ do

    it "empty builder" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx mempty
      received `shouldBe` BS.empty

    it "single byte" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (word8 0x42)
      received `shouldBe` BS.singleton 0x42

    it "100 bytes" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (byteStringCopy (patternBytes 100))
      received `shouldBe` patternBytes 100

    it "exactly ring size" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (byteStringCopy (patternBytes 4000))
      received `shouldBe` patternBytes 4000

    it "2x ring size (overflow)" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (byteString (patternBytes 8192))
      received `shouldBe` patternBytes 8192

    it "5x ring size" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (byteString (patternBytes 20000))
      received `shouldBe` patternBytes 20000

    it "mixed builder (header + payload)" $ do
      received <- roundtrip mediumCfg $ \tx ->
        sendBuilderDirect tx (mixedBuilder 5000)
      received `shouldBe` mixedBuilderBS 5000

    it "large mixed builder exceeding ring" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx (mixedBuilder 15000)
      received `shouldBe` mixedBuilderBS 15000

    it "256 small builders concatenated" $ do
      let b = mconcat (fmap (\i -> word8 (fromIntegral (i :: Int))) [0..255])
      received <- roundtrip smallCfg $ \tx ->
        sendBuilderDirect tx b
      received `shouldBe` BS.pack [0..255]

  -- ----------------------------------------------------------------
  -- sendByteString roundtrip
  -- ----------------------------------------------------------------
  describe "sendByteString" $ do

    it "small payload" $ do
      received <- roundtrip smallCfg $ \tx ->
        sendByteString tx (patternBytes 200)
      received `shouldBe` patternBytes 200

    it "multiple sendByteString in sequence" $ do
      received <- roundtrip smallCfg $ \tx -> do
        sendByteString tx (patternBytes 100)
        sendByteString tx (patternBytes 200)
        sendByteString tx (patternBytes 50)
      received `shouldBe` BS.concat [patternBytes 100, patternBytes 200, patternBytes 50]

    it "sendByteStringMany" $ do
      let bss = [patternBytes 100, patternBytes 200, patternBytes 50]
      received <- roundtrip smallCfg $ \tx ->
        sendByteStringMany tx bss
      received `shouldBe` BS.concat bss

  -- ----------------------------------------------------------------
  -- withSendCork roundtrip
  -- ----------------------------------------------------------------
  describe "withSendCork" $ do

    it "corked builder + bytestring" $ do
      let header = word32BE 0xDEAD <> word32BE 0xBEEF
          body   = patternBytes 500
      received <- roundtrip largeCfg $ \tx ->
        withSendCork tx $ \corked -> do
          sendBuilderDirect corked header
          sendByteString corked body
      BS.take 8 received `shouldBe` BS.pack [0x00, 0x00, 0xDE, 0xAD, 0x00, 0x00, 0xBE, 0xEF]
      BS.drop 8 received `shouldBe` body

    it "corked builder exceeding ring (backpressure)" $ do
      let payload = patternBytes 15000
      received <- roundtrip smallCfg $ \tx ->
        withSendCork tx $ \corked ->
          sendBuilderDirect corked (byteString payload)
      received `shouldBe` payload

    it "corked many small sends exceeding ring" $ do
      let chunk = patternBytes 500
          n = 30 :: Int
      received <- roundtrip smallCfg $ \tx ->
        withSendCork tx $ \corked ->
          mapM_ (\_ -> sendByteString corked chunk) [1..n]
      received `shouldBe` BS.concat (replicate n chunk)

    it "two sequential corks" $ do
      received <- roundtrip largeCfg $ \tx -> do
        withSendCork tx $ \corked -> do
          sendByteString corked (patternBytes 100)
          sendByteString corked (patternBytes 200)
        withSendCork tx $ \corked -> do
          sendByteString corked (patternBytes 300)
          sendByteString corked (patternBytes 400)
      received `shouldBe` BS.concat
        [patternBytes 100, patternBytes 200, patternBytes 300, patternBytes 400]

    it "cork then uncorked send" $ do
      received <- roundtrip largeCfg $ \tx -> do
        withSendCork tx $ \corked ->
          sendBuilderDirect corked (mixedBuilder 200)
        sendByteString tx (patternBytes 100)
      received `shouldBe` BS.append (mixedBuilderBS 200) (patternBytes 100)

  -- ----------------------------------------------------------------
  -- sendBuilderDirect vs sendBuilderViaByteString equivalence
  -- ----------------------------------------------------------------
  describe "sendBuilderDirect vs sendBuilderViaByteString equivalence" $ do

    it "small payload" $ do
      let b = mixedBuilder 200
      direct <- roundtrip largeCfg $ \tx -> sendBuilderDirect tx b
      via    <- roundtrip largeCfg $ \tx -> sendBuilderViaByteString tx b
      direct `shouldBe` via

    it "medium payload" $ do
      let b = mixedBuilder 5000
      direct <- roundtrip largeCfg $ \tx -> sendBuilderDirect tx b
      via    <- roundtrip largeCfg $ \tx -> sendBuilderViaByteString tx b
      direct `shouldBe` via

    it "large payload (multi-overflow)" $ do
      let b = mixedBuilder 40000
      direct <- roundtrip largeCfg $ \tx -> sendBuilderDirect tx b
      via    <- roundtrip largeCfg $ \tx -> sendBuilderViaByteString tx b
      direct `shouldBe` via

  -- ----------------------------------------------------------------
  -- Property-based roundtrip
  -- ----------------------------------------------------------------
  describe "property-based roundtrip" $ do

    it "arbitrary builder size" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 50000
        received <- run $ roundtrip smallCfg $ \tx ->
          sendBuilderDirect tx (byteString (patternBytes sz))
        assert (received == patternBytes sz)

    it "arbitrary corked builder size" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 50000
        received <- run $ roundtrip smallCfg $ \tx ->
          withSendCork tx $ \corked ->
            sendBuilderDirect corked (byteString (patternBytes sz))
        assert (received == patternBytes sz)

    it "direct == viaByteString for arbitrary size" $
      property $ \(Positive (n :: Int)) -> monadicIO $ do
        let sz = min n 30000
            b = mixedBuilder sz
        direct <- run $ roundtrip largeCfg $ \tx -> sendBuilderDirect tx b
        via    <- run $ roundtrip largeCfg $ \tx -> sendBuilderViaByteString tx b
        assert (direct == via)
