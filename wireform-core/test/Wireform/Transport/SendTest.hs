{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wireform.Transport.SendTest (spec) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import Foreign.Ptr (plusPtr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import Wireform.Builder (word8, byteString, byteStringCopy)
import Wireform.Ring.Internal (withMagicRing, ringBase, ringSize, ringMask)
import Wireform.Transport.Send

spec :: Spec
spec = describe "SendTransport" $ do

  describe "sendBuilderDirect" $ do
    it "sends a small builder" $ do
      (bytes, _) <- withTestTransport 4096 $ \t -> do
        sendBuilderDirect t (word8 0x42 <> word8 0x43)
      readChunks bytes `shouldBe` BS.pack [0x42, 0x43]

    it "sends a builder larger than the default chunk" $ do
      let payload = BS.replicate 40000 0xAA
      (bytes, _) <- withTestTransport 65536 $ \t -> do
        sendBuilderDirect t (byteStringCopy payload)
      readChunks bytes `shouldBe` payload

    it "sends a builder larger than ring via byteString insert" $ do
      let payload = BS.replicate 12000 0xBB
      (bytes, _) <- withTestTransport 4096 $ \t -> do
        sendBuilderDirect t (byteString payload)
      readChunks bytes `shouldBe` payload

  describe "withSendCork" $ do
    it "batches two sends into one publish" $ do
      (bytes, publishes) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \corked -> do
          sendByteString corked (BS.replicate 100 0xAA)
          sendByteString corked (BS.replicate 200 0xBB)
      readChunks bytes `shouldBe`
        (BS.replicate 100 0xAA <> BS.replicate 200 0xBB)
      readIORef publishes `shouldReturn` 1

    it "batches builder + bytestring into one publish" $ do
      (bytes, publishes) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \corked -> do
          sendBuilderDirect corked (word8 0x01 <> word8 0x02)
          sendByteString corked (BS.replicate 50 0xFF)
      let result = readChunks bytes
      BS.take 2 result `shouldBe` BS.pack [0x01, 0x02]
      BS.drop 2 result `shouldBe` BS.replicate 50 0xFF
      readIORef publishes `shouldReturn` 1

    it "handles payload fitting in ring (via builder)" $ do
      (bytes, _) <- withTestTransport 4096 $ \t -> do
        withSendCork t $ \corked -> do
          sendBuilderDirect corked (byteStringCopy (BS.replicate 3000 0xCC))
      readChunks bytes `shouldBe` BS.replicate 3000 0xCC

    it "backpressure: builder payload larger than ring does not deadlock" $ do
      let payload = BS.replicate 12000 0xDD
      (bytes, publishes) <- withTestTransport 4096 $ \t -> do
        withSendCork t $ \corked -> do
          sendBuilderDirect corked (byteString payload)
      readChunks bytes `shouldBe` payload
      n <- readIORef publishes
      n `shouldSatisfy` (> 1)

    it "backpressure: many small sends exceeding ring size" $ do
      let chunk = BS.replicate 500 0xEE
          numChunks = 30 :: Int
      (bytes, _) <- withTestTransport 4096 $ \t -> do
        withSendCork t $ \corked -> do
          mapM_ (\_ -> sendByteString corked chunk) [1..numChunks]
      readChunks bytes `shouldBe` BS.concat (replicate numChunks chunk)

    it "empty cork produces zero publishes" $ do
      (_, publishes) <- withTestTransport 4096 $ \t -> do
        withSendCork t $ \_ -> pure ()
      readIORef publishes `shouldReturn` 0

    it "backpressure: sendBuilderDirect inside cork exceeding ring" $ do
      let payload = BS.replicate 12000 0x77
      (bytes, publishes) <- withTestTransport 4096 $ \t -> do
        withSendCork t $ \corked -> do
          sendBuilderDirect corked (byteString payload)
      readChunks bytes `shouldBe` payload
      n <- readIORef publishes
      n `shouldSatisfy` (> 1)

    it "uncorked send after cork works normally" $ do
      (bytes, _) <- withTestTransport 65536 $ \t -> do
        withSendCork t $ \corked -> do
          sendByteString corked (BS.pack [0x01])
        sendByteString t (BS.pack [0x02])
      readChunks bytes `shouldBe` BS.pack [0x01, 0x02]


------------------------------------------------------------------------
-- Test infrastructure
------------------------------------------------------------------------

withTestTransport
  :: Int
  -> (SendTransport -> IO a)
  -> IO (IORef [BS.ByteString], IORef Int)
withTestTransport requestedRingSz action =
  withMagicRing requestedRingSz $ \ring -> do
    let !base = ringBase ring
        !sz   = ringSize ring
        !msk  = ringMask ring
    chunksRef <- newIORef ([] :: [BS.ByteString])
    publishCount <- newIORef (0 :: Int)
    headRef <- newIORef (0 :: Word64)
    tailRef <- newIORef (0 :: Word64)
    stateRef <- newIORef True

    let drainTo !hd = do
          tl <- readIORef tailRef
          drainLoop tl
          where
            drainLoop !cur
              | cur >= hd = pure ()
              | otherwise = do
                  let !off  = fromIntegral cur .&. msk
                      !want = min (fromIntegral (hd - cur)) (sz - off)
                      !ptr  = base `plusPtr` off
                  bs <- BS.packCStringLen (castPtr ptr, want)
                  modifyIORef' chunksRef (bs :)
                  let !newTail = cur + fromIntegral want
                  writeIORef tailRef newTail
                  drainLoop newTail

    let publish h = do
          modifyIORef' publishCount (+ 1)
          writeIORef headRef h
          drainTo h

    let waitSpace _pos = do
          isOpen <- readIORef stateRef
          if isOpen
            then do
              tl <- readIORef tailRef
              pure (SendSpaceAvailable tl)
            else pure SendPeerClosed

    let transport = SendTransport
          { sendRingBase     = base
          , sendRingSize     = sz
          , sendRingMask     = msk
          , sendLoadTail     = readIORef tailRef
          , sendLoadHead     = readIORef headRef
          , sendPublishHead  = publish
          , sendWaitSpace    = waitSpace
          , sendFlush        = readIORef headRef >>= drainTo
          , sendShutdownWrite = pure ()
          , sendClose        = writeIORef stateRef False
          }
    _ <- action transport
    pure (chunksRef, publishCount)

readChunks :: IORef [BS.ByteString] -> BS.ByteString
readChunks ref = unsafePerformIO $ do
  chunks <- readIORef ref
  pure (BS.concat (reverse chunks))
{-# NOINLINE readChunks #-}
