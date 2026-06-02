{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (IOException, SomeException, toException)
import Control.Exception qualified as E
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Word
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket hiding (close)
import Network.Socket qualified as S
import Network.Socket.ByteString (recv, sendAll)
import Wireform.Parser
import Wireform.Parser.Driver
import Wireform.Parser.Internal (ParserMode, Pure, Stream)
import Wireform.Ring.Internal (MagicRing, ringBase, ringMask, ringSize, withMagicRing)
import Wireform.Transport
import Wireform.Transport.Config (defaultTransportConfig, ringSizeHint)


word32sP :: ParserMode m => Int -> Parser m () ()
word32sP 0 = pure ()
word32sP n = do
  !_ <- anyWord32be
  word32sP (n - 1)
{-# INLINE word32sP #-}


mkWord32Input :: Int -> BS.ByteString
mkWord32Input n =
  LBS.toStrict . BSB.toLazyByteString $
    mconcat [BSB.word32BE (fromIntegral i) | i <- [0 .. n - 1]]


connectedPair :: IO (Socket, Socket)
connectedPair = socketPair AF_UNIX Stream defaultProtocol


mkMemoryTransport :: MagicRing s -> BS.ByteString -> IO ReceiveTransport
mkMemoryTransport ring payload = do
  let !base = ringBase ring
      !payloadLen = BS.length payload
  BSU.unsafeUseAsCStringLen payload \(src, len) ->
    copyBytes base (castPtr src) len
  let !headPos = fromIntegral payloadLen :: Word64
  pure
    ReceiveTransport
      { receiveRingBase = ringBase ring
      , receiveRingSize = ringSize ring
      , receiveRingMask = ringMask ring
      , receiveLoadHead = pure headPos
      , receiveAdvanceTail = \_ -> pure ()
      , receiveWaitData = \_ -> pure ReceiveEndOfInput
      , receiveClose = pure ()
      }


withRecvTransportReuse :: MagicRing s -> Socket -> (ReceiveTransport -> IO a) -> IO a
withRecvTransportReuse ring sock action = do
  let !base = ringBase ring
      !msk = ringMask ring
      !sz = ringSize ring
  headRef <- newIORef (0 :: Word64)
  tailRef <- newIORef (0 :: Word64)
  eofRef <- newIORef False
  errRef <- newIORef (Nothing :: Maybe SomeException)
  let loadHead = readIORef headRef
      advanceTail pos = writeIORef tailRef pos
      waitData pos = do
        isEof <- readIORef eofRef
        if isEof
          then pure ReceiveEndOfInput
          else do
            mbErr <- readIORef errRef
            case mbErr of
              Just e -> pure (ReceiveFailed e)
              Nothing -> doRecv pos
      doRecv pos = do
        h <- readIORef headRef
        if h > pos
          then pure (ReceiveMoreData h)
          else do
            t <- readIORef tailRef
            let !writeOff = fromIntegral h .&. msk
                !writePtr = base `plusPtr` writeOff
                !available = sz - fromIntegral (h - t)
                !maxRecv = min available (sz - writeOff)
            if maxRecv <= 0
              then pure (ReceiveMoreData h)
              else do
                result <- E.try @IOException (S.recvBuf sock writePtr maxRecv)
                case result of
                  Left exc -> do
                    writeIORef errRef (Just (toException exc))
                    pure (ReceiveFailed (toException exc))
                  Right n
                    | n == 0 -> do
                        writeIORef eofRef True
                        pure ReceiveEndOfInput
                    | otherwise -> do
                        let !newHead = h + fromIntegral n
                        writeIORef headRef newHead
                        pure (ReceiveMoreData newHead)
      transport =
        ReceiveTransport
          { receiveRingBase = base
          , receiveRingSize = sz
          , receiveRingMask = msk
          , receiveLoadHead = loadHead
          , receiveAdvanceTail = advanceTail
          , receiveWaitData = waitData
          , receiveClose = writeIORef eofRef True
          }
  action transport


wallTimeIO :: String -> IO a -> IO a
wallTimeIO label act = do
  t0 <- getMonotonicTimeNSec
  r <- act
  t1 <- getMonotonicTimeNSec
  let !us = fromIntegral (t1 - t0) / (1000 :: Double)
  putStrLn $ label <> ": " <> show (round us :: Int) <> " μs"
  pure r


main :: IO ()
main = do
  let !n = 10000
  let !payload = mkWord32Input n
  putStrLn $ "Payload: " <> show (BS.length payload) <> " bytes, " <> show n <> " word32s"

  putStrLn "\n=== 1. Pure parse (parseByteString, no streaming) ==="
  sequence_ $
    replicate 10 $
      wallTimeIO "  pure" $
        case parseByteString (word32sP @Pure n) payload of
          Right () -> pure ()
          Left e -> error (show e)

  putStrLn "\n=== 2. Streaming parse via ring (no I/O, no suspension) ==="
  putStrLn "(Data pre-filled in ring, head already advanced)"
  putStrLn "(Uses runParserInternal with startPos=0)"
  withMagicRing (ringSizeHint defaultTransportConfig) \ring -> do
    sequence_ $ replicate 10 $ do
      t <- mkMemoryTransport ring payload
      wallTimeIO "  stream-nosuspend" $ do
        r <- runParserInternal t (word32sP @Stream n) 0
        case r of
          IRDone _ () -> pure ()
          _ -> error "streaming parse failed"

  putStrLn "\n=== 3. Streaming parse via ring (no I/O, WITH 1 suspension) ==="
  putStrLn "(Data pre-filled but head starts at 0; waitData advances to payloadLen)"
  withMagicRing (ringSizeHint defaultTransportConfig) \ring -> do
    sequence_ $ replicate 10 $ do
      BSU.unsafeUseAsCStringLen payload \(src, len) ->
        copyBytes (ringBase ring) (castPtr src) len
      let !headPos = fromIntegral (BS.length payload) :: Word64
      headRef <- newIORef (0 :: Word64)
      let t =
            ReceiveTransport
              { receiveRingBase = ringBase ring
              , receiveRingSize = ringSize ring
              , receiveRingMask = ringMask ring
              , receiveLoadHead = readIORef headRef
              , receiveAdvanceTail = \_ -> pure ()
              , receiveWaitData = \_ -> do
                  writeIORef headRef headPos
                  pure (ReceiveMoreData headPos)
              , receiveClose = pure ()
              }
      wallTimeIO "  stream-1suspend" $ do
        r <- runParser t (word32sP @Stream n)
        case r of
          Right () -> pure ()
          Left e -> error (show e)

  putStrLn "\n=== 4. ReceiveTransport + network (data pre-sent) ==="
  withMagicRing (ringSizeHint defaultTransportConfig) \ring -> do
    sequence_ $ replicate 10 $ do
      (sender, receiver) <- connectedPair
      sendAll sender payload
      wallTimeIO "  transport+net" $
        withRecvTransportReuse ring receiver \t -> do
          r <- runParser t (word32sP @Stream n)
          case r of
            Right () -> pure ()
            Left e -> error (show e)
      S.close sender
      S.close receiver

  putStrLn "\n=== 5. Standard recv+concat+parse (data pre-sent) ==="
  sequence_ $ replicate 10 $ do
    (sender, receiver) <- connectedPair
    sendAll sender payload
    wallTimeIO "  recv+parse" $ do
      allData <- recvAllBytes receiver (BS.length payload)
      case parseByteString (word32sP @Pure n) allData of
        Right () -> pure ()
        Left e -> error (show e)
    S.close sender
    S.close receiver

  putStrLn "\nDone."


recvAllBytes :: Socket -> Int -> IO BS.ByteString
recvAllBytes sock total = go [] total
  where
    go acc 0 = pure $! BS.concat (reverse acc)
    go acc left = do
      chunk <- recv sock (min left (64 * 1024))
      if BS.null chunk
        then pure $! BS.concat (reverse acc)
        else go (chunk : acc) (left - BS.length chunk)
