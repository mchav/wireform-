{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Head-to-head benchmark: classic recv-based Kafka response framing
('connectionGetExact' + 'runGet' for the length\/correlation-id
prefixes) vs the new wireform magic-ring transport path
('Kafka.Network.FrameParser.kafkaFrameParser' driven by
'runKafkaFrameLoop').

Drives both implementations against the SAME in-memory byte stream
of pre-framed Kafka responses so the difference is only the
framing\/parsing cost, not the wire I\/O.

The classic implementation is recreated here in the benchmark (the
pre-change shape of 'Kafka.Client.Pipeline.readFrame') so we can run
both side by side without time-travelling the source tree.
-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (replicateM_, void)
import Criterion.Main
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Int (Int32)
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)

import Wireform.Network
  ( chunkedRecvFn
  , withRecvBufTransport
  )
import Wireform.Parser.Driver
  ( InternalResult (..)
  , LoopControl (..)
  , runParserInternal
  )
import Wireform.Ring.Internal
  ( MagicRing
  , destroyMagicRing
  , newMagicRing
  , ringBase
  , ringMask
  , ringSize
  )
import Wireform.Transport
import Wireform.Transport.Config (defaultTransportConfig, ringSizeHint)

import qualified Kafka.Network.FrameParser as FP

------------------------------------------------------------------------
-- Sample wire payloads
------------------------------------------------------------------------

-- | One Kafka response frame: [4-byte BE length][4-byte BE corrId][body].
oneFrame :: Int32 -> BS.ByteString -> BS.ByteString
oneFrame cid body =
  let !payload = BL.toStrict $ BP.runPut $ do
        BP.putInt32be cid
        BP.putByteString body
      !hdr = BL.toStrict $ BP.runPut $
        BP.putInt32be (fromIntegral (BS.length payload))
  in hdr <> payload

-- | A stream of @n@ small response frames (~64 byte bodies, mirrors
-- a chatty producer / consumer round-trip).
smallStream :: Int -> BS.ByteString
smallStream n = BS.concat
  [ oneFrame (fromIntegral i) (BS.replicate 64 0x61)
  | i <- [1 .. n]
  ]

-- | A stream of @n@ big response frames (~4 KiB bodies, mirrors a
-- fetch response).
bigStream :: Int -> BS.ByteString
bigStream n = BS.concat
  [ oneFrame (fromIntegral i) (BS.replicate 4096 0x61)
  | i <- [1 .. n]
  ]

------------------------------------------------------------------------
-- Classic recv path (faithful copy of the pre-migration
-- 'Kafka.Client.Pipeline.readFrame')
------------------------------------------------------------------------

-- | A pinned in-memory "connection": each 'classicGetExact' call
-- consumes from a slowly draining 'IORef [ByteString]'.
data MockConn = MockConn !(IORef [BS.ByteString])

mkMockConn :: [BS.ByteString] -> IO MockConn
mkMockConn bs = MockConn <$> newIORef bs

-- | Drain exactly @n@ bytes from the mock connection, allocating a
-- single contiguous 'BS.ByteString' result.  Mirrors
-- 'Network.Connection.connectionGetExact'.
classicGetExact :: MockConn -> Int -> IO BS.ByteString
classicGetExact (MockConn ref) n = do
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp $ \dst -> fill ref dst 0
  pure $! BSI.fromForeignPtr fp 0 n
  where
    fill :: IORef [BS.ByteString] -> Ptr Word8 -> Int -> IO ()
    fill r dst off
      | off >= n = pure ()
      | otherwise = do
          cs <- readIORef r
          case cs of
            [] -> error "classicGetExact: short read"
            c : rest -> do
              let !want   = n - off
                  !avail  = BS.length c
                  !take_  = min want avail
                  !taken  = BS.take take_ c
                  !leftover = BS.drop take_ c
              writeIORef r (if BS.null leftover then rest else leftover : rest)
              copyBSInto (dst `plusPtr` off) taken
              fill r dst (off + take_)

-- | Pre-migration 'readFrame' shape: pull 4 bytes, decode length,
-- pull length bytes, decode correlation id, slice off body.
classicReadFrame :: MockConn -> IO (Either String (Int32, BS.ByteString))
classicReadFrame conn = do
  lenBytes <- classicGetExact conn 4
  if BS.length lenBytes < 4
    then pure (Left "short read on frame length")
    else do
      let !len = fromIntegral
                   (BG.runGet BG.getInt32be (BL.fromStrict lenBytes)) :: Int
      if len < 4
        then pure (Left "frame too short for correlation id")
        else do
          payload <- classicGetExact conn len
          if BS.length payload < len
            then pure (Left "short read on frame body")
            else
              let !cidBs = BS.take 4 payload
                  !body  = BS.drop 4 payload
                  !cid   = fromIntegral
                             (BG.runGet BG.getInt32be (BL.fromStrict cidBs))
               in pure (Right (cid, body))

classicReadN :: BS.ByteString -> Int -> IO ()
classicReadN payload n = do
  conn <- mkMockConn [payload]
  replicateM_ n $ do
    r <- classicReadFrame conn
    case r of
      Right _ -> pure ()
      Left e  -> error ("classic read failed: " <> e)

------------------------------------------------------------------------
-- New transport path
------------------------------------------------------------------------

transportReadN :: BS.ByteString -> Int -> IO ()
transportReadN payload n = do
  recvFn <- chunkedRecvFn [payload]
  countRef <- newIORef (0 :: Int)
  void $ withRecvBufTransport defaultTransportConfig recvFn $ \t ->
    FP.runKafkaFrameLoop t $ \(_cid, _body) -> do
      c <- readIORef countRef
      let !c' = c + 1
      writeIORef countRef c'
      pure (if c' >= n then Stop else Continue)

-- | Long-lived-connection variant: pre-fill the supplied ring with
-- the payload and parse all N frames in one go.  Apples-to-apples
-- against the classic 'classicReadN' (both pay framing cost only;
-- neither does real I/O on the hot path).
transportReadNReuse :: MagicRing -> BS.ByteString -> Int -> IO ()
transportReadNReuse ring payload n = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  go t 0 0
  where
    go t !c !startPos
      | c >= n = pure ()
      | otherwise = do
          r <- runParserInternal t FP.kafkaFrameParser startPos
          case r of
            IRDone newPos _ -> go t (c + 1) newPos
            _ -> error ("transport reuse parse failed: c=" <> show c)

------------------------------------------------------------------------
-- Ring helpers
------------------------------------------------------------------------

prefillRing :: MagicRing -> BS.ByteString -> IO ()
prefillRing ring payload =
  BSU.unsafeUseAsCStringLen payload $ \(src, len) ->
    copyBytes (ringBase ring) (castPtr src) len

mkPrefilledTransport :: MagicRing -> Int -> IO Transport
mkPrefilledTransport ring payloadLen = do
  let !headPos = fromIntegral payloadLen :: Word64
  pure Transport
    { transportRing        = ring
    , transportLoadHead    = pure headPos
    , transportAdvanceTail = \_ -> pure ()
    , transportWaitData    = \_ -> pure EndOfInput
    , transportClose       = pure ()
    }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
copyBSInto dst bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in withForeignPtr fp $ \src ->
       copyBytes dst (src `plusPtr` off) len

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main =
  bracket
    (newMagicRing (ringSizeHint defaultTransportConfig))
    destroyMagicRing $ \ring ->
  defaultMain
    [ bgroup "100 small frames (64 byte body)"
        [ env (pure (smallStream 100)) $ \payload ->
            bench "classic (connectionGetExact + runGet)" $
              nfIO (classicReadN payload 100)
        , env (pure (smallStream 100)) $ \payload ->
            bench "transport reuse (prefilled ring)" $
              nfIO (transportReadNReuse ring payload 100)
        ]
    , bgroup "1000 small frames (64 byte body)"
        [ env (pure (smallStream 1000)) $ \payload ->
            bench "classic (connectionGetExact + runGet)" $
              nfIO (classicReadN payload 1000)
        , env (pure (smallStream 1000)) $ \payload ->
            bench "transport reuse (prefilled ring)" $
              nfIO (transportReadNReuse ring payload 1000)
        ]
    , bgroup "100 big frames (4 KiB body)"
        [ env (pure (bigStream 100)) $ \payload ->
            bench "classic (connectionGetExact + runGet)" $
              nfIO (classicReadN payload 100)
        , env (pure (bigStream 100)) $ \payload ->
            bench "transport reuse (prefilled ring)" $
              nfIO (transportReadNReuse ring payload 100)
        ]
    ]
