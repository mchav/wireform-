{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Head-to-head benchmark: classic recv-buffer based HTTP\/2 frame
decode vs the new wireform magic-ring transport path.

The classic path is what the connection layer currently runs:
'RecvBuffer' filled by a 'RecvFn', then a pair of
'recvBufferRead' calls (9-byte header + N-byte payload) handed to
'decodeFrameHeader' + 'decodeFramePayload'.

The new path uses 'withRecvBufTransport' + 'frameParser' from
'Network.HTTP2.Frame.Stream'.

Both paths consume the same recv chunks and decode the same frames.
-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (replicateM_, void)
import Criterion.Main
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
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
  , runParserLoop
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

import qualified Network.HTTP2.Internal.RecvBuffer as RB
import Network.HTTP2.Frame
  ( Frame (..)
  , FrameHeader (..)
  , FramePayload (..)
  , decodeFrameHeader
  , decodeFramePayload
  , encodeFrame
  , flagEndStream
  , frameHeaderLength
  )
import Network.HTTP2.Frame.Stream (frameParser, runFrameLoop)
import qualified Network.HTTP2.Frame.StreamingReader as SR
import Network.HTTP2.Types (FrameType (..))

------------------------------------------------------------------------
-- Sample frame streams
------------------------------------------------------------------------

-- Build a stream of @n@ small DATA frames.
buildDataFrames :: Int -> BS.ByteString
buildDataFrames n = BS.concat
  [ encodeFrame
      (Frame (FrameHeader 11 FrameData flagEndStream (fromIntegral sid))
             (DataFrame "hello world"))
  | sid <- [1 .. n]
  ]

-- Build a stream of @n@ medium DATA frames (~1 KiB each).
buildBigDataFrames :: Int -> BS.ByteString
buildBigDataFrames n = BS.concat
  [ encodeFrame
      (Frame (FrameHeader 1024 FrameData flagEndStream (fromIntegral sid))
             (DataFrame (BS.replicate 1024 0x61)))
  | sid <- [1 .. n]
  ]

------------------------------------------------------------------------
-- Classic recv path
------------------------------------------------------------------------

-- | One iteration: drain @n@ frames from a fresh recv buffer using
-- the classic pair of 'recvBufferRead' calls + 'decodeFrameHeader'
-- + 'decodeFramePayload'.
classicDecodeN :: BS.ByteString -> Int -> IO ()
classicDecodeN payload n = do
  rb <- RB.newRecvBuffer
  recvFn <- mkRecvFn [payload]
  replicateM_ n $ do
    hdrBs <- RB.recvBufferRead rb recvFn frameHeaderLength
    case decodeFrameHeader hdrBs of
      Right hdr -> do
        bodyBs <- RB.recvBufferRead rb recvFn (fromIntegral (fhLength hdr))
        case decodeFramePayload hdr bodyBs of
          Right _ -> pure ()
          Left e  -> error ("classic payload decode failed: " <> show e)
      Left e -> error ("classic header decode failed: " <> show e)

------------------------------------------------------------------------
-- New transport path
------------------------------------------------------------------------

transportDecodeN :: BS.ByteString -> Int -> IO ()
transportDecodeN payload n = do
  recvFn <- chunkedRecvFn [payload]
  countRef <- newIORef (0 :: Int)
  void $ withRecvBufTransport defaultTransportConfig recvFn $ \t ->
    runFrameLoop t $ \_fr -> do
      c <- readIORef countRef
      let !c' = c + 1
      writeIORef countRef c'
      pure (if c' >= n then Stop else Continue)

-- | Long-lived-connection variant for the wireform-Parser-based
-- frame loop: pre-fill the supplied ring with the payload and parse
-- all N frames in one go through 'frameParser' (Stream mode).
transportDecodeNReuse :: MagicRing -> BS.ByteString -> Int -> IO ()
transportDecodeNReuse ring payload n = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  go t 0 0
  where
    go t !c !startPos
      | c >= n = pure ()
      | otherwise = do
          r <- runParserInternal t frameParser startPos
          case r of
            IRDone newPos _fr -> go t (c + 1) newPos
            _ -> error ("transport (reuse) parser-based decode failed: c="
                         <> show c)

-- | Same workload but using the direct-on-ring 'StreamingReader'
-- (no wireform-Parser monad).  This is what the connection layer
-- should call on its hot path.
readerDecodeNReuse :: MagicRing -> BS.ByteString -> Int -> IO ()
readerDecodeNReuse ring payload n = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  go t 0 0
  where
    go t !c !startPos
      | c >= n = pure ()
      | otherwise = do
          r <- SR.readFrameFrom t startPos
          case r of
            Right (_fr, newPos) -> go t (c + 1) newPos
            Left e -> error ("reader (reuse) decode failed: c=" <> show c
                              <> ": " <> show e)

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

-- | A minimal recv-style chunk feeder for the HTTP/2 RecvBuffer.
mkRecvFn :: [BS.ByteString] -> IO (Ptr Word8 -> Int -> IO Int)
mkRecvFn chunks0 = do
  ref <- newIORef chunks0
  pure $ \dst want -> do
    cs <- readIORef ref
    case cs of
      [] -> pure 0
      c : rest -> do
        let !take_    = min want (BS.length c)
            !taken    = BS.take take_ c
            !leftover = BS.drop take_ c
        writeIORef ref (if BS.null leftover then rest else leftover : rest)
        copyBSInto dst taken
        pure take_

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
    [ bgroup "100 small DATA frames"
        [ env (pure (buildDataFrames 100)) $ \payload ->
            bench "classic (RecvBuffer + decodeFrameHeader/Payload)" $
              nfIO (classicDecodeN payload 100)
        , env (pure (buildDataFrames 100)) $ \payload ->
            bench "reader reuse (ring + StreamingReader)" $
              nfIO (readerDecodeNReuse ring payload 100)
        , env (pure (buildDataFrames 100)) $ \payload ->
            bench "wireform-parser reuse (ring + frameParser)" $
              nfIO (transportDecodeNReuse ring payload 100)
        ]
    , bgroup "1000 small DATA frames"
        [ env (pure (buildDataFrames 1000)) $ \payload ->
            bench "classic (RecvBuffer + decodeFrameHeader/Payload)" $
              nfIO (classicDecodeN payload 1000)
        , env (pure (buildDataFrames 1000)) $ \payload ->
            bench "reader reuse (ring + StreamingReader)" $
              nfIO (readerDecodeNReuse ring payload 1000)
        , env (pure (buildDataFrames 1000)) $ \payload ->
            bench "wireform-parser reuse (ring + frameParser)" $
              nfIO (transportDecodeNReuse ring payload 1000)
        ]
    , bgroup "100 big DATA frames (1 KiB)"
        [ env (pure (buildBigDataFrames 100)) $ \payload ->
            bench "classic (RecvBuffer + decodeFrameHeader/Payload)" $
              nfIO (classicDecodeN payload 100)
        , env (pure (buildBigDataFrames 100)) $ \payload ->
            bench "reader reuse (ring + StreamingReader)" $
              nfIO (readerDecodeNReuse ring payload 100)
        , env (pure (buildBigDataFrames 100)) $ \payload ->
            bench "wireform-parser reuse (ring + frameParser)" $
              nfIO (transportDecodeNReuse ring payload 100)
        ]
    ]
