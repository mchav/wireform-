{-# LANGUAGE BlockArguments #-}

module Wireform.Parser.Adapter (
  -- * ChunkParser type
  ChunkParser (..),
  ChunkStep (..),
  ChunkFinal (..),
  ChunkParseError (..),

  -- * Running
  runChunked,
  runChunkedLoop,

  -- * Chunk mode
  ChunkMode (..),
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Wireform.Parser.Driver (LoopControl (..))
import Wireform.Ring.Internal (ringBase, ringMask, ringSize)
import Wireform.Transport


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

{- | A chunked-feeding parser.  The lingua franca for incremental
parser libraries (attoparsec, binary, cereal, etc.).
-}
data ChunkParser a = ChunkParser
  { stepChunk :: !(ByteString -> ChunkStep a)
  -- ^ Feed non-empty input.
  , stepEof :: !(ChunkFinal a)
  -- ^ Signal end of input.
  }


data ChunkStep a
  = -- | Consumed N bytes; needs more.
    ChunkConsumed {-# UNPACK #-} !Int !(ChunkParser a)
  | -- | Done; consumed N bytes of the fed chunk (rest is leftover).
    ChunkDone !a {-# UNPACK #-} !Int
  | ChunkFailed !ChunkParseError


data ChunkFinal a
  = FinalDone !a
  | FinalFailed !ChunkParseError


data ChunkParseError = ChunkParseError
  { chunkErrorMessage :: !String
  , chunkErrorContext :: ![String]
  , chunkErrorPosition :: {-# UNPACK #-} !Word64
  }
  deriving stock (Show)


------------------------------------------------------------------------
-- Chunk mode
------------------------------------------------------------------------

data ChunkMode
  = -- | Allocate a fresh 'ByteString' per chunk (safe, default).
    ChunkCopy
  | {- | Reference ring memory directly.  Caller must not retain the
    'ByteString' past the @stepChunk@ return.
    -}
    ChunkZeroCopy
  deriving stock (Eq, Show)


------------------------------------------------------------------------
-- Running
------------------------------------------------------------------------

-- | Run a 'ChunkParser' against a transport.
runChunked
  :: ReceiveTransport
  -> ChunkMode
  -> ChunkParser a
  -> IO (Either ChunkParseError a)
runChunked t mode cp0 = do
  startPos <- receiveLoadHead t
  loop cp0 startPos startPos startPos
  where
    ring = receiveRing t
    base = ringBase ring
    msk = ringMask ring
    sz = ringSize ring

    advanceThreshold = sz `div` 4

    loop cp parserStart parserPos lastTailAdvance = do
      h <- receiveLoadHead t
      if h > parserPos
        then do
          let !chunkLen = fromIntegral (min (h - parserPos) (fromIntegral sz))
              !off = fromIntegral parserPos .&. msk
              !ptr = base `plusPtr` off
          chunk <- makeChunk mode ptr chunkLen
          case stepChunk cp chunk of
            ChunkDone a consumed -> do
              let !newPos = parserPos + fromIntegral consumed
              receiveAdvanceTail t newPos
              pure (Right a)
            ChunkConsumed consumed cp' -> do
              let !newPos = parserPos + fromIntegral consumed
              if newPos - lastTailAdvance >= fromIntegral advanceThreshold
                then do
                  receiveAdvanceTail t newPos
                  loop cp' parserStart newPos newPos
                else loop cp' parserStart newPos lastTailAdvance
            ChunkFailed e -> pure (Left e)
        else do
          r <- receiveWaitData t parserPos
          case r of
            ReceiveMoreData _ ->
              loop cp parserStart parserPos lastTailAdvance
            ReceiveEndOfInput ->
              case stepEof cp of
                FinalDone a
                  | parserPos == parserStart -> pure (Right a)
                  | otherwise -> pure (Right a)
                FinalFailed e -> pure (Left e)
            ReceiveFailed exc ->
              pure (Left (ChunkParseError (show exc) [] parserPos))


-- | Loop variant for repeated parsing.
runChunkedLoop
  :: ReceiveTransport
  -> ChunkMode
  -> ChunkParser a
  -> (a -> IO LoopControl)
  -> IO (Either ChunkParseError ())
runChunkedLoop t mode mkParser k = loop
  where
    loop = do
      r <- runChunked t mode mkParser
      case r of
        Right a -> do
          ctl <- k a
          case ctl of
            Continue -> loop
            Stop -> pure (Right ())
        Left e -> pure (Left e)


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

makeChunk :: ChunkMode -> Ptr Word8 -> Int -> IO ByteString
makeChunk ChunkCopy ptr len =
  BSI.create len \dst -> copyBytes dst ptr len
makeChunk ChunkZeroCopy ptr len = do
  fptr <- newForeignPtr_ ptr
  pure (BSI.fromForeignPtr fptr 0 len)
