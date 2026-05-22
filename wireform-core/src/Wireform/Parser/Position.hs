{-# LANGUAGE BlockArguments #-}

module Wireform.Parser.Position
  ( Pos (..)
  , Span (..)
  , subPos
  , getPos
  , setPos
  , withSpan
  , byteStringOf
  , spanToByteString
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, castPtr)

import Wireform.Parser.Internal

-- | Absolute position in the byte stream (monotonically increasing).
newtype Pos = Pos { unPos :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

-- | A contiguous span in the byte stream.
data Span = Span !Pos !Pos
  deriving stock (Eq, Show)

-- | Number of bytes between two positions.
subPos :: Pos -> Pos -> Int
subPos (Pos a) (Pos b) = fromIntegral (a - b)

-- | Get the current parser position.
getPos :: Parser e Pos
getPos = Parser \tag env cur ->
  pure (OK (Pos (curToPos env cur)) cur)
{-# INLINE getPos #-}

-- | Jump to a previously-recorded position.
setPos :: Pos -> Parser e ()
setPos (Pos target) = Parser \tag env _cur -> do
  let ringBase = peBaseAddr env
      mask     = peMask env
      offset   = fromIntegral target .&. mask
      newCur   = ringBase `plusPtr` offset
  end <- readIORef (peEndRef env)
  if newCur `minusPtr` ringBase < 0
    then pure Fail
    else pure (OK () newCur)
{-# INLINE setPos #-}

-- | Run a parser and capture the span of bytes it consumed.
withSpan :: Parser e a -> (a -> Span -> Parser e b) -> Parser e b
withSpan p f = Parser \tag env cur -> do
  let startPos = Pos (curToPos env cur)
  r <- unParser p tag env cur
  case r of
    OK a cur' -> do
      let endPos = Pos (curToPos env cur')
      unParser (f a (Span startPos endPos)) tag env cur'
    Fail  -> pure Fail
    Err e -> pure (Err e)
{-# INLINE withSpan #-}

-- | Run a parser and return a copy of the bytes it consumed.
byteStringOf :: Parser e a -> Parser e ByteString
byteStringOf p = Parser \tag env cur -> do
  r <- unParser p tag env cur
  case r of
    OK _ cur' -> do
      let !len = cur' `minusPtr` cur
      bs <- copyFromRing cur len
      pure (OK bs cur')
    Fail  -> pure Fail
    Err e -> pure (Err e)
{-# INLINE byteStringOf #-}

-- | Copy bytes from a span (must still be in the ring window).
spanToByteString :: Span -> Parser e ByteString
spanToByteString (Span (Pos s) (Pos e)) = Parser \tag env cur -> do
  let !len  = fromIntegral (e - s)
      base  = peBaseAddr env
      mask  = peMask env
      off   = fromIntegral s .&. mask
      ptr   = base `plusPtr` off
  bs <- copyFromRing ptr len
  pure (OK bs cur)

copyFromRing :: Ptr Word8 -> Int -> IO ByteString
copyFromRing src len = BSI.create len (\dst -> BSI.memcpy dst (castPtr src) len)
