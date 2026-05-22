{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module Wireform.Parser.Position
  ( Pos (..), Span (..), subPos
  , getPos, withSpan, byteStringOf, spanToByteString
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr (..), plusPtr, minusPtr)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr (..))
import System.IO.Unsafe (unsafeDupablePerformIO)

import Wireform.Parser.Internal

newtype Pos = Pos { unPos :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

data Span = Span !Pos !Pos
  deriving stock (Eq, Show)

subPos :: Pos -> Pos -> Int
subPos (Pos a) (Pos b) = fromIntegral (a - b)

getPos :: Parser e Pos
getPos = Parser \tag env eob s st ->
  (# st, OK# (Pos (curToPos env s)) s #)
{-# INLINE getPos #-}

withSpan :: Parser e a -> (a -> Span -> Parser e b) -> Parser e b
withSpan (Parser p) f = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# a s' #) ->
      let !sp = Span (Pos (curToPos env s)) (Pos (curToPos env s'))
      in runParser# (f a sp) tag env eob s' st'
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withSpan #-}

byteStringOf :: Parser e a -> Parser e ByteString
byteStringOf (Parser p) = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# _ s' #) ->
      let !len = I# (minusAddr# s' s)
          !bs  = BSI.BS (ForeignPtr s (peBackingFp env)) len
      in (# st', OK# bs s' #)
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE byteStringOf #-}

spanToByteString :: Span -> Parser e ByteString
spanToByteString (Span (Pos start) (Pos end)) = Parser \tag env eob s st ->
  let !len = fromIntegral (end - start)
      base = peBaseAddr env
      mask = peMask env
      off  = fromIntegral start .&. mask
      !(Ptr ptr) = base `plusPtr` off
      !bs  = unsafeDupablePerformIO (BSI.create len \dst -> BSI.memcpy dst (Ptr ptr) len)
  in (# st, OK# bs s #)
