{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module Wireform.Parser.Position
  ( Pos (..), Span (..), subPos
  , getPos, withSpan, byteStringOf, spanToByteString
  , inSpan
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

getPos :: Parser m e Pos
getPos = Parser \env eob s st ->
  case curToPos env s st of
    (# st', pos #) -> (# st', OK# (Pos pos) s #)
{-# INLINE getPos #-}

withSpan :: Parser m e a -> (a -> Span -> Parser m e b) -> Parser m e b
withSpan (Parser p) f = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# a s' #) ->
      case curToPos env s st' of
        (# st'', startPos #) ->
          case curToPos env s' st'' of
            (# st''', endPos #) ->
              let !sp = Span (Pos startPos) (Pos endPos)
              in runParser# (f a sp) env eob s' st'''
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withSpan #-}

byteStringOf :: Parser m e a -> Parser m e ByteString
byteStringOf (Parser p) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# _ s' #) ->
      let !len = I# (minusAddr# s' s)
          !bs  = BSI.BS (ForeignPtr s (peBackingFp env)) len
      in (# st', OK# bs s' #)
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE byteStringOf #-}

spanToByteString :: Span -> Parser m e ByteString
spanToByteString (Span (Pos start) (Pos end)) = Parser \env eob s st ->
  let !len = fromIntegral (end - start)
      base = peBaseAddr env
      mask = peMask env
      off  = fromIntegral start .&. mask
      !(Ptr ptr) = base `plusPtr` off
      !bs  = unsafeDupablePerformIO (BSI.create len \dst -> BSI.memcpy dst (Ptr ptr) len)
  in (# st, OK# bs s #)

-- | Run a parser within an explicitly bounded byte window.
-- Temporarily restricts @eob@ to the span's end position, then
-- restores it on completion.
inSpan :: Span -> Parser m e a -> Parser m e a
inSpan (Span (Pos start) (Pos end)) (Parser p) = Parser \env eob s st ->
  let base = peBaseAddr env
      mask = peMask env
      !endOff  = fromIntegral end .&. mask
      !(Ptr spanEnd) = base `plusPtr` endOff
      !startOff = fromIntegral start .&. mask
      !(Ptr spanStart) = base `plusPtr` startOff
  in case p env spanEnd spanStart st of
       (# st', OK# a _ #) -> (# st', OK# a s #)
       (# st', x #)       -> (# st', unsafeCoerce# x #)
{-# INLINE inSpan #-}
