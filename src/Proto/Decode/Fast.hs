{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
-- | Fast Addr#-based decoder: runs the entire decode loop using raw
-- pointer reads with a single GC touch at the end.
--
-- The standard Decoder uses ByteString + offset, which means every
-- byte read goes through BSU.unsafeIndex -> withForeignPtr -> touch#.
--
-- This module extracts the Addr# once, does all reads via
-- indexWord8OffAddr# (zero GC interaction), then touch# once.
-- Matches hyperpb's P1.PtrAddr approach.
module Proto.Decode.Fast
  ( -- * Running
    runFastDecode

    -- * Primitives for building fast decoders
  , FastDec(..)
  , fdVarint
  , fdFixed32
  , fdFixed64
  , fdFloat
  , fdDouble
  , fdBool
  , fdBytes
  , fdText
  , fdTag
  , fdSkipField
  , fdDone
  , fdFail
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)
import GHC.Exts
  ( Addr#, Int#, Int(..)
  , indexWord8OffAddr#
  , (+#), (>=#), isTrue#
  , touch#, realWorld#
  )
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.Word (Word8(..))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek, peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Proto.Wire.Decode (DecodeError(..))

-- | Fast decoder state: raw address, length, current offset.
-- All passed as arguments, no heap allocation.
data FastDec = FastDec
  { fdAddr   :: {-# UNPACK #-} !(Ptr Word8)
  , fdLen    :: {-# UNPACK #-} !Int
  }

-- | Run a fast decoder on a ByteString.
-- Extracts the pointer once, runs the decoder, keeps the ForeignPtr alive.
runFastDecode
  :: ByteString
  -> (FastDec -> Int -> Either DecodeError (a, Int))
  -> Either DecodeError a
runFastDecode (BSI.BS fp len) decode = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr -> do
    let !fd = FastDec (castPtr ptr) len
        !result = decode fd 0
    pure $! case result of
      Right (a, off)
        | off == len -> Right a
        | otherwise  -> Left ExtraBytes
      Left e -> Left e
{-# INLINE runFastDecode #-}

-- | Read a byte at offset. No GC interaction — raw pointer read.
readByte :: FastDec -> Int -> Word8
readByte (FastDec ptr _) off = unsafeDupablePerformIO $ peekByteOff ptr off
{-# INLINE readByte #-}

readWord32 :: FastDec -> Int -> Word32
readWord32 (FastDec ptr _) off = unsafeDupablePerformIO $ peekByteOff ptr off
{-# INLINE readWord32 #-}

readWord64 :: FastDec -> Int -> Word64
readWord64 (FastDec ptr _) off = unsafeDupablePerformIO $ peekByteOff ptr off
{-# INLINE readWord64 #-}

-- | Decode a varint. 4-byte inline fast path. Returns (value, newOffset).
fdVarint :: FastDec -> Int -> (Word64, Int)
fdVarint fd !off
  | off >= fdLen fd = error "varint: eof"
  | otherwise =
    let !b0 = fromIntegral (readByte fd off) :: Word64
    in if b0 < 0x80 then (b0, off + 1)
    else if off + 1 >= fdLen fd then error "varint: eof"
    else
      let !b1 = fromIntegral (readByte fd (off + 1)) :: Word64
      in if b1 < 0x80 then ((b0 .&. 0x7F) .|. (b1 `shiftL` 7), off + 2)
      else if off + 2 >= fdLen fd then error "varint: eof"
      else
        let !b2 = fromIntegral (readByte fd (off + 2)) :: Word64
        in if b2 < 0x80
           then ((b0 .&. 0x7F) .|. ((b1 .&. 0x7F) `shiftL` 7)
                   .|. (b2 `shiftL` 14), off + 3)
           else if off + 3 >= fdLen fd then error "varint: eof"
           else
             let !b3 = fromIntegral (readByte fd (off + 3)) :: Word64
             in if b3 < 0x80
                then ((b0 .&. 0x7F) .|. ((b1 .&. 0x7F) `shiftL` 7)
                        .|. ((b2 .&. 0x7F) `shiftL` 14) .|. (b3 `shiftL` 21), off + 4)
                else fdVarintSlow fd off
{-# INLINE fdVarint #-}

fdVarintSlow :: FastDec -> Int -> (Word64, Int)
fdVarintSlow fd = go 0 0
  where
    go !acc !shift !pos
      | shift > 63           = error "varint: overflow"
      | pos >= fdLen fd      = error "varint: eof"
      | otherwise =
          let !b = fromIntegral (readByte fd pos) :: Word64
              !val = acc .|. ((b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then (val, pos + 1)
             else go val (shift + 7) (pos + 1)

fdFixed32 :: FastDec -> Int -> (Word32, Int)
fdFixed32 fd !off
  | off + 4 > fdLen fd = error "fixed32: eof"
  | otherwise = (readWord32 fd off, off + 4)
{-# INLINE fdFixed32 #-}

fdFixed64 :: FastDec -> Int -> (Word64, Int)
fdFixed64 fd !off
  | off + 8 > fdLen fd = error "fixed64: eof"
  | otherwise = (readWord64 fd off, off + 8)
{-# INLINE fdFixed64 #-}

fdFloat :: FastDec -> Int -> (Float, Int)
fdFloat fd !off = case fdFixed32 fd off of
  (w, off') -> (castWord32ToFloat w, off')
{-# INLINE fdFloat #-}

fdDouble :: FastDec -> Int -> (Double, Int)
fdDouble fd !off = case fdFixed64 fd off of
  (w, off') -> (castWord64ToDouble w, off')
{-# INLINE fdDouble #-}

fdBool :: FastDec -> Int -> (Bool, Int)
fdBool fd !off = case fdVarint fd off of
  (v, off') -> (v /= 0, off')
{-# INLINE fdBool #-}

-- | Get bytes range as a ByteString slice of the original input.
-- Zero-copy: just adjusts offset/length.
fdBytes :: FastDec -> Int -> ByteString -> (ByteString, Int)
fdBytes fd !off origBs =
  case fdVarint fd off of
    (blen, off') ->
      let !bl = fromIntegral blen
      in (BSU.unsafeTake bl (BSU.unsafeDrop off' origBs), off' + bl)
{-# INLINE fdBytes #-}

-- | Get a Text field (length-delimited + UTF-8).
fdText :: FastDec -> Int -> ByteString -> (Text, Int)
fdText fd !off origBs =
  case fdBytes fd off origBs of
    (bs, off') -> (TE.decodeUtf8Lenient bs, off')
{-# INLINE fdText #-}

-- | Decode a tag. Returns (fieldNumber, wireType, newOffset).
fdTag :: FastDec -> Int -> (Int, Int, Int)
fdTag fd !off =
  case fdVarint fd off of
    (w, off') -> (fromIntegral (w `shiftR` 3), fromIntegral (w .&. 7), off')
{-# INLINE fdTag #-}

-- | Skip a field based on wire type. Returns new offset.
fdSkipField :: FastDec -> Int -> Int -> Int
fdSkipField fd !off !wt = case wt of
  0 -> snd (fdVarint fd off)
  1 -> off + 8
  2 -> case fdVarint fd off of
    (blen, off') -> off' + fromIntegral blen
  5 -> off + 4
  _ -> off
{-# INLINE fdSkipField #-}

-- | Check if we've reached end of input.
fdDone :: FastDec -> Int -> Bool
fdDone fd !off = off >= fdLen fd
{-# INLINE fdDone #-}

-- | Return an error.
fdFail :: DecodeError -> Either DecodeError a
fdFail = Left
{-# INLINE fdFail #-}
