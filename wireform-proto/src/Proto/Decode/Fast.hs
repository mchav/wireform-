{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Fast Addr#-based decoder: runs the entire decode loop using raw
pointer reads with a single GC touch at the end.

The standard Decoder uses ByteString + offset, which means every
byte read goes through BSU.unsafeIndex -> withForeignPtr -> touch#.

This module extracts the @Addr#@ once, does all reads via
@indexWord8OffAddr#@ (zero GC interaction), then @touch#@ once.
Matches hyperpb's P1.PtrAddr approach.

@
import Proto.Decode.Fast (runFastDecode, fdVarint)

case runFastDecode fdVarint bytes of
  Right val -> print val
  Left err  -> putStrLn (show err)
@
-}
module Proto.Decode.Fast (
  -- * Running
  runFastDecode,

  -- * Primitives for building fast decoders
  FastDec (..),
  readByte,
  fdVarint,
  fdFixed32,
  fdFixed64,
  fdFloat,
  fdDouble,
  fdBool,
  fdBytes,
  fdText,
  fdTag,
  fdTagCPS,
  fdSkipField,
  fdDone,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (castPtr)
import GHC.Exts (
  ByteArray#,
  Int (..),
  Int#,
  Ptr (..),
  copyAddrToByteArray#,
  indexWord32OffAddr#,
  indexWord64OffAddr#,
  indexWord8OffAddr#,
  newByteArray#,
  plusAddr#,
  runRW#,
  touch#,
  unsafeFreezeByteArray#,
 )
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents)
import GHC.Word (Word32 (..), Word64 (..), Word8 (..))
import Proto.Wire.Decode (DecodeError (..))


{- | Fast decoder state: raw 'Addr#' + length.
Uses unboxed 'Addr#' so reads compile to single load instructions
with no 'IO' round-trip.
-}
data FastDec = FastDec
  { fdAddr :: {-# UNPACK #-} !(Ptr Word8)
  , fdLen :: {-# UNPACK #-} !Int
  }


{- | Run a fast decoder on a ByteString.
Extracts the pointer once, runs the pure decode loop, then
touches the ForeignPtr to keep the buffer alive. No 'withForeignPtr'
bracket — the decode loop itself is pure.
-}
runFastDecode
  :: ByteString
  -> (FastDec -> Int -> Either DecodeError (a, Int))
  -> Either DecodeError a
runFastDecode (BSI.BS (ForeignPtr addr# contents) len) decode =
  let !fd = FastDec (Ptr addr#) len
      !result = decode fd 0
      !out = case result of
        Right (a, off)
          | off == len -> Right a
          | otherwise -> Left ExtraBytes
        Left e -> Left e
  in -- touch# keeps the ForeignPtr alive without entering IO.
     -- runRW# is cheaper than unsafeDupablePerformIO.
     case runRW# (touch# contents) of _ -> out
{-# INLINE runFastDecode #-}


{- | Read a byte at offset. Pure — compiles to a single
@indexWord8OffAddr#@ instruction with no IO wrapper.
-}
readByte :: FastDec -> Int -> Word8
readByte (FastDec ptr _) (I# off#) =
  case ptr of Ptr addr# -> W8# (indexWord8OffAddr# addr# off#)
{-# INLINE readByte #-}


readWord32 :: FastDec -> Int -> Word32
readWord32 (FastDec (Ptr addr#) _) (I# off#) =
  W32# (indexWord32OffAddr# (addr# `plusAddr#` off#) 0#)
{-# INLINE readWord32 #-}


readWord64 :: FastDec -> Int -> Word64
readWord64 (FastDec (Ptr addr#) _) (I# off#) =
  W64# (indexWord64OffAddr# (addr# `plusAddr#` off#) 0#)
{-# INLINE readWord64 #-}


{- | Decode a varint. Reads a Word64 in one shot and extracts
the value using bit manipulation — one memory access for
varints up to 8 bytes, no per-byte branching.

For 1-byte varints (the common case: field tags, bool values,
small integers), this is: one load + one mask + one compare.
-}
fdVarint :: FastDec -> Int -> (Word64, Int)
fdVarint fd !off
  -- If we have at least 8 bytes remaining, use the fast bulk-read path.
  -- This avoids per-byte bounds checks entirely.
  | off + 8 <= fdLen fd = fdVarintBulk fd off
  -- Otherwise fall back to byte-at-a-time (near end of buffer).
  | off < fdLen fd = fdVarintSafe fd off
  | otherwise = error "varint: eof"
{-# INLINE fdVarint #-}


{- | Bulk varint decode: read 8 bytes as a Word64, extract varint
with bit ops. One memory access, minimal branching.
-}
fdVarintBulk :: FastDec -> Int -> (Word64, Int)
fdVarintBulk fd !off =
  let !w = readWord64 fd off
      -- For a 1-byte varint (high bit clear), value is just the low 7 bits.
      !b0 = w .&. 0xFF
  in if b0 < 0x80
      then (b0, off + 1)
      else
        let
          -- Check how many continuation bytes by scanning for first byte < 0x80.
          -- Each varint byte has bit 7 set except the last.
          -- We want to find the first byte with bit 7 clear.
          !b1 = (w `shiftR` 8) .&. 0xFF
        in
          if b1 < 0x80
            then ((b0 .&. 0x7F) .|. (b1 `shiftL` 7), off + 2)
            else
              let !b2 = (w `shiftR` 16) .&. 0xFF
              in if b2 < 0x80
                  then
                    ( (b0 .&. 0x7F)
                        .|. ((b1 .&. 0x7F) `shiftL` 7)
                        .|. (b2 `shiftL` 14)
                    , off + 3
                    )
                  else
                    let !b3 = (w `shiftR` 24) .&. 0xFF
                    in if b3 < 0x80
                        then
                          ( (b0 .&. 0x7F)
                              .|. ((b1 .&. 0x7F) `shiftL` 7)
                              .|. ((b2 .&. 0x7F) `shiftL` 14)
                              .|. (b3 `shiftL` 21)
                          , off + 4
                          )
                        else fdVarintSlow fd off
{-# INLINE fdVarintBulk #-}


-- | Safe varint decode near buffer end (< 8 bytes remaining).
fdVarintSafe :: FastDec -> Int -> (Word64, Int)
fdVarintSafe fd !off =
  let !b0 = fromIntegral (readByte fd off) :: Word64
  in if b0 < 0x80
      then (b0, off + 1)
      else
        if off + 1 >= fdLen fd
          then error "varint: eof"
          else
            let !b1 = fromIntegral (readByte fd (off + 1)) :: Word64
            in if b1 < 0x80
                then ((b0 .&. 0x7F) .|. (b1 `shiftL` 7), off + 2)
                else
                  if off + 2 >= fdLen fd
                    then error "varint: eof"
                    else
                      let !b2 = fromIntegral (readByte fd (off + 2)) :: Word64
                      in if b2 < 0x80
                          then
                            ( (b0 .&. 0x7F)
                                .|. ((b1 .&. 0x7F) `shiftL` 7)
                                .|. (b2 `shiftL` 14)
                            , off + 3
                            )
                          else fdVarintSlow fd off
{-# INLINE fdVarintSafe #-}


fdVarintSlow :: FastDec -> Int -> (Word64, Int)
fdVarintSlow fd = go 0 0
  where
    go !acc !shift !pos
      | shift > 63 = error "varint: overflow"
      | pos >= fdLen fd = error "varint: eof"
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


{- | Get bytes range as a ByteString slice of the original input.
Zero-copy: just adjusts offset/length.
-}
fdBytes :: FastDec -> Int -> ByteString -> (ByteString, Int)
fdBytes fd !off origBs =
  case fdVarint fd off of
    (blen, off') ->
      let !bl = fromIntegral blen
      in (BSU.unsafeTake bl (BSU.unsafeDrop off' origBs), off' + bl)
{-# INLINE fdBytes #-}


{- | Get a Text field (length-delimited + UTF-8).

Constructs a 'Text' directly from the input pointer via
'copyAddrToByteArray#' — no intermediate 'ByteString', no
UTF-8 validation (protobuf encoder guarantees valid UTF-8).
On text-2.x this is safe because Text stores raw UTF-8 in a
'ByteArray#'.
-}
fdText :: FastDec -> Int -> ByteString -> (Text, Int)
fdText fd !off _origBs =
  case fdVarint fd off of
    (blen, off') ->
      let !bl = fromIntegral blen
      in if bl == 0
          then (mempty, off')
          else
            let !t = textFromAddr fd off' bl
            in (t, off' + bl)
{-# INLINE fdText #-}


{- | Construct a Text directly from an Addr# region.
Uses raw GHC primops: newByteArray# → copyAddrToByteArray# →
unsafeFreezeByteArray#. One allocation, one memcpy, no
intermediate types, no UTF-8 validation.
-}
textFromAddr :: FastDec -> Int -> Int -> Text
textFromAddr (FastDec (Ptr addr#) _) (I# off#) (I# len#) =
  case runRW#
    ( \s0 ->
        case newByteArray# len# s0 of
          (# s1, mba# #) ->
            case copyAddrToByteArray# (addr# `plusAddr#` off#) mba# 0# len# s1 of
              s2 ->
                case unsafeFreezeByteArray# mba# s2 of
                  (# s3, ba# #) ->
                    (# s3, ba# #)
    ) of
    (# _, ba# #) -> TI.Text (TA.ByteArray ba#) 0 (I# len#)
{-# INLINE textFromAddr #-}


-- | Decode a tag. Returns (fieldNumber, wireType, newOffset).
fdTag :: FastDec -> Int -> (Int, Int, Int)
fdTag fd !off =
  case fdVarint fd off of
    (w, off') -> (fromIntegral (w `shiftR` 3), fromIntegral (w .&. 7), off')
{-# INLINE fdTag #-}


{- | CPS version of fdTag that avoids tuple allocation.
The continuation receives (fieldNumber, wireType, newOffset) as
separate arguments — GHC passes them in registers.
-}
fdTagCPS :: FastDec -> Int -> (Int -> Int -> Int -> a) -> a
fdTagCPS fd !off k =
  case fdVarint fd off of
    (w, off') -> k (fromIntegral (w `shiftR` 3)) (fromIntegral (w .&. 7)) off'
{-# INLINE fdTagCPS #-}


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
