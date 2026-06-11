{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Cap'n Proto binary decoding.

Decodes a Cap'n Proto single-segment message from a 'ByteString'.
Reads the segment table, follows struct and list pointers, and
reconstructs a 'CapnProto.Value.Value' tree.
-}
module CapnProto.Decode (
  decode,
) where

import CapnProto.Value qualified as C
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)


decode :: ByteString -> Either String C.Value
decode !bs
  | BS.length bs < 8 = Left "CapnProto.Decode: input too short for segment table"
  | otherwise = do
      let !segCount = fromIntegral (readLE32 bs 0) + 1 :: Int
      if segCount /= 1
        then Left "CapnProto.Decode: only single-segment messages supported"
        else do
          let !segSizeWords = fromIntegral (readLE32 bs 4) :: Int
              !segStart = 8
              !segEnd = segStart + segSizeWords * 8
          if segEnd > BS.length bs
            then Left "CapnProto.Decode: segment truncated"
            else
              if segSizeWords == 0
                then Right C.Void
                else decodeValueAtWord bs segStart 0


rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}


withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $
    \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}


readLE16 :: ByteString -> Int -> Word16
readLE16 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE16 #-}


readLE32 :: ByteString -> Int -> Word32
readLE32 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE32 #-}


readLE64 :: ByteString -> Int -> Word64
readLE64 bs off = withBSPtrOff bs off $ \p -> peekByteOff p 0
{-# INLINE readLE64 #-}


decodeValueAtWord :: ByteString -> Int -> Int -> Either String C.Value
decodeValueAtWord bs segStart wordIdx = do
  let !byteOff = segStart + wordIdx * 8
  if byteOff + 8 > BS.length bs
    then Left "CapnProto.Decode: read past segment end"
    else do
      let !w = readLE64 bs byteOff
      if w == 0
        then Right C.Void
        else Right (C.UInt64 w)


decodeFromMsg :: ByteString -> Int -> Either String C.Value
decodeFromMsg bs segStart
  | segStart >= BS.length bs = Right C.Void
  | otherwise =
      let !w0 = readLE64 bs segStart
          !ptrType = w0 .&. 0x03
      in case ptrType of
           0 -> decodeStructPtr bs segStart w0
           1 -> decodeListPtr bs segStart w0
           _ -> Right (C.UInt64 w0)


decodeStructPtr :: ByteString -> Int -> Word64 -> Either String C.Value
decodeStructPtr bs segStart w0 = do
  let !ptrOff = fromIntegral (fromIntegral ((w0 `shiftR` 2) .&. 0x3FFFFFFF) :: Int32)
      !dataSz = fromIntegral ((w0 `shiftR` 32) .&. 0xFFFF) :: Int
      !ptrSz = fromIntegral ((w0 `shiftR` 48) .&. 0xFFFF) :: Int
      !structStart = segStart + 8 + ptrOff * 8
      !dataStart = structStart
      !ptrStart = structStart + dataSz * 8
  if dataSz == 0 && ptrSz == 0
    then Right C.Void
    else do
      datas <-
        mapM
          ( \i -> do
              let !bo = dataStart + i * 8
              if bo + 8 > BS.length bs
                then Left "CapnProto.Decode: data section out of bounds"
                else Right (C.UInt64 (readLE64 bs bo))
          )
          [0 .. dataSz - 1]
      ptrs <-
        mapM
          ( \i -> do
              let !bo = ptrStart + i * 8
              if bo + 8 > BS.length bs
                then Left "CapnProto.Decode: pointer section out of bounds"
                else decodeFromMsg bs bo
          )
          [0 .. ptrSz - 1]
      Right $ C.Struct (V.fromList datas) (V.fromList ptrs)


decodeListPtr :: ByteString -> Int -> Word64 -> Either String C.Value
decodeListPtr bs segStart w0 = do
  let !ptrOff = fromIntegral (fromIntegral ((w0 `shiftR` 2) .&. 0x3FFFFFFF) :: Int32)
      !elemSize = fromIntegral ((w0 `shiftR` 32) .&. 0x07) :: Int
      !numElems = fromIntegral ((w0 `shiftR` 35) .&. 0x1FFFFFFF) :: Int
      !dataStart = segStart + 8 + ptrOff * 8
  case elemSize of
    2 -> do
      if dataStart + numElems > BS.length bs
        then Left "CapnProto.Decode: byte list out of bounds"
        else do
          let !raw = BSU.unsafeTake numElems (BSU.unsafeDrop dataStart bs)
          if numElems > 0 && BSU.unsafeIndex raw (numElems - 1) == 0x00
            then case TE.decodeUtf8' (BS.init raw) of
              Left _ -> Right (C.Data raw)
              Right t -> Right (C.Text t)
            else Right (C.Data raw)
    _ -> do
      let !elems =
            map
              ( \i ->
                  let !bo = dataStart + i * 8
                      !val = readLE64 bs bo
                  in C.UInt64 val
              )
              [0 .. numElems - 1]
      Right $ C.List (V.fromList elems)
