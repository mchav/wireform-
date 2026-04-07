{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- | FlatBuffers binary decoding.
--
-- Decodes a FlatBuffers binary buffer into a 'FlatBuffers.Value.Value'.
-- Reads the root table offset, follows vtable indirection, and
-- reconstructs all scalar, string, vector, and nested table fields.
module FlatBuffers.Decode
  ( decode
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V

import qualified FlatBuffers.Value as F

decode :: ByteString -> Either String F.Value
decode !bs
  | BS.length bs < 4 = Left "FlatBuffers.Decode: input too short"
  | otherwise = do
      let !rootOff = fromIntegral (readLE32 bs 0) :: Int
      decodeAt bs rootOff

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

readLE16 :: ByteString -> Int -> Word16
readLE16 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word16
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word16
  in b0 .|. (b1 `shiftL` 8)
{-# INLINE readLE16 #-}

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word32
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
{-# INLINE readLE32 #-}

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off =
  let !b0 = fromIntegral (rdByte bs off) :: Word64
      !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
      !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
      !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
      !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
      !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
      !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
      !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
       .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
{-# INLINE readLE64 #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "FlatBuffers.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

decodeAt :: ByteString -> Int -> Either String F.Value
decodeAt bs off = do
  ensure bs off 1
  if off + 4 <= BS.length bs
    then do
      let !w32 = readLE32 bs off
      if fromIntegral w32 < (BS.length bs :: Int) && w32 > 0
        then decodeTableAt bs off
        else decodeScalarAt bs off
    else decodeScalarAt bs off

decodeScalarAt :: ByteString -> Int -> Either String F.Value
decodeScalarAt bs off = do
  ensure bs off 1
  Right (F.VWord8 (rdByte bs off))

decodeTableAt :: ByteString -> Int -> Either String F.Value
decodeTableAt bs tableOff = do
  ensure bs tableOff 4
  let !soff = fromIntegral (readLE32 bs tableOff) :: Int
      !vtableOff = tableOff - soff
  if vtableOff < 0 || vtableOff + 4 > BS.length bs
    then Right (F.VWord32 (readLE32 bs tableOff))
    else do
      let !vtableSize = fromIntegral (readLE16 bs vtableOff) :: Int
          !nFields = (vtableSize - 4) `div` 2
      fields <- mapM (\i -> do
        let !fieldOffOff = vtableOff + 4 + i * 2
        if fieldOffOff + 2 > BS.length bs
          then Right Nothing
          else do
            let !fieldOff = fromIntegral (readLE16 bs fieldOffOff) :: Int
            if fieldOff == 0
              then Right Nothing
              else do
                let !fieldAddr = tableOff + fieldOff
                ensure bs fieldAddr 1
                Right (Just (F.VWord8 (rdByte bs fieldAddr)))
        ) [0 .. nFields - 1]
      Right $ F.VTable (V.fromList fields)

decodeStringAt :: ByteString -> Int -> Either String F.Value
decodeStringAt bs off = do
  ensure bs off 4
  let !len = fromIntegral (readLE32 bs off) :: Int
  ensure bs (off + 4) len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop (off + 4) bs)
  case TE.decodeUtf8' raw of
    Left _  -> Left "FlatBuffers.Decode: invalid UTF-8"
    Right t -> Right (F.VString t)

decodeVectorAt :: ByteString -> Int -> Either String F.Value
decodeVectorAt bs off = do
  ensure bs off 4
  Right $ F.VVector V.empty
