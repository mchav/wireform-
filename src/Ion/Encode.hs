{-# LANGUAGE BangPatterns #-}
-- | Amazon Ion binary encoding.
--
-- Encodes an 'Ion.Value.Value' to Amazon Ion binary format. The output
-- begins with the Ion Binary Version Marker (BVM: @0xE0 0x01 0x00 0xEA@)
-- followed by the encoded value. Uses 'Proto.Encode.Direct.directEncode'
-- for direct buffer writes with pre-computed sizes.
module Ion.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import Proto.Encode.Direct (directEncode)
import qualified Ion.Value as I

encode :: I.Value -> ByteString
encode !val =
  let !sz = 4 + valueSize val
  in directEncode sz (\p off -> do
       off1 <- writeBVM p off
       writeValue p off1 val)
{-# NOINLINE encode #-}

writeBVM :: Ptr Word8 -> Int -> IO Int
writeBVM p off = do
  pokeByteOff p off       (0xE0 :: Word8)
  pokeByteOff p (off + 1) (0x01 :: Word8)
  pokeByteOff p (off + 2) (0x00 :: Word8)
  pokeByteOff p (off + 3) (0xEA :: Word8)
  pure $! off + 4

valueSize :: I.Value -> Int
valueSize = \case
  I.Null          -> 1
  I.Bool _        -> 1
  I.Int n
    | n == 0      -> 1
    | n > 0       -> 1 + magnitudeBytes (fromIntegral n)
    | otherwise   -> 1 + magnitudeBytes (fromIntegral (negate n))
  I.Float d
    | d == 0      -> 1
    | otherwise   -> 1 + 8
  I.String t      ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    in tdSize 8 len + len
  I.Blob bs       ->
    let !len = BS.length bs
    in tdSize 10 len + len
  I.Clob bs       ->
    let !len = BS.length bs
    in tdSize 9 len + len
  I.List vs       ->
    let !payloadSz = V.foldl' (\acc v -> acc + valueSize v) 0 vs
    in tdSize 11 payloadSz + payloadSz
  I.Struct fields ->
    let !payloadSz = V.foldl' (\acc (k, v) -> acc + fieldSize k v) 0 fields
    in tdSize 13 payloadSz + payloadSz
  I.Symbol t      ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    in tdSize 7 len + len
  I.Annotation ann inner ->
    let !annBs = TE.encodeUtf8 ann
        !annLen = BS.length annBs
        !annLenSz = varUIntSize annLen
        !innerSz = valueSize inner
        !wrapLen = varUIntSize (annLenSz + annLen) + annLenSz + annLen + innerSz
    in tdSize 14 wrapLen + wrapLen

fieldSize :: T.Text -> I.Value -> Int
fieldSize k v =
  let !kbs = TE.encodeUtf8 k
      !klen = BS.length kbs
  in varUIntSize klen + klen + valueSize v

tdSize :: Int -> Int -> Int
tdSize _typeCode len
  | len < 14   = 1
  | otherwise  = 1 + varUIntSize len

varUIntSize :: Int -> Int
varUIntSize n
  | n <= 0x7F       = 1
  | n <= 0x3FFF     = 2
  | n <= 0x1FFFFF   = 3
  | n <= 0x0FFFFFFF = 4
  | otherwise       = 5

magnitudeBytes :: Word64 -> Int
magnitudeBytes n
  | n <= 0xFF             = 1
  | n <= 0xFFFF           = 2
  | n <= 0xFFFFFF         = 3
  | n <= 0xFFFFFFFF       = 4
  | n <= 0xFFFFFFFFFF     = 5
  | n <= 0xFFFFFFFFFFFF   = 6
  | n <= 0xFFFFFFFFFFFFFF = 7
  | otherwise             = 8

writeTD :: Ptr Word8 -> Int -> Word8 -> Int -> IO Int
writeTD p off typeNibble len
  | len < 14 = do
      pokeByteOff p off ((typeNibble `shiftL` 4) .|. fromIntegral len :: Word8)
      pure $! off + 1
  | otherwise = do
      pokeByteOff p off ((typeNibble `shiftL` 4) .|. 0x0E :: Word8)
      writeVarUInt p (off + 1) len

writeVarUInt :: Ptr Word8 -> Int -> Int -> IO Int
writeVarUInt p off n = do
  let !sz = varUIntSize n
      -- Write bytes from last to first (big-endian VarUInt)
      -- Last byte has MSB=1 (stop bit), preceding bytes have MSB=0
      go !i !remaining
        | i < 0 = pure $! off + sz
        | i == sz - 1 = do
            pokeByteOff p (off + i) (fromIntegral (remaining .&. 0x7F .|. 0x80) :: Word8)
            go (i - 1) (remaining `shiftR` 7)
        | otherwise = do
            pokeByteOff p (off + i) (fromIntegral (remaining .&. 0x7F) :: Word8)
            go (i - 1) (remaining `shiftR` 7)
  go (sz - 1) n

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}

writeMagnitude :: Ptr Word8 -> Int -> Word64 -> Int -> IO Int
writeMagnitude p off mag nbytes = go off 0
  where
    go !o !i
      | i >= nbytes = pure o
      | otherwise = do
          let !byteIdx = nbytes - 1 - i
              !b = fromIntegral ((mag `shiftR` (byteIdx * 8)) .&. 0xFF) :: Word8
          pokeByteOff p o b
          go (o + 1) (i + 1)

writeValue :: Ptr Word8 -> Int -> I.Value -> IO Int
writeValue p off = \case
  I.Null -> do
    pokeByteOff p off (0x0F :: Word8)
    pure $! off + 1

  I.Bool b -> do
    pokeByteOff p off (if b then 0x11 else 0x10 :: Word8)
    pure $! off + 1

  I.Int n
    | n == 0 -> do
        pokeByteOff p off (0x20 :: Word8)
        pure $! off + 1
    | n > 0 -> do
        let !mag = fromIntegral n :: Word64
            !nb = magnitudeBytes mag
        off1 <- writeTD p off 0x02 nb
        writeMagnitude p off1 mag nb
    | otherwise -> do
        let !mag = fromIntegral (negate n) :: Word64
            !nb = magnitudeBytes mag
        off1 <- writeTD p off 0x03 nb
        writeMagnitude p off1 mag nb

  I.Float d
    | d == 0 -> do
        pokeByteOff p off (0x40 :: Word8)
        pure $! off + 1
    | otherwise -> do
        off1 <- writeTD p off 0x04 8
        let !w = castDoubleToWord64 d
        writeBE64 p off1 w

  I.String t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    off1 <- writeTD p off 0x08 len
    writeRawBytes p off1 bs

  I.Blob bs -> do
    let !len = BS.length bs
    off1 <- writeTD p off 0x0A len
    writeRawBytes p off1 bs

  I.Clob bs -> do
    let !len = BS.length bs
    off1 <- writeTD p off 0x09 len
    writeRawBytes p off1 bs

  I.List vs -> do
    let !payloadSz = V.foldl' (\acc v -> acc + valueSize v) 0 vs
    off1 <- writeTD p off 0x0B payloadSz
    V.foldM' (\o v -> writeValue p o v) off1 vs

  I.Struct fields -> do
    let !payloadSz = V.foldl' (\acc (k, v) -> acc + fieldSize k v) 0 fields
    off1 <- writeTD p off 0x0D payloadSz
    V.foldM' (\o (k, v) -> writeField p o k v) off1 fields

  I.Symbol t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    off1 <- writeTD p off 0x07 len
    writeRawBytes p off1 bs

  I.Annotation ann inner -> do
    let !annBs = TE.encodeUtf8 ann
        !annLen = BS.length annBs
        !annLenSz = varUIntSize annLen
        !innerSz = valueSize inner
        !wrapLen = varUIntSize (annLenSz + annLen) + annLenSz + annLen + innerSz
    off1 <- writeTD p off 0x0E wrapLen
    off2 <- writeVarUInt p off1 (annLenSz + annLen)
    off3 <- writeVarUInt p off2 annLen
    off4 <- writeRawBytes p off3 annBs
    writeValue p off4 inner

writeField :: Ptr Word8 -> Int -> T.Text -> I.Value -> IO Int
writeField p off k v = do
  let !kbs = TE.encodeUtf8 k
      !klen = BS.length kbs
  off1 <- writeVarUInt p off klen
  off2 <- writeRawBytes p off1 kbs
  writeValue p off2 v

writeBE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeBE64 p off w = do
  pokeByteOff p off       (fromIntegral (w `shiftR` 56) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 48) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 40) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 32) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral (w .&. 0xFF) :: Word8)
  pure $! off + 8
{-# INLINE writeBE64 #-}
