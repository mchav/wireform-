{-# LANGUAGE BangPatterns #-}
-- | Cap'n Proto binary encoding.
--
-- Single-segment message format:
--   Segment table: 4 bytes (segment count - 1 = 0 as LE32) + 4 bytes (segment size in words as LE32)
--   Root struct pointer at word 0 of the segment.
module CapnProto.Encode
  ( encode
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int16, Int32)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Proto.Encode.Direct (directEncode)
import qualified CapnProto.Value as C

encode :: C.Value -> ByteString
encode !val =
  let !segWords = valueWords val
      !segBytes = segWords * 8
      !totalBytes = 8 + segBytes
  in directEncode totalBytes (\p off -> do
       off1 <- writeLE32 p off 0
       off2 <- writeLE32 p off1 (fromIntegral segWords)
       writeValue p off2 0 val)
{-# NOINLINE encode #-}

valueWords :: C.Value -> Int
valueWords = \case
  C.Void      -> 0
  C.Bool _    -> 1
  C.Int8 _    -> 1
  C.Int16 _   -> 1
  C.Int32 _   -> 1
  C.Int64 _   -> 1
  C.UInt8 _   -> 1
  C.UInt16 _  -> 1
  C.UInt32 _  -> 1
  C.UInt64 _  -> 1
  C.Float32 _ -> 1
  C.Float64 _ -> 1
  C.Enum _    -> 1
  C.Text t    ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs + 1
        !dataWords = (len + 7) `div` 8
    in 1 + dataWords
  C.Data bs   ->
    let !len = BS.length bs
        !dataWords = (len + 7) `div` 8
    in 1 + dataWords
  C.List vs   ->
    let !elemWords = V.foldl' (\acc v -> acc + valueWords v) 0 vs
    in 1 + elemWords
  C.Struct datas ptrs ->
    let !dataWords = V.length datas
        !ptrWords  = V.length ptrs
        !ptrContentWords = V.foldl' (\acc v -> acc + ptrContentSize v) 0 ptrs
    in max 1 dataWords + ptrWords + ptrContentWords

ptrContentSize :: C.Value -> Int
ptrContentSize = \case
  C.Text t    ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs + 1
    in (len + 7) `div` 8
  C.Data bs   ->
    let !len = BS.length bs
    in (len + 7) `div` 8
  C.List vs   ->
    V.foldl' (\acc v -> acc + valueWords v) 0 vs
  C.Struct datas ptrs ->
    let !dw = V.length datas
        !pw = V.length ptrs
        !pc = V.foldl' (\acc v -> acc + ptrContentSize v) 0 ptrs
    in dw + pw + pc
  _ -> 0

writeLE32 :: Ptr Word8 -> Int -> Word32 -> IO Int
writeLE32 p off w = do
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pure $! off + 4
{-# INLINE writeLE32 #-}

writeLE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
writeLE64 p off w = do
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral ((w `shiftR` 32) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral ((w `shiftR` 40) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral ((w `shiftR` 48) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral ((w `shiftR` 56) .&. 0xFF) :: Word8)
  pure $! off + 8
{-# INLINE writeLE64 #-}

writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}

padTo8 :: Ptr Word8 -> Int -> Int -> IO Int
padTo8 p off dataLen = do
  let !padded = ((dataLen + 7) `div` 8) * 8
      !padBytes = padded - dataLen
  mapM_ (\i -> pokeByteOff p (off + i) (0x00 :: Word8)) [0 .. padBytes - 1]
  pure $! off + padBytes
{-# INLINE padTo8 #-}

writeValue :: Ptr Word8 -> Int -> Int -> C.Value -> IO Int
writeValue p off _wordOff = \case
  C.Void -> pure off
  C.Bool b -> do
    pokeByteOff p off (if b then 0x01 else 0x00 :: Word8)
    padTo8 p (off + 1) 1

  C.Int8 n -> do
    pokeByteOff p off (fromIntegral n :: Word8)
    padTo8 p (off + 1) 1

  C.Int16 n -> do
    writeLE16Val p off n
    padTo8 p (off + 2) 2

  C.Int32 n -> do
    _ <- writeLE32 p off (fromIntegral n)
    padTo8 p (off + 4) 4

  C.Int64 n -> writeLE64 p off (fromIntegral n)

  C.UInt8 n -> do
    pokeByteOff p off n
    padTo8 p (off + 1) 1

  C.UInt16 n -> do
    writeLE16Val p off (fromIntegral n :: Int16)
    padTo8 p (off + 2) 2

  C.UInt32 n -> do
    _ <- writeLE32 p off n
    padTo8 p (off + 4) 4

  C.UInt64 n -> writeLE64 p off n

  C.Float32 f -> do
    _ <- writeLE32 p off (castFloatToWord32 f)
    padTo8 p (off + 4) 4

  C.Float64 d -> writeLE64 p off (castDoubleToWord64 d)

  C.Enum n -> do
    writeLE16Val p off (fromIntegral n :: Int16)
    padTo8 p (off + 2) 2

  C.Text t -> do
    let !bs = TE.encodeUtf8 t
        !bsLen = BS.length bs
        !totalLen = bsLen + 1
        !listPtr = makeListPtr 0 (fromIntegral totalLen) 2
    off1 <- writeLE64 p off listPtr
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    padTo8 p (off2 + 1) totalLen

  C.Data bs -> do
    let !bsLen = BS.length bs
        !listPtr = makeListPtr 0 (fromIntegral bsLen) 2
    off1 <- writeLE64 p off listPtr
    off2 <- writeRawBytes p off1 bs
    padTo8 p off2 bsLen

  C.List vs -> do
    let !cnt = V.length vs
        !listPtr = makeListPtr 0 (fromIntegral cnt) 0
    off1 <- writeLE64 p off listPtr
    V.foldM' (\o v -> writeValue p o 0 v) off1 vs

  C.Struct datas ptrs -> do
    let !dw = V.length datas
    V.ifoldM' (\o _i v -> writeDataField p o v) off datas
      >>= \offAfterData ->
        let !offPtrSection = if dw == 0 then offAfterData + 8 else offAfterData
        in do
          when (dw == 0) $ do
            _ <- writeLE64 p offAfterData 0
            pure ()
          V.ifoldM' (\o _i v -> writePtrField p o v) offPtrSection ptrs

writeDataField :: Ptr Word8 -> Int -> C.Value -> IO Int
writeDataField p off = \case
  C.Bool b -> do
    pokeByteOff p off (if b then 0x01 else 0x00 :: Word8)
    padTo8 p (off + 1) 1
  C.Int8 n -> do
    pokeByteOff p off (fromIntegral n :: Word8)
    padTo8 p (off + 1) 1
  C.Int16 n -> do
    writeLE16Val p off n
    padTo8 p (off + 2) 2
  C.Int32 n -> do
    _ <- writeLE32 p off (fromIntegral n)
    padTo8 p (off + 4) 4
  C.Int64 n -> writeLE64 p off (fromIntegral n)
  C.UInt8 n -> do
    pokeByteOff p off n
    padTo8 p (off + 1) 1
  C.UInt16 n -> do
    writeLE16Val p off (fromIntegral n :: Int16)
    padTo8 p (off + 2) 2
  C.UInt32 n -> do
    _ <- writeLE32 p off n
    padTo8 p (off + 4) 4
  C.UInt64 n -> writeLE64 p off n
  C.Float32 f -> do
    _ <- writeLE32 p off (castFloatToWord32 f)
    padTo8 p (off + 4) 4
  C.Float64 d -> writeLE64 p off (castDoubleToWord64 d)
  C.Enum n -> do
    writeLE16Val p off (fromIntegral n :: Int16)
    padTo8 p (off + 2) 2
  C.Void -> writeLE64 p off 0
  _ -> writeLE64 p off 0

writePtrField :: Ptr Word8 -> Int -> C.Value -> IO Int
writePtrField p off val = writeValue p off 0 val

when :: Bool -> IO () -> IO ()
when True  a = a
when False _ = pure ()

writeLE16Val :: Ptr Word8 -> Int -> Int16 -> IO ()
writeLE16Val p off n = do
  let !w = fromIntegral n :: Word16
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)

makeListPtr :: Int32 -> Word32 -> Word32 -> Word64
makeListPtr ptrOffset numElements elemSize =
  let !a = (fromIntegral ptrOffset :: Word64) .&. 0x3FFFFFFF
      !b = fromIntegral elemSize :: Word64
      !c = fromIntegral numElements :: Word64
  in 0x01 .|. (a `shiftL` 2) .|. (b `shiftL` 32) .|. (c `shiftL` 35)
