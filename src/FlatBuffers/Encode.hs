{-# LANGUAGE BangPatterns #-}
-- | FlatBuffers binary encoding.
--
-- FlatBuffers builds the buffer back-to-front. We use a two-pass approach:
-- 1. Compute sizes and offsets
-- 2. Write into pre-allocated buffer using directEncode
--
-- Buffer layout (front to back):
--   [root_offset (4 bytes LE)] [vtables...] [tables/strings/vectors...]
module FlatBuffers.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Proto.Encode.Direct (directEncode)
import qualified FlatBuffers.Value as F

encode :: F.Value -> ByteString
encode !val =
  let !sz = 4 + totalSize val
      !buf = directEncode sz (\p off -> do
               let !rootOff = (4 :: Word32)
               _ <- writeLE32 p off rootOff
               writeValueAt p (off + 4) val)
  in buf
{-# NOINLINE encode #-}

totalSize :: F.Value -> Int
totalSize = \case
  F.VBool _    -> 1
  F.VInt8 _    -> 1
  F.VInt16 _   -> 2
  F.VInt32 _   -> 4
  F.VInt64 _   -> 8
  F.VWord8 _   -> 1
  F.VWord16 _  -> 2
  F.VWord32 _  -> 4
  F.VWord64 _  -> 8
  F.VFloat _   -> 4
  F.VDouble _  -> 8
  F.VString t  ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    in 4 + len + 1 + padding (4 + len + 1)
  F.VVector vs ->
    let !elemSz = V.foldl' (\acc v -> acc + totalSize v) 0 vs
    in 4 + elemSz
  F.VTable fields ->
    let !nFields = V.length fields
        !vtableSize = 4 + 2 * nFields
        !vtablePadded = vtableSize + padding vtableSize
        !inlineSize = V.foldl' (\acc mf -> case mf of
                Nothing -> acc
                Just v  -> acc + fieldInlineSize v) 0 fields
        !tableSz = 4 + inlineSize
        !tablePadded = tableSz + padding tableSz
        !contentSz = V.foldl' (\acc mf -> case mf of
                Nothing -> acc
                Just v  -> acc + fieldContentSize v) 0 fields
    in vtablePadded + tablePadded + contentSz
  F.VStruct vs ->
    V.foldl' (\acc v -> acc + totalSize v) 0 vs

fieldInlineSize :: F.Value -> Int
fieldInlineSize = \case
  F.VBool _    -> 1
  F.VInt8 _    -> 1
  F.VInt16 _   -> 2
  F.VInt32 _   -> 4
  F.VInt64 _   -> 8
  F.VWord8 _   -> 1
  F.VWord16 _  -> 2
  F.VWord32 _  -> 4
  F.VWord64 _  -> 8
  F.VFloat _   -> 4
  F.VDouble _  -> 8
  F.VString _  -> 4
  F.VVector _  -> 4
  F.VTable _   -> 4
  F.VStruct vs -> V.foldl' (\acc v -> acc + fieldInlineSize v) 0 vs

fieldContentSize :: F.Value -> Int
fieldContentSize = \case
  F.VString t ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
        !raw = 4 + len + 1
    in raw + padding raw
  F.VVector vs ->
    4 + V.foldl' (\acc v -> acc + totalSize v) 0 vs
  F.VTable _ -> totalSize (F.VTable V.empty)
  _ -> 0

padding :: Int -> Int
padding n = (4 - (n `mod` 4)) `mod` 4

writeLE16 :: Ptr Word8 -> Int -> Word16 -> IO Int
writeLE16 p off w = do
  pokeByteOff p off       (fromIntegral (w .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pure $! off + 2
{-# INLINE writeLE16 #-}

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

writePadding :: Ptr Word8 -> Int -> Int -> IO Int
writePadding p off n = do
  mapM_ (\i -> pokeByteOff p (off + i) (0x00 :: Word8)) [0 .. n - 1]
  pure $! off + n

writeValueAt :: Ptr Word8 -> Int -> F.Value -> IO Int
writeValueAt p off = \case
  F.VBool b -> do
    pokeByteOff p off (if b then 0x01 else 0x00 :: Word8)
    pure $! off + 1

  F.VInt8 n -> do
    pokeByteOff p off (fromIntegral n :: Word8)
    pure $! off + 1

  F.VInt16 n -> writeLE16 p off (fromIntegral n)

  F.VInt32 n -> writeLE32 p off (fromIntegral n)

  F.VInt64 n -> writeLE64 p off (fromIntegral n)

  F.VWord8 n -> do
    pokeByteOff p off n
    pure $! off + 1

  F.VWord16 n -> writeLE16 p off n

  F.VWord32 n -> writeLE32 p off n

  F.VWord64 n -> writeLE64 p off n

  F.VFloat f -> writeLE32 p off (castFloatToWord32 f)

  F.VDouble d -> writeLE64 p off (castDoubleToWord64 d)

  F.VString t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    off1 <- writeLE32 p off (fromIntegral len)
    off2 <- writeRawBytes p off1 bs
    pokeByteOff p off2 (0x00 :: Word8)
    let !raw = 4 + len + 1
        !pad = padding raw
    writePadding p (off2 + 1) pad

  F.VVector vs -> do
    let !cnt = V.length vs
    off1 <- writeLE32 p off (fromIntegral cnt)
    V.foldM' (\o v -> writeValueAt p o v) off1 vs

  F.VTable fields -> do
    let !nFields = V.length fields
        !vtableByteSize = 4 + 2 * nFields
        !vtablePad = padding vtableByteSize
        !inlineSize = V.foldl' (\acc mf -> case mf of
                Nothing -> acc
                Just v  -> acc + fieldInlineSize v) 0 fields
        !tableByteSize = 4 + inlineSize
        !tablePad = padding tableByteSize

    -- Write vtable: vtable_size (u16), table_size (u16), field offsets (u16 each)
    off1 <- writeLE16 p off (fromIntegral vtableByteSize)
    off2 <- writeLE16 p off1 (fromIntegral tableByteSize)
    let writeFieldOff !o !fieldIdx !curOff
          | fieldIdx >= nFields = pure o
          | otherwise = case fields V.! fieldIdx of
              Nothing -> do
                o1 <- writeLE16 p o 0
                writeFieldOff o1 (fieldIdx + 1) curOff
              Just v -> do
                o1 <- writeLE16 p o (fromIntegral curOff)
                writeFieldOff o1 (fieldIdx + 1) (curOff + fieldInlineSize v)
    off3 <- writeFieldOff off2 0 (4 :: Int)
    off4 <- writePadding p off3 vtablePad

    -- Write table: soffset to vtable, then inline fields
    let !vtableOff = off
        !tableStart = off4
        !soff = tableStart - vtableOff :: Int
    off5 <- writeLE32 p off4 (fromIntegral soff)

    -- Write inline fields + content area
    let !contentStart = off5 + inlineSize + tablePad
    writeTableFields p off5 contentStart fields nFields 0

  F.VStruct vs ->
    V.foldM' (\o v -> writeValueAt p o v) off vs

writeTableFields :: Ptr Word8 -> Int -> Int -> V.Vector (Maybe F.Value) -> Int -> Int -> IO Int
writeTableFields p inlineOff contentOff fields nFields idx
  | idx >= nFields = do
      let !tablePad = padding 4
      _ <- writePadding p inlineOff tablePad
      pure contentOff
  | otherwise = case fields V.! idx of
      Nothing ->
        writeTableFields p inlineOff contentOff fields nFields (idx + 1)
      Just v -> do
        case v of
          F.VString _ -> do
            let !relOff = contentOff - inlineOff
            off1 <- writeLE32 p inlineOff (fromIntegral relOff)
            contentOff' <- writeValueAt p contentOff v
            writeTableFields p off1 contentOff' fields nFields (idx + 1)
          F.VVector _ -> do
            let !relOff = contentOff - inlineOff
            off1 <- writeLE32 p inlineOff (fromIntegral relOff)
            contentOff' <- writeValueAt p contentOff v
            writeTableFields p off1 contentOff' fields nFields (idx + 1)
          _ -> do
            off1 <- writeValueAt p inlineOff v
            writeTableFields p off1 contentOff fields nFields (idx + 1)
