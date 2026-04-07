{-# LANGUAGE BangPatterns #-}
-- | Python Pickle protocol 2 encoder.
--
-- Encodes a 'Pickle.Value.Value' to Python Pickle protocol 2 wire format.
-- Emits the protocol 2 header (@0x80 0x02@) followed by opcodes and data,
-- terminated by the STOP opcode. Compatible with Python's @pickle.loads@.
--
-- Uses direct buffer writes via 'Proto.Encode.Direct.directEncode'.
module Pickle.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64)

import qualified Pickle.Value as P
import Proto.Encode.Direct (directEncode)

encode :: P.Value -> ByteString
encode val = directEncode (pickleSize val) (writePickle val)
{-# INLINE encode #-}

-- Size computation

pickleSize :: P.Value -> Int
pickleSize val = 2 + valueSize val + 1

valueSize :: P.Value -> Int
valueSize = \case
  P.None       -> 1
  P.Bool True  -> 1
  P.Bool False -> 1
  P.Int n      -> intSize n
  P.Float _    -> 9
  P.Bytes bs   -> bytesSize bs
  P.String t   -> stringSize t
  P.List vs    -> listSize vs
  P.Tuple vs   -> tupleSize vs
  P.Dict kvs   -> dictSize kvs
  P.Set vs     -> setSize vs

intSize :: Int64 -> Int
intSize n
  | n >= -128 && n <= 127       = 3
  | n >= -32768 && n <= 32767   = 4
  | n >= -2147483648 && n <= 2147483647 = 6
  | otherwise                   = 10

bytesSize :: ByteString -> Int
bytesSize bs
  | BS.length bs < 256 = 2 + BS.length bs
  | otherwise          = 5 + BS.length bs

stringSize :: T.Text -> Int
stringSize t =
  let !bs = TE.encodeUtf8 t
  in if BS.length bs < 256
     then 2 + BS.length bs
     else 5 + BS.length bs

listSize :: V.Vector P.Value -> Int
listSize vs
  | V.null vs = 1
  | otherwise = 1 + 1 + V.foldl' (\s v -> s + valueSize v) 0 vs + 1

tupleSize :: V.Vector P.Value -> Int
tupleSize vs
  | V.null vs = 1
  | V.length vs == 1 = valueSize (V.head vs) + 1
  | V.length vs == 2 = valueSize (vs V.! 0) + valueSize (vs V.! 1) + 1
  | V.length vs == 3 = valueSize (vs V.! 0) + valueSize (vs V.! 1) + valueSize (vs V.! 2) + 1
  | otherwise = 1 + V.foldl' (\s v -> s + valueSize v) 0 vs + 1

dictSize :: V.Vector (P.Value, P.Value) -> Int
dictSize kvs
  | V.null kvs = 1
  | otherwise = 1 + 1 + V.foldl' (\s (k, v) -> s + valueSize k + valueSize v) 0 kvs + 1

setSize :: V.Vector P.Value -> Int
setSize vs = 1 + V.foldl' (\s v -> s + valueSize v) 0 vs + 1

-- Offset-based writers

writePickle :: P.Value -> Ptr Word8 -> Int -> IO Int
writePickle val p off = do
  pokeByteOff p off (0x80 :: Word8)
  pokeByteOff p (off + 1) (0x02 :: Word8)
  off1 <- writeValue val p (off + 2)
  pokeByteOff p off1 (0x2E :: Word8)
  pure $! off1 + 1

writeValue :: P.Value -> Ptr Word8 -> Int -> IO Int
writeValue val p off = case val of
  P.None -> do pokeByteOff p off (0x4E :: Word8); pure $! off + 1
  P.Bool True -> do pokeByteOff p off (0x88 :: Word8); pure $! off + 1
  P.Bool False -> do pokeByteOff p off (0x89 :: Word8); pure $! off + 1
  P.Int n -> writeInt p off n
  P.Float d -> do
    pokeByteOff p off (0x47 :: Word8)
    writeFloat64BE p (off + 1) d
  P.Bytes bs -> writeBytes p off bs
  P.String t -> writeString p off t
  P.List vs -> writeList p off vs
  P.Tuple vs -> writeTuple p off vs
  P.Dict kvs -> writeDict p off kvs
  P.Set vs -> writeSet p off vs

writeInt :: Ptr Word8 -> Int -> Int64 -> IO Int
writeInt p off n
  | n >= -128 && n <= 127 = do
      pokeByteOff p off (0x8A :: Word8)
      pokeByteOff p (off + 1) (1 :: Word8)
      pokeByteOff p (off + 2) (fromIntegral n :: Word8)
      pure $! off + 3
  | n >= -32768 && n <= 32767 = do
      pokeByteOff p off (0x8A :: Word8)
      pokeByteOff p (off + 1) (2 :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n .&. 0xFF) :: Word8)
      pokeByteOff p (off + 3) (fromIntegral ((n `shiftR` 8) .&. 0xFF) :: Word8)
      pure $! off + 4
  | n >= -2147483648 && n <= 2147483647 = do
      pokeByteOff p off (0x8A :: Word8)
      pokeByteOff p (off + 1) (4 :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n .&. 0xFF) :: Word8)
      pokeByteOff p (off + 3) (fromIntegral ((n `shiftR` 8) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 4) (fromIntegral ((n `shiftR` 16) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 5) (fromIntegral ((n `shiftR` 24) .&. 0xFF) :: Word8)
      pure $! off + 6
  | otherwise = do
      pokeByteOff p off (0x8A :: Word8)
      pokeByteOff p (off + 1) (8 :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n .&. 0xFF) :: Word8)
      pokeByteOff p (off + 3) (fromIntegral ((n `shiftR` 8) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 4) (fromIntegral ((n `shiftR` 16) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 5) (fromIntegral ((n `shiftR` 24) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 6) (fromIntegral ((n `shiftR` 32) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 7) (fromIntegral ((n `shiftR` 40) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 8) (fromIntegral ((n `shiftR` 48) .&. 0xFF) :: Word8)
      pokeByteOff p (off + 9) (fromIntegral ((n `shiftR` 56) .&. 0xFF) :: Word8)
      pure $! off + 10

writeFloat64BE :: Ptr Word8 -> Int -> Double -> IO Int
writeFloat64BE p off d = do
  let !w = castDoubleToWord64 d
  pokeByteOff p off (fromIntegral (w `shiftR` 56) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((w `shiftR` 48) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((w `shiftR` 40) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((w `shiftR` 32) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral ((w `shiftR` 24) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral (w .&. 0xFF) :: Word8)
  pure $! off + 8
{-# INLINE writeFloat64BE #-}

writeBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeBytes p off bs
  | BS.length bs < 256 = do
      pokeByteOff p off (0x43 :: Word8)
      pokeByteOff p (off + 1) (fromIntegral (BS.length bs) :: Word8)
      writeRaw p (off + 2) bs
  | otherwise = do
      pokeByteOff p off (0x42 :: Word8)
      off1 <- writeLE32 p (off + 1) (BS.length bs)
      writeRaw p off1 bs

writeString :: Ptr Word8 -> Int -> T.Text -> IO Int
writeString p off t = do
  let !bs = TE.encodeUtf8 t
  if BS.length bs < 256
    then do
      pokeByteOff p off (0x8C :: Word8)
      pokeByteOff p (off + 1) (fromIntegral (BS.length bs) :: Word8)
      writeRaw p (off + 2) bs
    else do
      pokeByteOff p off (0x58 :: Word8)
      off1 <- writeLE32 p (off + 1) (BS.length bs)
      writeRaw p off1 bs

writeList :: Ptr Word8 -> Int -> V.Vector P.Value -> IO Int
writeList p off vs
  | V.null vs = do pokeByteOff p off (0x5D :: Word8); pure $! off + 1
  | otherwise = do
      pokeByteOff p off (0x5D :: Word8)
      pokeByteOff p (off + 1) (0x28 :: Word8)
      off1 <- V.foldM' (\o v -> writeValue v p o) (off + 2) vs
      pokeByteOff p off1 (0x65 :: Word8)
      pure $! off1 + 1

writeTuple :: Ptr Word8 -> Int -> V.Vector P.Value -> IO Int
writeTuple p off vs
  | V.null vs = do pokeByteOff p off (0x29 :: Word8); pure $! off + 1
  | V.length vs == 1 = do
      off1 <- writeValue (V.head vs) p off
      pokeByteOff p off1 (0x85 :: Word8)
      pure $! off1 + 1
  | V.length vs == 2 = do
      off1 <- writeValue (vs V.! 0) p off
      off2 <- writeValue (vs V.! 1) p off1
      pokeByteOff p off2 (0x86 :: Word8)
      pure $! off2 + 1
  | V.length vs == 3 = do
      off1 <- writeValue (vs V.! 0) p off
      off2 <- writeValue (vs V.! 1) p off1
      off3 <- writeValue (vs V.! 2) p off2
      pokeByteOff p off3 (0x87 :: Word8)
      pure $! off3 + 1
  | otherwise = do
      pokeByteOff p off (0x28 :: Word8)
      off1 <- V.foldM' (\o v -> writeValue v p o) (off + 1) vs
      pokeByteOff p off1 (0x74 :: Word8)
      pure $! off1 + 1

writeDict :: Ptr Word8 -> Int -> V.Vector (P.Value, P.Value) -> IO Int
writeDict p off kvs
  | V.null kvs = do pokeByteOff p off (0x7D :: Word8); pure $! off + 1
  | otherwise = do
      pokeByteOff p off (0x7D :: Word8)
      pokeByteOff p (off + 1) (0x28 :: Word8)
      off1 <- V.foldM' (\o (k, v) -> do o1 <- writeValue k p o; writeValue v p o1) (off + 2) kvs
      pokeByteOff p off1 (0x75 :: Word8)
      pure $! off1 + 1

writeSet :: Ptr Word8 -> Int -> V.Vector P.Value -> IO Int
writeSet p off vs = do
  pokeByteOff p off (0x28 :: Word8)
  off1 <- V.foldM' (\o v -> writeValue v p o) (off + 1) vs
  pokeByteOff p off1 (0x74 :: Word8)
  pure $! off1 + 1

writeLE32 :: Ptr Word8 -> Int -> Int -> IO Int
writeLE32 p off n = do
  pokeByteOff p off (fromIntegral (n .&. 0xFF) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral ((n `shiftR` 8) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral ((n `shiftR` 16) .&. 0xFF) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral ((n `shiftR` 24) .&. 0xFF) :: Word8)
  pure $! off + 4
{-# INLINE writeLE32 #-}

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw p off (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}
