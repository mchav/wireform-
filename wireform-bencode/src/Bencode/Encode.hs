{-# LANGUAGE BangPatterns #-}
-- | Bencode binary encoding using directEncode.
module Bencode.Encode
  ( encode
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)

import Wireform.Encode.Direct (directEncode)
import qualified Bencode.Value as B

encode :: B.Value -> ByteString
encode val =
  let !sz = valueSize val
  in directEncode sz (\p off -> writeValue p off val)

valueSize :: B.Value -> Int
valueSize = \case
  B.BString bs ->
    let !len = BS.length bs
        !digits = intDigits len
    in digits + 1 + len
  B.BInteger n ->
    let !s = show n
    in 1 + length s + 1
  B.BList vs -> 1 + V.foldl' (\acc v -> acc + valueSize v) 0 vs + 1
  B.BDict kvs -> 1 + V.foldl' (\acc (k, v) -> acc + valueSize (B.BString k) + valueSize v) 0 kvs + 1

intDigits :: Int -> Int
intDigits n
  | n < 10    = 1
  | n < 100   = 2
  | n < 1000  = 3
  | n < 10000 = 4
  | otherwise = length (show n)

writeValue :: Ptr Word8 -> Int -> B.Value -> IO Int
writeValue p off = \case
  B.BString bs -> do
    let !len = BS.length bs
        !lenBS = BS8.pack (show len)
    off1 <- writeRaw p off lenBS
    off2 <- pokeByte p off1 0x3A -- ':'
    writeRaw p off2 bs

  B.BInteger n -> do
    off1 <- pokeByte p off 0x69 -- 'i'
    let !numBS = BS8.pack (show n)
    off2 <- writeRaw p off1 numBS
    pokeByte p off2 0x65 -- 'e'

  B.BList vs -> do
    off1 <- pokeByte p off 0x6C -- 'l'
    off2 <- V.foldM' (\o v -> writeValue p o v) off1 vs
    pokeByte p off2 0x65 -- 'e'

  B.BDict kvs -> do
    off1 <- pokeByte p off 0x64 -- 'd'
    off2 <- V.foldM' (\o (k, v) -> do
      o1 <- writeValue p o (B.BString k)
      writeValue p o1 v) off1 kvs
    pokeByte p off2 0x65 -- 'e'

pokeByte :: Ptr Word8 -> Int -> Word8 -> IO Int
pokeByte !p !off !b = do
  pokeByteOff p off b
  pure $! off + 1
{-# INLINE pokeByte #-}

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}
