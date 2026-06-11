{-# LANGUAGE BangPatterns #-}

{- | Bencode binary encoding using directEncode.

Per BEP-3 (\"keys must be strings and appear in sorted order
(sorted as raw strings, not alphanumerics)\"), 'encode' sorts every
'B.BDict''s entries by raw byte-string key on output, so any
caller producing a 'B.BDict' \xE2\x80\x94 hand-built, generic-derived,
or TH-derived \xE2\x80\x94 ends up emitting BEP-3-conformant bytes.
-}
module Bencode.Encode (
  encode,
) where

import Bencode.Value qualified as B
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Internal qualified as BSI
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector qualified as V
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Wireform.Encode.Direct (directEncode)


encode :: B.Value -> ByteString
encode val =
  let !sorted = sortDictKeys val
      !sz = valueSize sorted
  in directEncode sz (\p off -> writeValue p off sorted)


{- | Recursively sort every 'B.BDict''s entries by raw byte key.
Bencode dicts are required to be sorted (BEP-3), and the encoder is
the only place we can guarantee it without forcing every caller to
pre-sort.
-}
sortDictKeys :: B.Value -> B.Value
sortDictKeys = \case
  B.BDict kvs ->
    B.BDict
      ( V.fromList
          ( sortBy
              (comparing fst)
              (V.toList (V.map (\(k, v) -> (k, sortDictKeys v)) kvs))
          )
      )
  B.BList vs -> B.BList (V.map sortDictKeys vs)
  v -> v


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
  | n < 10 = 1
  | n < 100 = 2
  | n < 1000 = 3
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
    off2 <-
      V.foldM'
        ( \o (k, v) -> do
            o1 <- writeValue p o (B.BString k)
            writeValue p o1 v
        )
        off1
        kvs
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
