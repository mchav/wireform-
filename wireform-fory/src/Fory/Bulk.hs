{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | Direct-buffer fast paths for the bulk encode operations:
-- primitive arrays and long Latin-1 / UTF-8 strings.
--
-- These bypass 'Data.ByteString.Builder' entirely. Each function
-- pre-computes the exact output size, allocates one
-- 'ByteString'-backing buffer, and writes elements with raw
-- 'pokeByteOff' / 'memcpy' calls.
--
-- The functions are pure — they 'unsafePerformIO' a single
-- in-place buffer initialisation and freeze the result, which is
-- safe because the buffer is not visible to any other thread
-- and is never mutated after the initialisation completes.
module Fory.Bulk
  ( -- * Encode-side bulk byte conversions
    boolArrayBytes
  , int8ArrayBytes
  , int16ArrayBytes
  , int32ArrayBytes
  , int64ArrayBytes
  , uint8ArrayBytes
  , uint16ArrayBytes
  , uint32ArrayBytes
  , uint64ArrayBytes
  , float32ArrayBytes
  , float64ArrayBytes

  , latin1Bytes

    -- * Decode-side bulk byte conversions
  , bytesToBoolArray
  , bytesToInt8Array
  , bytesToInt16Array
  , bytesToInt32Array
  , bytesToInt64Array
  , bytesToUint8Array
  , bytesToUint16Array
  , bytesToUint32Array
  , bytesToUint64Array
  , bytesToFloat32Array
  , bytesToFloat64Array
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64,
                  castWord32ToFloat, castWord64ToDouble)

import Wireform.Encode.Direct (directEncode)

-- ---------------------------------------------------------------------------
-- Primitive arrays
-- ---------------------------------------------------------------------------

-- The boxed 'Data.Vector.Vector' stores heap-allocated values
-- one indirection away from contiguous memory; we copy each
-- element to a freshly-allocated little-endian buffer in a
-- tight 'V.foldM_' loop. On x86-64 (little-endian) the
-- 'pokeByteOff' below stores values in their natural byte order
-- and matches the spec's fixed-width LE element payload.

boolArrayBytes :: V.Vector Bool -> ByteString
boolArrayBytes vs =
  let !n = V.length vs
  in directEncode n $ \p off0 -> do
       _ <- V.foldM' (\off b -> do
         pokeByteOff p off (if b then (1 :: Word8) else 0)
         pure (off + 1)) off0 vs
       pure (off0 + n)

int8ArrayBytes :: V.Vector Int8 -> ByteString
int8ArrayBytes vs =
  let !n = V.length vs
  in directEncode n $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Int8)
         pure (off + 1)) off0 vs
       pure (off0 + n)

int16ArrayBytes :: V.Vector Int16 -> ByteString
int16ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 2
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Int16)
         pure (off + 2)) off0 vs
       pure (off0 + sz)

int32ArrayBytes :: V.Vector Int32 -> ByteString
int32ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 4
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Int32)
         pure (off + 4)) off0 vs
       pure (off0 + sz)

int64ArrayBytes :: V.Vector Int64 -> ByteString
int64ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 8
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Int64)
         pure (off + 8)) off0 vs
       pure (off0 + sz)

uint8ArrayBytes :: V.Vector Word8 -> ByteString
uint8ArrayBytes vs =
  let !n = V.length vs
  in directEncode n $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Word8)
         pure (off + 1)) off0 vs
       pure (off0 + n)

uint16ArrayBytes :: V.Vector Word16 -> ByteString
uint16ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 2
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Word16)
         pure (off + 2)) off0 vs
       pure (off0 + sz)

uint32ArrayBytes :: V.Vector Word32 -> ByteString
uint32ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 4
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Word32)
         pure (off + 4)) off0 vs
       pure (off0 + sz)

uint64ArrayBytes :: V.Vector Word64 -> ByteString
uint64ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 8
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (x :: Word64)
         pure (off + 8)) off0 vs
       pure (off0 + sz)

float32ArrayBytes :: V.Vector Float -> ByteString
float32ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 4
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (castFloatToWord32 x)
         pure (off + 4)) off0 vs
       pure (off0 + sz)

float64ArrayBytes :: V.Vector Double -> ByteString
float64ArrayBytes vs =
  let !n = V.length vs
      !sz = n * 8
  in directEncode sz $ \p off0 -> do
       _ <- V.foldM' (\off x -> do
         pokeByteOff p off (castDoubleToWord64 x)
         pure (off + 8)) off0 vs
       pure (off0 + sz)

-- ---------------------------------------------------------------------------
-- Latin-1 strings
-- ---------------------------------------------------------------------------

-- | Convert a 'Text' known to be Latin-1 (every code point
-- < 256) into a tight 'ByteString' where each byte is the
-- character's code point.
--
-- Fast path: if the input is pure ASCII (every code point
-- < 128), Text 2.x's internal representation is already the
-- byte-for-byte Latin-1 representation, so we delegate to the
-- GHC-internal UTF-8 encoder which is one @memcpy@.
--
-- For Latin-1 non-ASCII characters (128–255) we fall through
-- to a per-character pokeByteOff loop. The check is amortised
-- O(n) so we always pay one O(n) traversal for the fast-path
-- case; this is much cheaper than the previous
-- @BS.pack . map (fromIntegral . ord) . T.unpack@ allocation
-- chain.
latin1Bytes :: Text -> ByteString
latin1Bytes !t
  | T.all (\c -> ord c < 128) t = TE.encodeUtf8 t
  | otherwise =
      let !n = T.length t
          !chars = T.unpack t
      in directEncode n $ \p off0 -> do
           let go []     !off = pure off
               go (c:cs) !off = do
                 pokeByteOff p off (fromIntegral (ord c) :: Word8)
                 go cs (off + 1)
           go chars off0

-- ---------------------------------------------------------------------------
-- Decode-side bulk: ByteString -> V.Vector
-- ---------------------------------------------------------------------------
--
-- The pattern: read the slice via @ByteString@ unsafe-index
-- primitives directly into a boxed @V.Vector@ via 'V.generate'.
-- This avoids the per-element @readByteD@ + 'V.replicateM' chain
-- that the original decoder uses, which paid for state-monad
-- bind on every byte.

bytesToBoolArray :: ByteString -> V.Vector Bool
bytesToBoolArray bs = V.generate (BS.length bs) $ \i ->
  BSU.unsafeIndex bs i /= 0

bytesToInt8Array :: ByteString -> V.Vector Int8
bytesToInt8Array bs = V.generate (BS.length bs) $ \i ->
  fromIntegral (BSU.unsafeIndex bs i)

bytesToUint8Array :: ByteString -> V.Vector Word8
bytesToUint8Array bs = V.generate (BS.length bs) (BSU.unsafeIndex bs)

-- | Read a 'Storable' value at byte offset @off@ in @bs@. The
-- ByteString must be backed by a foreign pointer; on x86-64 the
-- @peekByteOff@ honours the platform's little-endian byte order
-- which matches the spec.
unsafePeekAt :: Storable a => ByteString -> Int -> a
unsafePeekAt (BSI.BS fp _) !off =
  BSI.accursedUnutterablePerformIO $
    BSI.unsafeWithForeignPtr fp $ \p -> peekByteOff p off
{-# INLINE unsafePeekAt #-}

bytesToInt16Array :: ByteString -> V.Vector Int16
bytesToInt16Array bs = V.generate (BS.length bs `quot` 2) $ \i ->
  unsafePeekAt bs (i * 2)

bytesToInt32Array :: ByteString -> V.Vector Int32
bytesToInt32Array bs = V.generate (BS.length bs `quot` 4) $ \i ->
  unsafePeekAt bs (i * 4)

bytesToInt64Array :: ByteString -> V.Vector Int64
bytesToInt64Array bs = V.generate (BS.length bs `quot` 8) $ \i ->
  unsafePeekAt bs (i * 8)

bytesToUint16Array :: ByteString -> V.Vector Word16
bytesToUint16Array bs = V.generate (BS.length bs `quot` 2) $ \i ->
  unsafePeekAt bs (i * 2)

bytesToUint32Array :: ByteString -> V.Vector Word32
bytesToUint32Array bs = V.generate (BS.length bs `quot` 4) $ \i ->
  unsafePeekAt bs (i * 4)

bytesToUint64Array :: ByteString -> V.Vector Word64
bytesToUint64Array bs = V.generate (BS.length bs `quot` 8) $ \i ->
  unsafePeekAt bs (i * 8)

bytesToFloat32Array :: ByteString -> V.Vector Float
bytesToFloat32Array bs = V.generate (BS.length bs `quot` 4) $ \i ->
  castWord32ToFloat (unsafePeekAt bs (i * 4))

bytesToFloat64Array :: ByteString -> V.Vector Double
bytesToFloat64Array bs = V.generate (BS.length bs `quot` 8) $ \i ->
  castWord64ToDouble (unsafePeekAt bs (i * 8))

-- Suppress unused-import warnings for symbols only used by some
-- entries above (e.g. 'castForeignPtr' and 'VS' could be used by
-- a future zero-copy storable-cast path).
_unusedFFI :: ()
_unusedFFI = const () (castForeignPtr, VS.empty :: VS.Vector Word8)
