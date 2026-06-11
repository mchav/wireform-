{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Direct, zero-copy conversions between 'Data.ByteString' and
'Data.Vector.Storable.Vector' for the primitive-array wire
payloads.

The Fory xlang spec uses LE bytes; Storable Vectors on
little-endian platforms (x86-64 / aarch64) share that layout
exactly, so we can reinterpret the underlying 'ForeignPtr'
between the two types without copying.

The functions are defined in terms of
'BSI.PS' / 'VS.unsafeFromForeignPtr0' /
'VS.unsafeToForeignPtr0' which are all O(1).

Endianness caveat: this module assumes a little-endian host.
Big-endian platforms would need a byte-swap on the wire path;
not currently exercised in CI.
-}
module Fory.Bulk (
  -- * Storable Vector <-> ByteString (zero-copy)
  bytesToVecS,
  vecSToBytes,

  -- * Encode-side bulk byte conversions
  boolArrayBytes,
  int8ArrayBytes,
  int16ArrayBytes,
  int32ArrayBytes,
  int64ArrayBytes,
  uint8ArrayBytes,
  uint16ArrayBytes,
  uint32ArrayBytes,
  uint64ArrayBytes,
  float32ArrayBytes,
  float64ArrayBytes,
  latin1Bytes,

  -- * Decode-side bulk byte conversions
  bytesToBoolArray,
  bytesToInt8Array,
  bytesToInt16Array,
  bytesToInt32Array,
  bytesToInt64Array,
  bytesToUint8Array,
  bytesToUint16Array,
  bytesToUint32Array,
  bytesToUint64Array,
  bytesToFloat32Array,
  bytesToFloat64Array,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Char (ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Storable (Storable, sizeOf)


-- ---------------------------------------------------------------------------
-- Zero-copy reinterpretation
-- ---------------------------------------------------------------------------

{- | Reinterpret a 'ByteString' as a 'VS.Vector' of any
'Storable' element. The returned vector aliases the
'ByteString'\'s underlying memory; do not modify either after
the conversion.

Truncates the byte length to a multiple of @sizeOf (undefined :: a)@.
-}
{-# INLINE bytesToVecS #-}
bytesToVecS :: forall a. Storable a => ByteString -> VS.Vector a
bytesToVecS (BSI.BS fp len) =
  let !sz = sizeOf (undefined :: a)
      !n = len `div` sz
  in VS.unsafeFromForeignPtr0 (castForeignPtr fp) n


{- | Reinterpret a 'VS.Vector' of any 'Storable' element as a
'ByteString'. The returned 'ByteString' aliases the vector's
underlying memory; do not modify either after the conversion.
-}
{-# INLINE vecSToBytes #-}
vecSToBytes :: forall a. Storable a => VS.Vector a -> ByteString
vecSToBytes v =
  let (!fp, !n) = VS.unsafeToForeignPtr0 v
      !sz = sizeOf (undefined :: a)
  in BSI.BS (castForeignPtr fp) (n * sz)


-- ---------------------------------------------------------------------------
-- Encode-side: Storable Vector -> ByteString (O(1))
-- ---------------------------------------------------------------------------

boolArrayBytes :: VS.Vector Word8 -> ByteString
boolArrayBytes = vecSToBytes


int8ArrayBytes :: VS.Vector Int8 -> ByteString
int8ArrayBytes = vecSToBytes


int16ArrayBytes :: VS.Vector Int16 -> ByteString
int16ArrayBytes = vecSToBytes


int32ArrayBytes :: VS.Vector Int32 -> ByteString
int32ArrayBytes = vecSToBytes


int64ArrayBytes :: VS.Vector Int64 -> ByteString
int64ArrayBytes = vecSToBytes


uint8ArrayBytes :: VS.Vector Word8 -> ByteString
uint8ArrayBytes = vecSToBytes


uint16ArrayBytes :: VS.Vector Word16 -> ByteString
uint16ArrayBytes = vecSToBytes


uint32ArrayBytes :: VS.Vector Word32 -> ByteString
uint32ArrayBytes = vecSToBytes


uint64ArrayBytes :: VS.Vector Word64 -> ByteString
uint64ArrayBytes = vecSToBytes


float32ArrayBytes :: VS.Vector Float -> ByteString
float32ArrayBytes = vecSToBytes


float64ArrayBytes :: VS.Vector Double -> ByteString
float64ArrayBytes = vecSToBytes


-- ---------------------------------------------------------------------------
-- Latin-1 strings
-- ---------------------------------------------------------------------------

{- | Convert a 'Text' to a Latin-1 (one byte per character)
'ByteString'. ASCII strings are encoded zero-copy via the
existing UTF-8 representation; strings with code points
128–255 require a manual 1-byte-per-char re-encode.
-}
latin1Bytes :: Text -> ByteString
latin1Bytes t
  | T.all (\c -> ord c < 128) t = TE.encodeUtf8 t
  | otherwise = BS.pack (map (fromIntegral . ord) (T.unpack t))


-- ---------------------------------------------------------------------------
-- Decode-side: ByteString -> Storable Vector (O(1))
-- ---------------------------------------------------------------------------

bytesToBoolArray :: ByteString -> VS.Vector Word8
bytesToBoolArray = bytesToVecS


bytesToInt8Array :: ByteString -> VS.Vector Int8
bytesToInt8Array = bytesToVecS


bytesToInt16Array :: ByteString -> VS.Vector Int16
bytesToInt16Array = bytesToVecS


bytesToInt32Array :: ByteString -> VS.Vector Int32
bytesToInt32Array = bytesToVecS


bytesToInt64Array :: ByteString -> VS.Vector Int64
bytesToInt64Array = bytesToVecS


bytesToUint8Array :: ByteString -> VS.Vector Word8
bytesToUint8Array = bytesToVecS


bytesToUint16Array :: ByteString -> VS.Vector Word16
bytesToUint16Array = bytesToVecS


bytesToUint32Array :: ByteString -> VS.Vector Word32
bytesToUint32Array = bytesToVecS


bytesToUint64Array :: ByteString -> VS.Vector Word64
bytesToUint64Array = bytesToVecS


bytesToFloat32Array :: ByteString -> VS.Vector Float
bytesToFloat32Array = bytesToVecS


bytesToFloat64Array :: ByteString -> VS.Vector Double
bytesToFloat64Array = bytesToVecS
