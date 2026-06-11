{-# LANGUAGE BangPatterns #-}

{- | Low-level Apache Fory wire-format primitives.

Two layers:

* Pure 'Builder' constructors mirroring the spec\'s primitive
  encodings (varuint32, varuint64, zigzag varint, tagged int64,
  varuint36_small, IEEE float\/double, fory header).

* Pure decoders threading a 'ByteString' offset, mirroring the
  spec's read algorithms.

All multi-byte integers are little-endian per the
<https://fory.apache.org/docs/specification/xlang_serialization_spec
xlang spec>.
-}
module Fory.Encoding (
  -- * Fory header
  foryXlangHeader,
  readForyHeader,

  -- * Reference flags
  refFlagNull,
  refFlagRef,
  refFlagNotNullValue,
  refFlagRefValue,

  -- * Builder primitives
  Builder,
  runBuilder,
  byte,
  bytes,
  word16LE,
  word32LE,
  word64LE,
  int16LE,
  int32LE,
  int64LE,
  float32LE,
  float64LE,
  varuint32,
  varuint64,
  varint32,
  varint64,
  taggedInt64,
  taggedUint64,
  varuint36Small,
  utf8String,

  -- * Decoders
  readByte,
  readBytes,
  readWord16LE,
  readWord32LE,
  readWord64LE,
  readInt16LE,
  readInt32LE,
  readInt64LE,
  readFloat32LE,
  readFloat64LE,
  readVaruint32,
  readVaruint64,
  readVarint32,
  readVarint64,
  readTaggedInt64,
  readTaggedUint64,
  readVaruint36Small,
  readUtf8String,
) where

import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (
  castDoubleToWord64,
  castFloatToWord32,
  castWord32ToFloat,
  castWord64ToDouble,
 )
import Wireform.Builder qualified as BB


-- ---------------------------------------------------------------------------
-- Builder
-- ---------------------------------------------------------------------------

{- | A fory output 'Builder'. Wrapping 'BB.Builder' lets us add a
few combinators without exporting the underlying type to keep the
module surface narrow.
-}
newtype Builder = Builder BB.Builder


instance Semigroup Builder where
  Builder a <> Builder b = Builder (a <> b)
  {-# INLINE (<>) #-}


instance Monoid Builder where
  mempty = Builder mempty
  {-# INLINE mempty #-}


runBuilder :: Builder -> ByteString
runBuilder (Builder b) = BB.toStrictByteString b


-- ---------------------------------------------------------------------------
-- Fory header
-- ---------------------------------------------------------------------------

{- | The 1-byte fory header used at the top of a serialised
non-null xlang value: bit 1 (xlang flag) is set, no other bits.
-}
foryXlangHeader :: Word8
foryXlangHeader = 0x02


{- | Parse the 1-byte fory header. Returns 'Right' with the header
byte and the new offset (pre-advanced past the byte) if the input
is non-empty.
-}
readForyHeader :: ByteString -> Int -> Either String (Word8, Int)
readForyHeader bs !off
  | off >= BS.length bs = Left "Fory.Encoding.readForyHeader: empty input"
  | otherwise = Right (BSU.unsafeIndex bs off, off + 1)


-- ---------------------------------------------------------------------------
-- Reference flags
-- ---------------------------------------------------------------------------

refFlagNull, refFlagRef, refFlagNotNullValue, refFlagRefValue :: Word8
refFlagNull = 0xFD
refFlagRef = 0xFE
refFlagNotNullValue = 0xFF
refFlagRefValue = 0x00


-- ---------------------------------------------------------------------------
-- Builder primitives
-- ---------------------------------------------------------------------------

byte :: Word8 -> Builder
byte = Builder . BB.word8
{-# INLINE byte #-}


bytes :: ByteString -> Builder
bytes = Builder . BB.byteString
{-# INLINE bytes #-}


word16LE :: Word16 -> Builder
word16LE = Builder . BB.word16LE
{-# INLINE word16LE #-}


word32LE :: Word32 -> Builder
word32LE = Builder . BB.word32LE
{-# INLINE word32LE #-}


word64LE :: Word64 -> Builder
word64LE = Builder . BB.word64LE
{-# INLINE word64LE #-}


int16LE :: Int16 -> Builder
int16LE = Builder . BB.int16LE
{-# INLINE int16LE #-}


int32LE :: Int32 -> Builder
int32LE = Builder . BB.int32LE
{-# INLINE int32LE #-}


int64LE :: Int64 -> Builder
int64LE = Builder . BB.int64LE
{-# INLINE int64LE #-}


float32LE :: Float -> Builder
float32LE !f = word32LE (castFloatToWord32 f)
{-# INLINE float32LE #-}


float64LE :: Double -> Builder
float64LE !d = word64LE (castDoubleToWord64 d)
{-# INLINE float64LE #-}


{- | Unsigned varint32. Each byte carries 7 bits with bit 7 set on
every byte except the last. 1-5 bytes total.
-}
varuint32 :: Word32 -> Builder
varuint32 = Builder . goVaru32
  where
    goVaru32 !v
      | v < 0x80 = BB.word8 (fromIntegral v)
      | otherwise =
          BB.word8 (fromIntegral (v .&. 0x7F) .|. 0x80)
            <> goVaru32 (v `shiftR` 7)
{-# INLINE varuint32 #-}


{- | Unsigned varint64 (1-9 bytes). The last byte is full 8-bit when
the value occupies the top byte, mirroring the spec\'s PVL
encoding.
-}
varuint64 :: Word64 -> Builder
varuint64 = Builder . goVaru64 0
  where
    goVaru64 :: Int -> Word64 -> BB.Builder
    goVaru64 !i !v
      | i >= 8 = BB.word8 (fromIntegral v)
      | v < 0x80 = BB.word8 (fromIntegral v)
      | otherwise =
          BB.word8 (fromIntegral (v .&. 0x7F) .|. 0x80)
            <> goVaru64 (i + 1) (v `shiftR` 7)
{-# INLINE varuint64 #-}


-- | Signed varint32 = zigzag + 'varuint32'.
varint32 :: Int32 -> Builder
varint32 !v = varuint32 (zigzag32 v)
{-# INLINE varint32 #-}


-- | Signed varint64 = zigzag + 'varuint64'.
varint64 :: Int64 -> Builder
varint64 !v = varuint64 (zigzag64 v)
{-# INLINE varint64 #-}


zigzag32 :: Int32 -> Word32
zigzag32 v = fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 31))
{-# INLINE zigzag32 #-}


zigzag64 :: Int64 -> Word64
zigzag64 v = fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 63))
{-# INLINE zigzag64 #-}


{- | Hybrid signed int64 (TAGGED_INT64). Values in
@[-2^30, 2^30 - 1]@ pack into 4 bytes via @value <\< 1@; everything
else uses a 9-byte form starting with @0x01@ followed by a
little-endian int64.
-}
taggedInt64 :: Int64 -> Builder
taggedInt64 !v
  | v >= -1073741824 && v <= 1073741823 =
      int32LE (fromIntegral (v `shiftL` 1))
  | otherwise = byte 0x01 <> int64LE v


{- | Hybrid unsigned int64 (TAGGED_UINT64). Values @<= 0x7FFFFFFF@
pack into 4 bytes via @value <\< 1@; everything else uses a
9-byte form starting with @0x01@ followed by a little-endian
uint64.
-}
taggedUint64 :: Word64 -> Builder
taggedUint64 !v
  | v <= 0x7FFFFFFF = int32LE (fromIntegral (v `shiftL` 1))
  | otherwise = byte 0x01 <> word64LE v


{- | Specialised varint used by string headers, encoding the bottom
36 bits of an @Int64@ in 1-5 bytes. 'varuint64' would also accept
the same bytes; this just bounds-checks the input range.
-}
varuint36Small :: Word64 -> Builder
varuint36Small !v
  | v `shiftR` 36 /= 0 =
      error "Fory.Encoding.varuint36Small: value exceeds 36 bits"
  | otherwise = varuint64 v


{- | Encode a @Text@ as a fory string: 'varuint36Small' header
@(byte_length \<\< 2) | encoding@ with @encoding = 2@ (UTF-8),
followed by the raw UTF-8 bytes.
-}
utf8String :: Text -> Builder
utf8String !t =
  let !raw = TE.encodeUtf8 t
      !len = BS.length raw
      !hdr = (fromIntegral len `shiftL` 2) .|. 2 :: Word64
  in varuint36Small hdr <> bytes raw


-- ---------------------------------------------------------------------------
-- Decoders
-- ---------------------------------------------------------------------------

readByte :: ByteString -> Int -> Either String (Word8, Int)
readByte bs !off
  | off >= BS.length bs = Left "Fory.Encoding.readByte: end of input"
  | otherwise = Right (BSU.unsafeIndex bs off, off + 1)
{-# INLINE readByte #-}


readBytes :: Int -> ByteString -> Int -> Either String (ByteString, Int)
readBytes !n !bs !off
  | off + n > BS.length bs = Left "Fory.Encoding.readBytes: truncated"
  | otherwise =
      let !slice = BSU.unsafeTake n (BSU.unsafeDrop off bs)
      in Right (slice, off + n)
{-# INLINE readBytes #-}


readWord16LE :: ByteString -> Int -> Either String (Word16, Int)
readWord16LE bs !off
  | off + 2 > BS.length bs = Left "Fory.Encoding.readWord16LE: truncated"
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word16
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word16
      in Right (b0 .|. (b1 `shiftL` 8), off + 2)


readWord32LE :: ByteString -> Int -> Either String (Word32, Int)
readWord32LE bs !off
  | off + 4 > BS.length bs = Left "Fory.Encoding.readWord32LE: truncated"
  | otherwise =
      let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
          !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
          !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
          !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
      in Right
           ( b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
           , off + 4
           )


readWord64LE :: ByteString -> Int -> Either String (Word64, Int)
readWord64LE bs !off
  | off + 8 > BS.length bs = Left "Fory.Encoding.readWord64LE: truncated"
  | otherwise = do
      (lo, off1) <- readWord32LE bs off
      (hi, off2) <- readWord32LE bs off1
      Right (fromIntegral lo .|. (fromIntegral hi `shiftL` 32), off2)


readInt16LE :: ByteString -> Int -> Either String (Int16, Int)
readInt16LE bs off = do
  (w, off') <- readWord16LE bs off
  Right (fromIntegral w, off')


readInt32LE :: ByteString -> Int -> Either String (Int32, Int)
readInt32LE bs off = do
  (w, off') <- readWord32LE bs off
  Right (fromIntegral w, off')


readInt64LE :: ByteString -> Int -> Either String (Int64, Int)
readInt64LE bs off = do
  (w, off') <- readWord64LE bs off
  Right (fromIntegral w, off')


readFloat32LE :: ByteString -> Int -> Either String (Float, Int)
readFloat32LE bs off = do
  (w, off') <- readWord32LE bs off
  Right (castWord32ToFloat w, off')


readFloat64LE :: ByteString -> Int -> Either String (Double, Int)
readFloat64LE bs off = do
  (w, off') <- readWord64LE bs off
  Right (castWord64ToDouble w, off')


readVaruint32 :: ByteString -> Int -> Either String (Word32, Int)
readVaruint32 bs = go 0 0
  where
    !len = BS.length bs
    go :: Int -> Word32 -> Int -> Either String (Word32, Int)
    go !shift !acc !off
      | off >= len = Left "Fory.Encoding.readVaruint32: truncated"
      | shift >= 35 = Left "Fory.Encoding.readVaruint32: overflow"
      | otherwise =
          let !b = BSU.unsafeIndex bs off
              !acc' = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
              !off' = off + 1
          in if b .&. 0x80 == 0
               then Right (acc', off')
               else go (shift + 7) acc' off'


readVaruint64 :: ByteString -> Int -> Either String (Word64, Int)
readVaruint64 bs = go 0 0 0
  where
    !len = BS.length bs
    go :: Int -> Int -> Word64 -> Int -> Either String (Word64, Int)
    go !i !shift !acc !off
      | off >= len = Left "Fory.Encoding.readVaruint64: truncated"
      | i >= 8 =
          -- Final 9th byte is full 8 bits.
          let !b = BSU.unsafeIndex bs off
              !acc' = acc .|. (fromIntegral b `shiftL` 56)
          in Right (acc', off + 1)
      | otherwise =
          let !b = BSU.unsafeIndex bs off
              !acc' = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
              !off' = off + 1
          in if b .&. 0x80 == 0
               then Right (acc', off')
               else go (i + 1) (shift + 7) acc' off'


readVarint32 :: ByteString -> Int -> Either String (Int32, Int)
readVarint32 bs off = do
  (u, off') <- readVaruint32 bs off
  Right (zigzagDecode32 u, off')


readVarint64 :: ByteString -> Int -> Either String (Int64, Int)
readVarint64 bs off = do
  (u, off') <- readVaruint64 bs off
  Right (zigzagDecode64 u, off')


zigzagDecode32 :: Word32 -> Int32
zigzagDecode32 u = fromIntegral (u `shiftR` 1) `xor` complement (fromIntegral (u .&. 1) - 1)


zigzagDecode64 :: Word64 -> Int64
zigzagDecode64 u = fromIntegral (u `shiftR` 1) `xor` complement (fromIntegral (u .&. 1) - 1)


readTaggedInt64 :: ByteString -> Int -> Either String (Int64, Int)
readTaggedInt64 bs off = do
  (i, off1) <- readInt32LE bs off
  if (i .&. 1) == 0
    then Right (fromIntegral (i `shiftR` 1), off1)
    -- The flag byte at @off@ is part of the int32 we just consumed;
    -- the next 8 bytes start at @off + 1@.
    else do
      (v, off2) <- readInt64LE bs (off + 1)
      Right (v, off2)


readTaggedUint64 :: ByteString -> Int -> Either String (Word64, Int)
readTaggedUint64 bs off = do
  (i, off1) <- readWord32LE bs off
  if (i .&. 1) == 0
    then Right (fromIntegral (i `shiftR` 1), off1)
    else do
      (v, off2) <- readWord64LE bs (off + 1)
      Right (v, off2)


readVaruint36Small :: ByteString -> Int -> Either String (Word64, Int)
readVaruint36Small = readVaruint64


readUtf8String :: ByteString -> Int -> Either String (Text, Int)
readUtf8String bs off = do
  (hdr, off1) <- readVaruint36Small bs off
  let !enc = hdr .&. 0x03
      !len = fromIntegral (hdr `shiftR` 2) :: Int
  if enc /= 2
    then
      Left $
        "Fory.Encoding.readUtf8String: encoding "
          ++ show enc
          ++ " (only UTF-8 = 2 supported)"
    else do
      (raw, off2) <- readBytes len bs off1
      case TE.decodeUtf8' raw of
        Left e -> Left ("Fory.Encoding.readUtf8String: " ++ show e)
        Right t -> Right (t, off2)
