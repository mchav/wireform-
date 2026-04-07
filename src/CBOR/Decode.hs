{-# LANGUAGE BangPatterns #-}
-- | CBOR (RFC 8949) binary decoding.
--
-- Uses 'Proto.Decode.Fast' for Ptr-based reading.
module CBOR.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import qualified CBOR.Value as C

-- | Decode a CBOR value from a strict 'ByteString'.
decode :: ByteString -> Either String C.Value
decode !bs
  | BS.length bs == 0 = Left "CBOR.Decode: empty input"
  | otherwise = case decodeOne bs 0 of
      Left err -> Left err
      Right (val, off)
        | off == BS.length bs -> Right val
        | otherwise -> Left $ "CBOR.Decode: " ++ show (BS.length bs - off) ++ " trailing bytes"

-- | Read a byte at offset using unsafe indexing.
rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

ensureBytes :: ByteString -> Int -> Int -> Either String ()
ensureBytes !bs !off !n
  | off + n > BS.length bs = Left "CBOR.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensureBytes #-}

-- | Read the argument value from additional info.
readArg :: ByteString -> Int -> Word8 -> Either String (Word64, Int)
readArg !bs !off !info
  | info <= 23 = Right (fromIntegral info, off)
  | info == 24 = do
      ensureBytes bs off 1
      Right (fromIntegral (rdByte bs off), off + 1)
  | info == 25 = do
      ensureBytes bs off 2
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
      Right ((b0 `shiftL` 8) .|. b1, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
      Right ((b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3, off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
          !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
          !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
          !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
          !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
      Right ( (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40)
              .|. (b3 `shiftL` 32) .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16)
              .|. (b6 `shiftL` 8) .|. b7
            , off + 8)
  | info == 31 = Left "CBOR.Decode: indefinite length not expected here"
  | otherwise  = Left $ "CBOR.Decode: reserved additional info: " ++ show info

-- | Decode a single CBOR item. Returns (value, newOffset).
decodeOne :: ByteString -> Int -> Either String (C.Value, Int)
decodeOne !bs !off
  | off >= BS.length bs = Left "CBOR.Decode: unexpected end of input"
  | otherwise =
    let !ib    = rdByte bs off
        !major = ib `shiftR` 5
        !info  = ib .&. 0x1f
    in case major of
      0 -> decodeUInt bs (off + 1) info
      1 -> decodeNInt bs (off + 1) info
      2 -> decodeBStr bs (off + 1) info
      3 -> decodeTStr bs (off + 1) info
      4 -> decodeArray bs (off + 1) info
      5 -> decodeMap bs (off + 1) info
      6 -> decodeTag bs (off + 1) info
      7 -> decodeSimpleOrFloat bs (off + 1) info
      _ -> Left $ "CBOR.Decode: invalid major type: " ++ show major

decodeUInt :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeUInt bs off info = do
  (n, off') <- readArg bs off info
  Right (C.UInt n, off')

decodeNInt :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeNInt bs off info = do
  (n, off') <- readArg bs off info
  Right (C.NInt n, off')

decodeBStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeBStr bs off info
  | info == 31 = decodeIndefiniteBStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !chunk = BSU.unsafeTake len (BSU.unsafeDrop off' bs)
      Right (C.ByteString chunk, off' + len)

decodeTStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeTStr bs off info
  | info == 31 = decodeIndefiniteTStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !raw = BSU.unsafeTake len (BSU.unsafeDrop off' bs)
      case TE.decodeUtf8' raw of
        Left _  -> Left "CBOR.Decode: invalid UTF-8 in text string"
        Right t -> Right (C.TextString t, off' + len)

decodeIndefiniteBStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteBStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "CBOR.Decode: unexpected end of input in indefinite byte string"
      | rdByte bs off == 0xff = Right (C.ByteString (BS.concat (reverse acc)), off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 2
            then Left "CBOR.Decode: non-byte-string chunk in indefinite byte string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BSU.unsafeTake len (BSU.unsafeDrop off' bs)
              go (off' + len) (chunk : acc)

decodeIndefiniteTStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteTStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "CBOR.Decode: unexpected end of input in indefinite text string"
      | rdByte bs off == 0xff = do
          let !combined = BS.concat (reverse acc)
          case TE.decodeUtf8' combined of
            Left _  -> Left "CBOR.Decode: invalid UTF-8 in indefinite text string"
            Right t -> Right (C.TextString t, off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 3
            then Left "CBOR.Decode: non-text-string chunk in indefinite text string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BSU.unsafeTake len (BSU.unsafeDrop off' bs)
              go (off' + len) (chunk : acc)

decodeArray :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeArray bs off info
  | info == 31 = decodeIndefiniteArray bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      decodeNItems bs off' len []

decodeNItems :: ByteString -> Int -> Int -> [C.Value] -> Either String (C.Value, Int)
decodeNItems _bs !off 0 !acc =
  Right (C.Array (V.fromList (reverse acc)), off)
decodeNItems bs !off !n !acc = do
  (v, off') <- decodeOne bs off
  decodeNItems bs off' (n - 1) (v : acc)

decodeIndefiniteArray :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteArray bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "CBOR.Decode: unexpected end of input in indefinite array"
      | rdByte bs off == 0xff = Right (C.Array (V.fromList (reverse acc)), off + 1)
      | otherwise = do
          (v, off') <- decodeOne bs off
          go off' (v : acc)

decodeMap :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeMap bs off info
  | info == 31 = decodeIndefiniteMap bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      decodeNPairs bs off' len []

decodeNPairs :: ByteString -> Int -> Int -> [(C.Value, C.Value)] -> Either String (C.Value, Int)
decodeNPairs _bs !off 0 !acc =
  Right (C.Map (V.fromList (reverse acc)), off)
decodeNPairs bs !off !n !acc = do
  (k, off')  <- decodeOne bs off
  (v, off'') <- decodeOne bs off'
  decodeNPairs bs off'' (n - 1) ((k, v) : acc)

decodeIndefiniteMap :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteMap bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "CBOR.Decode: unexpected end of input in indefinite map"
      | rdByte bs off == 0xff = Right (C.Map (V.fromList (reverse acc)), off + 1)
      | otherwise = do
          (k, off')  <- decodeOne bs off
          (v, off'') <- decodeOne bs off'
          go off'' ((k, v) : acc)

decodeTag :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeTag bs off info = do
  (tagNum, off') <- readArg bs off info
  (content, off'') <- decodeOne bs off'
  Right (C.Tag tagNum content, off'')

decodeSimpleOrFloat :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeSimpleOrFloat bs off info
  | info <= 19 = Right (C.Simple info, off)
  | info == 20 = Right (C.Bool False, off)
  | info == 21 = Right (C.Bool True, off)
  | info == 22 = Right (C.Null, off)
  | info == 23 = Right (C.Undefined, off)
  | info == 24 = do
      ensureBytes bs off 1
      let !sv = rdByte bs off
      Right (C.Simple sv, off + 1)
  | info == 25 = do
      ensureBytes bs off 2
      let !b0 = rdByte bs off
          !b1 = rdByte bs (off + 1)
          !halfBits = (fromIntegral b0 :: Word16) `shiftL` 8
                      .|. fromIntegral b1
          !f = halfToFloat halfBits
      Right (C.Float16 f, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      let !b0 = fromIntegral (rdByte bs off) :: Word32
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word32
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word32
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word32
          !w  = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
      Right (C.Float32 (castWord32ToFloat w), off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      let !b0 = fromIntegral (rdByte bs off) :: Word64
          !b1 = fromIntegral (rdByte bs (off + 1)) :: Word64
          !b2 = fromIntegral (rdByte bs (off + 2)) :: Word64
          !b3 = fromIntegral (rdByte bs (off + 3)) :: Word64
          !b4 = fromIntegral (rdByte bs (off + 4)) :: Word64
          !b5 = fromIntegral (rdByte bs (off + 5)) :: Word64
          !b6 = fromIntegral (rdByte bs (off + 6)) :: Word64
          !b7 = fromIntegral (rdByte bs (off + 7)) :: Word64
          !w  = (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40)
                .|. (b3 `shiftL` 32) .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16)
                .|. (b6 `shiftL` 8) .|. b7
      Right (C.Float64 (castWord64ToDouble w), off + 8)
  | info == 31 = Left "CBOR.Decode: break (0xff) at top level is not valid"
  | otherwise  = Left $ "CBOR.Decode: reserved simple value info: " ++ show info

-- | Convert IEEE 754 half-precision (16-bit) to Float.
halfToFloat :: Word16 -> Float
halfToFloat !h =
  let !sign = (fromIntegral h :: Word32) `shiftR` 15
      !expo = (fromIntegral h :: Word32) `shiftR` 10 .&. 0x1f
      !mant = fromIntegral h .&. 0x03ff :: Word32
      !signBit = sign `shiftL` 31
  in if expo == 0
     then if mant == 0
          then castWord32ToFloat signBit
          else let !f = (fromIntegral mant :: Float) / 1024.0 * (2 ** (-14))
               in if sign /= 0 then negate f else f
     else if expo == 0x1f
     then if mant == 0
          then castWord32ToFloat (signBit .|. 0x7f800000)
          else castWord32ToFloat (signBit .|. 0x7fc00000)
     else castWord32ToFloat
            (signBit .|. ((expo + 112) `shiftL` 23) .|. (mant `shiftL` 13))
