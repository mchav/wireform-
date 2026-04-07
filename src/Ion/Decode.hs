{-# LANGUAGE BangPatterns #-}
-- | Amazon Ion binary decoding.
--
-- Decodes an Amazon Ion binary 'ByteString' into an 'Ion.Value.Value'.
-- Validates the Binary Version Marker, then uses unsafe indexing for
-- high-performance reading of type descriptors, VarUInt lengths, and
-- payload data.
module Ion.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64, byteSwap64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Ion.Value as I

decode :: ByteString -> Either String I.Value
decode !bs
  | BS.length bs < 4 = Left "Ion.Decode: input too short"
  | BSU.unsafeIndex bs 0 /= 0xE0
    || BSU.unsafeIndex bs 1 /= 0x01
    || BSU.unsafeIndex bs 2 /= 0x00
    || BSU.unsafeIndex bs 3 /= 0xEA = Left "Ion.Decode: invalid BVM"
  | BS.length bs == 4 = Left "Ion.Decode: no data after BVM"
  | otherwise = case decodeValue bs 4 of
      Left err -> Left err
      Right (val, off)
        | off == BS.length bs -> Right val
        | otherwise -> Left $ "Ion.Decode: " ++ show (BS.length bs - off) ++ " trailing bytes"

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "Ion.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}

readVarUInt :: ByteString -> Int -> Either String (Int, Int)
readVarUInt bs off = go off 0
  where
    go !i !acc
      | i >= BS.length bs = Left "Ion.Decode: truncated VarUInt"
      | otherwise =
          let !b = rdByte bs i
          in if b >= 0x80
             then Right (acc `shiftL` 7 .|. fromIntegral (b .&. 0x7F), i + 1)
             else go (i + 1) (acc `shiftL` 7 .|. fromIntegral b)

readMagnitude :: ByteString -> Int -> Int -> Either String (Word64, Int)
readMagnitude bs off nbytes
  | nbytes == 0 = Right (0, off)
  | otherwise = do
      ensure bs off nbytes
      let go !i !acc
            | i >= nbytes = Right (acc, off + nbytes)
            | otherwise =
                let !b = fromIntegral (rdByte bs (off + i)) :: Word64
                in go (i + 1) ((acc `shiftL` 8) .|. b)
      go 0 0

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readBE64 :: ByteString -> Int -> Word64
readBE64 bs off = withBSPtrOff bs off $ \p ->
  byteSwap64 <$> (peekByteOff p 0 :: IO Word64)
{-# INLINE readBE64 #-}

decodeValue :: ByteString -> Int -> Either String (I.Value, Int)
decodeValue bs off = do
  ensure bs off 1
  let !td = rdByte bs off
      !typeNibble = td `shiftR` 4
      !lenNibble  = td .&. 0x0F
  case typeNibble of
    0x00 -> Right (I.Null, off + 1)
    0x01 -> do
      Right (I.Bool (lenNibble /= 0), off + 1)
    0x02 -> decodeInt bs (off + 1) lenNibble False
    0x03 -> decodeInt bs (off + 1) lenNibble True
    0x04 -> decodeIonFloat bs (off + 1) lenNibble
    0x07 -> decodeSymbol bs (off + 1) lenNibble
    0x08 -> decodeString bs (off + 1) lenNibble
    0x09 -> decodeClob bs (off + 1) lenNibble
    0x0A -> decodeBlob bs (off + 1) lenNibble
    0x0B -> decodeList bs (off + 1) lenNibble
    0x0D -> decodeStruct bs (off + 1) lenNibble
    0x0E -> decodeAnnotation bs (off + 1) lenNibble
    _    -> Left $ "Ion.Decode: unsupported type nibble: " ++ show typeNibble

readLength :: ByteString -> Int -> Word8 -> Either String (Int, Int)
readLength bs off lenNibble
  | lenNibble < 0x0E = Right (fromIntegral lenNibble, off)
  | lenNibble == 0x0E = readVarUInt bs off
  | otherwise = Left "Ion.Decode: reserved length nibble 0x0F"

decodeInt :: ByteString -> Int -> Word8 -> Bool -> Either String (I.Value, Int)
decodeInt bs off lenNibble isNeg
  | lenNibble == 0 = Right (I.Int 0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      (mag, off2) <- readMagnitude bs off1 len
      let !val = if isNeg
                 then negate (fromIntegral mag) :: Int64
                 else fromIntegral mag
      Right (I.Int val, off2)

decodeIonFloat :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeIonFloat bs off lenNibble
  | lenNibble == 0 = Right (I.Float 0.0, off)
  | otherwise = do
      (len, off1) <- readLength bs off lenNibble
      case len of
        8 -> do
          ensure bs off1 8
          let !w = readBE64 bs off1
          Right (I.Float (castWord64ToDouble w), off1 + 8)
        _ -> Left $ "Ion.Decode: unsupported float size: " ++ show len

decodeString :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeString bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  case TE.decodeUtf8' raw of
    Left _  -> Left "Ion.Decode: invalid UTF-8 in string"
    Right t -> Right (I.String t, off1 + len)

decodeSymbol :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeSymbol bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  case TE.decodeUtf8' raw of
    Left _  -> Left "Ion.Decode: invalid UTF-8 in symbol"
    Right t -> Right (I.Symbol t, off1 + len)

decodeBlob :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeBlob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (I.Blob raw, off1 + len)

decodeClob :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeClob bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  ensure bs off1 len
  let !raw = BSU.unsafeTake len (BSU.unsafeDrop off1 bs)
  Right (I.Clob raw, off1 + len)

decodeList :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeList bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  (items, _) <- readItems bs off1 end
  Right (I.List (V.fromList items), end)

readItems :: ByteString -> Int -> Int -> Either String ([I.Value], Int)
readItems bs off end
  | off >= end = Right ([], off)
  | otherwise = do
      (v, off1) <- decodeValue bs off
      (rest, off2) <- readItems bs off1 end
      Right (v : rest, off2)

decodeStruct :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeStruct bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  (fields, _) <- readFields bs off1 end
  Right (I.Struct (V.fromList fields), end)

readFields :: ByteString -> Int -> Int -> Either String ([(T.Text, I.Value)], Int)
readFields bs off end
  | off >= end = Right ([], off)
  | otherwise = do
      (klen, off1) <- readVarUInt bs off
      ensure bs off1 klen
      let !kraw = BSU.unsafeTake klen (BSU.unsafeDrop off1 bs)
      case TE.decodeUtf8' kraw of
        Left _  -> Left "Ion.Decode: invalid UTF-8 in struct field name"
        Right k -> do
          (v, off2) <- decodeValue bs (off1 + klen)
          (rest, off3) <- readFields bs off2 end
          Right ((k, v) : rest, off3)

decodeAnnotation :: ByteString -> Int -> Word8 -> Either String (I.Value, Int)
decodeAnnotation bs off lenNibble = do
  (len, off1) <- readLength bs off lenNibble
  let !end = off1 + len
  (annsSz, off2) <- readVarUInt bs off1
  _ <- pure annsSz
  (annLen, off3) <- readVarUInt bs off2
  ensure bs off3 annLen
  let !annRaw = BSU.unsafeTake annLen (BSU.unsafeDrop off3 bs)
  case TE.decodeUtf8' annRaw of
    Left _   -> Left "Ion.Decode: invalid UTF-8 in annotation"
    Right ann -> do
      (inner, _off4) <- decodeValue bs (off3 + annLen)
      Right (I.Annotation ann inner, end)
