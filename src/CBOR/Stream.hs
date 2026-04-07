{-# LANGUAGE BangPatterns #-}
-- | Incremental\/streaming decode for CBOR values.
--
-- CBOR values are self-delimiting, so the streaming decoder reads
-- one complete value at a time. When the input is incomplete, it
-- returns 'Partial' requesting more bytes.
module CBOR.Stream
  ( DecodeStep(..)
  , streamDecode
  , feedMore
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified CBOR.Value as C

-- | Result of an incremental decode step.
data DecodeStep a
  = Done !a !ByteString
  | Partial (ByteString -> DecodeStep a)
  | Fail !String

instance Show a => Show (DecodeStep a) where
  show (Done a bs) = "Done " ++ show a ++ " (" ++ show (BS.length bs) ++ " leftover)"
  show (Partial _) = "Partial _"
  show (Fail e)    = "Fail " ++ show e

-- | Begin streaming decode of a single CBOR value.
streamDecode :: ByteString -> DecodeStep C.Value
streamDecode = tryDecode BS.empty

-- | Feed more bytes into a 'Partial' continuation.
feedMore :: DecodeStep a -> ByteString -> DecodeStep a
feedMore (Partial k) bs = k bs
feedMore step _         = step

tryDecode :: ByteString -> ByteString -> DecodeStep C.Value
tryDecode !accum !new =
  let !buf = if BS.null accum then new else accum <> new
  in if BS.null buf
     then Partial $ \more ->
            if BS.null more
            then Fail "CBOR.Stream: unexpected end of input"
            else tryDecode buf more
     else case decodeOneWithLeftover buf of
            Right (val, leftover) -> Done val leftover
            Left _ -> Partial $ \more ->
              if BS.null more
              then Fail "CBOR.Stream: incomplete CBOR value"
              else tryDecode buf more

decodeOneWithLeftover :: ByteString -> Either String (C.Value, ByteString)
decodeOneWithLeftover bs
  | BS.null bs = Left "empty input"
  | otherwise = case decodeOne bs 0 of
      Left e -> Left e
      Right (val, off) -> Right (val, BS.drop off bs)

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BS.index bs off
{-# INLINE rdByte #-}

ensureBytes :: ByteString -> Int -> Int -> Either String ()
ensureBytes !bs !off !n
  | off + n > BS.length bs = Left "unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensureBytes #-}

withBSPtrOff :: ByteString -> Int -> (Ptr Word8 -> IO a) -> a
withBSPtrOff (BSI.BS fp _) off f = unsafeDupablePerformIO $
  withForeignPtr fp $ \p -> f (castPtr p `plusPtr` off)
{-# INLINE withBSPtrOff #-}

readBE16BS :: ByteString -> Int -> Word64
readBE16BS bs off = withBSPtrOff bs off $ \p ->
  fromIntegral . byteSwap16 <$> (peekByteOff p 0 :: IO Word16)
{-# INLINE readBE16BS #-}

readBE32BS :: ByteString -> Int -> Word64
readBE32BS bs off = withBSPtrOff bs off $ \p ->
  fromIntegral . byteSwap32 <$> (peekByteOff p 0 :: IO Word32)
{-# INLINE readBE32BS #-}

readBE64BS :: ByteString -> Int -> Word64
readBE64BS bs off = withBSPtrOff bs off $ \p ->
  byteSwap64 <$> (peekByteOff p 0 :: IO Word64)
{-# INLINE readBE64BS #-}

readArg :: ByteString -> Int -> Word8 -> Either String (Word64, Int)
readArg !bs !off !info
  | info <= 23 = Right (fromIntegral info, off)
  | info == 24 = do
      ensureBytes bs off 1
      Right (fromIntegral (rdByte bs off), off + 1)
  | info == 25 = do
      ensureBytes bs off 2
      Right (readBE16BS bs off, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      Right (readBE32BS bs off, off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      Right (readBE64BS bs off, off + 8)
  | info == 31 = Left "indefinite length not supported in stream decoder"
  | otherwise  = Left $ "reserved additional info: " ++ show info

decodeOne :: ByteString -> Int -> Either String (C.Value, Int)
decodeOne !bs !off
  | off >= BS.length bs = Left "unexpected end of input"
  | otherwise =
    let !ib    = rdByte bs off
        !major = ib `shiftR` 5
        !info  = ib .&. 0x1f
    in case major of
      0 -> do (n, off') <- readArg bs (off + 1) info; Right (C.UInt n, off')
      1 -> do (n, off') <- readArg bs (off + 1) info; Right (C.NInt n, off')
      2 -> decodeBStr bs (off + 1) info
      3 -> decodeTStr bs (off + 1) info
      4 -> decodeArray bs (off + 1) info
      5 -> decodeMap bs (off + 1) info
      6 -> decodeTag bs (off + 1) info
      7 -> decodeSimpleOrFloat bs (off + 1) info
      _ -> Left $ "invalid major type: " ++ show major

decodeBStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeBStr bs off info
  | info == 31 = decodeIndefiniteBStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !chunk = BS.take len (BS.drop off' bs)
      Right (C.ByteString chunk, off' + len)

decodeTStr :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeTStr bs off info
  | info == 31 = decodeIndefiniteTStr bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      ensureBytes bs off' len
      let !raw = BS.take len (BS.drop off' bs)
      case TE.decodeUtf8' raw of
        Left _  -> Left "invalid UTF-8 in text string"
        Right t -> Right (C.TextString t, off' + len)

decodeIndefiniteBStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteBStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite byte string"
      | rdByte bs off == 0xff = Right (C.ByteString (BS.concat (reverse acc)), off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 2
            then Left "non-byte-string chunk in indefinite byte string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BS.take len (BS.drop off' bs)
              go (off' + len) (chunk : acc)

decodeIndefiniteTStr :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteTStr bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite text string"
      | rdByte bs off == 0xff = do
          let !combined = BS.concat (reverse acc)
          case TE.decodeUtf8' combined of
            Left _  -> Left "invalid UTF-8 in indefinite text string"
            Right t -> Right (C.TextString t, off + 1)
      | otherwise = do
          let !ib = rdByte bs off
              !m  = ib `shiftR` 5
              !ai = ib .&. 0x1f
          if m /= 3
            then Left "non-text-string chunk in indefinite text string"
            else do
              (len64, off') <- readArg bs (off + 1) ai
              let !len = fromIntegral len64
              ensureBytes bs off' len
              let !chunk = BS.take len (BS.drop off' bs)
              go (off' + len) (chunk : acc)

decodeArray :: ByteString -> Int -> Word8 -> Either String (C.Value, Int)
decodeArray bs off info
  | info == 31 = decodeIndefiniteArray bs off
  | otherwise = do
      (len64, off') <- readArg bs off info
      let !len = fromIntegral len64
      decodeNItems bs off' len []

decodeNItems :: ByteString -> Int -> Int -> [C.Value] -> Either String (C.Value, Int)
decodeNItems _bs !off 0 !acc = Right (C.Array (V.fromList (reverse acc)), off)
decodeNItems bs !off !n !acc = do
  (v, off') <- decodeOne bs off
  decodeNItems bs off' (n - 1) (v : acc)

decodeIndefiniteArray :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteArray bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite array"
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
decodeNPairs _bs !off 0 !acc = Right (C.Map (V.fromList (reverse acc)), off)
decodeNPairs bs !off !n !acc = do
  (k, off')  <- decodeOne bs off
  (v, off'') <- decodeOne bs off'
  decodeNPairs bs off'' (n - 1) ((k, v) : acc)

decodeIndefiniteMap :: ByteString -> Int -> Either String (C.Value, Int)
decodeIndefiniteMap bs off0 = go off0 []
  where
    go !off !acc
      | off >= BS.length bs = Left "unexpected end of input in indefinite map"
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
      let !halfBits = fromIntegral (readBE16BS bs off) :: Word16
          !f = halfToFloat halfBits
      Right (C.Float16 f, off + 2)
  | info == 26 = do
      ensureBytes bs off 4
      let !w = fromIntegral (readBE32BS bs off) :: Word32
      Right (C.Float32 (castWord32ToFloat w), off + 4)
  | info == 27 = do
      ensureBytes bs off 8
      let !w = readBE64BS bs off
      Right (C.Float64 (castWord64ToDouble w), off + 8)
  | info == 31 = Left "break at top level is not valid"
  | otherwise  = Left $ "reserved simple value info: " ++ show info

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
