{-# LANGUAGE BangPatterns #-}
-- | ASN.1 DER (Distinguished Encoding Rules) encoder.
module ASN1.Encode
  ( encode
  ) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word64)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import ASN1.Value

encode :: Value -> ByteString
encode = BL.toStrict . B.toLazyByteString . buildValue

buildValue :: Value -> B.Builder
buildValue = \case
  Boolean b -> buildTLV 0x01 (BS.singleton (if b then 0xFF else 0x00))
  Integer n -> buildTLV 0x02 (encodeInteger n)
  BitString unused dat ->
    buildTLV 0x03 (BS.cons (fromIntegral unused) dat)
  OctetString bs -> buildTLV 0x04 bs
  Null -> B.word8 0x05 <> B.word8 0x00
  OID components -> buildTLV 0x06 (encodeOID components)
  UTF8String t -> buildTLV 0x0C (TE.encodeUtf8 t)
  PrintableString t -> buildTLV 0x13 (TE.encodeUtf8 t)
  IA5String t -> buildTLV 0x16 (TE.encodeUtf8 t)
  UTCTime t -> buildTLV 0x17 (TE.encodeUtf8 t)
  GeneralizedTime t -> buildTLV 0x18 (TE.encodeUtf8 t)
  Sequence vs -> buildConstructed 0x30 vs
  Set vs -> buildConstructed 0x31 vs
  Tagged tc tagNum v ->
    let !inner = BL.toStrict (B.toLazyByteString (buildValue v))
        !tagByte = encodeTagByte tc True tagNum
    in buildTagBytes tagByte tagNum <> buildLength (BS.length inner) <> B.byteString inner
  Other tc constructed tagNum raw ->
    let !tagByte = encodeTagByte tc constructed tagNum
    in buildTagBytes tagByte tagNum <> buildLength (BS.length raw) <> B.byteString raw

buildTLV :: Word8 -> ByteString -> B.Builder
buildTLV tag content =
  B.word8 tag <> buildLength (BS.length content) <> B.byteString content

buildConstructed :: Word8 -> V.Vector Value -> B.Builder
buildConstructed tag vs =
  let !inner = BL.toStrict $ B.toLazyByteString $ V.foldl' (\acc v -> acc <> buildValue v) mempty vs
  in B.word8 tag <> buildLength (BS.length inner) <> B.byteString inner

buildLength :: Int -> B.Builder
buildLength !n
  | n < 128 = B.word8 (fromIntegral n)
  | otherwise =
      let !bs = encodeNonNegativeInteger (fromIntegral n)
          !numBytes = BS.length bs
      in B.word8 (0x80 .|. fromIntegral numBytes) <> B.byteString bs

encodeNonNegativeInteger :: Integer -> ByteString
encodeNonNegativeInteger 0 = BS.singleton 0
encodeNonNegativeInteger n = BS.pack (go n [])
  where
    go 0 acc = acc
    go v acc = go (v `shiftR` 8) (fromIntegral (v .&. 0xFF) : acc)

encodeInteger :: Integer -> ByteString
encodeInteger n = BS.pack (integerToTwosComplement n)

integerToTwosComplement :: Integer -> [Word8]
integerToTwosComplement 0 = [0]
integerToTwosComplement n
  | n > 0 =
      let bytes = toBytesBE n []
      in case bytes of
           (b:_) | testBit b 7 -> 0x00 : bytes
           _ -> bytes
  | otherwise =
      let !posVal = abs n
          !numBytes = byteWidth posVal
          !twosComp = (1 `shiftL` (8 * numBytes)) - posVal
          bytes = intToBE twosComp numBytes
      in trimFF bytes
  where
    toBytesBE 0 acc = acc
    toBytesBE v acc = toBytesBE (v `shiftR` 8) (fromIntegral (v .&. 0xFF) : acc)

    byteWidth v = go 1
      where go k = if v <= (1 `shiftL` (8 * k - 1)) then k else go (k + 1)

    intToBE val k = [fromIntegral ((val `shiftR` (8 * (k - 1 - i))) .&. 0xFF) | i <- [0 .. k - 1]]

    trimFF (0xFF : b : rest)
      | testBit b 7 = trimFF (b : rest)
    trimFF bs = bs

encodeOID :: V.Vector Word64 -> ByteString
encodeOID components
  | V.length components < 2 = BS.empty
  | otherwise =
      let !first = V.unsafeIndex components 0
          !second = V.unsafeIndex components 1
          !firstByte = fromIntegral (first * 40 + second) :: Word8
          !rest = V.toList (V.drop 2 components)
      in BS.singleton firstByte <> mconcat (map encodeOIDComponent rest)

encodeOIDComponent :: Word64 -> ByteString
encodeOIDComponent n
  | n < 128 = BS.singleton (fromIntegral n)
  | otherwise = BS.pack (go n [])
  where
    go 0 acc = acc
    go v [] = go (v `shiftR` 7) [fromIntegral (v .&. 0x7F)]
    go v acc = go (v `shiftR` 7) (fromIntegral ((v .&. 0x7F) .|. 0x80) : acc)

encodeTagByte :: TagClass -> Bool -> Int -> Word8
encodeTagByte tc constructed tagNum =
  let !classBits = case tc of
        Universal       -> 0x00
        Application     -> 0x40
        ContextSpecific -> 0x80
        Private         -> 0xC0
      !consBit = if constructed then 0x20 else 0x00
      !tagBits = if tagNum < 31 then fromIntegral tagNum else 0x1F
  in classBits .|. consBit .|. tagBits

buildTagBytes :: Word8 -> Int -> B.Builder
buildTagBytes tagByte tagNum
  | tagNum < 31 = B.word8 tagByte
  | otherwise = B.word8 tagByte <> B.byteString (encodeBase128 (fromIntegral tagNum))

encodeBase128 :: Word64 -> ByteString
encodeBase128 n
  | n < 128 = BS.singleton (fromIntegral n)
  | otherwise = BS.pack (go n [])
  where
    go 0 acc = acc
    go v [] = go (v `shiftR` 7) [fromIntegral (v .&. 0x7F)]
    go v acc = go (v `shiftR` 7) (fromIntegral ((v .&. 0x7F) .|. 0x80) : acc)
