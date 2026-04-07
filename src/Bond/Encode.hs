{-# LANGUAGE BangPatterns #-}
-- | Bond Compact Binary v1 encoder.
--
-- Implements the Microsoft Bond Compact Binary protocol v1 wire format.
-- Field headers use packed delta/type encoding when possible.
-- Signed integers use ZigZag + LEB128 varint encoding.
module Bond.Encode
  ( encode
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word64)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Bond.Value

btStop :: Word8
btStop = 0

btStopBase :: Word8
btStopBase = 1

encode :: Value -> ByteString
encode = BL.toStrict . BB.toLazyByteString . encodeValue

encodeValue :: Value -> BB.Builder
encodeValue (Bool b)     = BB.word8 (if b then 1 else 0)
encodeValue (Int8 n)     = encodeZigZagVarint (fromIntegral n :: Int64)
encodeValue (Int16 n)    = encodeZigZagVarint (fromIntegral n :: Int64)
encodeValue (Int32 n)    = encodeZigZagVarint (fromIntegral n :: Int64)
encodeValue (Int64 n)    = encodeZigZagVarint n
encodeValue (UInt8 n)    = encodeVarint (fromIntegral n :: Word64)
encodeValue (UInt16 n)   = encodeVarint (fromIntegral n :: Word64)
encodeValue (UInt32 n)   = encodeVarint (fromIntegral n :: Word64)
encodeValue (UInt64 n)   = encodeVarint n
encodeValue (Float f)    = BB.word32LE (castFloatToWord32 f)
encodeValue (Double d)   = BB.word64LE (castDoubleToWord64 d)
encodeValue (String t)   = let !bs = TE.encodeUtf8 t
                           in encodeVarint (fromIntegral (BL.length (BL.fromStrict bs)) :: Word64)
                              <> BB.byteString bs
encodeValue (WString t)  = let !bs = TE.encodeUtf8 t
                           in encodeVarint (fromIntegral (BL.length (BL.fromStrict bs)) :: Word64)
                              <> BB.byteString bs
encodeValue (Blob bs)    = BB.byteString bs
encodeValue (List et vs) = encodeContainerHeader et (V.length vs)
                           <> V.foldl' (\b v -> b <> encodeValue v) mempty vs
encodeValue (Set et vs)  = encodeContainerHeader et (V.length vs)
                           <> V.foldl' (\b v -> b <> encodeValue v) mempty vs
encodeValue (Map kt vt kvs) = BB.word8 (bondTypeId kt)
                               <> encodeContainerHeader vt (V.length kvs)
                               <> V.foldl' (\b (k, v) -> b <> encodeValue k <> encodeValue v) mempty kvs
encodeValue (Struct bases fields) = encodeStruct bases fields
encodeValue (Nullable Nothing)  = BB.word8 0
encodeValue (Nullable (Just v)) = BB.word8 1 <> encodeValue v
encodeValue (Enum n)     = encodeZigZagVarint (fromIntegral n :: Int64)

encodeStruct :: V.Vector Value -> V.Vector (Word16, BondType, Value) -> BB.Builder
encodeStruct bases fields =
  let basePart = V.foldl' (\b baseVal -> b <> encodeBase baseVal) mempty bases
      fieldPart = encodeFields 0 fields
  in basePart <> fieldPart <> BB.word8 btStop

encodeBase :: Value -> BB.Builder
encodeBase (Struct bases fields) =
  let basePart = V.foldl' (\b baseVal -> b <> encodeBase baseVal) mempty bases
      fieldPart = encodeFields 0 fields
  in basePart <> fieldPart <> BB.word8 btStopBase
encodeBase _ = BB.word8 btStopBase

encodeFields :: Word16 -> V.Vector (Word16, BondType, Value) -> BB.Builder
encodeFields !_ fields | V.null fields = mempty
encodeFields !prevId fields =
  let (fid, bt, val) = V.head fields
      rest = V.tail fields
      hdr = encodeFieldHeader prevId fid bt
  in hdr <> encodeValue val <> encodeFields fid rest

encodeFieldHeader :: Word16 -> Word16 -> BondType -> BB.Builder
encodeFieldHeader prevId fieldId bt =
  let !delta = fieldId - prevId
      !tid = bondTypeId bt
  in if delta >= 1 && delta <= 5
     then BB.word8 (fromIntegral (delta `shiftL` 5) .|. tid)
     else BB.word8 tid <> encodeVarint (fromIntegral fieldId :: Word64)

encodeContainerHeader :: BondType -> Int -> BB.Builder
encodeContainerHeader bt count =
  let !tid = bondTypeId bt
  in if count >= 1 && count <= 7
     then BB.word8 (fromIntegral (count `shiftL` 5) .|. tid)
     else BB.word8 tid <> encodeVarint (fromIntegral count :: Word64)

-- | ZigZag encode a signed integer then write as unsigned varint.
encodeZigZagVarint :: Int64 -> BB.Builder
encodeZigZagVarint !n = encodeVarint (zigZagEncode n)
{-# INLINE encodeZigZagVarint #-}

zigZagEncode :: Int64 -> Word64
zigZagEncode !n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZagEncode #-}

-- | LEB128 unsigned varint encoding.
encodeVarint :: Word64 -> BB.Builder
encodeVarint !n
  | n < 0x80  = BB.word8 (fromIntegral n)
  | otherwise = BB.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> encodeVarint (n `shiftR` 7)
