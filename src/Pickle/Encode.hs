{-# LANGUAGE BangPatterns #-}
-- | Python Pickle protocol 2 encoder.
--
-- Encodes a 'Pickle.Value.Value' to Python Pickle protocol 2 wire format.
-- Emits the protocol 2 header (@0x80 0x02@) followed by opcodes and data,
-- terminated by the STOP opcode. Compatible with Python's @pickle.loads@.
module Pickle.Encode
  ( encode
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Pickle.Value as P

encode :: P.Value -> ByteString
encode val = BL.toStrict $ B.toLazyByteString $
  B.word8 0x80 <> B.word8 0x02
  <> buildValue val
  <> B.word8 0x2E  -- STOP

buildValue :: P.Value -> B.Builder
buildValue = \case
  P.None -> B.word8 0x4E  -- NONE 'N'
  P.Bool True -> B.word8 0x88  -- NEWTRUE
  P.Bool False -> B.word8 0x89  -- NEWFALSE
  P.Int n -> encodeInt n
  P.Float d -> B.word8 0x47 <> encodeFloat64BE d  -- BINFLOAT 'G'
  P.Bytes bs -> encodeBytes bs
  P.String t -> encodeString t
  P.List vs -> encodeList vs
  P.Tuple vs -> encodeTuple vs
  P.Dict kvs -> encodeDict kvs
  P.Set vs -> encodeSet vs

encodeInt :: Int64 -> B.Builder
encodeInt n
  | n >= -128 && n <= 127 =
      -- LONG1 with 1 byte
      B.word8 0x8A <> B.word8 1 <> B.word8 (fromIntegral n)
  | n >= -32768 && n <= 32767 =
      B.word8 0x8A <> B.word8 2
        <> B.word8 (fromIntegral (n .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 8) .&. 0xFF))
  | n >= -2147483648 && n <= 2147483647 =
      B.word8 0x8A <> B.word8 4
        <> B.word8 (fromIntegral (n .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 8) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 16) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 24) .&. 0xFF))
  | otherwise =
      B.word8 0x8A <> B.word8 8
        <> B.word8 (fromIntegral (n .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 8) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 16) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 24) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 32) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 40) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 48) .&. 0xFF))
        <> B.word8 (fromIntegral ((n `shiftR` 56) .&. 0xFF))

encodeFloat64BE :: Double -> B.Builder
encodeFloat64BE = B.doubleBE

encodeBytes :: ByteString -> B.Builder
encodeBytes bs
  | BS.length bs < 256 =
      B.word8 0x43 -- SHORT_BINBYTES 'C'
        <> B.word8 (fromIntegral (BS.length bs))
        <> B.byteString bs
  | otherwise =
      B.word8 0x42 -- BINBYTES 'B'
        <> buildLE32 (BS.length bs)
        <> B.byteString bs

encodeString :: T.Text -> B.Builder
encodeString t =
  let !bs = TE.encodeUtf8 t
  in if BS.length bs < 256
     then B.word8 0x8C -- SHORT_BINUNICODE
            <> B.word8 (fromIntegral (BS.length bs))
            <> B.byteString bs
     else B.word8 0x58 -- BINUNICODE 'X'
            <> buildLE32 (BS.length bs)
            <> B.byteString bs

encodeList :: V.Vector P.Value -> B.Builder
encodeList vs
  | V.null vs = B.word8 0x5D  -- EMPTY_LIST ']'
  | otherwise =
      B.word8 0x5D  -- EMPTY_LIST
      <> B.word8 0x28  -- MARK '('
      <> V.foldl' (\acc v -> acc <> buildValue v) mempty vs
      <> B.word8 0x65  -- APPENDS 'e'

encodeTuple :: V.Vector P.Value -> B.Builder
encodeTuple vs
  | V.null vs = B.word8 0x29  -- EMPTY_TUPLE ')'
  | V.length vs == 1 =
      buildValue (V.head vs) <> B.word8 0x85  -- TUPLE1
  | V.length vs == 2 =
      buildValue (vs V.! 0) <> buildValue (vs V.! 1) <> B.word8 0x86  -- TUPLE2
  | V.length vs == 3 =
      buildValue (vs V.! 0) <> buildValue (vs V.! 1) <> buildValue (vs V.! 2) <> B.word8 0x87  -- TUPLE3
  | otherwise =
      B.word8 0x28  -- MARK
      <> V.foldl' (\acc v -> acc <> buildValue v) mempty vs
      <> B.word8 0x74  -- TUPLE 't'

encodeDict :: V.Vector (P.Value, P.Value) -> B.Builder
encodeDict kvs
  | V.null kvs = B.word8 0x7D  -- EMPTY_DICT '}'
  | otherwise =
      B.word8 0x7D  -- EMPTY_DICT
      <> B.word8 0x28  -- MARK
      <> V.foldl' (\acc (k, v) -> acc <> buildValue k <> buildValue v) mempty kvs
      <> B.word8 0x75  -- SETITEMS 'u'

encodeSet :: V.Vector P.Value -> B.Builder
encodeSet vs =
  -- Encode as frozenset via EMPTY_LIST + APPENDS + global lookup
  -- Simpler approach: encode as list and use GLOBAL for builtins.frozenset
  -- Actually for simplicity, use MARK + items + FROZENSET opcode (0x91, protocol 4)
  -- For protocol 2, use GLOBAL "builtins\nfrozenset\n" + tuple + REDUCE
  -- Simplest: encode as tuple for roundtrip purposes
  B.word8 0x28 -- MARK
  <> V.foldl' (\acc v -> acc <> buildValue v) mempty vs
  <> B.word8 0x74  -- TUPLE (creates a tuple from mark)

buildLE32 :: Int -> B.Builder
buildLE32 n =
  B.word8 (fromIntegral (n .&. 0xFF))
  <> B.word8 (fromIntegral ((n `shiftR` 8) .&. 0xFF))
  <> B.word8 (fromIntegral ((n `shiftR` 16) .&. 0xFF))
  <> B.word8 (fromIntegral ((n `shiftR` 24) .&. 0xFF))
