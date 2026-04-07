{-# LANGUAGE BangPatterns #-}
-- | High-level Thrift encoding for Binary and Compact protocols.
--
-- Converts a 'Thrift.Value.Value' tree into a wire-format 'ByteString'.
module Thrift.Encode
  ( encodeBinary
  , encodeCompact
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Thrift.Value as TV
import Thrift.Wire

--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

encodeBinary :: TV.Value -> ByteString
encodeBinary = BL.toStrict . B.toLazyByteString . buildBinary

buildBinary :: TV.Value -> B.Builder
buildBinary = \case
  TV.Bool b   -> tBinEncodeBool b
  TV.Byte v   -> tBinEncodeI8 v
  TV.I16 v    -> tBinEncodeI16 v
  TV.I32 v    -> tBinEncodeI32 v
  TV.I64 v    -> tBinEncodeI64 v
  TV.Double d -> tBinEncodeDouble d
  TV.String t -> tBinEncodeString (TE.encodeUtf8 t)
  TV.Binary b -> tBinEncodeBinary b
  TV.UUID b   -> B.byteString b

  TV.Struct fields ->
    let sorted = sortBy (comparing fst) (V.toList fields)
    in mconcat [ tBinEncodeFieldBegin (TV.thriftTypeOf v) fid <> buildBinary v
               | (fid, v) <- sorted
               ]
       <> tBinEncodeFieldStop

  TV.Map kt vt entries ->
    tBinEncodeMapBegin kt vt (fromIntegral (V.length entries) :: Int32)
    <> V.foldl' (\acc (k, v) -> acc <> buildBinary k <> buildBinary v) mempty entries

  TV.List et elems ->
    tBinEncodeListBegin et (fromIntegral (V.length elems) :: Int32)
    <> V.foldl' (\acc v -> acc <> buildBinary v) mempty elems

  TV.Set et elems ->
    tBinEncodeSetBegin et (fromIntegral (V.length elems) :: Int32)
    <> V.foldl' (\acc v -> acc <> buildBinary v) mempty elems

--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

encodeCompact :: TV.Value -> ByteString
encodeCompact = BL.toStrict . B.toLazyByteString . buildCompact

buildCompact :: TV.Value -> B.Builder
buildCompact val = case val of
  TV.Struct fields -> buildCompactStruct fields
  _                -> buildCompactValue val

buildCompactStruct :: V.Vector (Int16, TV.Value) -> B.Builder
buildCompactStruct fields =
  let sorted = sortBy (comparing fst) (V.toList fields)
  in go 0 sorted <> tCompEncodeFieldStop
  where
    go :: Int16 -> [(Int16, TV.Value)] -> B.Builder
    go _ [] = mempty
    go !lastFid ((fid, v) : rest) =
      let boolVal = case v of
                      TV.Bool b -> b
                      _         -> False
      in tCompEncodeFieldBegin (TV.thriftTypeOf v) fid lastFid boolVal
         <> (case v of
               TV.Bool _ -> mempty
               _         -> buildCompactValue v)
         <> go fid rest

buildCompactValue :: TV.Value -> B.Builder
buildCompactValue = \case
  TV.Bool b   -> tCompEncodeBool b
  TV.Byte v   -> tCompEncodeI8 v
  TV.I16 v    -> tCompEncodeI16 v
  TV.I32 v    -> tCompEncodeI32 v
  TV.I64 v    -> tCompEncodeI64 v
  TV.Double d -> tCompEncodeDouble d
  TV.String t -> tCompEncodeString (TE.encodeUtf8 t)
  TV.Binary b -> tCompEncodeBinary b
  TV.UUID b   -> B.byteString b

  TV.Struct fields -> buildCompactStruct fields

  TV.Map kt vt entries ->
    tCompEncodeMapBegin kt vt (fromIntegral (V.length entries) :: Int32)
    <> V.foldl' (\acc (k, v) -> acc <> buildCompactValue k <> buildCompactValue v) mempty entries

  TV.List et elems ->
    tCompEncodeListBegin et (fromIntegral (V.length elems) :: Int32)
    <> V.foldl' (\acc v -> acc <> buildCompactValue v) mempty elems

  TV.Set et elems ->
    tCompEncodeSetBegin et (fromIntegral (V.length elems) :: Int32)
    <> V.foldl' (\acc v -> acc <> buildCompactValue v) mempty elems
