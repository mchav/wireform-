{-# LANGUAGE BangPatterns #-}
-- | High-level Thrift encoding for Binary and Compact protocols.
--
-- Converts a 'ThriftValue' tree into a wire-format 'ByteString'.
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

import Thrift.Value
import Thrift.Wire

--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

encodeBinary :: ThriftValue -> ByteString
encodeBinary = BL.toStrict . B.toLazyByteString . buildBinary

buildBinary :: ThriftValue -> B.Builder
buildBinary = \case
  TVBool b   -> tBinEncodeBool b
  TVByte v   -> tBinEncodeI8 v
  TVI16 v    -> tBinEncodeI16 v
  TVI32 v    -> tBinEncodeI32 v
  TVI64 v    -> tBinEncodeI64 v
  TVDouble d -> tBinEncodeDouble d
  TVString t -> tBinEncodeString (TE.encodeUtf8 t)
  TVBinary b -> tBinEncodeBinary b
  TVUUID b   -> B.byteString b

  TVStruct fields ->
    let sorted = sortBy (comparing fst) fields
    in mconcat [ tBinEncodeFieldBegin (thriftTypeOf v) fid <> buildBinary v
               | (fid, v) <- sorted
               ]
       <> tBinEncodeFieldStop

  TVMap kt vt entries ->
    tBinEncodeMapBegin kt vt (fromIntegral (length entries) :: Int32)
    <> mconcat [ buildBinary k <> buildBinary v | (k, v) <- entries ]

  TVList et elems ->
    tBinEncodeListBegin et (fromIntegral (length elems) :: Int32)
    <> mconcat (map buildBinary elems)

  TVSet et elems ->
    tBinEncodeSetBegin et (fromIntegral (length elems) :: Int32)
    <> mconcat (map buildBinary elems)

--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

encodeCompact :: ThriftValue -> ByteString
encodeCompact = BL.toStrict . B.toLazyByteString . buildCompact

buildCompact :: ThriftValue -> B.Builder
buildCompact val = case val of
  TVStruct fields -> buildCompactStruct fields
  _               -> buildCompactValue val

buildCompactStruct :: [(Int16, ThriftValue)] -> B.Builder
buildCompactStruct fields =
  let sorted = sortBy (comparing fst) fields
  in go 0 sorted <> tCompEncodeFieldStop
  where
    go :: Int16 -> [(Int16, ThriftValue)] -> B.Builder
    go _ [] = mempty
    go !lastFid ((fid, v) : rest) =
      let boolVal = case v of
                      TVBool b -> b
                      _        -> False
      in tCompEncodeFieldBegin (thriftTypeOf v) fid lastFid boolVal
         <> (case v of
               TVBool _ -> mempty
               _        -> buildCompactValue v)
         <> go fid rest

buildCompactValue :: ThriftValue -> B.Builder
buildCompactValue = \case
  TVBool b   -> tCompEncodeBool b
  TVByte v   -> tCompEncodeI8 v
  TVI16 v    -> tCompEncodeI16 v
  TVI32 v    -> tCompEncodeI32 v
  TVI64 v    -> tCompEncodeI64 v
  TVDouble d -> tCompEncodeDouble d
  TVString t -> tCompEncodeString (TE.encodeUtf8 t)
  TVBinary b -> tCompEncodeBinary b
  TVUUID b   -> B.byteString b

  TVStruct fields -> buildCompactStruct fields

  TVMap kt vt entries ->
    tCompEncodeMapBegin kt vt (fromIntegral (length entries) :: Int32)
    <> mconcat [ buildCompactValue k <> buildCompactValue v | (k, v) <- entries ]

  TVList et elems ->
    tCompEncodeListBegin et (fromIntegral (length elems) :: Int32)
    <> mconcat (map buildCompactValue elems)

  TVSet et elems ->
    tCompEncodeSetBegin et (fromIntegral (length elems) :: Int32)
    <> mconcat (map buildCompactValue elems)
