{-# LANGUAGE BangPatterns #-}
-- | High-level Thrift decoding for Binary and Compact protocols.
--
-- Reads a wire-format 'ByteString' and produces a 'Thrift.Value.Value' tree.
module Thrift.Decode
  ( decodeBinary
  , decodeCompact
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Thrift.Value as TV
import Thrift.Wire

--------------------------------------------------------------------------------
-- Binary Protocol
--------------------------------------------------------------------------------

decodeBinary :: ByteString -> Either String TV.Value
decodeBinary bs = case decodeBinStruct bs 0 of
  Nothing       -> Left "decodeBinary: failed to decode struct"
  Just (v, _off) -> Right v

decodeBinStruct :: ByteString -> Int -> Maybe (TV.Value, Int)
decodeBinStruct bs off = go off []
  where
    go !o !acc = case tBinDecodeFieldBegin bs o of
      Nothing -> Nothing
      Just (TT_STOP, _, o') -> Just (TV.Struct (V.fromList (reverse acc)), o')
      Just (tt, fid, o') -> case decodeBinValue tt bs o' of
        Nothing -> Nothing
        Just (v, o'') -> go o'' ((fid, v) : acc)

decodeBinValue :: ThriftType -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeBinValue !tt !bs !off = case tt of
  TT_BOOL -> case tBinDecodeBool bs off of
    Just (b, o) -> Just (TV.Bool b, o)
    Nothing -> Nothing

  TT_BYTE -> case tBinDecodeI8 bs off of
    Just (v, o) -> Just (TV.Byte v, o)
    Nothing -> Nothing

  TT_I16 -> case tBinDecodeI16 bs off of
    Just (v, o) -> Just (TV.I16 v, o)
    Nothing -> Nothing

  TT_I32 -> case tBinDecodeI32 bs off of
    Just (v, o) -> Just (TV.I32 v, o)
    Nothing -> Nothing

  TT_I64 -> case tBinDecodeI64 bs off of
    Just (v, o) -> Just (TV.I64 v, o)
    Nothing -> Nothing

  TT_DOUBLE -> case tBinDecodeDouble bs off of
    Just (d, o) -> Just (TV.Double d, o)
    Nothing -> Nothing

  TT_STRING -> case tBinDecodeString bs off of
    Just (b, o) -> case TE.decodeUtf8' b of
      Right t -> Just (TV.String t, o)
      Left _  -> Just (TV.Binary b, o)
    Nothing -> Nothing

  TT_STRUCT -> decodeBinStruct bs off

  TT_MAP -> case tBinDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o) -> decodeBinMapEntries kt vt (fromIntegral sz) bs o []

  TT_LIST -> case tBinDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) -> decodeBinListEntries et (fromIntegral sz) bs o []

  TT_SET -> case tBinDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) -> decodeBinSetEntries et (fromIntegral sz) bs o []

  TT_UUID
    | off + 16 > BS.length bs -> Nothing
    | otherwise -> Just (TV.UUID (BS.take 16 (BS.drop off bs)), off + 16)

  TT_STOP -> Nothing

decodeBinMapEntries :: ThriftType -> ThriftType -> Int -> ByteString -> Int
                    -> [(TV.Value, TV.Value)] -> Maybe (TV.Value, Int)
decodeBinMapEntries kt vt !n bs !off !acc
  | n <= 0 = Just (TV.Map kt vt (V.fromList (reverse acc)), off)
  | otherwise = case decodeBinValue kt bs off of
      Nothing -> Nothing
      Just (k, o1) -> case decodeBinValue vt bs o1 of
        Nothing -> Nothing
        Just (v, o2) -> decodeBinMapEntries kt vt (n - 1) bs o2 ((k, v) : acc)

decodeBinListEntries :: ThriftType -> Int -> ByteString -> Int
                     -> [TV.Value] -> Maybe (TV.Value, Int)
decodeBinListEntries et !n bs !off !acc
  | n <= 0 = Just (TV.List et (V.fromList (reverse acc)), off)
  | otherwise = case decodeBinValue et bs off of
      Nothing -> Nothing
      Just (v, o) -> decodeBinListEntries et (n - 1) bs o (v : acc)

decodeBinSetEntries :: ThriftType -> Int -> ByteString -> Int
                    -> [TV.Value] -> Maybe (TV.Value, Int)
decodeBinSetEntries et !n bs !off !acc
  | n <= 0 = Just (TV.Set et (V.fromList (reverse acc)), off)
  | otherwise = case decodeBinValue et bs off of
      Nothing -> Nothing
      Just (v, o) -> decodeBinSetEntries et (n - 1) bs o (v : acc)

--------------------------------------------------------------------------------
-- Compact Protocol
--------------------------------------------------------------------------------

decodeCompact :: ByteString -> Either String TV.Value
decodeCompact bs = case decodeCompStruct bs 0 of
  Nothing        -> Left "decodeCompact: failed to decode struct"
  Just (v, _off) -> Right v

decodeCompStruct :: ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompStruct bs off = go off 0 []
  where
    go :: Int -> Int16 -> [(Int16, TV.Value)] -> Maybe (TV.Value, Int)
    go !o !lastFid !acc = case tCompDecodeFieldBegin bs o lastFid of
      Nothing -> Nothing
      Just (TT_STOP, _, o', _) -> Just (TV.Struct (V.fromList (reverse acc)), o')
      Just (tt, fid, o', boolVal) -> case tt of
        TT_BOOL -> go o' fid ((fid, TV.Bool boolVal) : acc)
        _ -> case decodeCompValue tt bs o' of
          Nothing -> Nothing
          Just (v, o'') -> go o'' fid ((fid, v) : acc)

decodeCompValue :: ThriftType -> ByteString -> Int -> Maybe (TV.Value, Int)
decodeCompValue !tt !bs !off = case tt of
  TT_BOOL -> case tCompDecodeBool bs off of
    Just (b, o) -> Just (TV.Bool b, o)
    Nothing -> Nothing

  TT_BYTE -> case tCompDecodeI8 bs off of
    Just (v, o) -> Just (TV.Byte v, o)
    Nothing -> Nothing

  TT_I16 -> case tCompDecodeI16 bs off of
    Just (v, o) -> Just (TV.I16 v, o)
    Nothing -> Nothing

  TT_I32 -> case tCompDecodeI32 bs off of
    Just (v, o) -> Just (TV.I32 v, o)
    Nothing -> Nothing

  TT_I64 -> case tCompDecodeI64 bs off of
    Just (v, o) -> Just (TV.I64 v, o)
    Nothing -> Nothing

  TT_DOUBLE -> case tCompDecodeDouble bs off of
    Just (d, o) -> Just (TV.Double d, o)
    Nothing -> Nothing

  TT_STRING -> case tCompDecodeString bs off of
    Just (b, o) -> case TE.decodeUtf8' b of
      Right t -> Just (TV.String t, o)
      Left _  -> Just (TV.Binary b, o)
    Nothing -> Nothing

  TT_STRUCT -> decodeCompStruct bs off

  TT_MAP -> case tCompDecodeMapBegin bs off of
    Nothing -> Nothing
    Just (kt, vt, sz, o)
      | sz == 0   -> Just (TV.Map kt vt V.empty, o)
      | otherwise -> decodeCompMapEntries kt vt (fromIntegral sz) bs o []

  TT_LIST -> case tCompDecodeListBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) -> decodeCompListEntries et (fromIntegral sz) bs o []

  TT_SET -> case tCompDecodeSetBegin bs off of
    Nothing -> Nothing
    Just (et, sz, o) -> decodeCompSetEntries et (fromIntegral sz) bs o []

  TT_UUID
    | off + 16 > BS.length bs -> Nothing
    | otherwise -> Just (TV.UUID (BS.take 16 (BS.drop off bs)), off + 16)

  TT_STOP -> Nothing

decodeCompMapEntries :: ThriftType -> ThriftType -> Int -> ByteString -> Int
                     -> [(TV.Value, TV.Value)] -> Maybe (TV.Value, Int)
decodeCompMapEntries kt vt !n bs !off !acc
  | n <= 0 = Just (TV.Map kt vt (V.fromList (reverse acc)), off)
  | otherwise = case decodeCompValue kt bs off of
      Nothing -> Nothing
      Just (k, o1) -> case decodeCompValue vt bs o1 of
        Nothing -> Nothing
        Just (v, o2) -> decodeCompMapEntries kt vt (n - 1) bs o2 ((k, v) : acc)

decodeCompListEntries :: ThriftType -> Int -> ByteString -> Int
                      -> [TV.Value] -> Maybe (TV.Value, Int)
decodeCompListEntries et !n bs !off !acc
  | n <= 0 = Just (TV.List et (V.fromList (reverse acc)), off)
  | otherwise = case decodeCompValue et bs off of
      Nothing -> Nothing
      Just (v, o) -> decodeCompListEntries et (n - 1) bs o (v : acc)

decodeCompSetEntries :: ThriftType -> Int -> ByteString -> Int
                     -> [TV.Value] -> Maybe (TV.Value, Int)
decodeCompSetEntries et !n bs !off !acc
  | n <= 0 = Just (TV.Set et (V.fromList (reverse acc)), off)
  | otherwise = case decodeCompValue et bs off of
      Nothing -> Nothing
      Just (v, o) -> decodeCompSetEntries et (n - 1) bs o (v : acc)
